module Hitweb.Hit
    ( HitwebDiff(..)
    , HitwebFileRef(..)
    , HitwebFileMode(..)
    , HitwebFileContent(..)
    , TextLine(..)
    , hitwebDiff
    , FilteredDiff(..)
    , hitwebDiffGetContext
    ) where

import Prelude

import Data.Git
import Data.Git.Diff

import Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 as BS

import Data.Algorithm.Patience

data TextLine = TextLine
    { lineNumber  :: Integer
    , lineContent :: L.ByteString
    }
instance Eq TextLine where
  a == b = (lineContent a) == (lineContent b)
  a /= b = not (a == b)

instance Ord TextLine where
  compare a b = compare (lineContent a) (lineContent b)
  a <  b     = (lineContent a) < (lineContent b)
  a <= b     = (lineContent a) <= (lineContent b)
  a >  b     = b < a
  a >= b     = b <= a

data HitwebFileContent = NewBinaryFile
                       | OldBinaryFile
                       | NewTextFile  [TextLine]
                       | OldTextFile  [TextLine]
                       | ModifiedBinaryFile
                       | ModifiedFile [Item TextLine]
                       | UnModifiedFile

data FilteredDiff = NormalLine (Item TextLine) | Separator
data HitwebAccu = AccuBottom | AccuTop

startWithSeparator :: [FilteredDiff] -> Bool
startWithSeparator [] = False
startWithSeparator (Separator:_) = True
startWithSeparator ((NormalLine l):xs) =
    case l of
        (Both _ _) -> startWithSeparator xs
        _          -> False

removeTrailingBoth :: [FilteredDiff] -> [FilteredDiff]
removeTrailingBoth list =
    let test = startWithSeparator list
    in  if test then Prelude.tail $ Prelude.dropWhile (\a -> not $ startWithSeparator [a]) list
                else list

hitwebDiffGetContext :: Int -> [Item TextLine] -> [FilteredDiff]
hitwebDiffGetContext 0 list = fmap NormalLine list
hitwebDiffGetContext context list =
    let (_, _, filteredDiff) = Prelude.foldr filterContext (0, AccuBottom, []) list
        theList = removeTrailingBoth filteredDiff
    in case Prelude.head theList of
        (NormalLine (Both l1 _)) -> if (lineNumber l1) > 1 then Separator:theList
                                                           else theList
        _ -> theList
    where filterContext :: (Item TextLine) -> (Int, HitwebAccu, [FilteredDiff]) -> (Int, HitwebAccu, [FilteredDiff])
          filterContext (Both l1 l2) (c, AccuBottom, acc) =
              if c < context then (c+1, AccuBottom, (NormalLine (Both l1 l2)):acc)
                             else (c  , AccuBottom, (NormalLine (Both l1 l2))
                                                    :((Prelude.take (context-1) acc)
                                                    ++ [Separator]
                                                    ++ (Prelude.drop (context+1) acc)))
          filterContext (Both l1 l2) (c, AccuTop, acc) =
              if c < context then (c+1, AccuTop   , (NormalLine (Both l1 l2)):acc)
                             else (0  , AccuBottom, (NormalLine (Both l1 l2)):acc)
          filterContext element (_, _, acc) =
              (0, AccuTop, (NormalLine element):acc)

data HitwebFileMode = NewMode        Int
                    | OldMode        Int
                    | ModifiedMode   Int Int
                    | UnModifiedMode Int

data HitwebFileRef = NewRef        Ref
                   | OldRef        Ref
                   | ModifiedRef   Ref Ref
                   | UnModifiedRef Ref

data HitwebDiff = HitwebDiff
    { hFileName    :: BS.ByteString
    , hFileContent :: HitwebFileContent
    , hFileMode    :: HitwebFileMode
    , hFileRef     :: HitwebFileRef
    }

hitwebDiff :: BlobStateDiff -> [HitwebDiff] -> [HitwebDiff]
hitwebDiff (OnlyOld   old    ) acc =
    let oldMode    = OldMode (bsMode old)
        oldRef     = OldRef  (bsRef  old)
        oldContent = case bsContent old of
                         BinaryContent _ -> OldBinaryFile
                         FileContent   l -> OldTextFile (Prelude.zipWith addLines [1..] l)
    in (HitwebDiff (bsFilename old) oldContent oldMode oldRef):acc
hitwebDiff (OnlyNew       new) acc =
    let newMode    = NewMode (bsMode new)
        newRef     = NewRef  (bsRef  new)
        newContent = case bsContent new of
                         BinaryContent _ -> NewBinaryFile
                         FileContent   l -> NewTextFile (Prelude.zipWith addLines [1..] l)
    in (HitwebDiff (bsFilename new) newContent newMode newRef):acc
hitwebDiff (OldAndNew old new) acc =
    let mode = if (bsMode old) /= (bsMode new) then ModifiedMode (bsMode old) (bsMode new)
                                               else UnModifiedMode (bsMode new)
        ref = if (bsRef old) == (bsRef new) then UnModifiedRef (bsRef new)
                                            else ModifiedRef (bsRef old) (bsRef new)
    in case (mode, ref) of
           ((UnModifiedMode _), (UnModifiedRef _)) -> acc
           _ -> (HitwebDiff (bsFilename new) (content ref) mode ref):acc
    where content :: HitwebFileRef -> HitwebFileContent
          content (UnModifiedRef _) = UnModifiedFile
          content _                 = createDiff (bsContent old) (bsContent new)

          createDiff :: BlobContent -> BlobContent -> HitwebFileContent
          createDiff (FileContent a) (FileContent b) =
              let linesA = Prelude.zipWith addLines [1..] a
                  linesB = Prelude.zipWith addLines [1..] b
              in ModifiedFile (diff linesA linesB)
          createDiff _ _ = ModifiedBinaryFile

addLines :: Integer -> L.ByteString -> TextLine
addLines a b = TextLine a b
