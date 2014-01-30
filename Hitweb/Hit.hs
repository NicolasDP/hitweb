module Hitweb.Hit
    ( HitwebDiff(..)
    , HitwebFileRef(..)
    , HitwebFileMode(..)
    , HitwebFileContent(..)
    , TextLine(..)
    , hitwebDiff
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
