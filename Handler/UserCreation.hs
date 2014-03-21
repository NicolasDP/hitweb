{-# LANGUAGE OverloadedStrings #-}
module Handler.UserCreation where

import Import

entryForm :: Key Identity -> Form User
entryForm identityId = renderDivs $ User
    <$> areq textField "Pseudo" Nothing
    <*> areq textField "Firstname" Nothing
    <*> areq textField "Lastname" Nothing
    <*> pure identityId

getUserCreationR :: Handler Html
getUserCreationR = do
    extra <- getExtra
    userIdentity <- maybeAuth
    createWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          (articleWidget, enctype) <- generateFormPost $ entryForm i
          defaultLayout $ do
              setTitle "Hit - User: Create"
              $(widgetFile "user-create")

postUserCreationR :: Handler Html
postUserCreationR = do
    userIdentity <- maybeAuth
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
           ((res,_),_) <- runFormPost $ entryForm i
           case res of
              FormSuccess user -> do
                  userId <- runDB $ insert user
                  runDB $ updateWhere [IdentityId ==. i] [IdentityStatus =. UserCreated, IdentityUserInfo =. (Just userId)]
                  setMessage $ toHtml $ "user created: " ++ (show $ userFirstname user) ++ " " ++ (show $ userLastname user)
                  redirect HomeR
              _ -> do
                  setMessage "error: ... incorrect... TODO"
                  redirect UserCreationR
