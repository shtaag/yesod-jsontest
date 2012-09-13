{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import 
-- import Data.Aeson hiding (object)


postTodosR :: Handler RepJson
postTodosR = do
        $(logInfo) "received json request"
        todo <- parseJsonBody_
        $(logInfo) "parse OK"
        tid <- runDB $ insert (todo :: Todo)
        jsonToRepJson $ object ["status" .= ("post" :: Text)]

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost (todoForm Nothing)
    defaultLayout $ do
        setTitle "Todo Registration"
        $(widgetFile "homepage")

todoForm :: Maybe Todo -> Form Todo
todoForm mtodo = renderDivs $ Todo
    <$> areq textField "todo text" (todoText <$> mtodo)
    <*> areq textField "done?" (todoDone <$> mtodo)
