{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import 
import Data.Aeson hiding (object)


{-
share[mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    text Text
    done Bool
|]

instance ToJSON (Entity Todo) where
        toJSON (Entity tid (Todo text done)) = object
            [ "id" .= id
            , "text" .= text
            , "done" .= done
            ]

instance FromJSON Todo where
        parseJSON (Object o) = Todo
            <$> o .= "text"
            <*> o .= "done"
        parseJSON _ = fail "invalid todo"
-}

getTodosR :: Handler RepJson
getTodosR =
        jsonToRepJson $ object [("status", "get" :: Text )]
        -- runDB (selectList [] []) >>= jsonToRepJson . asTodoEntities
    where
        asTodoEntities = id

postTodosR :: Handler RepJson
postTodosR = do
        todo <- parseJsonBody_
        tid <- runDB $ insert (todo :: Todo)
        jsonToRepJson $ object [("status", "post" :: Text)]
{-

getTodoR :: TodoId -> Handler RepJson
getTodoR tid = runDB (get404 tid) >>= jsonToRepJson . object [("status", "OK")]
-}

{-
deleteTodoR :: TodoId -> Handler ()
deleteTodoR tid = do
        runDB (delete tid)
        sendResponseCreated status204 ()
jsTodoText :: JSValue (Entity Todo) -> JSValue Text
jsTodoText = jsGetter "text"

jsTodoDone :: JSValue (Entity Todo) -> JSValue Bool
jsTodoDone = jsGetter "done"

jsTodo :: JSValue Text -> JSValue Bool -> JSValue Todo
jsTodo text done = jsonObject
    [ ("text", jsCast text)
    , ("done", jsCast done)
    ]
-}
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost (todoForm Nothing)
    defaultLayout $ do
        setTitle "Todo Registration"
        $(widgetFile "homepage")

todoForm :: Maybe Todo -> Form Todo
todoForm mtodo = renderDivs $ Todo
    <$> areq textField "todo text" (todoText <$> mtodo)
    <*> areq boolField "done?" (todoDone <$> mtodo)
