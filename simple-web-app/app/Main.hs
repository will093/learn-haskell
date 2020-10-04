{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty as Scotty
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R
import Network.Wai.Middleware.Static (staticPolicy, noDots)

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy noDots
    get "/" $ Scotty.html $ renderHtml page

page :: Html
page = H.html $ do
    H.head $ do
        H.title "Simple Web App"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "app/assets/css/app.css"
    H.body $ do
        H.h1 "Simple Web App"
        H.p "This is a simple web app"
        H.p "etc..."
