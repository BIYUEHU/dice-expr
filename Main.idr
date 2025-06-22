module Main

import Json
import Tauri
import Utils
import Dom
import Event

main : IO ()
main = do
  greetInputEl <- querySelector "#greet-input"
  greetMsgEl <- querySelector "#greet-msg"
  greetFormEl <- querySelector "#greet-form"

  listen "submit" greetFormEl $ \event => do
    preventDefault event
    text <- getValue greetInputEl
    invoke "greet" (JObject [("name", JString $ text ++ " (sent by Idris)" )]) $ \message => do
      setTextContent greetMsgEl message
      clog $ "Received greeting: " ++ message