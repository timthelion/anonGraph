module JSInput where
import Text.JSON
import Data.Ratio
import Graphics.UI.Gtk as GTK
import Data.IORef
import Control.Monad.IO.Class

{-main :: IO ()
main = do
   GTK.initGUI       -- is start
   window <- GTK.windowNew
   let
    feilds =
     [("string","String",JSString $ toJSString "")
     ,("bool","Bool",JSBool False)
     ,("rational","Rational",JSRational False (0%1))]
   jsInput <- jsInputNew feilds
    (\newValuesR->
     case newValuesR of
      Ok values -> putStrLn $ show values
      Error err -> putStrLn err)
   GTK.containerAdd window jsInput
   GTK.onDestroy window GTK.mainQuit
   GTK.widgetShowAll window
   GTK.mainGUI
   return () -}

jsInputNew ::
 [(String,String,JSValue)] ->
 (Result [(String,JSValue)] -> IO())->
 IO Widget
jsInputNew
 feilds
 onUpdate
  = do
 vb <- GTK.vBoxNew False 0
 let
  (JSObject initialObject) =
   makeObj
    $ map
       (\(k,_,v)->(k,v))
       feilds
 valuesIORef <- newIORef $ map (\(k,_,v)->(k,v)) feilds
 let
  addFeild (key,label,value) = do
   element <- case value of
    JSBool checked -> do
     b <- GTK.checkButtonNewWithLabel label
     set b [toggleButtonActive := checked
           ,toggleButtonMode   := True]
     b `on` GTK.toggled $ do
      values <- readIORef valuesIORef
      value <- get b toggleButtonActive
      let
       newValues =
        map
         (\(k,v)->
          case k == key of
           True -> (k,JSBool value)
           False -> (k,v))
         values
      writeIORef valuesIORef newValues
      onUpdate $ Ok newValues
     return $ castToWidget b
    r@JSRational{} -> do
     hb <- hBoxNew False 0
     l <- labelNew $ Just label
     GTK.boxPackStart hb l GTK.PackNatural 0
     e <- GTK.entryNew
     entrySetText e $ encode r
     e `on` GTK.focusOutEvent $ liftIO $ do
      values <- readIORef valuesIORef
      text <- get e entryText
      let
       valueR' = decode text
       valueR  =
        case valueR' of
         Ok (rational@JSRational{}) -> Ok rational
         Ok _ -> Error "Not a rational."
         Error err -> Error err
       newValuesR =
        case valueR of
         Ok val ->
          Ok $ map
           (\(k,v)->
            case k == key of
             True -> (k, val)
             False -> (k,v))
           values
         Error err -> Error err
      case newValuesR of
       Ok newValues ->  writeIORef valuesIORef newValues
       _ -> return ()
      onUpdate $ newValuesR
      return False
     GTK.boxPackStart hb e GTK.PackNatural 0
     return $ castToWidget hb
    JSString jsstring -> do
     hb <- hBoxNew False 0
     l <- labelNew $ Just label
     GTK.boxPackStart hb l GTK.PackNatural 0
     e <- GTK.entryNew
     e `on` GTK.focusOutEvent $ liftIO $ do
      values <- readIORef valuesIORef
      text <- get e entryText
      let
       newValuesR =
          Ok $ map
           (\(k,v)->
            case k == key of
             True ->  (k, JSString $ toJSString text)
             False -> (k,v))
           values
      case newValuesR of
       Ok newValues ->  writeIORef valuesIORef newValues
       _ -> return ()
      onUpdate $ newValuesR
      return False
     entrySetText e $ fromJSString jsstring
     GTK.boxPackStart hb e GTK.PackNatural 0
     return $ castToWidget hb
    _ -> return $ error "Unsupported value type.  We only support Bool Rational and String, sorry!"
   GTK.boxPackStart vb element GTK.PackNatural 0
 mapM_ addFeild feilds
 return $ castToWidget vb
