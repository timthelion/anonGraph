module ToggleButtonList where
import Graphics.UI.Gtk as GTK

{-main :: IO ()
main = do
   GTK.initGUI       -- is start
   window <- GTK.windowNew
   toggleButtonList <- toggleButtonListNew (\item->putStrLn item) ["Hi","Bye","Bar","Foo","LadyDa"]
   GTK.containerAdd window toggleButtonList
   GTK.onDestroy window GTK.mainQuit
   GTK.widgetShowAll window
   GTK.mainGUI
   return ()-}

toggleButtonListNew :: (String->IO()) -> [String] -> IO GTK.HBox
toggleButtonListNew
 onToggle
 list
  = do
 hb <- GTK.hBoxNew False 0
 let
  addToggleButton label = do
   tb <- GTK.toggleButtonNewWithLabel label
   GTK.boxPackStart hb tb GTK.PackNatural 0
   return tb
 tbs <- mapM addToggleButton list
 let
  tbWithLabels = zip tbs list
  setupToggleButton
   (tb,label)
    =
   tb `on` GTK.toggled $ do
    active <- get tb GTK.toggleButtonActive
    if active
    then do
     onToggle label
     mapM_
      (\(tb',label') ->
       if label == label'
       then return ()
       else set tb' [GTK.toggleButtonActive := False])
      tbWithLabels
    else
     return ()
 mapM setupToggleButton tbWithLabels
 return hb
