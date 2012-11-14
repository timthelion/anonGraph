module ToggleButtonList where
import Graphics.UI.Gtk as GTK

{-main :: IO ()
main = do
   GTK.initGUI       -- is start
   window <- GTK.windowNew
   (toggleButtonList,updater) <- toggleButtonListNew (\item->putStrLn item) Vertical (\button->GTK.buttonSetRelief button GTK.ReliefNone) ["Hi","Bye","Bar","Foo","LadyDa"]
   GTK.containerAdd window toggleButtonList
   GTK.onDestroy window GTK.mainQuit
   updater ["Bob","Fred"]
   GTK.widgetShowAll window
   GTK.mainGUI
   return ()-}

type ToggleButtonListUpdater = [String]->IO ()

data Orientation = Horizontal | Vertical

{-toggleButtonListNew ::
 (String->IO()) ->
 ToggleButtonList.Orientation ->
 [String] ->
 IO (GTK.Box,ToggleButtonListUpdater)-}

toggleButtonListNew
 onToggle
 orientation
 drawHelper
 labels
  = do
 b <- case orientation of
  Horizontal -> do
   hb <- GTK.hBoxNew False 0
   return $ castToBox hb
  Vertical -> do
   vb <- GTK.vBoxNew False 0
   return $ castToBox vb
 let
  addToggleButton label = do
   tb <- GTK.toggleButtonNewWithLabel label
   GTK.boxPackStart b tb GTK.PackNatural 0
   return tb
  clearBox = do
   GTK.containerForall b GTK.widgetDestroy
  fillBox labels = do
   tbs<-mapM addToggleButton labels
   mapM drawHelper tbs
   GTK.widgetShowAll b
   return tbs
 tbs <- fillBox labels
 let
  tbWithLabels tbs labels = zip tbs labels
  setupToggleButton
   labels'
   tbs
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
      (tbWithLabels tbs labels')
    else
     return ()
  updateToggleButtons :: [String] -> IO()
  updateToggleButtons labels = do
    clearBox
    tbs <- fillBox labels
    mapM_ (setupToggleButton labels tbs) (tbWithLabels tbs labels)

 mapM (setupToggleButton labels tbs) (tbWithLabels tbs labels)
 return (b,updateToggleButtons)
