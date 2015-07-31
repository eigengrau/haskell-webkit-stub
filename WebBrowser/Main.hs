{-# LANGUAGE UnicodeSyntax #-}

import Graphics.UI.Gtk
import Data.IORef
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Prelude.Unicode
import Control.Monad.Unicode


(≔) ∷ ReadWriteAttr o α β → β → AttrOp o
(≔) = (:=)


main ∷ IO ()
main = do

  initGUI

  window ← windowNew
  set window [
           windowTitle         ≔ "Akash’s Web Browser",
           windowDefaultWidth  ≔ 300,
           windowDefaultHeight ≔ 200
          ]
  windowMaximize window
  widgetModifyBg window StateNormal colorBackground

  ntbk ← notebookNew
  containerAdd window ntbk
  set ntbk [
         notebookScrollable ≔ True,
         notebookTabPos     ≔ PosTop
        ]
  stls ← stockListIds

  tab    ← tableNew 1 10 True
  button ← buttonNew
  buttonSetFocusOnClick button False
  buttonSetRelief button ReliefNone
  im ← imageNewFromStock stockClose (IconSizeUser 3)
  buttonSetImage button im
  label  ← labelNew (Just "label")
  label1 ← labelNew (Just "label1")
  a      ← labelNew (Just "New Tab")
  tableAttachDefaults tab a 0 1 0 1
  tableAttachDefaults tab button 4 5 0 1
  img ← imageNewFromStock stockAdd (IconSizeUser 1)
  widgetShowAll tab

  newButton ← buttonNew
  buttonSetFocusOnClick newButton False
  buttonSetRelief newButton ReliefNone
  im1 ← imageNewFromStock stockAdd (IconSizeUser 1)
  buttonSetImage newButton im1

  lb1 ← labelNew (Just "Web Browser")
  lb2 ← labelNew (Just "In")
  lb3 ← labelNew (Just "Haskell")
  srcfont ← fontDescriptionFromString "Courier Bold 60"
  widgetModifyFont lb1 (Just srcfont)
  widgetModifyFont lb2 (Just srcfont)
  widgetModifyFont lb3 (Just srcfont)

  tableForMessage ← tableNew 3 1 True
  tableAttachDefaults tableForMessage lb1 0 1 0 1
  tableAttachDefaults tableForMessage lb2 0 1 1 2
  tableAttachDefaults tableForMessage lb3 0 1 2 3

  pagenum2 ← notebookAppendPageMenu ntbk tableForMessage newButton label1
  widgetModifyBg   ntbk StateNormal (Color 6851  48114 48114)
  widgetModifyFg   ntbk StateNormal (Color 48114 48114 6851)
  widgetModifyBase ntbk StateNormal (Color 48114 48114 6851)
  notebookSetShowBorder ntbk True
  widgetShowAll ntbk

  cnt ← newIORef 1

  onClicked newButton $ do
         wv ← repeating ntbk cnt
         print "opened new tab"

  onSwitchPage ntbk (putStrLn ∘ (⧺ "Page: ") ∘ show)
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI
  where

    colorBackground, limeGreen ∷ Color
    colorBackground = limeGreen
    limeGreen       = Color 6851 48114 6851

    but = do
         button ← buttonNew
         buttonSetFocusOnClick button False
         buttonSetRelief button ReliefNone
         im ← imageNewFromStock stockClose (IconSizeUser 3)
         buttonSetImage button im
         return button


--repeating ∷ Notebook → (IORef Int) → IO ()
repeating ntbk cnt = do

  --putStrLn "new button clicked"

  widgetShowAll ntbk
  tab2 ← tableNew 1 5 False
  button2 ← buttonNew
  buttonSetFocusOnClick button2 False
  buttonSetRelief button2 ReliefNone
  im2 ← imageNewFromStock stockClose (IconSizeUser 1)
  buttonSetImage button2 im2
  label2   ← labelNew (Just "label2")
  pageName ← labelNew (Just "New Tab")
  tableAttachDefaults tab2 pageName 0 1 0 1
  tableAttachDefaults tab2 button2 4 5 0 1

  pageIcon ← imageNew

  v ← vBoxNew False 2
  tool  ← toolbarNew
  table ← tableNew 1 20 True
  h ← hBoxNew False 0

  go_back ← buttonNew
  img1 ← imageNewFromStock stockGoBack (IconSizeUser 2)
  buttonSetImage go_back img1
  buttonSetFocusOnClick go_back False
  buttonSetRelief go_back ReliefNone

  go_forward ← buttonNew
  img2 ← imageNewFromStock stockGoForward (IconSizeUser 2)
  buttonSetImage go_forward img2
  buttonSetFocusOnClick go_forward False
  buttonSetRelief go_forward ReliefNone

  refresh ← buttonNew
  img3 ← imageNewFromStock stockRefresh (IconSizeUser 2)
  buttonSetImage refresh img3
  buttonSetFocusOnClick refresh False
  buttonSetRelief refresh ReliefNone

  address_bar ← entryNew
  sw ← scrolledWindowNew Nothing Nothing
  wv ← webViewNew

  entrySetWidthChars address_bar 125

  boxPackStartDefaults h go_back
  boxPackStart h go_forward  PackNatural 0
  boxPackStart h refresh     PackNatural 0
  boxPackStart h address_bar PackNatural 0
  widgetShowAll address_bar
  widgetShowAll h
  toolbarAppendNewWidget tool h (Nothing ∷ Maybe (String,String))
  boxPackStart v tool PackNatural 0
  boxPackStart v sw   PackGrow 0
  set sw [ containerChild ≔ wv ]

  onClicked go_back    $ webViewGoBack wv
  onClicked go_forward $ webViewGoForward wv
  onClicked refresh    $ webViewReload wv
  onEntryActivate address_bar $ do
    text ← entryGetText address_bar
    let url = checkVal text
    webViewLoadUri wv url

  wv `on` createWebView $ browserViewNewTab ntbk cnt

  wv `on` loadFinished $ \frame →
      (webFrameGetUri frame ∷ IO (Maybe String)) ≫= \_uri →
      do
        ch ← webViewGetTitle wv
        let Just a = if ch ≡ Nothing
                       then Just ""
                       else ch
        labelSetText pageName (take 8 a)
        --print a

  wv `on` loadCommitted $ \frame →
    -- Intercept new uri.
    webFrameGetUri frame ≫= \uri → do
      let Just aa = uri
      print uri
      if uri ≡ Nothing
        then putStrLn "nothing"
        else if take 7 aa ≡ "http://"
               then entrySetText address_bar (drop 7 aa)
               else if take 7 aa ≡ "file://"
                      then print "file"
                      else entrySetText address_bar aa
      back ← webViewCanGoBack wv
      forward ← webViewCanGoForward wv
      widgetSetSensitive go_back back
      widgetSetSensitive go_forward forward

  widgetShowAll v
  cn   ← readIORef cnt
  disp ← labelNew (Just $ show cn)
  widgetShowAll tab2
  n ← notebookGetNPages ntbk
  pagenum3 ← notebookInsertPageMenu ntbk v tab2 label2 (n-1)
  widgetShowAll ntbk
  notebookSetCurrentPage ntbk pagenum3

  number ← notebookGetNPages ntbk
  print number
  widgetShowAll ntbk

  ab ← onClicked button2 $ do
                       putStrLn "clicked"
                       Just pg ← notebookPageNum ntbk v
                       print pg
                       notebookRemovePage ntbk pg
  putStrLn "new start"
  return wv


browserViewNewTab ntbk cnt _ = repeating ntbk cnt


checkVal str = if isHttp ∨ isHttps ∨ isFile
                 then str
                 else "http://" ⧺ str
    where
      isHttp  = take 7 str ≡ "http://"
      isHttps = take 8 str ≡ "https://"
      isFile  = take 7 str ≡ "file://"
