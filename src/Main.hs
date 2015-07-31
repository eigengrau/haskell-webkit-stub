{-# LANGUAGE UnicodeSyntax #-}

import           Control.Monad
import qualified Data.List                       as List
import           Data.Maybe
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.WebKit.WebFrame
import           Graphics.UI.Gtk.WebKit.WebView
import           Prelude.Unicode


(≔) ∷ ReadWriteAttr o α β → β → AttrOp o
(≔) = (:=)


main ∷ IO ()
main = do

  initGUI

  window ← windowNew
  set window [
           windowTitle         ≔ "BagelBrowse",
           windowDefaultWidth  ≔ 300,
           windowDefaultHeight ≔ 200
          ]
  windowMaximize window

  notebook ← notebookNew
  notebookSetShowBorder notebook True
  containerAdd window notebook
  set notebook [
         notebookScrollable ≔ True,
         notebookTabPos     ≔ PosTop
        ]

  newTabButton ← buttonNew
  onClicked newTabButton (void $ browserViewNewTab notebook)
  buttonSetFocusOnClick newTabButton False
  buttonSetRelief       newTabButton ReliefNone
  newTabIcon ← imageNewFromStock stockAdd (IconSizeUser 1)
  buttonSetImage newTabButton newTabIcon

  startPageLabel ← labelNew (Just "Web Browser in Haskell")
  startPageFont  ← fontDescriptionFromString "Courier Bold 60"
  widgetModifyFont startPageLabel (Just startPageFont)

  notebookAppendPageMenu notebook startPageLabel newTabButton newTabButton
  widgetShowAll notebook

  widgetShowAll window
  onDestroy window mainQuit
  mainGUI


browserViewNewTab ∷ Notebook → IO WebView
browserViewNewTab notebook = do

  webView ← webViewNew

  ---------------------
  -- Navigation bar. --
  ---------------------

  navBar ← hBoxNew False 0

  backButton    ← mkButton stockGoBack    2
  forwardButton ← mkButton stockGoForward 2
  refreshButton ← mkButton stockRefresh   2
  addressBar    ← entryNew
  boxPackStart navBar backButton    PackNatural 0
  boxPackStart navBar forwardButton PackNatural 0
  boxPackStart navBar refreshButton PackNatural 0
  boxPackStart navBar addressBar    PackGrow    0

  onClicked backButton    $ webViewGoBack    webView
  onClicked forwardButton $ webViewGoForward webView
  onClicked refreshButton $ webViewReload    webView
  onEntryActivate addressBar $ do
    text ← entryGetText addressBar
    let url = checkVal text
    webViewLoadUri webView url

  -------------------
  -- Page content. --
  -------------------

  currentTab ← vBoxNew False 2
  scrolledWindow ← scrolledWindowNew Nothing Nothing
  boxPackStart currentTab navBar PackNatural 0
  boxPackStart currentTab scrolledWindow PackGrow 0
  set scrolledWindow [ containerChild ≔ webView ]
  widgetShowAll currentTab
  pageTitle ← labelNew (Just "New tab")

  -- E. g., open link in new window.
  webView `on` createWebView $ const (browserViewNewTab notebook)
  webView `on` loadFinished $ const $ do
      title ← webViewGetTitle webView
      let title' = fromMaybe "" title
      labelSetText pageTitle (take 8 title')
  webView `on` loadCommitted $ \frame → do
    uriVal ← webFrameGetUri frame ∷ IO (Maybe String)
    forM_ uriVal $ \uri → do
      entrySetText addressBar uri
      back    ← webViewCanGoBack webView
      forward ← webViewCanGoForward webView
      widgetSetSensitive backButton back
      widgetSetSensitive forwardButton forward

  --------------------------------------------------------------------
  -- Insert navigation bar & page content into notebook as new tab. --
  --------------------------------------------------------------------

  tabLabel ← do
      tabHeader ← hBoxNew False 0

      boxPackStart tabHeader pageTitle PackNatural 0

      closeButton ← mkButton stockClose 1
      onClicked closeButton $ do
          pageNum ← notebookPageNum notebook currentTab
          forM_ pageNum (notebookRemovePage notebook)
      boxPackStart tabHeader closeButton PackNatural 0

      widgetShowAll pageTitle
      widgetShowAll closeButton

      return tabHeader

  newPage ← do
    numPages ← notebookGetNPages notebook
    notebookInsertPageMenu notebook currentTab tabLabel tabLabel (numPages - 1)
  notebookSetCurrentPage notebook newPage

  return webView

      where
        mkButton stock size = do
          button ← buttonNew
          buttonSetFocusOnClick button False
          buttonSetRelief button ReliefNone
          icon ← imageNewFromStock stock (IconSizeUser size)
          buttonSetImage button icon
          return button


checkVal ∷ String → String
checkVal str = if any ($ str) [isHttp, isHttps, isFile]
                 then str
                 else schemeHttp ⧺ str
    where
      schemeHttp  = "http://"
      schemeHttps = "https://"
      schemeFile  = "file://"
      isHttp  = List.isPrefixOf schemeHttp
      isHttps = List.isPrefixOf schemeHttps
      isFile  = List.isPrefixOf schemeFile
