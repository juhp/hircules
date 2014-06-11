--  GUI.hs: gtk UI for hircules IRC client
--
--  Author : Jens-Ulrik Petersen
--  Created: May 2003
--
--  Version: $Revision: 1.9 $ from $Date: 2008/11/03 03:14:11 $
--
--  Copyright (c) 2003, 2008-2010 Jens-Ulrik Holger Petersen
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.

module Hircules.GUI (addIRCchannel,
            alertchannel,
            allchannel,
            chanI,
            displayChannelTab,
            hideIRCchannel,
            newIRCchannel,
            rawchannel,
            renameChannelTab,
            setupGUI,
            timeStamp,
            updateChannelTab,
            writeTextLn,
            writeTextRaw,
            Interactive)
where

import Control.Concurrent
import Control.Monad (when, unless)
import Data.Char (isSpace, toLower)
import Data.Map (empty)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getZonedTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Locale

import Hircules.Channel
import Debug.State
import Hircules.EntryArea (setEditable)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Keymap
import Control.Monad.MaybeDo
import Hircules.Threads
import Text.WordString

-- tabchannel :: MVar (FiniteMap Int Channel)
-- tabchannel = unsafePerformIO $ newMVar empty

book :: Notebook
book = unsafePerformIO notebookNew

allchannel :: IRCChannel
allchannel = unsafePerformIO $ addIRCchannel "%all" "/" False

rawchannel :: IRCChannel
rawchannel = unsafePerformIO $ newIRCchannel "%raw" "/" False

alertchannel :: IRCChannel
alertchannel = unsafePerformIO $ newIRCchannel "%alert" "/" False

type MainWidget = Container

toMainWidget :: ContainerClass a => a -> MainWidget
toMainWidget = castToContainer

mainwidget :: MVar MainWidget
mainwidget = unsafePerformIO newEmptyMVar

mainwindow :: Window
mainwindow = unsafePerformIO windowNew

setupGUI :: IO ()
setupGUI = do
  _ <- initGUI
  let window = mainwindow
  windowSetDefaultSize window 640 512
  windowSetTitle window "Hircules IRC client"
  _ <- onDelete window (const $ shutDown >> return True)
  notebookSetScrollable book True
  notebookSetPopup book True
  _ <- onSwitchPage book updateTabN
  let initwidget = book
  putMVar mainwidget $ toMainWidget initwidget
  containerAdd window initwidget
  widgetShowAll window
  keymap <- newKeymap
  mapM_ (keymapAdd keymap) globalKeyBindings
  _ <- onKeyPress window $ keyPressCB keymap
  _ <- timeoutAdd (yield >> return True) 10
  return ()

globalKeyBindings :: [Keybinding]
globalKeyBindings =
    [ KB [Alt] "1" (switchToTab 0)
    , KB [Alt] "2" (switchToTab 1)
    , KB [Alt] "3" (switchToTab 2)
    , KB [Alt] "4" (switchToTab 3)
    , KB [Alt] "5" (switchToTab 4)
    , KB [Alt] "6" (switchToTab 5)
    , KB [Alt] "7" (switchToTab 6)
    , KB [Alt] "8" (switchToTab 7)
    , KB [Alt] "9" (switchToTab 8)
    , KB [Alt] "0" (switchToTab 9)
    , KB [Control,Shift] "W" hideCurrentChannel
    , KB [Control] "q" shutDown
    ]
  where
  switchToTab :: Int -> IO ()
  switchToTab n = do
      bookon <- widgetIsAncestor mainwindow book 
      unless bookon (switchTo book)
      notebookSetCurrentPage book n
  switchTo :: ContainerClass a => a -> IO ()
  switchTo widget = do
      displaying <- widgetIsAncestor mainwindow widget
      unless displaying $ do
        current <- takeMVar mainwidget
        containerRemove mainwindow current
        containerAdd mainwindow widget
        putMVar mainwidget $ toMainWidget widget

bufferKeyBindings :: IRCChannel -> [Keybinding]
bufferKeyBindings chan =
  [ KB [] "Return" (sendInput chan)
  , KB [Control] "r" (searchChannel chan True)
  , KB [Control] "s" (searchChannel chan False)
  ]

addIRCchannel :: String -> String -> Bool -> IO IRCChannel
addIRCchannel title nick real = do
  chan <- newIRCchannel title nick real
  displayChannelTab True chan
  widgetGrabFocus $ chanview chan
  return chan

displayChannelTab :: Bool -> IRCChannel -> IO ()
displayChannelTab switch chan = do
  let mainbox = chanbox chan
  pageno <- notebookPageNum book mainbox
  page <- case pageno of
             (Just p) -> return p
             Nothing -> do
                  n <- notebookGetNPages book
                  let lbltxt = show (n + 1) ++ " " ++ channame chan
                  label <- labelNew $ Just lbltxt
                  menulabel <- labelNew $ Just lbltxt
                  _ <- notebookAppendPageMenu book mainbox label menulabel
                  return n
  when switch $
      notebookSetCurrentPage book page

newIRCchannel :: String -> String -> Bool -> IO IRCChannel
newIRCchannel title nick real = do
--   mainbox <- vBoxNew False 5
  scrollwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollwin PolicyAutomatic PolicyAlways
--   boxPackStart mainbox scrollwin PackGrow 0
  buffer <- textBufferNew Nothing
  tTable <- textBufferGetTagTable buffer
  e <- textTagNew $ Just "editable"
  set e [textTagEditable := True]
  ne <- textTagNew $ Just "not-editable"
  set ne [textTagEditable := False]
  font <- textTagNew Nothing
  set font [textTagFamily := "Monospace"]
  textTagTableAdd tTable e
  textTagTableAdd tTable ne
  textTagTableAdd tTable font
  view <- textViewNewWithBuffer buffer
  textViewSetWrapMode view WrapWordChar
  textViewSetEditable view True
  containerAdd scrollwin view
  textBufferSetText buffer $ 
                    if real
                       then nick
                       else ""
--   nstart <- textBufferGetStartIter buffer
--   nickstart <- textBufferCreateMark buffer Nothing nstart True
  nend' <- textBufferGetEndIter buffer
  nickend' <- textBufferCreateMark buffer Nothing nend' True
--   textMarkSetVisible nickstart True
--   textMarkSetVisible nickend True
  textBufferInsert buffer nend' $
                    if real
                       then "@" ++ title ++ "> "
                       else title +-+ nick
  start' <- textBufferGetStartIter buffer
  textBufferInsert buffer start' "\n"
  start <- textBufferGetStartIter buffer
  end <- textBufferGetEndIter buffer
  textBufferApplyTagByName buffer "not-editable" start end
  textBufferApplyTag buffer font start end
  endmark <- textBufferCreateMark buffer Nothing start False
  entry <- textBufferGetEndIter buffer
  entrymark <- textBufferCreateMark buffer Nothing entry True
  nend <- textBufferGetIterAtMark buffer nickend'
  nickend <- textBufferCreateMark buffer Nothing nend False
  let result = IRCChan {chanbuffer = buffer, channame = map toLower title, chanreal = real, chanend = endmark, channick = nickend, chanbox = toMainWidget scrollwin, chanentry = entrymark, chanview = view, chanusers = empty, chantopic = "", chancoding = Nothing}
  keymap <- newKeymap
  mapM_ (keymapAdd keymap) $ bufferKeyBindings result
  _ <- onKeyPress view $ keyPressCB keymap
  _ <- after view pasteClipboard (setEditable result)
  widgetShowAll scrollwin
  return result

hideCurrentChannel :: IO ()
hideCurrentChannel =
  notebookGetCurrentPage book >>= doRemoveNthPage

hideIRCchannel :: IRCChannel -> IO ()
hideIRCchannel chan = do
  total <- notebookGetNPages book
  unless (total <= 1) $ do
    let mainbox = chanbox chan
    p <- notebookPageNum book mainbox
    maybeDo_ p doRemoveNthPage

doRemoveNthPage :: Int -> IO ()
doRemoveNthPage page = do
  total <- notebookGetNPages book
  unless (total <= 1) $ do
    notebookRemovePage book page
    updateTabLabels

updateTabLabels :: IO ()
updateTabLabels = return ()

writeTextLn :: IRCChannel -> Bool -> String -> IO ()
writeTextLn chan alert str = do
  let buffer = chanbuffer chan
      endmark = chanend chan
  end <- textBufferGetIterAtMark buffer endmark
  newmark <- textBufferCreateMark buffer Nothing end True
  time <- timeStamp
  no_text <- textIterIsStart end
  textBufferInsert buffer end $ (if no_text then "" else "\n") ++ time ++ " " ++ cleanup str
  start <- textBufferGetIterAtMark buffer newmark
  end' <- textBufferGetIterAtMark buffer endmark
  textBufferApplyTagByName buffer "not-editable" start end'
--   let view = chanview chan
--   viewfocus <- widgetIsFocus view
--   unless viewfocus $ do
--       textViewScrollMarkOnscreen view endmark
--       textBufferPlaceCursor buffer end
--       displayChannelTab False chan
  textBufferGetInsert buffer >>= textViewScrollMarkOnscreen (chanview chan)
--   when (cursorAtEnd && not autoscroll)
--       (textBufferGetIterAtMark buffer endmark >>= textBufferPlaceCursor buffer)
  when alert $ do
      displayChannelTab False chan
      updateChannelTab chan alert
  where
  cleanup :: String -> String
  -- remove trailing whitespace
  cleanup = reverse . dropWhile isSpace . reverse 

writeTextRaw :: String -> IO ()
writeTextRaw = writeTextLn rawchannel True

timeStamp :: IO String
timeStamp = do
  ct <- getZonedTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ct

type Interactive = ([String], Channel)

chanI :: Chan Interactive
chanI = unsafePerformIO newChan

sendInput :: IRCChannel -> IO ()
sendInput chan = do
  let entry = chanentry chan
      buffer = chanbuffer chan
  start <- textBufferGetIterAtMark buffer entry
  end <- textBufferGetEndIter buffer
  text <- textBufferGetText buffer start end False
  debug "sendInput" text
  textBufferDelete buffer start end
  writeChan chanI (lines text, channame chan)

updateChannelTab :: IRCChannel -> Bool -> IO ()
updateChannelTab chan alert = do
  page <- notebookPageNum book mainbox
  current <- notebookGetCurrentPage book
  case page of
      (Just p) -> do
          let text = show (p + 1) ++ " " ++ channame chan
              markup = if alert && current /= p
                          then highlightText text
                          else text
          label <- labelNew Nothing
          labelSetMarkup label markup
          notebookSetTabLabel book mainbox label
      Nothing -> return ()
  where
  mainbox = chanbox chan

highlightText :: String -> Markup
highlightText = markSpan [FontForeground "red"]

unhighlightText :: String -> String
unhighlightText str | whead str == open && wlast str == close =
                        take rawlength $ drop (length open) str
                    | otherwise = str
  where
  template = highlightText encl
  (open,close) = breakString encl template
  encl = "  "
  rawlength = length str - length open - length close

updateTabN :: Int -> IO ()
updateTabN n = do
     mw <- notebookGetNthPage book n
     case mw of
         Just w -> do
             mlbl <- notebookGetTabLabel book w
             case mlbl of
                   Just lbl -> do
                       let label = castToLabel lbl
                       txt <- labelGetText label
                       labelSetText label $ unhighlightText txt
                   Nothing -> return ()
         Nothing -> return ()

-- FIXME todo
renameChannelTab :: Channel -> Channel -> IO ()
renameChannelTab _old _new = return ()

lastSearchText :: MVar String
lastSearchText = unsafePerformIO $ newMVar ""

searchChannel :: IRCChannel -> Bool -> IO ()
searchChannel chan backwds = do
    let buffer = chanbuffer chan
    startIter <- textBufferGetInsert buffer >>= textBufferGetIterAtMark buffer
    slctn <- textBufferHasSelection buffer
    text <- if slctn
               then do
                    slctIter <- textBufferGetSelectionBound buffer >>= textBufferGetIterAtMark buffer
                    textBufferGetText buffer startIter slctIter False
               else readMVar lastSearchText
    unless (null text) $ do
      _ <- swapMVar lastSearchText text
      result <- (if backwds then textIterBackwardSearch else textIterForwardSearch) startIter text [] Nothing
      maybe (return ()) (\ (start, end) -> do
                            textBufferPlaceCursor buffer (if backwds then start else end)
                            textBufferMoveMarkByName buffer "selection_bound" (if backwds then end else start)
                            textBufferGetInsert buffer >>= textViewScrollMarkOnscreen (chanview chan))
        result
