--  GUI.hs: gtk UI for hircules IRC client
--
--  Author : Jens-Ulrik Petersen
--  Created: May 2003
--
--  Version: $Revision: 1.4 $ from $Date: 2003/07/03 21:30:46 $
--
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
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

module GUI (addIRCchannel,
            alertchannel,
            allchannel,
            chanI,
            displayChannelTab,
            hideIRCchannel,
            newIRCchannel,
            rawchannel,
            setupGUI,
            timeStamp,
            writeTextLn,
            IRCChannel(..),
            Interactive)
where

import Control.Concurrent
import Control.Exception
import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.FiniteMap
import Data.Maybe (fromMaybe, fromJust, isJust)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import System.Time
import System.Locale

import Gtk
import Hierarchy (toContainer)
import GtkKeymap
import Threads

import Debug
-- import NotebookGetNPages

data IRCChannel = IRCChan { chanbuffer :: TextBuffer
                          , channame :: String
                          , chanreal :: Bool
                          , chanbox :: Container
                          , chanlabel :: Label
                          , chanentry :: Entry
                          , chanview :: TextView
                          , chanend :: TextMark
                          , chanusers :: [String]
                          , chantopic :: String }

-- tabchannel :: MVar (FiniteMap Int IRCChannel)
-- tabchannel = unsafePerformIO $ newMVar emptyFM

book :: Notebook
book = unsafePerformIO notebookNew

allchannel :: IRCChannel
allchannel = unsafePerformIO $ addIRCchannel "all" "/" False

rawchannel :: IRCChannel
rawchannel = unsafePerformIO $ addIRCchannel "raw" "/" False

alertchannel :: IRCChannel
alertchannel = unsafePerformIO $ addIRCchannel "alert" "/" False

type MainWidget = Container

toMainWidget :: ContainerClass a => a -> MainWidget
toMainWidget = toContainer

mainwidget :: MVar MainWidget
mainwidget = unsafePerformIO $ newEmptyMVar

mainwindow :: Window
mainwindow = unsafePerformIO windowNew

setupGUI :: IO ()
setupGUI = do
  initGUI
  let window = mainwindow
  windowSetDefaultSize window 600 500
  windowSetTitle window "Hircules IRC client"
  onDelete window (const $ shutDown >> return True)
  let initwidget = book
  putMVar mainwidget $ toMainWidget initwidget
  containerAdd window initwidget
  widgetShowAll window
  keymap <- newKeymap
  mapM_ (keymapAdd keymap) globalKeyBindings
  onKeyPress window (keyPressCB keymap)
  timeoutAdd (yield >> return True) 10
  return ()


globalKeyBindings :: [Keybinding]
globalKeyBindings =
    [ KB [ModCtrl] "1" (switchToTab 0)
    , KB [ModCtrl] "2" (switchToTab 1)
    , KB [ModCtrl] "3" (switchToTab 2)
    , KB [ModCtrl] "4" (switchToTab 3)
    , KB [ModCtrl] "5" (switchToTab 4)
    , KB [ModCtrl] "6" (switchToTab 5)
    , KB [ModCtrl] "7" (switchToTab 6)
    , KB [ModCtrl] "8" (switchToTab 7)
    , KB [ModCtrl] "9" (switchToTab 8)
    , KB [ModCtrl] "0" (switchToTab 9)
    , KB [ModCtrl,ModShift] "W" hideCurrentChannel
    , KB [ModCtrl] "q" shutDown
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

addIRCchannel :: String -> String -> Bool -> IO IRCChannel
addIRCchannel title nick real = do
  chan <- newIRCchannel title nick real
  displayChannelTab True chan
  widgetGrabFocus $ chanentry chan
  return chan

displayChannelTab :: Bool -> IRCChannel -> IO ()
displayChannelTab switch chan = do
  let mainbox = chanbox chan
  pageno <- notebookPageNum book mainbox
  page <- case pageno of
             (Just p) -> return p
             Nothing -> do
                  n <- notebookGetNPages book
                  notebookAppendPage book mainbox $ show (n + 1) ++ " " ++ (channame chan)
                  return n
  when switch $
      notebookSetCurrentPage book page

newIRCchannel :: String -> String -> Bool -> IO IRCChannel
newIRCchannel title nick real = do
  mainbox <- vBoxNew False 5
  scrollwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollwin PolicyAutomatic PolicyAutomatic 
  boxPackStart mainbox scrollwin PackGrow 0
  buffer <- textBufferNew Nothing
  view <- textViewNewWithBuffer buffer
  textViewSetWrapMode view WrapChar
  textViewSetEditable view False
  end <- textBufferGetEndIter buffer
  endmark <- textBufferCreateMark buffer Nothing end False
  containerAdd scrollwin view
  entrybox <- hBoxNew False 3
  boxPackStart mainbox entrybox PackNatural 0
  label <- labelNew $ Just nick
  boxPackStart entrybox label PackNatural 0
  entry <- entryNew
  boxPackStart entrybox entry PackGrow 0
  let result = IRCChan {chanbuffer = buffer, channame = (map toLower title), chanreal = real, chanlabel = label, chanbox = toMainWidget mainbox, chanentry = entry, chanview = view, chanend = endmark, chanusers = [], chantopic = ""}
  onEntryActivate entry $ sendLine result
  widgetShowAll mainbox
  return result

hideCurrentChannel :: IO ()
hideCurrentChannel = do
  notebookGetCurrentPage book >>= (doRemoveNthPage book)

hideIRCchannel :: IRCChannel -> IO ()
hideIRCchannel chan = do
  total <- notebookGetNPages book
  if total <= 1
     then return ()
     else do
          let mainbox = chanbox chan
          p <- notebookPageNum book mainbox
          maybe (return ()) (doRemoveNthPage book) p

doRemoveNthPage :: Notebook -> Int -> IO ()
doRemoveNthPage book page = do
  total <- notebookGetNPages book
  if total <= 1
     then return ()
     else do
          notebookRemovePage book page
          updateTabLabels

updateTabLabels :: IO ()
updateTabLabels = return ()

writeTextLn :: IRCChannel -> String -> IO ()
writeTextLn chan str = do
  let buffer = chanbuffer chan
      endmark = chanend chan
  end <- textBufferGetIterAtMark buffer endmark
  time <- timeStamp
  textBufferInsert buffer end $ time ++ " " ++ str ++ "\n"
  textViewScrollMarkOnscreen (chanview chan) endmark
  textBufferPlaceCursor buffer end
  displayChannelTab False chan

timeStamp :: IO String
timeStamp = do
  ct <- (getClockTime >>= toCalendarTime)
  return $ formatCalendarTime defaultTimeLocale (timeFmt defaultTimeLocale) ct

type Interactive = (String, IRCChannel)
-- signature is in IRC module
chanI = unsafePerformIO newChan

sendLine :: IRCChannel -> IO ()
sendLine chan = do
  let entry = chanentry chan
  line <- entryGetText entry
  entrySetText entry ""
  writeChan chanI (line, chan)

