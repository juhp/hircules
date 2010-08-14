--  GUI.hs: gtk UI for hircules IRC client
--
--  Author : Jens-Ulrik Petersen
--  Created: May 2003
--
--  Version: $Revision: 1.4 $ from $Date: 2003/05/15 20:43:43 $
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
	    allchannel,
	    chanI,
	    setupGUI,
	    writeTextLn,
	    IRCChannel(..),
	    Interactive)
where

import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Data.FiniteMap
import Data.Maybe (fromMaybe, fromJust, isJust)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import System.Time
import System.Locale

import Gtk
import GtkKeymap
import Threads

data IRCChannel = IRCChan { chanbuffer :: TextBuffer
			  , channame :: String
			  , chanreal :: Bool
			  , chanlabel :: Label
			  , chanentry :: Entry
			  , chanview :: TextView
			  , chanend :: TextMark}

tabchannel :: MVar (FiniteMap Int IRCChannel)
tabchannel = unsafePerformIO $ newMVar emptyFM

book :: Notebook
book = unsafePerformIO $ notebookNew

allchannel :: IRCChannel
allchannel = unsafePerformIO $ addIRCchannel "all" "" False

setupGUI :: IO ()
setupGUI = do
  initGUI
  window <- windowNew
  windowSetDefaultSize window 600 500
  windowSetTitle window "Hircules IRC client"
  onDelete window (const $ shutDown >> return True)
--   pane <- vPanedNew
--   containerAdd window pane
  afterSwitchPage book switchPageHandler
--   scrollwin <- scrolledWindowNew Nothing Nothing
--   scrolledWindowSetPolicy scrollwin PolicyAutomatic PolicyAutomatic 
--   view <- textViewNewWithBuffer (chanbuffer allchannel)
--   textViewSetWrapMode view WrapChar
--   textViewSetEditable view False
--   containerAdd scrollwin view
--   panedPack1 pane scrollwin True True
--   panedAdd2 pane book
  containerAdd window book
  widgetShowAll window
  keymap <- newKeymap
  mapM_ (keymapAdd keymap) globalKeyBindings
  onKeyPress window (keyPressCB keymap)
  timeoutAdd (yield >> return True) 10
  return ()
  where
  switchPageHandler :: Int -> IO ()
  switchPageHandler n = withCurrentChan (\ c -> widgetGrabFocus $ chanentry c)

withCurrentChan :: (IRCChannel -> IO ()) -> IO ()
withCurrentChan act = do
  page <- notebookGetCurrentPage book
  fm <- readMVar tabchannel
  let mchan = lookupFM fm page
  maybe (return ()) act mchan


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
    , KB [ModCtrl] "q" shutDown
    ]
  where
  switchToTab :: Int -> IO ()
  switchToTab n = withCurrentChan (\ _ -> notebookSetCurrentPage book n)
      

addIRCchannel :: String -> String -> Bool -> IO IRCChannel
addIRCchannel title nick real = do
  mainbox <- vBoxNew False 5
  notebookAppendPage book mainbox title
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
  let result = IRCChan {chanbuffer = buffer, channame = title, chanreal = real, chanlabel = label, chanentry = entry, chanview = view, chanend = endmark}
  onEntryActivate entry $ sendLine result real
  widgetShowAll mainbox
  pageno <- notebookPageNum book mainbox
  case pageno
       of
       (Just p) -> do
		   notebookSetCurrentPage book p
		   modifyMVar_ tabchannel (\ fm -> return $ addToFM fm p result)
       Nothing -> return ()
  widgetGrabFocus entry
  return result

-- removeIRCchannel :: IRCChannel -> IO ()
-- removeIRCchannel chan

writeTextLn :: IRCChannel -> String -> IO ()
writeTextLn chan str = do
  let buffer = chanbuffer chan
      endmark = chanend chan
  end <- textBufferGetIterAtMark buffer endmark
  ct <- (getClockTime >>= toCalendarTime)
  let time = formatCalendarTime defaultTimeLocale (timeFmt defaultTimeLocale) ct
  textBufferInsert buffer end $ time ++ " " ++ str ++ "\n"
  textViewScrollMarkOnscreen (chanview chan) endmark

-- signature is in IRC module
type Interactive = (String, IRCChannel)
chanI = unsafePerformIO newChan

sendLine :: IRCChannel -> Bool -> IO ()
sendLine chan real = do
  let entry = chanentry chan
  line <- entryGetText entry
  case words line of
       (c:ps) | (head c == '/') || real -> do
	   entrySetText entry ""
	   writeChan chanI (line, chan)
       _ -> return ()
