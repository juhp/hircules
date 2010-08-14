--  A small keymap library for gtk
--
--  Author : Jens-Ulrik Petersen
--  Created: 15 July 2002
--
--  Version:  $Revision: 1.6 $ from $Date: 2008/11/03 03:14:11 $
--
--  Copyright (c) 2002 Jens-Ulrik Holger Petersen
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- Description
--

module Graphics.UI.Gtk.Keymap (keymapAdd, keyPressCB, newKeymap, Keymap, Keybinding(..))
where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar, MVar)

import Graphics.UI.Gtk.Gdk.Events

-- import Debug
-- import GdkKeys

type Keymap = MVar KeymapHash

type KeymapHash = Map (String, Int) (IO ())

data Keybinding = KB [Modifier] String (IO ())

-- -- need to map meta, alt, hyper, super, et al
-- data ModSym = ModShift | ModLock | ModCtrl | Mod1 | Mod2 | Mod3 | Mod4 | Mod5
--   deriving Enum

newKeymap :: IO Keymap
newKeymap = newMVar Map.empty

keymapAdd :: Keymap -> Keybinding -> IO ()
keymapAdd keymap (KB modi name act) =
    modifyMVar_ keymap $ \keyfm -> do
--       debug $ symsToInt modi
      let bitmap = sum $ map fromEnum modi
      return $ Map.insert (name, bitmap) act keyfm
--   where
--       symsToInt :: [ModSym] -> Modifier
--       symsToInt ss = foldl (+) 0 $ map (\s -> 2^(fromEnum s)) ss

keyPressCB :: Keymap -> Event -> IO Bool
keyPressCB keymap Key { eventKeyName = keyName,
			eventModifier = modi } =
    do
    keyfm <- readMVar keymap
    let bitmap = sum $ map fromEnum modi
    case Map.lookup (keyName, bitmap) keyfm of
		Just act -> act >> return True
		Nothing -> return False
keyPressCB _ _ = return False

