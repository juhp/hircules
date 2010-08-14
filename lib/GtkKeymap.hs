--  A small keymap library for gtk
--
--  Author : Jens-Ulrik Petersen
--  Created: 15 July 2002
--
--  Version:  $Revision: 1.6 $ from $Date: 2002/01/25 06:44:01 $
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

module GtkKeymap (keymapAdd, keyPressCB, newKeymap,
    Keymap, Keybinding(..), ModSym(..))
where
import FiniteMap
import MVar (newMVar, modifyMVar_, readMVar, MVar)

import Events
import GdkKeys

type Keymap = MVar KeymapHash

type KeymapHash = FiniteMap (String, Modifier) (IO ())

data Keybinding = KB [ModSym] String (IO ())

-- need to map meta, alt, hyper, super, et al
data ModSym = ModShift | ModLock | ModCtrl | Mod1 | Mod2 | Mod3 | Mod4 | Mod5
  deriving Enum

newKeymap :: IO Keymap
newKeymap = newMVar emptyFM

keymapAdd :: Keymap -> Keybinding -> IO ()
keymapAdd keymap (KB mod name act) =
    modifyMVar_ keymap $ \keyfm ->
      return $ addToFM keyfm (name, symsToInt mod) act
  where
      symsToInt :: [ModSym] -> Modifier
      symsToInt ss = foldl (+) 0 $ map (\s -> 2^(fromEnum s)) ss

keyPressCB :: Keymap -> Event -> IO Bool
keyPressCB keymap ev =
    do
    keyfm <- readMVar keymap
    let mod = modif ev
	keyname = keyvalName $ keyval ev
--     print mod
--     print keyname
    case keyname of
	 Just key ->
	   case lookupFM keyfm (key, mod) of
		Just act -> do act
		               return True
		Nothing -> return False
	 Nothing -> return False
