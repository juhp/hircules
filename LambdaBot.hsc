--  LambdaBot.hs: lambdabot integration
--
--  Version: $Revision: 1.1 $ from $Date: 2003/07/03 21:30:47 $
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

module LambdaBot where

import IRC

#ifdef LAMBDABOT
import HelloModule
-- import FactModule
-- import StateModule
-- import TopicModule
-- import FortuneModule
-- import KarmaModule
import SystemModule
import EvalModule
#endif

installModules :: IRC ()
botApology :: String -> String -> IRC ()

#ifdef LAMBDABOT
installModules = do
    ircInstallModule helloModule
    -- ircInstallModule factModule
    -- ircInstallModule stateModule
    -- ircInstallModule topicModule
    -- ircInstallModule fortuneModule
    -- ircInstallModule karmaModule
    ircInstallModule systemModule
    ircInstallModule evalModule

botApology who txt = ircPrivmsg who txt
#else
installModules = return ()
botApology _ _ = return ()
#endif
