--  Main.hs: hircules IRC client
--
--  Version: $Revision: 1.4 $ from $Date: 2003/05/15 20:43:43 $
--
--  Copyright (c) 2003 Andrew J. Bromage
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
--
--  See the "COPYING" file for license information.

module Main where

import IRC
import GHC.IO
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isDigit, toUpper)
import Data.FiniteMap
import PosixProcEnv (getLoginName)
import System.Console.GetOpt (getOpt, usageInfo, ArgDescr(..), OptDescr(..), ArgOrder(..))
import System.Environment (getArgs, getProgName)
import System.Time (ClockTime(..), calendarTimeToString, toCalendarTime)
import System.IO.Unsafe (unsafePerformIO)

import Gtk

import GUI
import Threads
import Util
import Version

import HelloModule
-- import FactModule
-- import StateModule
-- import TopicModule
-- import FortuneModule
-- import KarmaModule
import SystemModule
import EvalModule

myuserinfo :: String
myuserinfo = "hircules user"

data Flag 
    = {-Debug |-} Version | Help
   deriving (Show, Eq)

options :: [OptDescr Flag]
options =
 [ Option ['v'] ["version"] (NoArg Version) "show version number"
 , Option ['h'] ["help"] (NoArg Help) "show this message"
-- , Option ['D'] ["debug"] (NoArg Debug) "show debugging info"
 ]

main :: IO ()
main = do
    cmdline <- getArgs
    case (getOpt Permute options cmdline) of
	 (opts, _, _)
	     | Help `elem` opts -> help
	     | Version `elem` opts -> version
	 (opts, [server], errs)
	     | null errs
	       -> main' opts server
	 (_, _, errs)
	     | length errs > 0 -> help >> error (concat errs)
	     | otherwise -> help

progname = unsafePerformIO getProgName

help :: IO ()
help = putStr $ usageInfo ("Usage: " ++ progname ++ " [OPTION]... [ircserver]\n") options

main' :: [Flag] -> String -> IO ()
main' opts server = do
    setupGUI
    thread <- newThread $ runIRC server ircMain
    mainGUI
    killThreads

ircMain :: IRC ()
ircMain = do
    installModules
    username <- liftIO getLoginName
    setNick username
    ircSignOn username myuserinfo
    mainloop
  where
  installModules :: IRC ()
  installModules = do
      ircInstallModule helloModule
      -- ircInstallModule factModule
      -- ircInstallModule stateModule
      -- ircInstallModule topicModule
      -- ircInstallModule fortuneModule
      -- ircInstallModule karmaModule
      ircInstallModule systemModule
      ircInstallModule evalModule

mainloop :: IRC ()
mainloop
  = do  
    haveRead <- asks ircReadChan >>= ircReady
    when haveRead (ircRead >>= processReadMessage)
    haveInput <- ircReady chanI
    when haveInput (ircInput >>= processInput)
    liftIO $ threadDelay 10000
    mainloop

processReadMessage :: IRCMessage -> IRC ()
processReadMessage msg = do
-- 	liftIO $ putStrLn $ show msg
        case msgCommand msg of
            "PRIVMSG" -> doPRIVMSG msg
            "JOIN" -> doJOIN msg
            "NICK" -> doNICK msg
	    "QUIT" -> doQUIT msg
	    "ERROR" -> doERROR msg
--             "NOTICE"  -> doNOTICE            msg
--             "MODE"    -> doMODE              msg
--             "PING"    -> doPING              msg
--             "TOPIC"   -> doTOPIC             msg

            "001" -> doRPL_WELCOME       msg
	    "433" -> do
	        nick <- getNick
		let nick' = nick ++ "_"
		setNick nick'
	        ircSignOn nick' myuserinfo

--             "002"     -> doRPL_YOURHOST      msg
--             "003"     -> doRPL_CREATED       msg
--             "004"     -> doRPL_MYINFO        msg
--             "005"     -> doRPL_BOUNCE        msg

--             ('2':_:_:[]) -> doRPL            cmd msg
--             ('3':_:_:[]) -> doRPL            cmd msg
--             ('4':_:_:[]) -> doERR            cmd msg
            _ -> return ()
-- 	liftIO $ putStrLn $ "2" ++ (show msg)
	doDisplay msg
-- 	liftIO $ putStrLn $ "3" ++ (show msg)
        mainloop

--   where
--     doERR _ msg = doUNKNOWN msg
-- 
--     doRPL "250" msg = doRPL_STATSCONN     msg
--     doRPL "251" msg = doRPL_LUSERCLIENT   msg
--     doRPL "252" msg = doRPL_LUSEROP       msg
--     doRPL "253" msg = doRPL_LUSERUNKNOWN  msg
--     doRPL "254" msg = doRPL_LUSERCHANNELS msg
--     doRPL "255" msg = doRPL_LUSERME       msg
--     doRPL "265" msg = doRPL_LOCALUSERS    msg
--     doRPL "266" msg = doRPL_GLOBALUSERS   msg
-- --     doRPL "332" msg = doRPL_TOPIC         msg
--     doRPL "353" msg = doRPL_NAMREPLY      msg
--     doRPL "366" msg = doRPL_ENDOFNAMES    msg
--     doRPL "372" msg = doRPL_MOTD          msg
--     doRPL "375" msg = doRPL_MOTDSTART     msg
--     doRPL "376" msg = doRPL_ENDOFMOTD     msg
--     doRPL _     msg = return ()

doUNKNOWN :: IRCMessage -> IRC ()
doUNKNOWN msg
  = ircDisplay allchannel $ show msg -- "<" ++ msgPrefix msg ++ "> " ++ msgCommand msg ++ " " ++ (unwords $ msgParams msg)

-- doIGNORE :: IRCMessage -> IRC ()
-- doIGNORE msg
--   = (ircDisplay allchannel $ "IGNORING> <" ++ msgPrefix msg ++
--                   "> [" ++ msgCommand msg ++ "] " ++ (msgParams msg))

-- doPING :: IRCMessage -> IRC ()
-- doPING msg
--   = (ircDisplay allchannel $ "ERROR> <" ++ msgPrefix msg ++
--                   "> [" ++ msgCommand msg ++ "] " ++ unwords (msgParams msg))

-- doNOTICE :: IRCMessage -> IRC ()
-- doNOTICE msg
--   = (ircDisplay allchannel $ "NOTICE: " ++ unwords (msgParams msg))

doJOIN :: IRCMessage -> IRC ()
doJOIN msg = do
    let loc = head (msgTail msg)
	nick = msgNick msg
    mynick <- getNick
    when (nick == mynick) $ addChannel loc True

-- doJoinChannel :: String -> IRC ()
-- doJoinChannel loc = do
--     inchan <- memberChannel loc
--     when (not inchan) $ do
--       nick <- getNick
--       liftIO (addIRCchannel loc nick True) >>= addChannel loc ""

doQUIT :: IRCMessage -> IRC ()
doQUIT msg = do
    chans <- getIRCChannels
    liftIO $ mapM_ (\ chn -> writeTextLn chn $ " " ++ (msgNick msg) ++ " has " ++ "quit" ++ " (" ++ (unwords $ msgTail msg) ++ ")") chans

doERROR :: IRCMessage -> IRC ()
doERROR msg =
    let tale = msgTail msg in
    if (take 2 tale) == ["Closing","Link:"]
       then return () -- reconnectServer
       else return ()

-- doPART :: IRCMessage -> IRC ()
-- doPART msg = do
--     let loc = head (msgMiddle msg)
--     chan <- getIRCChannel loc
--     removeChannel loc
--     removeIRCchannel chan

-- head :: [a] -> a
-- head ls = Prelude.head $ trace "HEAD " ls

doNICK :: IRCMessage -> IRC ()
doNICK msg = do
    current <- getNick
    let nick = msgNick msg
	new = head $ msgTail msg
    when (current == nick) (setNick new)

-- doMODE :: IRCMessage -> IRC ()
-- doMODE msg
--   = doIGNORE msg


-- doTOPIC :: IRCMessage -> IRC ()
-- doTOPIC msg
--     = do let loc = (head (msgParams msg))
--          s <- get
--          put (s { ircChannels = addToFM (ircChannels s) loc (tail $ head $ tail $ msgParams msg) })

doRPL_WELCOME :: IRCMessage -> IRC ()
doRPL_WELCOME msg = do
  let nick = head $ msgMiddle msg
  setNick nick
  joinMany []
  where
    joinMany :: [String] -> IRC ()
    joinMany [] = return ()
    joinMany (c:cs) = ircJoin c >> joinMany cs

-- doRPL_YOURHOST :: IRCMessage -> IRC ()
-- doRPL_YOURHOST msg = return ()
-- 
-- doRPL_CREATED :: IRCMessage -> IRC ()
-- doRPL_CREATED msg = return ()
-- 
-- doRPL_MYINFO :: IRCMessage -> IRC ()
-- doRPL_MYINFO msg = return ()
-- 
-- doRPL_BOUNCE :: IRCMessage -> IRC ()
-- doRPL_BOUNCE msg = (ircDisplay allchannel "Bounce!")
-- 
-- doRPL_STATSCONN :: IRCMessage -> IRC ()
-- doRPL_STATSCONN msg = return ()
-- 
-- doRPL_LUSERCLIENT :: IRCMessage -> IRC ()
-- doRPL_LUSERCLIENT msg = return ()
-- 
-- doRPL_LUSEROP :: IRCMessage -> IRC ()
-- doRPL_LUSEROP msg = return ()
-- 
-- doRPL_LUSERUNKNOWN :: IRCMessage -> IRC ()
-- doRPL_LUSERUNKNOWN msg = return ()
-- 
-- doRPL_LUSERCHANNELS :: IRCMessage -> IRC ()
-- doRPL_LUSERCHANNELS msg = return ()
-- 
-- doRPL_LUSERME :: IRCMessage -> IRC ()
-- doRPL_LUSERME msg = return ()
-- 
-- doRPL_LOCALUSERS :: IRCMessage -> IRC ()
-- doRPL_LOCALUSERS msg = return ()
-- 
-- doRPL_GLOBALUSERS :: IRCMessage -> IRC ()
-- doRPL_GLOBALUSERS msg = return ()
-- 
-- doRPL_TOPIC :: IRCMessage -> IRC ()
-- doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of msgParams
--     = do let loc = (msgParams msg) !! 1
--          s <- get
--          put (s { ircChannels = addToFM (ircChannels s) loc (tail $ last $ msgParams msg) })
-- 
-- doRPL_NAMREPLY :: IRCMessage -> IRC ()
-- doRPL_NAMREPLY msg = return ()
-- 
-- doRPL_ENDOFNAMES :: IRCMessage -> IRC ()
-- doRPL_ENDOFNAMES msg = return ()
-- 
-- doRPL_MOTD :: IRCMessage -> IRC ()
-- doRPL_MOTD msg = return ()
-- 
-- doRPL_MOTDSTART :: IRCMessage -> IRC ()
-- doRPL_MOTDSTART msg = return ()
-- 
-- doRPL_ENDOFMOTD :: IRCMessage -> IRC ()
-- doRPL_ENDOFMOTD msg = return ()

doDisplay :: IRCMessage -> IRC ()
doDisplay msg = do
  mynick <- getNick
  let user = msgNick msg
      cmd = msgCommand msg
      mid = msgMiddle msg
      tale = msgTail msg
      (output,chan) = transform user cmd mid tale mynick
  ircDisplay allchannel $ (if null chan then "" else chan ++ ":") ++ output
  mchan <- getIRCChannel chan
  case mchan of
       Just chann -> ircDisplay chann output
       Nothing -> return ()
  where  
  transform :: String -> String -> [String] -> [String] -> String -> (String, String)
  transform user cmd mid tale mynick =
      case cmd of
	       "PRIVMSG" -> let chan = head mid
				public = head chan == '#' in
	           ("<" ++ user ++ ">" ++ " " ++ rest, if public then chan else msgNick msg)
	       "JOIN" -> (" " ++ (theuser "have" "has") ++ "joined ", head tale)
	       "PART" -> (" " ++ (theuser "have" "has") ++ "left ", head mid)
	       "NOTICE" -> ("NOTICE: " ++ all, "")
	       "NICK" -> (" " ++ (theuser "are" "is") ++ "now known as " ++ (head tale), "")
	       "TOPIC" -> (" " ++ (theuser "have" "has") ++ "changed the topic to: " ++ rest, middle)
	       "QUIT" -> (" " ++ (theuser "have" "has") ++ "quit" ++ " (" ++ rest ++ ")", "")
	       "PONG" -> ("Ping reply from " ++ (head tale), "")
	       "332" -> ("Topic: " ++ rest, last mid)
	       "333" -> ("set by " ++ (mid !! 2) ++ " at " ++ (time $ read $ last mid), mid !! 1)
	       "353" -> (" Users: " ++ rest, (last mid))
	       _ | and (map isDigit cmd) -> ((unwords $ tail mid) ++ " - " ++ rest, "")
	       _ -> (" " ++ cmd ++ " " ++ all, "")
    where
    middle = unwords mid
    rest = unwords tale
    all = middle ++ " " ++ rest
    time :: Integer -> String
    time t = calendarTimeToString $ unsafePerformIO $ toCalendarTime $ TOD t 0 
    theuser :: String -> String -> String
    theuser second third =
	(if user == mynick then "You " ++ second else user ++ " " ++ third) ++ " "

doPRIVMSG :: IRCMessage -> IRC ()
doPRIVMSG msg = do
  mynick <- getNick
  doPRIVMSG' msg mynick
  where
  doPRIVMSG' :: IRCMessage -> String -> IRC ()
  doPRIVMSG' msg mynick | mynick `elem` targets = doPersonalMsg (head text) (tail text)
			| (mynick ++ ":") == head text
			    = let (cmd:params) = tail text in
			      doPublicMsg cmd params
			| otherwise = return ()
    where
    alltargets = head (msgMiddle msg)
    targets = split "," alltargets
    text = msgTail msg

    doPersonalMsg ('@':cmd) rest
        = do let who = msgNick msg
             maybecmd <- gets (\s -> lookupFM (ircCommands s) cmd)
             case maybecmd of
                           Just (MODULE m) -> process m msg who cmd (unwords rest)
                           Nothing         -> ircPrivmsg who "Sorry, I don't know that command."
    doPersonalMsg (c:"PING") _ = ircWrite $ mkIrcMessageWithPrefix mynick "NOTICE" [msgNick msg] text
    doPersonalMsg _ _
      = addChannel (msgNick msg) True
    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) rest
     = do maybecmd <- gets (\s -> lookupFM (ircCommands s) cmd)
          case maybecmd of 
                        Just (MODULE m) -> process m msg alltargets cmd (unwords rest)
                        Nothing         -> ircPrivmsg alltargets $ "Sorry, I don't know that command, try \"" ++ mynick ++ ": @listcommands\""
    doPublicMsg _ _
      = liftIO $ putStr "\BEL"

processInput :: Interactive -> IRC ()
processInput (str, chan) =
  case parseCmd (words str) chan of
       Just (cmd,middle,tale,noslash) -> do
           let realchan = chanreal chan
           nick <- getNick
	   when realchan $
		ircDisplay chan $ (if noslash then "<" ++ nick ++ "> " else "") ++ str
	   ircDisplay allchannel $ (if noslash then (if realchan then channame chan else "") else "") ++ "<" ++ nick ++ "> " ++ str
	   let msg = mkIrcMessage cmd middle tale
	       encmsg = encodeMessage msg "\r"
           ircSend encmsg
       Nothing -> return ()
  where
  parseCmd :: [String] -> IRCChannel -> Maybe (String, [String], [String], Bool)
  parseCmd [] _ = Nothing
  parseCmd (('/':cmd):[]) chan =
      case cmd of
	   "join" | publicChan chan -> Just (ircCmd cmd, [], [channame chan], False)
           "part" | publicChan chan -> Just (ircCmd cmd, [channame chan], [], False)
	   "topic" | publicChan chan -> Just (ircCmd cmd, [], [channame chan], False)
	   "quit" -> Just (ircCmd cmd, [], ["hircules", versionString], False)
	   _ -> Just (ircCmd cmd, [], [], False)
  parseCmd (('/':cmd):params) _ =
      let (middle,tale) = break startColon params in
      Just (ircCmd cmd,if null tale then [head middle] else middle, if null tale then tail middle else removeLeadColon tale, False)
  parseCmd str chan = 
      if (chanreal chan)
	 then Just (ircCmd "msg", [channame chan], str, True)
	 else Nothing
  ircCmd :: String -> String
  ircCmd "msg" = "PRIVMSG"
  ircCmd cmd = map toUpper cmd
  startColon :: String -> Bool
  startColon (':':_) = True
  startColon _ = False
  publicChan :: IRCChannel -> Bool
  publicChan chan =
      let name = channame chan in
      head name == '#'
