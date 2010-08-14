--  Main.hs: hircules IRC client
--
--  Version: $Revision: 1.3 $ from $Date: 2003/07/03 11:24:16 $
--
--  Copyright (c) 2003 Andrew J. Bromage
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
--
--  See the "COPYING" file for license information.

module Main where

import GHC.IO
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isDigit, toLower, toUpper)
import Data.FiniteMap
import Data.Maybe
-- import PosixProcEnv (getLoginName)
import System.Console.GetOpt (getOpt, usageInfo, ArgDescr(..), OptDescr(..), ArgOrder(..))
import System.Environment (getArgs, getEnv, getProgName)
import System.Time (ClockTime(..), calendarTimeToString, toCalendarTime)
import System.IO.Unsafe (unsafePerformIO)

import Gtk

import CTCP
import Config (configDir,logDir)
import Debug
-- comment this out if your gtk2hs provides Gdk.beep
import Display
import Directories
import GUI
import IRC
import LambdaBot
import MaybeDo
import Threads
import Version
import WordString

myuserinfo :: String
myuserinfo = "hircules user"

data Flag 
    = Debug | Version | Help
   deriving (Show, Eq)

options :: [OptDescr Flag]
options =
 [ Option ['v'] ["version"] (NoArg Version) "show version number"
 , Option ['h'] ["help"] (NoArg Help) "show this message"
 , Option ['D'] ["debug"] (NoArg Debug) "show debugging info"
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
    setDebug $ Debug `elem` opts
    makeConfigDir
    setupGUI
    thread <- newThread $ runIRC server ircMain
    mainGUI
    killThreads
  where
  makeConfigDir :: IO ()
  makeConfigDir = do
      makeDirectory configDir
      makeDirectory logDir

ircMain :: IRC ()
ircMain = do
    installModules
    username <- liftIO $ getEnv "USER"
    setNick username
    ircSignOn username myuserinfo
    mainloop

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
        debug msg
        case msgCommand msg of
            "PRIVMSG" -> doPRIVMSG msg
            "JOIN" -> doJOIN msg
            "NICK" -> doNICK msg
            "PART" -> doPART msg
            "QUIT" -> doQUIT msg
            "ERROR" -> doERROR msg
--             "NOTICE"  -> doNOTICE            msg
--             "MODE"    -> doMODE              msg
--             "PING"    -> doPING              msg
--             "TOPIC"   -> doTOPIC             msg

            "001" -> doRPL_WELCOME       msg
            "353" -> doRPL_NAMREPLY      msg
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
--         liftIO $ putStrLn $ "2" ++ (show msg)
        doDisplay msg
--         liftIO $ putStrLn $ "3" ++ (show msg)
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
--     doRPL "366" msg = doRPL_ENDOFNAMES    msg
--     doRPL "372" msg = doRPL_MOTD          msg
--     doRPL "375" msg = doRPL_MOTDSTART     msg
--     doRPL "376" msg = doRPL_ENDOFMOTD     msg
--     doRPL _     msg = return ()

doUNKNOWN :: IRCMessage -> IRC ()
doUNKNOWN msg
  = ircDisplay allchannel $ show msg -- "<" ++ msgPrefix msg ++ "> " ++ msgCommand msg +-+ (msgParams msg)

-- doIGNORE :: IRCMessage -> IRC ()
-- doIGNORE msg
--   = (ircDisplay allchannel $ "IGNORING> <" ++ msgPrefix msg ++
--                   "> [" ++ msgCommand msg ++ "] " ++ (msgParams msg))

-- doPING :: IRCMessage -> IRC ()
-- doPING msg
--   = (ircDisplay allchannel $ "ERROR> <" ++ msgPrefix msg ++
--                   "> [" ++ msgCommand msg ++ "] " ++ (msgParams msg))

-- doNOTICE :: IRCMessage -> IRC ()
-- doNOTICE msg
--   = (ircDisplay allchannel $ "NOTICE: " ++ (msgParams msg))

doJOIN :: IRCMessage -> IRC ()
doJOIN msg = do
    let loc = map toLower $ whead (msgTail msg)
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
    let nick = msgNick msg
    chans <- getUserChannels nick
    debug chans
    chanmap <- gets ircChannels
    let ichans = mapMaybe (lookupFM chanmap) chans
    liftIO $ mapM_ (\ chn -> writeTextLn chn $ " " ++ (msgNick msg) ++ " has quit (" ++ (msgTail msg) ++ ")") ichans
    removeUser nick

doERROR :: IRCMessage -> IRC ()
doERROR msg =
    let tale = msgTail msg in
    if (take 13 tale) == "Closing Link:"
       then return () -- reconnectServer
       else return ()

doPART :: IRCMessage -> IRC ()
doPART msg = do
    let loc = map toLower $ whead (msgMiddle msg)
        nick = msgNick msg
    chan <- getIRCChannel loc
    mynick <- getNick
    when (nick == mynick) $ liftIO $ maybeDo hideIRCchannel chan
    partChanUser nick loc
--     removeChannel loc

doNICK :: IRCMessage -> IRC ()
doNICK msg = do
    current <- getNick
    chans <- getUserChannels nick
    debug chans
    chanmap <- gets ircChannels
    let ichans = mapMaybe (lookupFM chanmap) chans
        new = whead $ msgTail msg
    when (current == nick) (setNick new)
    liftIO $ mapM_ (\ chn -> writeTextLn chn $ (" " ++ (theuser current "are" "is") +-+ "now known as " ++ new)) ichans
    renameUser nick new
  where
  nick = msgNick msg
  theuser :: String -> String -> String -> String
  theuser you second third =
        (if you == nick then "You " ++ second else nick +-+ third)

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
  let nick = whead $ msgMiddle msg
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

doRPL_NAMREPLY :: IRCMessage -> IRC ()
doRPL_NAMREPLY msg = do
    let users = map removeOp $ words $ msgTail msg
        chan = wlast $ msgMiddle msg
    setChanUsers users chan
  where
  removeOp :: String -> String
  removeOp ('@':cs) = cs
  removeOp cs = cs

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
  debug msg
  mynick <- getNick
  let (output,chan) = transform mynick
      text = (if null chan then "" else chan ++ ":") ++ output
  ircDisplay allchannel text
  logMessage text
  mchan <- getIRCChannel chan
  debug $ maybe "nochan" channame mchan
  case mchan of
       Just chann -> ircDisplay chann output
       Nothing -> return ()
  where  
  user = msgNick msg
  cmd = msgCommand msg
  mid = msgMiddle msg
  tale = msgTail msg
  transform :: String -> (String, String)
  transform mynick = let (out,chan) = transform' mynick in
                (out, map toLower chan)
  transform' :: String -> (String, String)
  transform' mynick =
      case cmd of
               "PRIVMSG" -> let chan = whead mid
                                public = head chan == '#'
                                output =
                                    if head tale == '\001'
                                       then formatCTCP user $ ctcpUnquote tale
                                       else "<" ++ user ++ ">" +-+ tale
                                in
                           (output, if public then chan else msgNick msg)
               "JOIN" -> (" " ++ (theuser "have" ("(" ++ (msgUser msg)  ++ ") has")) ++ "joined ", tale)
               "PART" -> (" " ++ (theuser "have" "has") ++ "left ", whead mid)
               "NOTICE" -> ("NOTICE: " ++ all, "")
               "NICK" -> (" " ++ (theuser "are" "is") ++ "now known as " ++ tale, "")
               "TOPIC" -> (" " ++ (theuser "have" "has") ++ "changed the topic to: " ++ tale, whead mid)
               "QUIT" -> (" " ++ (theuser "have" "has") ++ "quit" ++ " (" ++ tale ++ ")", "")
               "PONG" -> ("Ping reply from " ++ tale, "")
               "MODE" -> (" " ++ (theuser "have" "has") ++ "changed mode " ++ (wtail mid), whead mid)
               "332" -> ("Topic: " ++ tale, wlast mid)
               "333" -> ("set by " ++ (wnth 3 mid) ++ " at " ++ (time $ read $ wlast mid), wnth 2 mid)
               "353" -> (" Users: " ++ tale, (wlast mid))
               _ | and (map isDigit cmd) -> ((wtail mid) ++ " - " ++ tale, "")
               _ -> (" " ++ cmd +-+ all, "")
    where
    all = mid +-+ tale
    time :: Integer -> String
    time t = calendarTimeToString $ unsafePerformIO $ toCalendarTime $ TOD t 0 
    theuser :: String -> String -> String
    theuser second third =
        (if user == mynick then "You " ++ second else user +-+ third) ++ " "
    formatCTCP :: String -> String -> String
    formatCTCP user ctxt | whead ctxt == "ACTION" = user +-+ (wtail ctxt)

doPRIVMSG :: IRCMessage -> IRC ()
doPRIVMSG msg = do
  mynick <- getNick
  case msg of
      _ | mynick `elem` targets -> doPersonalMsg (whead text) (wtail text) mynick
      _ | (mynick ++ ":") == whead text
            -> let rest = wtail text
                   (cmd,params) = (whead rest,wtail rest) in
               doPublicMsg cmd params mynick
      _ | otherwise -> return ()
    where
    alltargets = whead (msgMiddle msg)
    targets = split "," alltargets
    text = msgTail msg
    doPersonalMsg ('@':cmd) tale _
        = do let who = msgNick msg
             maybecmd <- gets (\s -> lookupFM (ircCommands s) cmd)
             case maybecmd of
                           Just (MODULE m) -> process m msg who cmd tale
                           Nothing         -> botApology who "Sorry, I don't know that command."
    doPersonalMsg ('\001':"PING") _ mynick = ircWrite $ mkIrcMessageWithPrefix mynick "NOTICE" (msgNick msg) text
    doPersonalMsg _ _ _ = do
        addChannel (msgNick msg) True
        liftIO beep
    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) tale mynick = do
        maybecmd <- gets (\s -> lookupFM (ircCommands s) cmd)
        case maybecmd of 
                      Just (MODULE m) -> process m msg alltargets cmd tale
                      Nothing         -> botApology alltargets $ "Sorry, I don't know that command, try \"" ++ mynick ++ ": @listcommands\""
    doPublicMsg _ _ _ = do
        ircDisplay alertchannel $ alltargets ++ ":<" ++ (msgNick msg) ++ "> " ++ text
        liftIO beep

processInput :: Interactive -> IRC ()
processInput (str, chan) =
  case parseCmd (words input) chan of
       Just (cmd,middle,tale,noslash) -> do
           nick <- getNick
           let realchan = chanreal chan
               text = (if noslash then "<" ++ nick ++ "> " else "") ++ input
               alltext = (if realchan then channame chan else "") +-+ text
           when realchan $
                ircDisplay chan text
           ircDisplay allchannel alltext
           logMessage alltext
           let msg = mkIrcMessage cmd middle tale
           case cmd of
                    "JOIN" | (not $ null middle) -> do
                               let chan = whead middle
                               chans <- getUserChannels nick
                               if elem chan chans
                                  then displayIRCchannel chan
                                  else if head chan == '#'
                                          then ircWrite msg
                                          else addChannel chan True
                    _ -> ircWrite msg
--            rchan <- asks ircReadChan
--            liftIO $ writeChan rchan (msg {msgPrefix = nick ++ "!"})
       Nothing -> return ()
  where
  input = if chanreal chan then str else '/':str
  parseCmd :: [String] -> IRCChannel -> Maybe (String, -- command
                                               String, -- middle
                                               String, -- tail
                                               Bool) -- noslash
  parseCmd [] _ = Nothing
  parseCmd (('/':cmd):[]) chan =
      case cmd of
           "join" | publicChan chan -> Just (ircCmd cmd, channame chan, "", False)
           "names" | publicChan chan -> Just (ircCmd cmd, channame chan, "", False)
           "part" | publicChan chan -> Just (ircCmd cmd, channame chan, "", False)
           "topic" | publicChan chan -> Just (ircCmd cmd, "", channame chan, False)
           "quit" -> Just (ircCmd cmd, "", ("hircules" +-+ versionString), False)
           _ -> Just (ircCmd cmd, "", "", False)
  parseCmd (("/me"):_) _ | chanreal chan = Just ("PRIVMSG", channame chan, ctcpQuote $ "ACTION" +-+ wtail input, True)
  parseCmd (('/':cmd):_) _ =
      let (middle,tale) = breakString " :" $ wtail input in
      Just (ircCmd cmd,if null tale then whead middle else middle, if null tale then wtail middle else tale, False)
  parseCmd _ chan = 
      if (chanreal chan)
         then Just ("PRIVMSG", channame chan, input, True)
         else Nothing
  ircCmd :: String -> String
  ircCmd "msg" = "PRIVMSG"
  ircCmd cmd = map toUpper cmd
--   startColon :: String -> Bool
--   startColon (':':_) = True
--   startColon _ = False
  publicChan :: IRCChannel -> Bool
  publicChan chan =
      let name = channame chan in
      head name == '#'
