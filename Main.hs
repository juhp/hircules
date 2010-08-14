--  Main.hs: hircules IRC client
--
--  Version: $Revision: 1.7 $ from $Date: 2008/11/03 03:14:11 $
--
--  Copyright (c) 2003 Andrew J. Bromage
--  Copyright (c) 2003, 2004, 2006 Jens-Ulrik Holger Petersen
--
--  See the "COPYING" file for license information.

module Main where

-- import GHC.IO
import Control.Concurrent
import Control.Exception (bracket_, try)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isAlpha, isDigit, toLower, toUpper)
import qualified Data.Map as Map
import Data.Maybe
import GHC.IOBase (BufferMode(..))
import GHC.Handle
import Network (connectTo, withSocketsDo, PortID(..))
-- import PosixProcEnv (getLoginName)
import System.Console.GetOpt (getOpt, usageInfo, ArgDescr(..), OptDescr(..), ArgOrder(..))
import System.Environment (getArgs, getEnv, getProgName)
import System.Time (ClockTime(..), calendarTimeToString, toCalendarTime)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.UI.Gtk (beep, mainGUI)

import CTCP
import Channel
import Config (configDir,logDir)
import Debug
-- -- uncomment next line if your gtk2hs doesn't provide Gdk.beep
-- import Display
import Directories
import GUI
import IRC
import LambdaBot
import MaybeDo
import QuickSort
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
         (opts, args, errs)
             | null errs && length args < 2
               -> main' opts $ listToMaybe args
             | length errs > 0 -> help >> error (concat errs)
             | otherwise -> help

progname :: String
progname = unsafePerformIO getProgName

help :: IO ()
help = putStr $ usageInfo ("Usage: " ++ progname ++ " [OPTION]... [ircserver]\n") options

main' :: [Flag] -> Maybe String -> IO ()
main' opts mserver = do
    setDebug debugging
    makeConfigDir
    setupGUI
    newThread $ runIRC $ maybe runIrcInit runIrcNoInit mserver
    mainGUI
    killThreads
  where
  debugging :: Bool
  debugging = Debug `elem` opts
  makeConfigDir :: IO ()
  makeConfigDir = do
      makeDirectory configDir
      makeDirectory logDir
  runIRC :: IO () -> IO ()
  runIRC m =
    try m >>= \exc ->
        case exc of
            Left exception -> putStrLn ("Exception: " ++ show exception)
            Right result   -> return result
  runIrcNoInit :: String -> IO ()
  runIrcNoInit server = withSocketsDo $ do
    s <- liftIO (connectTo hostname (PortNumber portnum))
    hSetBuffering s NoBuffering
    threadmain <- myThreadId
    chanR <- newChan
    chanW <- newChan
    threadr <- newThread (readerLoop threadmain chanR chanW s)
    threadw <- newThread (writerLoop threadmain chanW s)
    h <- liftIO $ openFile (logDir +/+ hostname) AppendMode
    hSetBuffering h LineBuffering
    let rstate = IRCRState ()
        initState =
	    IRCRWState { ircServer      = hostname
                       , ircReadChan    = chanR
                       , ircReadThread  = threadr
                       , ircWriteChan   = chanW
                       , ircWriteThread = threadw
                       , ircSocket      = s
                       , ircLogFile     = h
		       , ircPrivilegedUsers = Map.empty  -- FIXME?: was listToFM [ (user,True) | user <- [] ]
		       , ircChannels = initChans
		       , ircNick = ""
		       , ircModules = Map.empty
		       , ircCommands = Map.empty
		       , ircModuleState = Map.empty
		       , ircUsers = Map.empty
                       }
    bracket_ (return ()) (killThread threadr >> killThread threadw)
		 (runReaderT (evalStateT ircMain initState) rstate)
    where
    (hostname,port') = break (== ':') server
    port = if null port' then "6667" else tail port'
    portnum = fromIntegral (read port :: Integer)
    initChans = Map.fromList $ [("%all", allchannel),
                            ("%alert", alertchannel)] ++
                                        (if debugging
                                            then [("%raw", rawchannel)]
                                            else [])

  runIrcInit :: IO ()
  runIrcInit = withSocketsDo $ do
    displayChannelTab True allchannel
--     s <- liftIO (connectTo hostname (PortNumber portnum))
--     hSetBuffering s NoBuffering
--     threadmain <- myThreadId
--     chanR <- newChan
--     chanW <- newChan
--     threadr <- newThread (readerLoop threadmain chanR chanW s)
--     threadw <- newThread (writerLoop threadmain chanW s)
--     h <- liftIO $ openFile (logDir +/+ hostname) AppendMode
--     hSetBuffering h LineBuffering
--     let rstate = IRCRState ()
--     bracket_ (return ()) (killThread threadr >> killThread threadw)
-- 		 (runReaderT (evalStateT ircMain initState) rstate)
--   where
--   (hostname,port') = break (== ':') server
--   port = if null port' then "6667" else tail port'
--   portnum = fromIntegral (read port :: Integer)
--   initChans = listToFM [("%all",allchannel),
--                         ("%raw",rawchannel),
--                         ("%raw",rawchannel)]
--   initState =
-- 	    IRCRWState { ircServer      = hostname
--                        , ircReadChan    = chanR
--                        , ircReadThread  = threadr
--                        , ircWriteChan   = chanW
--                        , ircWriteThread = threadw
--                        , ircSocket      = s
--                        , ircLogFile     = h
-- 		       , ircPrivilegedUsers = listToFM [ (user,True) | user <- [] ]
-- 		       , ircChannels = initChans
-- 		       , ircNick = ""
-- 		       , ircModules = Map.empty
-- 		       , ircCommands = Map.empty
-- 		       , ircModuleState = Map.empty
-- 		       , ircUsers = Map.empty
--                        }

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
    haveRead <- gets ircReadChan >>= ircReady
    when haveRead (ircRead >>= processReadMessage)
    haveInput <- ircReady chanI
    when haveInput (ircInput >>= processInput)
    liftIO $ threadDelay 10000
    mainloop

processReadMessage :: IRCMessage -> IRC ()
processReadMessage msg = do
        debug "msg" msg
        case msgCommand msg of
            "PRIVMSG" -> doPRIVMSG msg
            "JOIN" -> doJOIN msg
            "NICK" -> doNICK msg
            "PART" -> doPART msg
            "QUIT" -> doQUIT msg
            "TOPIC" -> doTOPIC msg
            "ERROR" -> doERROR msg
--             "NOTICE"  -> doNOTICE            msg
--             "MODE"    -> doMODE              msg
--             "PING"    -> doPING              msg

            "001" -> doRPL_WELCOME       msg
            "353" -> doRPL_NAMREPLY      msg
            "366" -> doRPL_ENDOFNAMES      msg
-- this leads to looping when length nick >= server_nicklen
--             "433" -> do
--                 nick <- getNick
--                 let nick' = nick ++ "_"
--                 setNick nick'
--                 ircSetNick nick'

--             "002"     -> doRPL_YOURHOST      msg
--             "003"     -> doRPL_CREATED       msg
--             "004"     -> doRPL_MYINFO        msg
--             "005"     -> doRPL_BOUNCE        msg

--             ('2':_:_:[]) -> doRPL            cmd msg
--             ('3':_:_:[]) -> doRPL            cmd msg
--             ('4':_:_:[]) -> doERR            cmd msg
            _ -> return ()
        doDisplay msg
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
--     doRPL "372" msg = doRPL_MOTD          msg
--     doRPL "375" msg = doRPL_MOTDSTART     msg
--     doRPL "376" msg = doRPL_ENDOFMOTD     msg
--     doRPL _     msg = return ()

doUNKNOWN :: IRCMessage -> IRC ()
doUNKNOWN msg
  = ircDisplayAll $ show msg -- "<" ++ msgPrefix msg ++ "> " ++ msgCommand msg +-+ (msgParams msg)

-- doIGNORE :: IRCMessage -> IRC ()
-- doIGNORE msg
--   = (ircDisplayAll $ "IGNORING> <" ++ msgPrefix msg ++
--                   "> [" ++ msgCommand msg ++ "] " ++ (msgParams msg))

-- doPING :: IRCMessage -> IRC ()
-- doPING msg
--   = (ircDisplayAll $ "ERROR> <" ++ msgPrefix msg ++
--                   "> [" ++ msgCommand msg ++ "] " ++ (msgParams msg))

-- doNOTICE :: IRCMessage -> IRC ()
-- doNOTICE msg
--   = (ircDisplayAll $ "NOTICE: " ++ (msgParams msg))

doJOIN :: IRCMessage -> IRC ()
doJOIN msg = do
    let loc = map toLower $ whead (msgTail msg)
        nick = msgNick msg
    mynick <- getNick
    when (nick == mynick) $ addChannel loc True
    joinChanUser nick loc

-- doJoinChannel :: String -> IRC ()
-- doJoinChannel loc = do
--     inchan <- memberChannel loc
--     when (not inchan) $ do
--       nick <- getNick
--       liftIO (addIRCchannel loc nick True) >>= addChannel loc ""

doQUIT :: IRCMessage -> IRC ()
doQUIT msg = do
    let nick = msgNick msg
    debug "nick" nick
    chans <- getUserChannels nick
    debug "chans" chans
--     chanmap <- gets ircChannels
--     let ichans = mapMaybe (lookupFM chanmap) chans
    mapM_ (\ chn -> (ircDisplay chn False $ " " ++ (msgNick msg) ++ " has quit (" ++ (msgTail msg) ++ ")") >> partChanUser nick chn) chans
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
    when (nick == mynick) $ liftIO $ maybeDo_ chan hideIRCchannel
    partChanUser nick loc
--     removeChannel loc

doNICK :: IRCMessage -> IRC ()
doNICK msg = do
    current <- getNick
    chans <- getUserChannels nick
    debug "doNICK" chans
--     chanmap <- gets ircChannels
    let new = whead $ msgTail msg
    debug "doNICK" "1"
    when (current == nick) (setNick new)
    debug "doNICK" "2"
    mapM_ (\ ch -> ircDisplay ch False $ (" " ++ (theuser current "are" "is") +-+ "now known as " ++ new)) chans
    debug "doNICK" "3"
    renameUser nick new
  where
  nick = msgNick msg
  theuser :: String -> String -> String -> String
  theuser you second third =
        (if you == nick then "You " ++ second else nick +-+ third)

-- doMODE :: IRCMessage -> IRC ()
-- doMODE msg
--   = doIGNORE msg


doTOPIC :: IRCMessage -> IRC ()
doTOPIC msg = do
    let loc = whead (msgMiddle msg)
    ircDisplayAlert True $ loc ++ ":" +-+ (msgNick msg) +-+ "has changed the topic to: " ++ (msgTail msg)

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
-- doRPL_BOUNCE msg = (ircDisplayAll "Bounce!")
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
  let userops = map splitOp $ words $ msgTail msg
      chan = wlast $ msgMiddle msg
  addChanUsers userops chan
  where
  splitOp :: String -> (String, Bool)
  splitOp ('@':cs) = (cs, True)
  splitOp cs = (cs, False)

doRPL_ENDOFNAMES :: IRCMessage -> IRC ()
doRPL_ENDOFNAMES msg = do
  let ch = wlast $ msgMiddle msg
  mchan <- getIRCChannel ch
  maybeDo_ mchan $ \ chan -> do
      let output = " Users:" +-+ (unwords . (map concOp) . quickSort . Map.toList) (chanusers chan)
--           chn = channame chan
      ircDisplayAll $ ch ++ output
      ircDisplay ch False output
  where
  concOp :: (String, Bool)-> String
  concOp (s,op) = if op then '@':s else s

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
  let (output,chan,hilite) = transform mynick
      text = (if null chan then "" else chan ++ ":") ++ output
  ircDisplayAll text
  logMessage text
--   mchan <- getIRCChannel chan
--   debug "mchan" $ maybe "nochan" channame mchan
--   case mchan of
--        Just chann -> ircDisplay chann hilite output
--        Nothing -> return ()
  ircDisplay chan hilite output
  where
  user = msgNick msg
  cmd = msgCommand msg
  mid = msgMiddle msg
  tale = msgTail msg
  transform :: String -> (String, String, Bool)
  transform mynick = let (out,chan,hilite) = transform' mynick in
                (out, map toLower chan, hilite)
  transform' :: String -> (String, String, Bool)
  transform' mynick =
      case cmd of
               "PRIVMSG" -> let chan = whead mid
                                public = head chan == '#'
                                output =
                                    if head tale == '\001'
                                       then formatCTCP $ ctcpUnquote tale
                                       else "<" ++ user ++ ">" +-+ tale
                                in
                           (output, if public then chan else msgNick msg, True)
               "JOIN" -> (" " ++ (theuser "have" ("(" ++ (msgUser msg)  ++ ") has")) ++ "joined", tale, False)
               "PART" -> (" " ++ (theuser "have" "has") ++ "left", whead mid, False)
               "NOTICE" -> ("NOTICE: " ++ rest, "", True)
               "NICK" -> (" " ++ (theuser "are" "is") ++ "now known as " ++ tale, "", False)
               "TOPIC" -> (" " ++ (theuser "have" "has") ++ "changed the topic to: " ++ tale, whead mid, True)
               "QUIT" -> (" " ++ (theuser "have" "has") ++ "quit" ++ " (" ++ tale ++ ")", "", False)
               "PONG" -> ("Ping reply from " ++ tale, "", True)
               "MODE" -> (" " ++ (theuser "have" "has") ++ "changed mode " ++ (wtail mid), whead mid, False)
               -- whois
               "311" -> let idname = wtail mid
                            uid = whead idname
                            rest' = (tail . wtail) idname
                            user' = whead rest'
                            machine = (whead . wtail) rest' in
                        (uid +-+ "is" +-+ tale +-+ "(" ++ user' ++ "@" ++ machine ++ ")", "", False)
               "312" -> ("on" +-+ (wtail . wtail) mid +-+ "(" ++ tale ++ ")", "", False)
               "317" -> let rest' = (wtail . wtail) mid in
                        (whead rest' ++ "s idle, on since " ++ (time $ read $ wtail rest'), "", False)
               "318" -> ("", "",False)
               "319" -> ("channels: " ++ tale, "", False)
               "331" -> (tale, wlast mid, False)
               "332" -> ("Topic: " ++ tale, wlast mid, False)
               "333" -> ("set by " ++ (wnth 3 mid) ++ " at " ++ (time $ read $ wlast mid), wnth 2 mid, False)
--                "353" -> (" Users: " ++ tale, (wlast mid), False)
               _ | and (map isDigit cmd) -> ((wtail mid) ++ " - " ++ tale, "", False)
               _ -> (" " ++ cmd +-+ rest, "", False)
    where
    rest = mid +-+ tale
    time :: Integer -> String
    time t = calendarTimeToString $ unsafePerformIO $ toCalendarTime $ TOD t 0 
    theuser :: String -> String -> String
    theuser second third =
        (if user == mynick then "You " ++ second else user +-+ third) ++ " "
    formatCTCP :: String -> String
    formatCTCP ctxt | whead ctxt == "ACTION" = user +-+ (wtail ctxt)
    formatCTCP ctxt = user ++ ": " ++ (wtail ctxt)

doPRIVMSG :: IRCMessage -> IRC ()
doPRIVMSG msg = do
  mynick <- getNick
  case msg of
      _ | mynick `elem` targets -> doPersonalMsg (whead text) (wtail text) mynick
      _ | mynick == take (length mynick) text
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
             maybecmd <- gets (\s -> Map.lookup cmd (ircCommands s))
             case maybecmd of
                           Just (MODULE m) -> botProcess m msg who cmd tale
                           Nothing         -> botApology who "Sorry, I don't know that command."
    doPersonalMsg ('\001':"PING") _ mynick = ircWrite $ mkIrcMessageWithPrefix mynick "NOTICE" (msgNick msg) text
    doPersonalMsg _ _ _ = do
        -- this should go away, once new channel tab creation fixed
        addChannel (msgNick msg) True
        liftIO beep
    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) tale mynick = do
        maybecmd <- gets (\s -> Map.lookup cmd (ircCommands s))
        case maybecmd of 
                      Just (MODULE m) -> botProcess m msg alltargets cmd tale
                      Nothing         -> botApology alltargets $ "Sorry, I don't know that command, try \"" ++ mynick ++ ": @listcommands\""
    doPublicMsg _ _ _ = do
        ircDisplayAlert True $ alltargets ++ ":<" ++ (msgNick msg) ++ "> " ++ text
        liftIO beep

processInput :: Interactive -> IRC ()
processInput (strss, ch) = do
  chn <- getIRCChannel ch
--   maybeDo_ chn $ \ chan -> debug "mchan" $ ch
  maybeDo_ chn $ \ chan -> processInput' strss chan
  where
  processInput' :: [String] -> IRCChannel -> IRC ()
  processInput' [] _ = return ()
  processInput' (str:strs) chan = do
    case parseCmd (words input) of
       Just (cmd,middle,tale,noslash) -> do
           nick <- getNick
           let realchan = chanreal chan
               text = (if noslash then "<" ++ nick ++ "> " else "") ++ input
               alltext = (if realchan then ch else "") ++ ":" ++ text
           when realchan $
                ircDisplay ch False text
           ircDisplayAll alltext
           logMessage alltext
           let msg = mkIrcMessage cmd middle tale
           case cmd of
                    "JOIN" | (not $ null middle) -> do
                              let chann = whead middle
                              chans <- getUserChannels nick
                              if elem chann chans || head chann == '%'
                                 then displayIRCchannel chann
                                 else if head chann == '#'
                                         then ircWrite msg
                                         else addChannel chann True
                    "CODING" | null tale ->
                              if null middle
                                 then ircDisplay ch False $ show $ chancoding chan
                                 else setChannelCoding ch
                                              (if null middle
                                                  then Nothing
                                                  else Just middle)
                    "PART" | isAlpha $ head middle ->
                               liftIO $ hideIRCchannel chan
                    _ -> ircWriteEnc ch msg
--            rchan <- gets ircReadChan
--            liftIO $ writeChan rchan (msg {msgPrefix = nick ++ "!"})
       Nothing -> return ()
    processInput' strs chan
    where
    input = if chanreal chan || head str == '/'
               then str
               else '/':str
    rest = wtail input
    parseCmd :: [String] -> Maybe (String, -- command
                                   String, -- middle
                                   String, -- tail
                                   Bool) -- noslash
    parseCmd [] = Nothing
    parseCmd (('/':cmd):[]) | and $ map isAlpha cmd =
      case cmd of
           "join" | publicChan -> Just (ircCmd cmd, ch, "", False)
           "mode" | publicChan -> Just (ircCmd cmd, ch, "", False)
           "names" | publicChan -> Just (ircCmd cmd, ch, "", False)
           "part" | chanreal chan -> Just (ircCmd cmd, ch, "", False)
           "topic" | publicChan -> Just (ircCmd cmd, "", ch, False)
           "quit" -> Just (ircCmd cmd, "", ("hircules" +-+ versionString), False)
           _ -> Just (ircCmd cmd, "", "", False)
    parseCmd (("/me"):_) | chanreal chan = Just ("PRIVMSG", ch, ctcpQuote $ "ACTION" +-+ rest, True)
    parseCmd (("/quit"):_) = Just ("QUIT", "", rest, True)
    parseCmd (("/msg"):_) = Just ("PRIVMSG", whead rest, wtail rest, True)
    parseCmd (("/mode"):_) | head rest /= '#' = Just ("MODE", ch +-+ rest, "", True)
    parseCmd (("/raw"):_) = Just (whead rest, wtail rest, "", True)
    parseCmd (("/topic"):_) | head rest /= '#' = Just ("TOPIC", ch, rest, True)
    parseCmd (('/':cmd):_) | and $ map isAlpha cmd =
      let (middle,tale) = breakString " :" rest in
      Just (ircCmd cmd,if null tale then whead middle else middle, if null tale then wtail middle else tale, False)
    parseCmd _ = 
      if (chanreal chan)
         then Just ("PRIVMSG", ch, input, True)
         else Nothing
    ircCmd :: String -> String
--     ircCmd "msg" = "PRIVMSG"
    ircCmd cmd = map toUpper cmd
--   startColon :: String -> Bool
--   startColon (':':_) = True
--   startColon _ = False
    publicChan :: Bool
    publicChan =
--       let name = channame chan in
      head ch == '#'
