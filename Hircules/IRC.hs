{-# OPTIONS_GHC -fglasgow-exts #-}

--  IRC.hs: an Internet Relay Chat library
--
--  Version: $Revision: 1.9 $ from $Date: 2008/11/03 03:14:11 $
--
--  Copyright (c) 2003 Andrew J. Bromage
--  Copyright (c) 2003, 2004, 2006, 2008-2010 Jens-Ulrik Holger Petersen
--
--  See the "COPYING" file for license information.

module Hircules.IRC (IRC,
            IRCMessage(..),
            IRCRState(..),
            IRCRWState(..),
            addChanUsers,
            addChannel,
            checkPrivs,
            displayIRCchannel,
            getChannels,
            getIRCChannel,
            getNick,
            getUserChannels,
            ircDisplay,
            ircDisplayAlert,
            ircDisplayAll,
            ircInput,
            ircInstallModule,
            ircJoin,
            ircPart,
            ircPrivmsg,
            ircRead,
            ircReady,
            {-ircSend,-}
            ircSetNick,
            ircSignOn,
            ircTopic,
            ircQuit,
            ircWrite,
            ircWriteEnc,
            {-deprecated-}ircnick,
            joinChanUser,
            logMessage,
            msgNick,
            msgUser,
            {-memberChannel,-}
            mkIrcMessage,
            mkIrcMessageWithPrefix,
            partChanUser,
            readerLoop,
            removeChannel,
            removeUser,
            renameUser,
            setChannelCoding,
            setNick,
            stripMS,
            writerLoop,
            Module(..),
            ModuleState(..),
            MODULE(..))
where

import Data.Maybe
import System.IO (Handle, hGetLine)
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toLower)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dynamic
import Data.IORef
import System.Exit
import System.IO (hClose, hIsEOF, hPutStrLn)

import Hircules.Channel
-- import Hircules.Charset
import Debug.State
import Hircules.EntryArea (setNickText)
import Hircules.GUI
import Control.Monad.MaybeDo
-- import Threads
import Codec.Binary.UTF8.String (encodeString)
import Text.WordString

data IRCRState
  = IRCRState ()

type ChannelList = Map String IRCChannel -- name chan

data IRCRWState
  = IRCRWState { ircServer      :: String
               , ircReadChan    :: Chan IRCMessage
               , ircReadThread  :: ThreadId
               , ircWriteChan   :: Chan IRCMessage
               , ircWriteThread :: ThreadId
               , ircSocket      :: Handle
               , ircLogFile     :: Handle
               , ircPrivilegedUsers :: Map String Bool
	       , ircChannels        :: ChannelList
	       , ircNick            :: String
	       , ircModules         :: Map String MODULE
	       , ircCommands        :: Map String MODULE
	       , ircModuleState     :: Map String (IORef ModuleState)
	       , ircUsers           :: Map String [String] -- nick chans
	       }

-- data IRCServer
--   = IRCServer { ircServer      :: String
--               , ircReadChan    :: Chan IRCMessage
--               , ircReadThread  :: ThreadId
--               , ircWriteChan   :: Chan IRCMessage
--               , ircWriteThread :: ThreadId
--               , ircSocket      :: Handle
--               , ircLogFile     :: Handle
--               , ircPrivilegedUsers :: Map String Bool
-- 	      , ircChannels        :: ChannelList
-- 	      , ircNick            :: String
-- 	      , ircModules         :: Map String MODULE
-- 	      , ircCommands        :: Map String MODULE
-- 	      , ircModuleState     :: Map String (IORef ModuleState)
-- 	      , ircUsers           :: Map String [String] -- nick chans
-- 	      }

data IRCMessage
  = IRCMessage {
        msgPrefix   :: String,
        msgCommand  :: String,
        msgMiddle   :: String,
	msgTail     :: String
  }

instance Show IRCMessage where
    showsPrec d (IRCMessage prefix cmd mid tale) = showParen (d >= 10) $ showString showStr
       where
       showStr = (if null prefix then "" else (':':prefix) ++ " ") ++ cmd ++
		 (if null mid then "" else " " ++ mid) ++
		 (if null tale then "" else " " ++ ':':tale)

msgNick :: IRCMessage -> String
msgNick msg = fst $ break (== '!') (msgPrefix msg)

msgUser :: IRCMessage -> String
msgUser msg =
    let rest = snd $ break (== '!') (msgPrefix msg) in
    if null rest
       then ""
       else case tail rest of
                "" -> ""
                ('~':cs) -> cs
                cs -> cs

-- (deprecated) lambdabot compatibility
ircnick :: IRCMessage -> String
ircnick = msgNick

data ModuleState = forall a. Typeable a => ModuleState a

type IRC a = StateT IRCRWState (ReaderT IRCRState IO) a

mkIrcMessage :: String -> String -> String -> IRCMessage
mkIrcMessage cmd middle tale
  = IRCMessage { msgPrefix = "", msgCommand = cmd, msgMiddle = middle, msgTail = tale }

mkIrcMessageWithPrefix :: String -> String -> String -> String -> IRCMessage
mkIrcMessageWithPrefix prefix cmd middle tale
  = IRCMessage { msgPrefix = prefix, msgCommand = cmd, msgMiddle = middle, msgTail = tale }

getNick :: IRC String
getNick = gets ircNick

setNick :: String -> IRC ()
setNick name = do
    modify (\ s -> s { ircNick = name })
    chans <- getIRCChannels
    liftIO $ mapM_ (\ chan -> setNickText chan name) $ filter chanreal chans

ircSignOn :: String -> String -> IRC ()
ircSignOn nick name
  = do  server <- gets ircServer
        ircWrite (mkIrcMessage "USER" (nick +-+ "localhost" +-+ server) name)
        ircWrite (mkIrcMessage "NICK" nick "")

ircSetNick :: String -> IRC ()
ircSetNick nick = ircWrite (mkIrcMessage "NICK" nick "")

-- ircGetChannels :: IRC [String]
-- ircGetChannels
--   = do  chans <- gets ircChannels
--         return (Map.keys chans)

getIRCChannel :: String -> IRC (Maybe IRCChannel)
getIRCChannel name = do
    chans <- gets ircChannels
    return $ Map.lookup (map toLower name) chans

getChannels :: IRC [Channel]
getChannels = do
    chans <- gets ircChannels
    return $ Map.keys chans

getIRCChannels :: IRC [IRCChannel]
getIRCChannels = do
    chans <- gets ircChannels
    return $ Map.elems chans

-- memberChannel :: String -> IRC Bool
-- memberChannel name = do
--     mchan <- getIRCChannel name
--     return $ isJust mchan

addChannel :: String -> Bool -> IRC ()
addChannel name real = do
    mchan <- getIRCChannel name
    case mchan of
      Just chan -> liftIO $ displayChannelTab True chan
      Nothing -> do
        let lname = map toLower name
        nick <- getNick
        chan <- liftIO $ addIRCchannel lname nick real
        modify (\ s -> s { ircChannels = Map.insert lname chan (ircChannels s) })
        joinChanUser nick name

removeChannel :: String -> IRC ()
removeChannel name = do
    modify (\ s -> s {ircChannels = Map.delete (map toLower name) (ircChannels s)})

ircPrivmsg :: String -> String -> IRC ()
ircPrivmsg who msg = do
    nick <- getNick
    if (who /= nick) then mapM_ ircPrivmsg' (lines msg) else return ()
  where
  ircPrivmsg' :: String -> IRC ()
  ircPrivmsg' line = ircWrite (mkIrcMessage "PRIVMSG" who line)

ircTopic :: String -> String -> IRC ()
ircTopic chan topic
  = ircWrite (mkIrcMessage "TOPIC" chan topic)

ircQuit :: IRC ()
ircQuit
  = do  ircWrite (mkIrcMessage "QUIT" "" "")
        liftIO (exitWith ExitSuccess)

ircJoin :: String -> IRC ()
ircJoin loc
  = ircWrite (mkIrcMessage "JOIN" loc "")

ircPart :: String -> IRC ()
ircPart loc
  = ircWrite (mkIrcMessage "PART" loc "")

ircReady :: Chan a -> IRC Bool
ircReady chan =
    liftIO $ liftM not $ isEmptyChan chan

ircRead :: IRC IRCMessage
ircRead = do
    chanR <- gets ircReadChan
    liftIO (readChan chanR)

ircWrite :: IRCMessage -> IRC ()
ircWrite msg = do
    s <- get
    liftIO $ writeChan (ircWriteChan s) msg

ircWriteEnc :: Channel -> IRCMessage -> IRC ()
ircWriteEnc ch msg = do
    s <- get
    chan <- getIRCChannel ch
    liftIO $ maybeDo_ chan $ \ chn -> writeChan (ircWriteChan s) (encodeMsg $ chancoding chn)
    where
    mid = msgMiddle msg
    tale = msgTail msg
    encodeMsg :: Maybe String -> IRCMessage
    encodeMsg _coding = msg { msgMiddle = mid,
                             msgTail = tale}

ircInput :: IRC Interactive
ircInput = liftIO (readChan chanI)

-- ircSend :: IRCMessage -> IRC ()
-- ircSend line
--   = do  h <- gets ircSocket
--         liftIO $ hPutStrLn h $ line ++ "\r"
-- 	ircDisplay rawchannel $ "i " ++ line
-- 	logMessage line
-- 	nick <- getNick
-- 	liftIO $ writeChan (ircReadChan s) (msg {msgPrefix = nick ++ "!"})

ircDisplay :: Channel -> Bool -> String -> IRC ()
ircDisplay ch alert str = do
    chan <- getIRCChannel ch
    liftIO $ maybeDo_ chan (\ chn -> writeTextLn chn alert str)

ircDisplayAll :: String -> IRC ()
ircDisplayAll str = liftIO $ writeTextLn allchannel False str

ircDisplayAlert :: Bool -> String -> IRC ()
ircDisplayAlert alert str = liftIO $ writeTextLn alertchannel alert str

readerLoop :: ThreadId -> Chan IRCMessage -> Chan IRCMessage -> Handle -> IO ()
readerLoop threadmain chanR chanW h
  = do  -- liftIO (putStrLn "Running reader loop...")
        catch readerLoop' (throwTo threadmain)
  where
    readerLoop' :: IO ()
    readerLoop'
      = do eof <- hIsEOF h
	   if eof
	      then hClose h
	      else do
		   input <- hGetLine h
                   debug "input" $ init input
		   -- remove trailing '^M'
		   let line = init input  -- [ c | c <- line, c /= '\n', c /= '\r' ]
		   debugDo $ writeTextRaw $ "r " ++ line
		   case (whead line) of
			"PING" -> writeChan chanW $ mkIrcMessage "PONG" "" (tail $ wtail line)
			_ -> writeChan chanR $ decodeMessage line
		   readerLoop'

-- removeLeadColon :: [String] -> [String]
-- removeLeadColon [] = []
-- removeLeadColon (t:ts) = (tail t):ts

writerLoop :: ThreadId -> Chan IRCMessage -> Handle -> IO ()
writerLoop threadmain chanW h
  = catch writerLoop' (throwTo threadmain)
  where
    writerLoop'
      = do msg <- readChan chanW
-- 	   putStrLn $ show msg
	   let encmsg = encodeMessage msg
	   debugDo $ writeTextRaw $ "w " ++ encmsg
           hPutStrLn h $ encmsg ++ "\r"
           writerLoop'

-- interactLoop :: ThreadId -> Chan Interactive -> Chan IRCMessage -> Handle -> IO ()
-- interactLoop threadmain chanI chanW h
--   = do exc <- try interactLoop'
--        case exc of
--            Left e  -> throwTo threadmain e
--            Right _ -> return ()
--   where
--     interactLoop'
--       = do (text, chan) <- readChan chanI
-- 	   let msg = mkIrcMessage target middle tail
-- 	       encmsg = encodeMessage msg
-- 	   debugDo $ writeTextRaw $ "i " ++ encmsg
--            hPutStr h encmsg
--            interactLoop'

encodeMessage :: IRCMessage -> String
encodeMessage msg =
    (encodePrefix (msgPrefix msg) 
     . encodeCommand (msgCommand msg)
     . encodeParams (msgMiddle msg)
     . encodeTail (msgTail msg)) ""
  where
    encodePrefix [] = id
    encodePrefix prefix = showChar ':' . showString prefix . showChar ' '
    encodeCommand cmd = showString cmd
    encodeParams [] = id
    encodeParams p = showChar ' ' . showString p
    encodeTail [] = id
    encodeTail t = showString " :" . showString t

decodeMessage :: String -> IRCMessage
decodeMessage txt =
    let first = whead txt
	(prefix,cmdparams) = if startColon first
			        then (tail first, wtail txt)
			        else ("",txt)
	(cmd, params) = (whead cmdparams, wtail cmdparams)
	(middle,tale) = breakString ":" params in
    IRCMessage { msgPrefix = prefix, msgCommand = cmd, msgMiddle = middle, msgTail = tale }

-- lowQuote :: String -> String
-- lowQuote [] = []
-- lowQuote ('\0':cs)   = '\020':'0'    : lowQuote cs
-- lowQuote ('\n':cs)   = '\020':'n'    : lowQuote cs
-- lowQuote ('\r':cs)   = '\020':'r'    : lowQuote cs
-- lowQuote ('\020':cs) = '\020':'\020' : lowQuote cs
-- lowQuote (c:cs)      = c : lowQuote cs
-- 
-- lowDequote :: String -> String
-- lowDequote [] = []
-- lowDequote ('\020':'0'   :cs) = '\0'   : lowDequote cs
-- lowDequote ('\020':'n'   :cs) = '\n'   : lowDequote cs
-- lowDequote ('\020':'r'   :cs) = '\r'   : lowDequote cs
-- lowDequote ('\020':'\020':cs) = '\020' : lowDequote cs
-- lowDequote ('\020'       :cs) = lowDequote cs
-- lowDequote (c:cs)             = c : lowDequote cs

-- ctcpQuote :: String -> String
-- ctcpQuote [] = []
-- ctcpQuote ('\001':cs) = '\134':'a'    : ctcpQuote cs
-- ctcpQuote ('\134':cs) = '\134':'\134' : ctcpQuote cs
-- ctcpQuote (c:cs)      = c : ctcpQuote cs
-- 
-- ctcpDequote :: String -> String
-- ctcpDequote [] = []
-- ctcpDequote ('\134':'a'   :cs) = '\001' : ctcpDequote cs
-- ctcpDequote ('\134':'\134':cs) = '\134' : ctcpDequote cs
-- ctcpDequote ('\134':cs)        = ctcpDequote cs
-- ctcpDequote (c:cs)             = c : ctcpDequote cs

class Module m where
  moduleName     :: m -> IRC String
  moduleSticky   :: m -> Bool
  commands       :: m -> IRC [String]
  moduleInit     :: m -> IRC ()
  process        :: m -> IRCMessage -> String -> String -> String -> IRC () -- msg target cmd rest
  moduleInit _ = return () -- by default, there's no initialisation

data MODULE = forall m. (Module m) => MODULE m

ircInstallModule :: (Module m) => m -> IRC ()
ircInstallModule modl
  = do  s <- get
        modname <- moduleName modl
        let modmap = ircModules s
        put (s { ircModules = Map.insert modname (MODULE modl) modmap })
        ircLoadModule modname

ircLoadModule :: String -> IRC ()
ircLoadModule modname = do
    maybemod   <- gets (\s -> Map.lookup modname (ircModules s))
    maybeDo_ maybemod $ \ (MODULE m) -> ircLoadModule' m >> moduleInit m
  where
    ircLoadModule' m
      = do  cmds <- commands m
            s <- get
            let cmdmap = ircCommands s        -- :: Map String MODULE
            put (s { ircCommands = foldr (\ cmd mp -> Map.insert cmd (MODULE m) mp) cmdmap cmds })

-- ircUnloadModule :: String -> IRC ()
-- ircUnloadModule modname
--     = do maybemod <- gets (\s -> lookup (ircModules s) modname)
--          case maybemod of
--                        Just (MODULE m) | moduleSticky m -> ircUnloadModule' m
--                        _ -> return ()
--     where
--     ircUnloadModule' m
--         = do modname <- moduleName m
--              cmds    <- commands m
--              s <- get
--              let modmap = ircModules s        -- :: Map String MODULE,
--                  cmdmap = ircCommands s        -- :: Map String MODULE
--                  in
--                  put (s { ircCommands = delListFrom cmdmap cmds })

-- for lambdabot
checkPrivs :: IRCMessage -> String -> IRC () -> IRC ()
checkPrivs msg target f = do 
                          maybepriv <- gets (\s -> Map.lookup (msgNick msg) (ircPrivilegedUsers s))
                          case maybepriv of
                                         Just _  -> f
                                         Nothing -> ircPrivmsg target "not enough privileges"

stripMS :: Typeable a => ModuleState -> a
stripMS (ModuleState x) = fromJust . fromDynamic . toDyn $ x

logMessage :: String -> IRC ()
logMessage txt = do
   h <- gets ircLogFile
   time <- liftIO timeStamp
   liftIO $ hPutStrLn h $ time +-+ (encodeString txt)

addChanUsers :: [(String,Bool)] -> String -> IRC ()
addChanUsers userops ch = do
    mchan <- getIRCChannel ch
    maybeDo_ mchan addChanUsers'
    mapM_ (addUserChan ch) $ map fst userops
  where
  addChanUsers' :: IRCChannel -> IRC ()
  addChanUsers' chan = do
      let userfm = chanusers chan
          chan' = chan { chanusers = foldr (\ (user,op) mp -> Map.insert user op mp) userfm userops }
      modify (\ s -> s { ircChannels = Map.insert ch chan' (ircChannels s) })

joinChanUser :: String -> String -> IRC ()
joinChanUser user ch = do
    mchan <- getIRCChannel ch
    maybeDo_ mchan addChanUser
    addUserChan ch user
  where
  addChanUser :: IRCChannel -> IRC ()
  addChanUser chan = do
      let userfm = chanusers chan
	  chan' = chan { chanusers = Map.insert user False userfm }
      modify (\ s -> s { ircChannels = Map.insert ch chan' (ircChannels s) })

partChanUser :: String -> String -> IRC ()
partChanUser user ch = do
    mchan <- getIRCChannel ch
    maybeDo_ mchan removeChanUser
    removeUserChan ch user
  where
  removeChanUser :: IRCChannel -> IRC ()
  removeChanUser chan = do
      let userfm = chanusers chan
	  chan' = chan { chanusers = Map.delete user userfm }
      modify (\ s -> s { ircChannels = Map.insert ch chan' (ircChannels s) })

-- userInChan :: String -> String -> IRC Bool
-- userInChan user ch = do
--     chans <- getUserChannels user
--     return $ elem ch chans

getUserChannels :: String -> IRC [String]
getUserChannels nick = do
    users <- gets ircUsers
    debug "users" $ Map.toList users
    let chans = Map.lookup (map toLower nick) users
    debug "getUserChannels" chans
    return $ fromMaybe [] chans

addUserChan :: String -> String -> IRC ()
addUserChan chan user = do
    chans <- getUserChannels user
    setUserChans user $ if elem chan chans
			   then chans
			   else chans ++ [chan]

removeUserChan :: String -> String -> IRC ()
removeUserChan chan user = do
    chans <- getUserChannels user
    setUserChans user $ delete chan chans

removeUser :: String -> IRC ()
removeUser user =
    modify (\ s -> s { ircUsers = Map.delete user (ircUsers s) })

setUserChans :: String -> [String] -> IRC ()
setUserChans user chans =
    modify (\ s -> s { ircUsers = Map.insert user chans (ircUsers s) })

renameUser :: Channel -> Channel -> IRC ()
renameUser old new = do
    chans <- getUserChannels old
    debug "renameUser" chans
    removeUser old
    setUserChans new chans
    mapM_ (partChanUser old) chans
    mapM_ (joinChanUser new) chans
    chanlist <- gets ircChannels
    when (Map.member old chanlist) $ renamePrivateChan chanlist
    debug "renameUser" "done"
  where
  renamePrivateChan :: ChannelList -> IRC ()
  renamePrivateChan chans = do
      let mchan = Map.lookup old chans
      maybeDo_ mchan $ \ chan -> do
          let chan' = chan { channame = new }
              chans' = Map.delete old chans
          modify (\ s -> s { ircChannels = Map.insert new chan' chans' })
          liftIO $ renameChannelTab old new

displayIRCchannel :: String -> IRC ()
displayIRCchannel ch = do
    chan <- getIRCChannel ch
    liftIO $ maybeDo_ chan (displayChannelTab True) 

setChannelCoding :: Channel -> Maybe String -> IRC ()
setChannelCoding ch coding = do
    chan <- getIRCChannel ch
    maybeDo_ chan $ \ chn -> modify (\ s -> s { ircChannels = Map.insert ch (chn { chancoding = coding }) (ircChannels s) })

-- getChannelCoding :: Channel -> IRC (Maybe String)
-- getChannelCoding ch = do
--     chan <- getIRCChannel ch
--     maybe (return Nothing) (\ chn -> return $ chancoding chn) chan
