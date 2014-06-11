{-# OPTIONS_GHC #-}

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
            displayIRCchannel,
            getChannels,
            getIRCChannel,
            getNick,
            getUserChannels,
            ircDisplay,
            ircDisplayAlert,
            ircDisplayAll,
            ircInput,
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
            writerLoop)
where

import Data.Maybe
import Control.Concurrent
import Control.Exception (catch)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toLower)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit
import System.IO (Handle, hGetLine, hClose, hIsEOF, hPutStrLn)

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
	       , ircUsers           :: Map String [String] -- nick chans
	       }

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
msgNick msg = takeWhile (/= '!') (msgPrefix msg)

msgUser :: IRCMessage -> String
msgUser msg =
    let rest = dropWhile (/= '!') (msgPrefix msg) in
    if null rest
       then ""
       else case tail rest of
                "" -> ""
                ('~':cs) -> cs
                cs -> cs

-- (deprecated) lambdabot compatibility
ircnick :: IRCMessage -> String
ircnick = msgNick

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
    liftIO $ mapM_ (`setNickText` name) $ filter chanreal chans

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
removeChannel name =
    modify (\ s -> s {ircChannels = Map.delete (map toLower name) (ircChannels s)})

ircPrivmsg :: String -> String -> IRC ()
ircPrivmsg who msg = do
    nick <- getNick
    when (who /= nick) $ mapM_ ircPrivmsg' (lines msg)
  where
  ircPrivmsg' :: String -> IRC ()
  ircPrivmsg' line = ircWrite (mkIrcMessage "PRIVMSG" who line)

ircTopic :: String -> String -> IRC ()
ircTopic chan topic
  = ircWrite (mkIrcMessage "TOPIC" chan topic)

ircQuit :: IRC ()
ircQuit
  = do  ircWrite (mkIrcMessage "QUIT" "" "")
        liftIO exitSuccess

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
  = readerLoop' `catch` (throwTo threadmain :: IOError -> IO ())
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
		   case whead line of
			"PING" -> writeChan chanW $ mkIrcMessage "PONG" "" (tail $ wtail line)
			_ -> writeChan chanR $ decodeMessage line
		   readerLoop'

-- removeLeadColon :: [String] -> [String]
-- removeLeadColon [] = []
-- removeLeadColon (t:ts) = (tail t):ts

writerLoop :: ThreadId -> Chan IRCMessage -> Handle -> IO ()
writerLoop threadmain chanW h
  = writerLoop' `catch` (throwTo threadmain :: IOError -> IO ())
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
    encodeCommand = showString
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

logMessage :: String -> IRC ()
logMessage txt = do
   h <- gets ircLogFile
   time <- liftIO timeStamp
   liftIO $ hPutStrLn h $ time +-+ encodeString txt

addChanUsers :: [(String,Bool)] -> String -> IRC ()
addChanUsers userops ch = do
    mchan <- getIRCChannel ch
    maybeDo_ mchan addChanUsers'
    mapM_ (addUserChan ch . fst) userops
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
    setUserChans user $ if chan `elem` chans
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
