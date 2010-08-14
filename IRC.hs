--  IRC.hs: an Internet Relay Chat library
--
--  Version: $Revision: 1.4 $ from $Date: 2003/07/03 21:30:47 $
--
--  Copyright (c) 2003 Andrew J. Bromage
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
--
--  See the "COPYING" file for license information.

module IRC (IRC,
            IRCMessage(..),
            addChannel,
            checkPrivs,
            displayIRCchannel,
--             encodeMessage,
            getIRCChannel,
            getIRCChannels,
            getNick,
            getUserChannels,
            ircCommands,
            ircChannels,
            ircDisplay,
            ircInput,
            ircInstallModule,
            ircJoin,
            ircModules,
            ircModuleState,
            ircPart,
            ircPrivmsg,
            ircRead,
            ircReadChan,
            ircReady,
            {-ircSend,-}
            ircSignOn,
            ircQuit,
            ircWrite,
            {-deprecated-}ircnick,
            joinChanUser,
            logMessage,
            msgNick,
            msgUser,
            {-memberChannel,-}
            mkIrcMessage,
            mkIrcMessageWithPrefix,
            partChanUser,
            removeChannel,
            removeUser,
            renameUser,
            runIRC,
            setChanUsers,
            setNick,
            stripMS,
            Module(..),
            ModuleState(..),
            MODULE(..))
where

import Network
import Monad
import Maybe
import GHC.IO
import GHC.IOBase (Handle, BufferMode(..))
import GHC.Handle
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toLower)
import Data.List
import Data.FiniteMap
import Data.Dynamic
import Data.IORef
import System.IO (hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit

import Gtk

import Config (logDir)
import Debug
import Directories ((+/+))
import GUI
import MaybeDo
import Threads
import WordString

data IRCRState
  = IRCRState { ircServer      :: String
              , ircReadChan    :: Chan IRCMessage
              , ircReadThread  :: ThreadId
              , ircWriteChan   :: Chan IRCMessage
              , ircWriteThread :: ThreadId
              ,	ircSocket      :: Handle
              ,	ircLogFile     :: Handle
              }

data IRCRWState
  = IRCRWState { ircPrivilegedUsers :: FiniteMap String Bool
	       , ircChannels        :: FiniteMap String IRCChannel -- name chan
	       , ircNick            :: String
	       , ircModules         :: FiniteMap String MODULE
	       , ircCommands        :: FiniteMap String MODULE
	       , ircModuleState     :: FiniteMap String (IORef ModuleState)
	       , ircUsers           :: FiniteMap String [String] -- nick chans
	       }

data IRCMessage
  = IRCMessage {
        msgPrefix   :: String,
        msgCommand  :: String,
        msgMiddle   :: String,
	msgTail     :: String
  }

instance Show IRCMessage where
    showsPrec d (IRCMessage prefix cmd mid tail) = showParen (d >= 10) $ showString showStr
       where
       showStr = (if null prefix then "" else (':':prefix) ++ " ") ++ cmd ++
		 (if null mid then "" else " " ++ mid) ++
		 (if null tail then "" else " " ++ ':':tail)

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
ircnick = msgNick

data ModuleState = forall a. (Typeable a) => ModuleState a

type IRC a = StateT IRCRWState (ReaderT IRCRState IO) a

mkIrcMessage :: String -> String -> String -> IRCMessage
mkIrcMessage cmd middle tail
  = IRCMessage { msgPrefix = "", msgCommand = cmd, msgMiddle = middle, msgTail = tail }

mkIrcMessageWithPrefix :: String -> String -> String -> String -> IRCMessage
mkIrcMessageWithPrefix prefix cmd middle tail
  = IRCMessage { msgPrefix = prefix, msgCommand = cmd, msgMiddle = middle, msgTail = tail }

getNick :: IRC String
getNick = gets ircNick

setNick :: String -> IRC ()
setNick name = do
    modify (\ s -> s { ircNick = name })
    chans <- getIRCChannels
    liftIO $ mapM_ (\ c -> do
		    let lbl = chanlabel c
		    labelSetText lbl name
		    widgetShow lbl) chans

ircSignOn :: String -> String -> IRC ()
ircSignOn nick name
  = do  server <- asks ircServer
        ircWrite (mkIrcMessage "USER" (nick +-+ "localhost" +-+ server) name)
        ircWrite (mkIrcMessage "NICK" nick "")

ircGetChannels :: IRC [String]
ircGetChannels
  = do  chans <- gets ircChannels
        return (keysFM chans)

getIRCChannel :: String -> IRC (Maybe IRCChannel)
getIRCChannel name = do
    chans <- gets ircChannels
    return $ lookupFM chans (map toLower name)

getIRCChannels :: IRC [IRCChannel]
getIRCChannels = do
    chans <- gets ircChannels
    return $ eltsFM chans

-- memberChannel :: String -> IRC Bool
-- memberChannel name = do
--     mchan <- getIRCChannel name
--     return $ isJust mchan

addChannel :: String -> Bool -> IRC ()
addChannel name real = do
    mchan <- getIRCChannel name
    when (isNothing mchan) $ do
      let lname = map toLower name
      nick <- getNick
      chan <- liftIO $ addIRCchannel lname nick real
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) lname chan })
      joinChanUser nick name

removeChannel :: String -> IRC ()
removeChannel name = do
    modify (\ s -> s {ircChannels = delFromFM (ircChannels s) (map toLower name)})

ircPrivmsg :: String -> String -> IRC ()
ircPrivmsg who msg = do
    nick <- getNick
    if (who /= nick) then mapM_ (ircPrivmsg' who) (lines msg) else return ()
  where
  ircPrivmsg' :: String -> String -> IRC ()
  ircPrivmsg' who msg = ircWrite (mkIrcMessage "PRIVMSG" who msg)

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
ircRead
  = do  chanR <- asks ircReadChan
        liftIO (readChan chanR)

ircWrite :: IRCMessage -> IRC ()
ircWrite msg
  = do  s <- ask
        liftIO $ writeChan (ircWriteChan s) msg
-- 	nick <- getNick
-- 	liftIO $ writeChan (ircReadChan s) (msg {msgPrefix = nick ++ "!"})

ircInput :: IRC Interactive
ircInput = liftIO (readChan chanI)

-- ircSend :: IRCMessage -> IRC ()
-- ircSend line
--   = do  h <- asks ircSocket
--         liftIO $ hPutStrLn h $ line ++ "\r"
-- 	ircDisplay rawchannel $ "i " ++ line
-- 	logMessage line
-- 	nick <- getNick
-- 	liftIO $ writeChan (ircReadChan s) (msg {msgPrefix = nick ++ "!"})

ircDisplay :: IRCChannel -> String -> IRC ()
ircDisplay chan str = liftIO $ writeTextLn chan str

runIRC :: String -> (IRC ()) -> IO ()
runIRC server m
  = withSocketsDo $
    try (runIrc' hostname port m) >>= \exc ->
        case exc of
            Left exception -> putStrLn ("Exception: " ++ show exception)
            Right result   -> return result
    where
    (hostname,port') = break (== ':') server
    port = if null port' then "6667" else tail port'
	

runIrc' :: String -> String -> (IRC ()) -> IO ()
runIrc' hostname port m
  = do  s <- liftIO (connectTo hostname (PortNumber portnum))
        hSetBuffering s NoBuffering
        threadmain <- myThreadId
        chanR <- newChan
        chanW <- newChan
        threadr <- newThread (readerLoop threadmain chanR chanW s)
        threadw <- newThread (writerLoop threadmain chanW s)
	h <- liftIO $ openFile (logDir +/+ hostname) AppendMode
	hSetBuffering h LineBuffering
        let chans = IRCRState { ircServer      = hostname
                              , ircReadChan    = chanR
                              , ircReadThread  = threadr
                              , ircWriteChan   = chanW
                              , ircWriteThread = threadw
                              , ircSocket      = s
                              , ircLogFile     = h
			      }
        bracket_ (return ()) (killThread threadr >> killThread threadw)
		     (runReaderT (evalStateT m initState) chans)
  where
        portnum = fromIntegral (read port :: Integer)
        initState =
	    IRCRWState { ircPrivilegedUsers = listToFM [ (user,True) | user <- [] ]
		       , ircChannels = emptyFM
		       , ircNick = ""
		       , ircModules = emptyFM
		       , ircCommands = emptyFM
		       , ircModuleState = emptyFM
		       , ircUsers = emptyFM
                       }

readerLoop :: ThreadId -> Chan IRCMessage -> Chan IRCMessage -> Handle -> IO ()
readerLoop threadmain chanR chanW h
  = do  -- liftIO (putStrLn "Running reader loop...")
        exc <- try readerLoop'
        case exc of
           Left err -> throwTo threadmain err
           Right _ -> return ()
  where
    readerLoop'
      = do eof <- hIsEOF h
	   if eof
	      then hClose h
	      else do
		   input <- hGetLine h
--                 debug $ init input
		   -- remove trailing '^M'
		   let line = init input  -- [ c | c <- line, c /= '\n', c /= '\r' ]
		   writeTextLn rawchannel $ "r " ++ line
		   case (whead line) of
			"PING" -> writeChan chanW $ mkIrcMessage "PONG" "" (tail $ wtail line)
			_ -> writeChan chanR $ decodeMessage line
		   readerLoop'

-- removeLeadColon :: [String] -> [String]
-- removeLeadColon [] = []
-- removeLeadColon (t:ts) = (tail t):ts

writerLoop :: ThreadId -> Chan IRCMessage -> Handle -> IO ()
writerLoop threadmain chanW h
  = do exc <- try writerLoop'
       case exc of
           Left e  -> throwTo threadmain e
           Right _ -> return ()
  where
    writerLoop'
      = do msg <- readChan chanW
-- 	   putStrLn $ show msg
	   let encmsg = encodeMessage msg
	   writeTextLn rawchannel $ "w " ++ encmsg
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
-- 	   writeTextLn rawchannel $ "i " ++ encmsg
--            hPutStr h encmsg
--            interactLoop'

encodeMessage :: IRCMessage -> String
encodeMessage msg
  = (encodePrefix (msgPrefix msg) . encodeCommand (msgCommand msg)
          . encodeParams (msgMiddle msg) . encodeTail (msgTail msg)) ""
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

lowQuote :: String -> String
lowQuote [] = []
lowQuote ('\0':cs)   = '\020':'0'    : lowQuote cs
lowQuote ('\n':cs)   = '\020':'n'    : lowQuote cs
lowQuote ('\r':cs)   = '\020':'r'    : lowQuote cs
lowQuote ('\020':cs) = '\020':'\020' : lowQuote cs
lowQuote (c:cs)      = c : lowQuote cs

lowDequote :: String -> String
lowDequote [] = []
lowDequote ('\020':'0'   :cs) = '\0'   : lowDequote cs
lowDequote ('\020':'n'   :cs) = '\n'   : lowDequote cs
lowDequote ('\020':'r'   :cs) = '\r'   : lowDequote cs
lowDequote ('\020':'\020':cs) = '\020' : lowDequote cs
lowDequote ('\020'       :cs) = lowDequote cs
lowDequote (c:cs)             = c : lowDequote cs

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
ircInstallModule mod
  = do  s <- get
        modname <- moduleName mod
        let modmap = ircModules s
        put (s { ircModules = addToFM modmap modname (MODULE mod) })
        ircLoadModule modname
  where
    modname = moduleName mod

ircLoadModule :: String -> IRC ()
ircLoadModule modname
  = do  maybemod   <- gets (\s -> lookupFM (ircModules s) modname)
        case maybemod of
            Just (MODULE m) -> do ircLoadModule' m
				  moduleInit m
            Nothing         -> return ()
  where
    ircLoadModule' m
      = do  cmds <- commands m
            s <- get
            let cmdmap = ircCommands s        -- :: FiniteMap String MODULE
            put (s { ircCommands = addListToFM cmdmap [ (cmd,(MODULE m)) | cmd <- cmds ] })

ircUnloadModule :: String -> IRC ()
ircUnloadModule modname
    = do maybemod <- gets (\s -> lookupFM (ircModules s) modname)
         case maybemod of
                       Just (MODULE m) | moduleSticky m -> ircUnloadModule' m
                       _ -> return ()
    where
    ircUnloadModule' m
        = do modname <- moduleName m
             cmds    <- commands m
             s <- get
             let modmap = ircModules s        -- :: FiniteMap String MODULE,
                 cmdmap = ircCommands s        -- :: FiniteMap String MODULE
                 in
                 put (s { ircCommands = delListFromFM cmdmap cmds })

checkPrivs msg target f = do 
                          maybepriv <- gets (\s -> lookupFM (ircPrivilegedUsers s) (msgNick msg))
                          case maybepriv of
                                         Just x  -> f
                                         Nothing -> ircPrivmsg target "not enough privileges"

stripMS (ModuleState x) = fromJust . fromDynamic . toDyn $ x


logMessage :: String -> IRC ()
logMessage txt = do
   h <- asks ircLogFile
   time <- liftIO timeStamp
   liftIO $ hPutStrLn h $ time +-+ txt


setChanUsers :: [String] -> String -> IRC ()
setChanUsers users ch = do
    mchan <- getIRCChannel ch
    maybeDo (setChanUsers' users) mchan
    mapM_ (addUserChan ch) users
  where
  setChanUsers' :: [String] -> IRCChannel -> IRC ()
  setChanUsers' users chan = do
      let chan' = chan { chanusers = users }
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) ch chan' })

joinChanUser :: String -> String -> IRC ()
joinChanUser user ch = do
    mchan <- getIRCChannel ch
    maybeDo (addChanUser user) mchan
    addUserChan ch user
  where
  addChanUser :: String -> IRCChannel -> IRC ()
  addChanUser user chan = do
      let users = chanusers chan
	  chan' = chan { chanusers = users ++ [user] }
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) ch chan' })

partChanUser :: String -> String -> IRC ()
partChanUser user ch = do
    mchan <- getIRCChannel ch
    maybeDo (removeChanUser user) mchan
    removeUserChan ch user
  where
  removeChanUser :: String -> IRCChannel -> IRC ()
  removeChanUser user chan = do
      let users = chanusers chan
	  chan' = chan { chanusers = delete user users }
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) ch chan' })

userInChan :: String -> String -> IRC Bool
userInChan user ch = do
    chans <- getUserChannels user
    return $ elem ch chans

getUserChannels :: String -> IRC [String]
getUserChannels nick = do
    users <- gets ircUsers
    let chans = lookupFM users (map toLower nick)
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
    modify (\ s -> s { ircUsers = delFromFM (ircUsers s) user })

setUserChans :: String -> [String] -> IRC ()
setUserChans user chans =
    modify (\ s -> s { ircUsers = addToFM (ircUsers s) user chans })

renameUser :: String -> String -> IRC ()
renameUser old new = do
    chans <- getUserChannels old
    debug chans
    removeUser old
    setUserChans new chans
    mapM_ (partChanUser old) chans
    mapM_ (joinChanUser new) chans

displayIRCchannel :: String -> IRC ()
displayIRCchannel ch = do
    chan <- getIRCChannel ch
    liftIO $ maybeDo (displayChannelTab True) chan 
