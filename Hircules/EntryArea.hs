module Hircules.EntryArea
where

import Hircules.Channel
import Debug.State
import Graphics.UI.Gtk

setEditable :: IRCChannel -> IO ()
setEditable chan = do
  let mark = chanentry chan
      buffer = chanbuffer chan
  start <- textBufferGetIterAtMark buffer mark
  end <- textBufferGetEndIter buffer
  textBufferApplyTagByName buffer "editable" start end

setNickText :: IRCChannel -> String -> IO ()
setNickText chan nick = do
  let endm = channick chan
  start <- getNickStart
  debug "setNickText" "1"
  end <- textBufferGetIterAtMark buffer endm
  debug "setNickText" "2"
  textBufferDelete buffer start end
  debug "setNickText" "3"
  begin <- getNickStart
  debug "setNickText" "4"
  textBufferInsert buffer begin nick
  debug "setNickText" "5"
  where
  endc = chanend chan
  buffer = chanbuffer chan
  getNickStart :: IO TextIter
  getNickStart = do
    start <- textBufferGetIterAtMark buffer endc
    _ <- textIterForwardChar start
    return start
