module Channel (Channel, IRCChannel(..))
where
import Data.Map (Map)
import Graphics.UI.Gtk (Container, TextBuffer, TextMark, TextView)

type Channel = String

data IRCChannel = IRCChan { chanbuffer :: TextBuffer
                          , channame :: String
                          , chanreal :: Bool
                          , chanbox :: Container
                          , chanend :: TextMark
                          , channick :: TextMark
                          , chanentry :: TextMark
                          , chanview :: TextView
                          , chanusers :: Map String Bool -- (nick,op)
                          , chantopic :: String
                          , chancoding :: Maybe String}
