import System.MIDI
import Control.Concurrent
import Control.Monad

openOutDev :: IO Connection
openOutDev = openDestination . head =<< filterM (fmap ("APC40 mkII" ==) . getName) =<< enumerateDestinations

setColor :: Connection -> Int -> Int -> IO ()
setColor dev note color = do
    send dev (MidiMessage 1 (NoteOn note color))
