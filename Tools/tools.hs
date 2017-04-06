import System.MIDI
import Control.Concurrent
import Control.Monad

openOutDev :: IO Connection
openOutDev = openDestination . head =<< filterM (fmap ("APC40 mkII" ==) . getName) =<< enumerateDestinations

openInDev :: IO Connection
openInDev = do
    conn <- flip openSource Nothing . head =<< filterM (fmap ("APC40 mkII" ==) . getName) =<< enumerateSources
    start conn
    return conn

setColor :: Connection -> Int -> Int -> IO ()
setColor dev note color = do
    send dev (MidiMessage 1 (NoteOn note color))

colorArray :: Connection -> Int -> IO ()
colorArray dev startColor = do
    forM_ [0..39] $ \i -> setColor dev i (startColor+i)

-- mode is 0, 1, or 2
setMode :: Connection -> Int -> IO ()
setMode dev mode = do
    sendSysEx dev [0x47, 0x7f, 0x29, 0x60, 0x00, 0x04, 0x40 + fromIntegral mode, 0x00 {- version high -}, 0x00 {-version low -}, 0x00 {-bugfix level-} ]
