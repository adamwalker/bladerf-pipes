module LibBladeRF.Pipes where

import Control.Monad
import Foreign.C.Types

import Foreign.ForeignPtr

import Data.ByteString
import Data.ByteString.Internal

import Data.Vector.Storable as VS

import Pipes
import qualified Pipes.Prelude as P

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Types
import LibBladeRF.Frequency
import LibBladeRF.Sampling
import LibBladeRF.Gain
import LibBladeRF.Sync

data BladeRFInfo = BladeRFInfo {
    libVersion  :: BladeRFVersion,
    fwVersion   :: BladeRFVersion,
    fpgaVersion :: BladeRFVersion,
    deviceSpeed :: BladeRFSpeed,
    deviceInfo  :: BladeRFDeviceInfo,
    serial      :: String,
    fpgaSize    :: BladeRFFPGASize 
} deriving (Show)

toVector :: ByteString -> VS.Vector CShort
toVector (PS p o l) = VS.unsafeFromForeignPtr (castForeignPtr p) o (l `quot` 2)

fromVector :: VS.Vector CShort -> ByteString
fromVector v = fromForeignPtr (castForeignPtr fp) 0 len 
    where
    (fp, len) = unsafeToForeignPtr0 v

getInfo :: DeviceHandle -> IO BladeRFInfo
getInfo dev = BladeRFInfo 
    <$> bladeRFLibVersion
    <*> bladeRFFwVersion   dev
    <*> bladeRFFPGAVersion dev
    <*> bladeRFDeviceSpeed dev
    <*> bladeRFGetDevInfo  dev
    <*> bladeRFGetSerial   dev
    <*> bladeRFGetFPGASize dev

bladeRFSource :: DeviceHandle
              -> Int
              -> Int
              -> Int 
              -> IO (Producer (VS.Vector CShort) IO ())
bladeRFSource dev frequency sampleRate bandwidth = do
    info <- getInfo dev
    print info

    ret1             <- bladeRFSetFrequency  dev MODULE_RX frequency
    actualSampleRate <- bladeRFSetSampleRate dev MODULE_RX sampleRate
    actualBandWidth  <- bladeRFSetBandwidth  dev MODULE_RX bandwidth

    ret2 <- bladeRFSetRXVGA1  dev 30
    ret3 <- bladeRFSetRXVGA2  dev 3
    ret4 <- bladeRFSetLNAGain dev LNA_GAIN_MAX

    print ret1
    print actualSampleRate
    print actualBandWidth

    print ret2
    print ret3
    print ret4

    ret <- bladeRFSyncConfig dev MODULE_RX FORMAT_SC16_Q11 16 8192 8 3500
    print ret

    ret <- bladeRFEnableModule dev MODULE_RX True
    print ret

    print "starting"

    return $ forever $ do
        ret <- lift $ bladeRFSyncRx dev 10000 5000
        case ret of
            Right ret  -> do
                yield $ toVector $ fst ret
            Left err -> lift $ print err

bladeRFSink :: DeviceHandle
            -> Int
            -> Int
            -> Int
            -> IO (Consumer (VS.Vector CShort) IO ())
bladeRFSink dev frequency sampleRate bandwidth = do
    info <- getInfo dev
    print info

    ret1             <- bladeRFSetFrequency  dev MODULE_TX frequency
    actualSampleRate <- bladeRFSetSampleRate dev MODULE_TX sampleRate
    actualBandWidth  <- bladeRFSetBandwidth  dev MODULE_TX bandwidth

    ret2 <- bladeRFSetTXVGA1  dev (-10)
    ret3 <- bladeRFSetTXVGA2  dev 20

    print ret1
    print actualSampleRate
    print actualBandWidth

    print ret2
    print ret3

    ret <- bladeRFSyncConfig dev MODULE_TX FORMAT_SC16_Q11 16 8192 8 3500
    print ret

    ret <- bladeRFEnableModule dev MODULE_TX True
    print ret

    return $ forever $ do
        dat <- await
        ret <- lift $ bladeRFSyncTx dev (fromVector dat) Nothing 5000
        case ret of
            Right ret -> return ()
            Left  err -> lift $ print err

