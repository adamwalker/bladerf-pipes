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

bladeRFPipe :: Int
            -> Int
            -> Int 
            -> IO (Producer (VS.Vector CShort) IO ())
bladeRFPipe frequency sampleRate bandwidth = do
    dev <- openBladeRF

    info <-  BladeRFInfo 
         <$> bladeRFLibVersion
         <*> bladeRFFwVersion   dev
         <*> bladeRFFPGAVersion dev
         <*> bladeRFDeviceSpeed dev
         <*> bladeRFGetDevInfo  dev
         <*> bladeRFGetSerial   dev
         <*> bladeRFGetFPGASize dev

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

    return $ forever $ do
        ret <- lift $ bladeRFSyncRx dev 10000 5000
        case ret of
            Right ret  -> do
                yield $ toVector $ fst ret
            Left err -> lift $ print err

