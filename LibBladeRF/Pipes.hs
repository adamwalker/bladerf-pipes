{-# LANGUAGE RecordWildCards #-}
module LibBladeRF.Pipes where

import Control.Monad
import Foreign.C.Types
import Foreign.ForeignPtr
import Data.ByteString hiding (putStrLn)
import Data.ByteString.Internal
import qualified Data.Vector.Storable as VS

import Control.Monad.Trans.Either
import Data.Either.Combinators
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
    (fp, len) = VS.unsafeToForeignPtr0 v

getInfo :: DeviceHandle -> IO BladeRFInfo
getInfo dev = BladeRFInfo 
    <$> bladeRFLibVersion
    <*> bladeRFFwVersion   dev
    <*> bladeRFFPGAVersion dev
    <*> bladeRFDeviceSpeed dev
    <*> bladeRFGetDevInfo  dev
    <*> bladeRFGetSerial   dev
    <*> bladeRFGetFPGASize dev

data BladeRFRxConfig = BladeRFRxConfig {
    frequency  :: Int,
    sampleRate :: Int,
    bandwidth  :: Int,
    rxVGA1     :: Int,
    rxVGA2     :: Int,
    lnaGain    :: BladeRFLNAGain
} 

bladeRFSource :: DeviceHandle
              -> BladeRFRxConfig
              -> EitherT String IO (Producer (VS.Vector CShort) IO ())
bladeRFSource dev BladeRFRxConfig{..} = do

    lift $ do
        info <- getInfo dev
        print info

    EitherT $ mapLeft show <$> bladeRFSetFrequency dev MODULE_RX frequency
    actualSampleRate <- lift $ bladeRFSetSampleRate dev MODULE_RX sampleRate
    actualBandWidth  <- lift $ bladeRFSetBandwidth  dev MODULE_RX bandwidth

    lift $ do
        putStrLn $ "Set sample rate to: " ++ show actualSampleRate
        putStrLn $ "Set bandwidth to: " ++ show actualBandWidth

    EitherT $ mapLeft show <$> bladeRFSetRXVGA1  dev rxVGA1
    EitherT $ mapLeft show <$> bladeRFSetRXVGA2  dev rxVGA2
    EitherT $ mapLeft show <$> bladeRFSetLNAGain dev lnaGain

    EitherT $ mapLeft show <$> bladeRFSyncConfig dev MODULE_RX FORMAT_SC16_Q11 16 8192 8 3500
    EitherT $ mapLeft show <$> bladeRFEnableModule dev MODULE_RX True

    lift $ print "Starting BladeRF RX"

    return $ forever $ do
        ret <- lift $ bladeRFSyncRx dev 8192 5000
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

