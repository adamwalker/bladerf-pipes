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
    rxFrequency  :: Int,
    rxSampleRate :: Int,
    rxBandwidth  :: Int,
    rxVGA1       :: Int,
    rxVGA2       :: Int,
    lnaGain      :: BladeRFLNAGain
} 

bladeRFSource :: DeviceHandle
              -> BladeRFRxConfig
              -> EitherT String IO (Producer (VS.Vector CShort) IO ())
bladeRFSource dev BladeRFRxConfig{..} = do

    lift $ do
        info <- getInfo dev
        print info

    EitherT $ mapLeft show <$> bladeRFSetFrequency dev MODULE_RX rxFrequency
    actualSampleRate <- lift $ bladeRFSetSampleRate dev MODULE_RX rxSampleRate
    actualBandWidth  <- lift $ bladeRFSetBandwidth  dev MODULE_RX rxBandwidth

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

data BladeRFTxConfig = BladeRFTxConfig {
    txFrequency  :: Int,
    txSampleRate :: Int,
    txBandwidth  :: Int,
    txVGA1       :: Int,
    txVGA2       :: Int
} 

bladeRFSink :: DeviceHandle
            -> BladeRFTxConfig
            -> EitherT String IO (Consumer (VS.Vector CShort) IO ())
bladeRFSink dev BladeRFTxConfig{..} = do

    lift $ do
        info <- getInfo dev
        print info

    EitherT $ mapLeft show <$> bladeRFSetFrequency  dev MODULE_TX txFrequency
    actualSampleRate <- lift $ bladeRFSetSampleRate dev MODULE_TX txSampleRate
    actualBandWidth  <- lift $ bladeRFSetBandwidth  dev MODULE_TX txBandwidth

    lift $ do
        putStrLn $ "Set sample rate to: " ++ show actualSampleRate
        putStrLn $ "Set bandwidth to: " ++ show actualBandWidth

    EitherT $ mapLeft show <$> bladeRFSetTXVGA1  dev txVGA1
    EitherT $ mapLeft show <$> bladeRFSetTXVGA2  dev txVGA2

    EitherT $ mapLeft show <$> bladeRFSyncConfig dev MODULE_TX FORMAT_SC16_Q11 16 8192 8 3500
    EitherT $ mapLeft show <$> bladeRFEnableModule dev MODULE_TX True

    return $ forever $ do
        dat <- await
        ret <- lift $ bladeRFSyncTx dev (fromVector dat) Nothing 5000
        case ret of
            Right ret -> return ()
            Left  err -> lift $ print err

