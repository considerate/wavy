module Sound.Wav.Core where

import Sound.Wav.Data

import Data.Binary.Get
import Data.Maybe (Maybe(..))

-- TODO Perhaps move this out into a core file
lookAheadRFV :: Get (RFV a) -> Get (RFV a)
lookAheadRFV = fmap maybeToRFV . lookAheadM . fmap rfvToMaybe
   where
      rfvToMaybe :: RFV a -> Maybe a
      rfvToMaybe (ValidRFV x) = Just x
      rfvToMaybe _            = Nothing

      maybeToRFV :: Maybe a -> RFV a
      maybeToRFV (Just x) = ValidRFV x
      maybeToRFV _        = NoDataRFV

