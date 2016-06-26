-- | Utilities for processing a list of acl entries
module Gown.Processor where

import Gown.Parser
import Control.Monad
import qualified Data.Map as Map

sampleData = liftM extractSample parseAclsSample
  where extractSample (Right xs) = xs
        extractSample _ = []