{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ondim.MultiWalk.Class where

import Control.MultiWalk.HasSub (AllMods, GSubTag, SubSpec)
import Ondim.MultiWalk.Basic
import {-# SOURCE #-} Ondim.MultiWalk.Core
import {-# SOURCE #-} Ondim.MultiWalk.Substructure

-- * Class

class
  ( HasSub GSubTag (ExpTypes t) t,
    AllMods CanLift (ExpTypes t),
    AllMods (Substructure t) (ExpTypes t),
    Typeable t
  ) =>
  OndimNode t
  where
  type ExpTypes t :: [SubSpec]
  identify :: t -> Maybe Text
  identify _ = Nothing
  fromText :: Maybe (Text -> [t])
  fromText = Nothing
  attributes :: Monad m => t -> Ondim m [Attribute]
  attributes _ = pure []