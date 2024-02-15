{-# LANGUAGE RankNTypes #-}

module Ondim.State
  ( -- * Namespace maps

    NamespaceMap,
    binding,
    mapToNamespace,
    (#:),
    unbind,

    -- ** Typed expansions
    (##),
    typedExpansion,
    typedExpansion',

    -- ** Polymorphic expansions
    (#*),
    polyExpansion,
    polyExpansion',

    -- ** Templates
    (#%),
    templateData,
    templateData',

    -- ** Textual templates
    (#@),
    textData,
    textData',

    -- ** Namespaces
    (#.),
    namespace,
    namespace',

    -- * Datatypes
    OndimState (..),
    NamespaceItem,
    Namespace,

    -- ** Manipulate the whole state
    getOndimS,
    modifyOndimS,
    putOndimS,

    -- ** Modify parts of the state
    withSomeExpansion,
    putSomeExpansion,
    withoutExpansions,
    withNamespace,

    -- ** Altering namespaces
    splitExpansionKey,
    lookupExpansion,
    insertExpansion,
    deleteExpansion,
    lookupExpansion',
    insertExpansion',
    deleteExpansion',
  )
where

import Data.Char (isLetter)
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as T
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class (OndimNode)
import Type.Reflection (typeRep)

-- * User API

-- State manipulation

getOndimS :: (Monad m) => Ondim m (OndimState m)
getOndimS = Ondim get

modifyOndimS :: (Monad m) => (OndimState m -> OndimState m) -> Ondim m ()
modifyOndimS = Ondim . modify'

putOndimS :: (Monad m) => OndimState m -> Ondim m ()
putOndimS = Ondim . put

withSomeExpansion ::
  (Monad m) =>
  Text ->
  Maybe (NamespaceItem m) ->
  Ondim m a ->
  Ondim m a
withSomeExpansion name ex st = do
  pEx <- Ondim $ gets (lookupExpansion name . expansions)
  Ondim $ modify' \s -> s {expansions = insOrDel ex (expansions s)}
  st <* modifyOndimS \s -> s {expansions = insOrDel pEx (expansions s)}
  where
    insOrDel = maybe (deleteExpansion name) (insertExpansion name)

-- | "Bind" new namespace locally.
withNamespace :: (Monad m) => Namespace m -> Ondim m a -> Ondim m a
withNamespace (Namespace exps) o =
  foldr (\(k, v) -> withSomeExpansion k (Just v)) o (HMap.toList exps)

-- | "Unbind" many expansions locally.
withoutExpansions :: (Monad m) => [Text] -> Ondim m a -> Ondim m a
withoutExpansions names o = foldr (`withSomeExpansion` Nothing) o names

-- | Put a new expansion into the local state, modifying the scope.
putSomeExpansion :: (Monad m) => Text -> NamespaceItem m -> Ondim m ()
putSomeExpansion key ex =
  modifyOndimS \s -> s {expansions = insertExpansion key ex (expansions s)}

infixr 0 #<>

(#<>) :: Text -> Maybe (NamespaceItem m) -> NamespaceMap m
name #<> ex = NamespaceMapM $ modify' ((name, ex) :)

unbind :: Text -> NamespaceMap m
unbind k = k #<> Nothing

-- Expansions

newtype NamespaceMapM m a = NamespaceMapM (State [(Text, Maybe (NamespaceItem m))] a)
  deriving newtype (Functor, Applicative, Monad)

{- | 'NamespaceMap' provides a monad interface for defining namespaces
   declaratively. You should enable the @BlockArguments@ extension when using
   it. For instance:

@
'Ondim.expandChildren' myNode
  \`'binding'\` do
    "something" '##' myTypedExp
    "is-cow" '#*' ifElse isCow
    "name" '#@' \"Joana\"
    "properties" '#.' do
      "age" '#@' show age
      "state" '#@' "hungry"
@
-}
type NamespaceMap m = NamespaceMapM m ()

infixr 0 #:

(#:) :: Text -> NamespaceItem m -> NamespaceMap m
name #: ex = name #<> Just ex

typedExpansion :: (HasCallStack, Typeable t) => Expansion m t -> NamespaceItem m
typedExpansion = TypedExpansion typeRep callStackSite

typedExpansion' :: (Typeable t) => DefinitionSite -> Expansion m t -> NamespaceItem m
typedExpansion' = TypedExpansion typeRep

infixr 0 ##

(##) :: (HasCallStack, Typeable t) => Text -> Expansion m t -> NamespaceMap m
name ## ex = name #: typedExpansion ex

templateData :: forall a m. (HasCallStack, OndimNode a) => a -> NamespaceItem m
templateData = Template typeRep callStackSite

templateData' :: forall a m. (HasCallStack, OndimNode a) => DefinitionSite -> a -> NamespaceItem m
templateData' = Template typeRep

infixr 0 #%

(#%) :: (HasCallStack, OndimNode a) => Text -> a -> NamespaceMap m
name #% ex = name #: templateData ex

textData :: (HasCallStack) => Text -> NamespaceItem m
textData = templateData

textData' :: DefinitionSite -> Text -> NamespaceItem m
textData' = templateData'

infixr 0 #@

(#@) :: (HasCallStack) => Text -> Text -> NamespaceMap m
name #@ ex = name #% ex

polyExpansion :: (HasCallStack) => PolyExpansion m -> NamespaceItem m
polyExpansion = PolyExpansion callStackSite

polyExpansion' :: DefinitionSite -> PolyExpansion m -> NamespaceItem m
polyExpansion' = PolyExpansion

infixr 0 #*

(#*) :: (HasCallStack) => Text -> PolyExpansion m -> NamespaceMap m
name #* ex = name #: polyExpansion ex

-- | Runs the 'NamespaceMap' monad to get a 'Namespace'.
mapToNamespace :: NamespaceMap m -> Namespace m
mapToNamespace (NamespaceMapM ex) = foldl' go mempty exps
  where
    go = flip $ uncurry insertExpansion
    exps = mapMaybe sequence $ execState ex []

namespace :: NamespaceMap m -> NamespaceItem m
namespace = NamespaceData . mapToNamespace

namespace' :: Namespace m -> NamespaceItem m
namespace' = NamespaceData

infixr 0 #.

(#.) :: Text -> NamespaceMap m -> NamespaceMap m
name #. ex = name #: namespace ex

{- | Infix version of 'withNamespace' meant to bind expansions more conveniently
   by using 'NamespaceMap's.
-}
binding ::
  (Monad m) =>
  Ondim m a ->
  NamespaceMap m ->
  Ondim m a
binding o (NamespaceMapM exps) =
  let kvs = execState exps []
   in foldl' (flip $ uncurry withSomeExpansion) o kvs

splitExpansionKey :: Text -> [Text]
splitExpansionKey = T.split (\c -> c /= '-' && not (isLetter c))

lookupExpansion' :: [Text] -> Namespace m -> Maybe (NamespaceItem m)
lookupExpansion' keys (Namespace e) = go keys e
  where
    go [] _ = Nothing
    go [k] m = HMap.lookup k m
    go (k : ks) m = case HMap.lookup k m of
      Just (NamespaceData (Namespace n)) -> go ks n
      _ -> Nothing

lookupExpansion :: Text -> Namespace m -> Maybe (NamespaceItem m)
lookupExpansion = lookupExpansion' . splitExpansionKey

insertExpansion' :: [Text] -> NamespaceItem m -> Namespace m -> Namespace m
insertExpansion' keys e (Namespace es) = Namespace $ go keys es
  where
    go [] = id
    go [k] = HMap.insert k e
    go (k : ks) =
      flip HMap.alter k $
        Just
          . NamespaceData
          . Namespace
          . \case
            Just (NamespaceData (Namespace n)) -> go ks n
            _ -> go ks mempty

insertExpansion :: Text -> NamespaceItem m -> Namespace m -> Namespace m
insertExpansion = insertExpansion' . splitExpansionKey

deleteExpansion' :: [Text] -> Namespace m -> Namespace m
deleteExpansion' keys v@(Namespace es) =
  case keys of
    [] -> v
    [k] -> Namespace $ HMap.delete k es
    (k : ks) -> Namespace $ go ks k es
  where
    go ks = HMap.update \case
      (NamespaceData n) ->
        case deleteExpansion' ks n of
          x@(Namespace hmap)
            | HMap.null hmap -> Nothing
            | otherwise -> Just $ NamespaceData x
      x -> Just x

deleteExpansion :: Text -> Namespace m -> Namespace m
deleteExpansion = deleteExpansion' . splitExpansionKey
