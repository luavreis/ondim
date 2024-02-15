{- | This module defines a user-friendly API over the core functionality
 (implemented in Ondim.MultiWalk.Core).
-}
module Ondim
  (-- * Monad
    Ondim,
    runOndimWith,
    evalOndimWith,
    evalOndim,

    -- * Nodes
    children,
    attributes,
    identify,
    ondimCast,
    OndimNode,

    -- * Running templates
    expandNode,
    expandNodes,
    expandSubstructures,

    -- * Data types
    Expansion,
    PolyExpansion,

    -- * State transformations
    module Ondim.State,
    -- Get parts of the state
    getExpansion,
    getTemplate,
    getNamespace,
    getTemplate',
    getText,
    getText',
    -- Calling
    callExpansion,
    callTemplate,
    callText,

    -- * Rendering
    renderNode,
    renderNodeOrError,
    renderTemplateOrError,

    -- * Structure
    getSubstructure,
    expandChildren,
    lookupAttr,

    -- * Auxiliary
    Attribute,
  )
where

import Data.List qualified as L
import Ondim.MultiWalk.Basic
import Ondim.MultiWalk.Class
import Ondim.MultiWalk.Core
import Ondim.State
import Ondim.MultiWalk.Substructure
import Prelude hiding (All)

-- | Runs the Ondim action with a given initial state.
evalOndimWith ::
  Monad m =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException a)
evalOndimWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialTraceData
      `evalStateT` s

-- | Runs the Ondim action with a given initial state, and also return the final state.
runOndimWith ::
  Monad m =>
  OndimState m ->
  Ondim m a ->
  m (Either OndimException (a, OndimState m))
runOndimWith s o =
  runExceptT $
    unOndimT o
      `runReaderT` initialTraceData
      `runStateT` s

-- | Runs the Ondim action with empty initial state.
evalOndim :: Monad m => Ondim m a -> m (Either OndimException a)
evalOndim = evalOndimWith mempty

-- Children

-- | Returns the children of a node after expanding them.
expandChildren ::
  forall t m.
  (OndimNode t, Monad m) =>
  Expansion m t
expandChildren = expandNodes . children

-- Attributes

-- | Lookup an attribute from a node by name.
lookupAttr ::
  (Monad m, OndimNode t) =>
  Text ->
  t ->
  Ondim m (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

-- | Render node as bytestring, if possible, or fail.
renderNodeOrError :: (HasCallStack, Monad m) => OndimNode a => a -> Ondim m LByteString
renderNodeOrError =
  case renderNode of
    Just render -> return . render
    Nothing -> const $ throwTemplateError "This type cannot be rendered."

-- | Expand and then render template called 'name' to bytestring.
renderTemplateOrError :: (HasCallStack, Monad m) => Text -> Ondim m LByteString
renderTemplateOrError name = do
  mbValue <- Ondim $ gets (lookupExpansion name . expansions)
  case mbValue of
    Just (Template _ site thing) ->
      renderNodeOrError
        =<< withSite site (expandSubstructures thing)
    Just _ -> throwExpFailure @() name (FailureOther "Identifier not bound to a template.")
    Nothing -> throwExpFailure @() name NotBound

-- | Either applies template 'name', or throws a failure if it does not exist.
callTemplate :: forall t m. (OndimNode t, Monad m) => Text -> Ondim m [t]
callTemplate name = do
  exps <- getTemplate name
  either (throwExpFailure @t name) return exps

-- | Either applies text 'name', or throws a failure if it does not exist.
callText :: Monad m => Text -> Ondim m Text
callText name = do
  exps <- getText name
  either (throwExpFailure @Text name) return exps

-- | Either applies expansion 'name', or throws a failure if it does not exist.
callExpansion :: forall t m. (OndimNode t, Monad m) => Text -> Expansion m t
callExpansion name arg = do
  exps <- getExpansion name
  either (throwExpFailure @t name) ($ arg) exps
