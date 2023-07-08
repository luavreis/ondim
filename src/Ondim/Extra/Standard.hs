module Ondim.Extra.Standard
  ( standardMap,
    ifBound,
    anyBound,
    matchBound,
    ignore,
    open,
    with,
    scope,
    call,
    bind,
    attrSub,
  ) where

import Data.Text qualified as T
import Ondim
import Ondim.Extra.Exceptions (tryExp)
import Ondim.Extra.Expansions

standardMap :: ExpansionMap m
standardMap = do
  -- Control flow
  "if" #* ifBound
  "any" #* anyBound
  "match" #* matchBound
  "try" #* tryExp
  "ignore" #* ignore
  -- Environment
  "open" #* open
  "with" #* with
  "scope" #* scope
  -- Calling
  "call" #* call
  -- Binding
  "bind" #* bind

-- * Control flow

ifBound :: forall t m. GlobalConstraints m t => Expansion m t
ifBound node = do
  attrs <- fst <<$>> attributes node
  bound <- allM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookupExpansion n . expansions <$> getOndimS

anyBound :: forall t m. GlobalConstraints m t => Expansion m t
anyBound node = do
  attrs <- fst <<$>> attributes node
  bound <- anyM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookupExpansion n . expansions <$> getOndimS

matchBound :: GlobalExpansion m
matchBound node = do
  tag <- getSingleAttr' "exp" node
  tagC <- getTemplateFold tag
  switchWithDefault (rightToMaybe tagC) node

ignore :: forall t m. Monad m => Expansion m t
ignore = const $ pure []

-- * Environment

open :: GlobalExpansion m
open node = do
  name' <- viaNonEmpty (fst . head) <$> attributes node
  name <- maybe (throwTemplateError "Namespace not provided.") pure name'
  exps <- getNamespace name
  withoutExpansions [name] $
    case exps of
      Right n -> withNamespace n $ liftChildren node
      Left e -> throwExpFailure @() name e

with :: GlobalExpansion m
with node = do
  exps <- expansions <$> getOndimS
  actions <-
    attributes node <&> map \(k, v) ->
      let expansion = lookupExpansion v exps
       in withoutExpansions [v] . withSomeExpansion k expansion
  foldr ($) (liftChildren node) actions

{- | This expansion creates a new scope for the its children, in the sense that
 the inner state does not leak outside.

  For this reason, it can be used to call other expansions with "arguments":

   > <bind animal-entry>There is a <animal /> with age <age /></bind>
   >
   > <scope>
   >   <bind animal>Lion</bind>
   >   <bind age>9 years</bind>
   >   <animal-entry />
   > <scope/>
-}
scope :: forall t m. GlobalConstraints m t => Expansion m t
scope node = do
  s <- getOndimS
  liftChildren node <* putOndimS s

-- * Calling

call :: GlobalExpansion m
call node = do
  attrs <- attributes node
  (name, _rest) <-
    case nonEmpty attrs of
      Just (name :| rest) -> return (fst name, rest)
      Nothing -> throwTemplateError "Expansion name not provided."
  callExpansion name node

-- * Binding

-- | This expansion works like Heist's `bind` splice
bind :: HasCallStack => GlobalExpansion m
bind node = do
  attrs <- attributes node
  defSite <- getCurrentSite
  let strict = any (("strict" ==) . fst) attrs
  thing <-
    if strict
      then liftChildren node
      else return $ children node
  case getSingleAttr "name" attrs of
    Just name -> do
      putSomeExpansion name $ templateData' defSite thing
      pure []
    Nothing -> throwTemplateError "No name for expansion"

-- * String interpolation

-- | Simple text interpolation ${name}.
attrEdit :: Monad m => Char -> (Char, Char) -> Text -> Ondim m Text
attrEdit start delims = go
  where
    go text = do
      let (beg, rest0) = T.break (== start) text
      (beg <>) <$> case T.uncons rest0 of
        Just (_, rest1)
          | Just (c0, rest2) <- T.uncons rest1,
            c0 == fst delims,
            let (name, rest3) = T.break (== snd delims) rest2,
            Just (_, rest4) <- T.uncons rest3 -> do
              res <- callTemplateFold name
              (res <>) <$> go rest4
          | otherwise -> T.cons start <$> go rest1
        _noStartChar -> return mempty

attrSub :: Monad m => Char -> (Char, Char) -> MapFilter m Text
attrSub start delims t = mapM (attrEdit start delims) =<< t
