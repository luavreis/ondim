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
  ) where

import Data.Text qualified as T
import Ondim
import Ondim.Debug
import Ondim.Extra.Exceptions (tryExp)
import Ondim.Extra.Expansions

standardMap :: NamespaceMap m
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

ifBound :: PolyExpansion m
ifBound node = do
  attrs <- T.split (== ',') <$> getSingleAttr' "id" node
  bound <- allM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookupExpansion n . expansions <$> getOndimS

anyBound :: PolyExpansion m
anyBound node = do
  attrs <- fst <<$>> attributes node
  bound <- anyM exists attrs
  ifElse bound node
  where
    exists n = isJust . lookupExpansion n . expansions <$> getOndimS

matchBound :: PolyExpansion m
matchBound node = do
  tag <- getSingleAttr' "id" node
  tagC <- getText tag
  switchWithDefault (rightToMaybe tagC) node

ignore :: PolyExpansion m
ignore = const $ pure []

-- * Environment

open :: PolyExpansion m
open node = do
  name <- getSingleAttr' "id" node
  exps <- getNamespace name
  withoutExpansions [name] $
    case exps of
      Right n -> withNamespace n $ expandChildren node
      Left e -> throwExpFailure @() name e

with :: PolyExpansion m
with node = do
  exps <- expansions <$> getOndimS
  actions <-
    attributes node <&> map \(k, v) ->
      let expansion = lookupExpansion v exps
       in withoutExpansions [v] . withSomeExpansion k expansion
  foldr ($) (expandChildren node) actions

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
scope :: PolyExpansion m
scope node = do
  s <- getOndimS
  expandChildren node <* putOndimS s

-- * Calling

call :: PolyExpansion m
call node = do
  name <- getSingleAttr' "id" node
  callExpansion name node

-- * Binding

-- | This expansion works like Heist's `bind` splice
bind :: (HasCallStack) => PolyExpansion m
bind node = do
  attrs <- attributes node
  defSite <- getCurrentSite
  let strict = any (("strict" ==) . fst) attrs
  thing <-
    if strict
      then expandChildren node
      else return $ children node
  case getSingleAttr "id" attrs of
    Just name -> do
      putSomeExpansion name $ templateData' defSite thing
      pure []
    Nothing -> throwTemplateError "No name for expansion"
