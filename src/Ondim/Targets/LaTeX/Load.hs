module Ondim.Targets.LaTeX.Load where

import Control.Exception (throw)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT (runNoLoggingT))
import Ondim
import Ondim.Extra.Loading
import Ondim.Targets.LaTeX.Instances
import Text.LaTeX.Base.Parser qualified as L
import Text.LaTeX.Base.Syntax qualified as L

loadTemplatesDynamic ::
  forall m n.
  (Monad n, MonadLogger m, MonadIO m, MonadUnliftIO m) =>
  [FilePath] ->
  m (OndimState n, (OndimState n -> m ()) -> m ())
loadTemplatesDynamic =
  loadTemplatesDynamic' patts ins
  where
    patts = [((), "**/*.tex")]
    ins () name (decodeUtf8 -> text) s =
      let template =
            either
              (throw . TemplateLoadingException . show)
              fromDocument
              (L.parseLaTeX text)
       in s {expansions = insertExpansion name (toSomeExpansion template) (expansions s)}

loadTemplates :: Monad n => [FilePath] -> IO (OndimState n)
loadTemplates dirs = fst <$> runNoLoggingT (loadTemplatesDynamic dirs)

-- * Template loading helpers

fromDocument :: Monad m => L.LaTeX -> Expansion m TeXNode
fromDocument = fromTemplate . fromHaTeX
