{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Types where

import Cleff
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Void
import Effect.Logging
import Neorg.Document
import Optics.TH (makeLenses)
import qualified Text.Megaparsec as P

type Parser :: [Effect] -> * -> *
type Parser effs = P.ParsecT Void Text (Eff effs)

-- type ParserC e m = (P.MonadParsec e Text m, MonadFail m, Ord e, P.ShowErrorComponent e)

-- type ParserE e m a = ParserC e m => m a

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserListLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)

makeLenses ''ParserState

instance Default ParserState where
  def = ParserState I0 I0 emptyDocumentMeta
