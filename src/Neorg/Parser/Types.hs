{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Types where

import Control.Monad.Trans.State (State)
import Data.Text (Text)
import Data.Void (Void)
import Neorg.Document
import Optics.TH (makeLenses)
import qualified Text.Megaparsec as P

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)

makeLenses ''ParserState

type Parser = P.ParsecT Void Text (State ParserState)

defaultParserState :: ParserState
defaultParserState = ParserState I0 emptyDocumentMeta
