module Neorg.ParserState where

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)
