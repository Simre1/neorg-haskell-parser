{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Types where

import Data.Default (Default, def)
import Data.Text (Text)
import Neorg.Document
import Optics.TH (makeLenses)
import qualified Text.Megaparsec as P

type Parser m a = forall e. ParserC e m => m a

type ParserC e m = (P.MonadParsec e Text m, MonadFail m, Ord e, P.ShowErrorComponent e)

type ParserE e m a = ParserC e m => m a

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserListLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)

makeLenses ''ParserState

-- type Parser = P.ParsecT Void Text (State ParserState)

instance Default ParserState where
  def = ParserState I0 I0 emptyDocumentMeta

--
-- newtype PureBlockInfo = PureBlockInfo {_pureBlockListLevel :: IndentationLevel}
--
-- instance Default PureBlockInfo where
--   def = PureBlockInfo I0
--
-- newtype BlockInfo = BlockInfo {_blockPureBlockInfo :: PureBlockInfo}
--
-- instance Default BlockInfo where
--   def = BlockInfo def
--
-- makeLenses 'BlockInfo
-- makeLenses 'PureBlockInfo
