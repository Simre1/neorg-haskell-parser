module Neorg.Parser.Document where
import Neorg.Parser.Base
import Neorg.Document
import Neorg.Parser.Block (blocks)


document :: Parser Document
document = Document <$> blocks
