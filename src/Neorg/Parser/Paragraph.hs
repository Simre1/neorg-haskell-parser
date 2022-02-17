{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Paragraph where

import Cleff
import Cleff.State
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Data.Char (isAlphaNum, isLetter)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Parser.Types (Parser)
import Neorg.Parser.Utils
import Optics.Core (view, (%~), (.~), (<&>), (^.))
import Optics.TH (makeLenses)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data InlineState = InlineState {_modifierInline :: ModifierInline, _delimitedActive :: Bool} deriving (Show)

data ModifierInline = NoModifier Inline | OpenModifier Text Inline ModifierInline deriving (Show)

makeLenses ''InlineState

makeLenses ''ModifierInline

hasModifier :: Text -> ModifierInline -> Bool
hasModifier _ (NoModifier _) = False
hasModifier c1 (OpenModifier c2 _ b) = c1 == c2 || hasModifier c1 b

initialInlineState :: InlineState
initialInlineState = InlineState (NoModifier (ConcatInline V.empty)) False

runInline :: Parser (State InlineState ': es) () -> Parser es Inline
runInline p = fmap canonalizeInline $ do
  inlineState <- runParserState initialInlineState (p >> lift get)
  pure $ reduceModifierInline $ inlineState ^. modifierInline
  where
    reduceModifierInline :: ModifierInline -> Inline
    reduceModifierInline (NoModifier i) = i
    reduceModifierInline (OpenModifier c i b) = ConcatInline $ V.fromList [reduceModifierInline b, Text c, i]

-- | Builds up the paragraph within InlineState until the end parser matches
paragraph' :: forall es. State InlineState :> es => Parser es () -> Parser es ()
paragraph' end' =
  (end >> pure ())
    <|> (P.hspace >> lookChar >>= \c -> attachedOpenings c <|> everywhere c)
  where
    end =
      end' <|> P.eof
    appendInlineToStack :: Inline -> Parser es ()
    appendInlineToStack t =
      lift $
        modify $
          modifierInline %~ \case
            NoModifier i -> NoModifier $ i <> t
            OpenModifier c i b -> OpenModifier c (i <> t) b
    popStack :: Text -> (Inline -> Inline) -> Parser es ()
    popStack c f = do
      s <- lift $ gets (view modifierInline)
      new <- close s
      lift $ modify $ modifierInline .~ new
      where
        close (NoModifier _) = fail "No closing"
        close (OpenModifier cm i b) =
          case b of
            (OpenModifier cd id' bd) -> if c == cm then pure (OpenModifier cd (id' <> f i) bd) else close (OpenModifier cd (id' <> Text cm <> i) bd)
            (NoModifier id') -> if c == cm then pure (NoModifier (id' <> f i)) else fail "No closing"
    pushStack :: Text -> Parser es ()
    pushStack c = do
      s <- lift $ gets (view modifierInline)
      new <- case s of
        NoModifier i -> pure $ OpenModifier c mempty (NoModifier i)
        stack@OpenModifier {} ->
          if not $ hasModifier c stack
            then pure $ OpenModifier c mempty stack
            else fail "No open modifier"
      lift $ modify $ modifierInline .~ new

    parseTextModifier :: Parser es () -> Text -> (Text -> Inline) -> Parser es ()
    parseTextModifier follow char f = P.string char >> P.try (go "") <|> word (T.head char)
      where
        go :: Text -> Parser es ()
        go previousText = do
          text <- P.takeWhileP (Just "Inline Text modifier") (\c -> c /= T.last char && c /= '\n' && c /= '\r')
          let fullText = previousText <> text
          ( do
              void $ P.string $ T.reverse char
              followedBy follow
              appendInlineToStack (f fullText)
              lift $ modify (delimitedActive .~ False)
              withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> everywhere c
            )
            <|> (newline >> P.hspace >> newline >> fail "No Text modifier")
            <|> (newline >> P.hspace >> go fullText)

    attachedOpenings :: Char -> Parser es ()
    attachedOpenings = \case
      '*' -> parseOpening "*"
      '/' -> parseOpening "/"
      '_' -> parseOpening "_"
      '-' -> parseOpening "-"
      ',' -> parseOpening ","
      '|' -> parseOpening "|"
      '^' -> parseOpening "^"
      '`' -> parseTextModifier follow "`" Verbatim
      '$' -> parseTextModifier follow "$" Math
      -- '=' -> parseOpening TODO: Behavior unclear
      _ -> fail "No attachedOpenings"
      where
        follow =
          singleSpace <|> lNewline
            <|> withNextChar
              (guard . flip S.member (punctuationSymbols <> attachedModifierSymbols))
        parseOpening c = do
          P.try $ do
            anyChar >> withNextChar (\c' -> guard $ (isAlphaNum c' || isLetter c') || S.member c' specialSymbols)
            pushStack c
            lift $ modify (delimitedActive .~ False)
            withNextChar (\cn -> attachedOpenings cn <|> everywhere cn)

    attachedClosings :: Char -> Parser es ()
    attachedClosings = \case
      '*' -> parseClosing "*" Bold
      '/' -> parseClosing "/" Italic
      '_' -> parseClosing "_" Underline
      '-' -> parseClosing "-" Strikethrough
      '^' -> parseClosing "^" Superscript
      ',' -> parseClosing "," Subscript
      '|' -> parseClosing "|" Spoiler
      _ -> fail "No attached closings"
      where
        parseClosing c f = do
          P.try $ do
            P.string c
              >> followedBy
                ( singleSpace <|> lNewline
                    <|> withNextChar
                      (guard . flip S.member (punctuationSymbols <> attachedModifierSymbols))
                )
            popStack c f
          lift $ modify (delimitedActive .~ False)
          withNextChar $ \c' -> parWhitespace c' <|> attachedClosings c' <|> attachedOpenings c' <|> everywhere c'

    word :: Char -> Parser es ()
    word c = do
      lift $ modify (delimitedActive .~ False)
      if S.member c (punctuationSymbols <> attachedModifierSymbols)
        then
          ( P.try
              ( do
                  guard (c == '\\')
                  a <- anyChar >> anyChar
                  guard (a > ' ')
                  pure a
              )
              >>= \x -> do
                appendInlineToStack
                  (Text $ T.pack [x])
                withNextChar $
                  \c' -> parWhitespace c' <|> attachedClosings c' <|> attachedOpenings c' <|> everywhere c'
          )
            <|> ( do
                    p <- anyChar <&> T.pack . (: [])
                    appendInlineToStack (Text p)
                    withNextChar $ \c' -> parWhitespace c' <|> attachedClosings c' <|> attachedOpenings c' <|> everywhere c'
                )
        else
          ( do
              w <- P.takeWhile1P (Just "Word") (\c' -> c' > ' ' && S.notMember c' (punctuationSymbols <> attachedModifierSymbols))
              appendInlineToStack (Text w)
              withNextChar $ \c' -> parWhitespace c' <|> attachedClosings c' <|> everywhere c'
          )

    punctuationSymbols = S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~"
    attachedModifierSymbols = S.fromList "*/_-^,|`$="

    specialSymbols = attachedModifierSymbols <> punctuationSymbols

    withNextChar :: (Char -> Parser es ()) -> Parser es ()
    withNextChar f = end <|> (lookChar >>= f)

    intersectingModifier :: Char -> Parser es ()
    intersectingModifier c1 = do
      c2 <- followedBy $ anyChar >> anyChar
      case c1 : [c2] of
        ":*" -> intersectingOpen ":*"
        ":/" -> intersectingOpen ":/"
        ":_" -> intersectingOpen ":_"
        ":-" -> intersectingOpen ":-"
        ":^" -> intersectingOpen ":^"
        ":," -> intersectingOpen ":,"
        ":|" -> intersectingOpen ":|"
        ":`" -> parseTextModifier (pure ()) ":`" Verbatim
        ":$" -> parseTextModifier (pure ()) ":$" Math
        "*:" -> intersectingClosed ":*" Bold
        "/:" -> intersectingClosed ":/" Italic
        "_:" -> intersectingClosed ":_" Underline
        "-:" -> intersectingClosed ":-" Strikethrough
        "^:" -> intersectingClosed ":^" Superscript
        ",:" -> intersectingClosed ":," Subscript
        "|:" -> intersectingClosed ":|" Spoiler
        _ -> fail "No intersecting modifier"
      where
        intersectingClosed mod' f = do
          void $ P.string $ T.reverse mod'
          popStack mod' f
          next
        intersectingOpen mod' = do
          void $ P.string mod'
          pushStack mod'
          next
        next = do
          lift $ modify (delimitedActive .~ True)
          withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> everywhere c

    parWhitespace :: Char -> Parser es ()
    parWhitespace c =
      case c of
        ' ' -> do
          appendInlineToStack Space
          next
        '~' -> do
          P.try (anyChar >> P.hspace >> newline)
          next
        '\n' -> do
          lNewline
          lift $ modify (delimitedActive .~ True)
          next
        '\r' -> do
          lNewline
          lift $ modify (delimitedActive .~ True)
          next
        _ -> fail "No newline or space"
      where
        next = do
          P.hspace
          withNextChar (\c' -> parWhitespace c' <|> attachedOpenings c' <|> word c')
    link :: Char -> Parser es ()
    link = \case
      '{' -> do
        P.try $ do
          target <- linkTarget
          text <- linkText
          appendInlineToStack $ Link $ LinkCons target (Just text) Nothing
        pure ()
      c -> do
        fail "No link"
      where
        linkTarget = do
          _ <- P.char '{'
          text <- P.takeWhileP (Just "Inline Text modifier") (\c -> c /= '}' && c /= '\n' && c /= '\r')
          _ <- P.char '}'
          pure $ LinkTargetUrl text
        linkText = do
          _ <- P.char '['
          para <- runInline $ do
            lift $ modify $ delimitedActive .~ False
            paragraph' $ P.lookAhead $ void (P.char ']') <|> newline
          _ <- P.char ']'
          pure para
    everywhere c = link c <|> intersectingModifier c <|> word c

paragraph :: forall es. Parser es Inline
paragraph = runInline $ do
  lift $ modify $ delimitedActive .~ False
  paragraph' $
    P.lookAhead $
      doubleNewline
        <|> P.try
          ( do
              lift (gets (view delimitedActive)) >>= guard
              isMarkupElement >>= guard
          )

singleLineParagraph :: Parser p Inline
singleLineParagraph = runInline $ do
  lift $ modify $ delimitedActive .~ False
  paragraph' (P.lookAhead newline)
