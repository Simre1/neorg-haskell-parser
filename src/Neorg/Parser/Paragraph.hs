{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Paragraph where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State
  ( StateT (runStateT),
    gets,
    modify,
  )
import Data.Char (isLetter)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Parser.Types (Parser, ParserC)
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

runInline :: P.MonadParsec e Text p => StateT InlineState p () -> p Inline
runInline p = fmap canonalizeInline $ do
  (_, inlineState) <- runStateT p initialInlineState
  pure $ reduceModifierInline $ inlineState ^. modifierInline
  where
    reduceModifierInline :: ModifierInline -> Inline
    reduceModifierInline (NoModifier i) = i
    reduceModifierInline (OpenModifier c i b) = ConcatInline $ V.fromList [reduceModifierInline b, Text c, i]

paragraph' :: forall p e. (ParserC e p) => StateT InlineState p () -> StateT InlineState p ()
paragraph' end' =
  (end >> pure ())
    <|> (lookChar >>= \c -> intersectingModifier c <|> attachedOpenings c <|> word c)
  where
    end =
      P.lookAhead end' <|> P.eof
    appendInlineToStack :: Inline -> StateT InlineState p ()
    appendInlineToStack t =
      modify $
        modifierInline %~ \case
          NoModifier i -> NoModifier $ i <> t
          OpenModifier c i b -> OpenModifier c (i <> t) b
    popStack :: Text -> (Inline -> Inline) -> StateT InlineState p ()
    popStack c f = do
      s <- gets (view modifierInline)
      new <- close s
      modify $ modifierInline .~ new
      where
        close (NoModifier _) = fail "No closing"
        close (OpenModifier cm i b) =
          case b of
            (OpenModifier cd id' bd) -> if c == cm then pure (OpenModifier cd (id' <> f i) bd) else close (OpenModifier cd (id' <> Text cm <> i) bd)
            (NoModifier id') -> if c == cm then pure (NoModifier (id' <> f i)) else fail "No closing"
    pushStack :: Text -> StateT InlineState p ()
    pushStack c = do
      s <- gets (view modifierInline)
      new <- case s of
        NoModifier i -> pure $ OpenModifier c mempty (NoModifier i)
        stack@OpenModifier {} ->
          if not $ hasModifier c stack
            then pure $ OpenModifier c mempty stack
            else fail "No open modifier"
      modify $ modifierInline .~ new

    parseTextModifier :: StateT InlineState p () -> Text -> (Text -> Inline) -> StateT InlineState p ()
    parseTextModifier follow char f = P.string char >> P.try (go "") <|> word (T.head char)
      where
        go :: Text -> StateT InlineState p ()
        go previousText = do
          text <- P.takeWhileP (Just "Inline Text modifier") (\c -> c /= T.last char && c /= '\n' && c /= '\r')
          let fullText = previousText <> text
          ( do
              void $ P.string $ T.reverse char
              followedBy follow
              appendInlineToStack (f fullText)
              modify (delimitedActive .~ False)
              withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c
            )
            <|> (newline >> P.hspace >> newline >> fail "No Text modifier")
            <|> (newline >> P.hspace >> go fullText)

    attachedOpenings :: Char -> StateT InlineState p ()
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
            anyChar >> withNextChar (\c' -> guard $ isLetter c' || S.member c' specialSymbols)
            pushStack c
          modify (delimitedActive .~ False)
          withNextChar (\c -> P.choice [attachedOpenings c, word c])

    attachedClosings :: Char -> StateT InlineState p ()
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
          modify (delimitedActive .~ False)
          withNextChar $ \c' -> parWhitespace c' <|> attachedClosings c' <|> attachedOpenings c' <|> word c'

    word :: Char -> StateT InlineState p ()
    word c = do
      modify (delimitedActive .~ False)
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
                  \c' -> parWhitespace c' <|> attachedClosings c' <|> word c'
          )
            <|> ( do
                    p <- lift anyChar <&> T.pack . (: [])
                    appendInlineToStack (Text p)
                    withNextChar $ \c' -> parWhitespace c' <|> attachedClosings c' <|> attachedOpenings c' <|> word c'
                )
        else
          ( do
              w <- P.takeWhile1P (Just "Word") (\c' -> c' > ' ' && S.notMember c' (punctuationSymbols <> attachedModifierSymbols))
              appendInlineToStack (Text w)
              withNextChar $ \c' -> parWhitespace c' <|> attachedClosings c' <|> word c'
          )

    punctuationSymbols = S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~"
    attachedModifierSymbols = S.fromList "*/_-^,|`$="

    specialSymbols = attachedModifierSymbols <> punctuationSymbols

    withNextChar :: (Char -> StateT InlineState p ()) -> StateT InlineState p ()
    withNextChar f = end <|> (lookChar >>= f)

    intersectingModifier :: Char -> StateT InlineState p ()
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
          modify (delimitedActive .~ True)
          withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c

    parWhitespace :: Char -> StateT InlineState p ()
    parWhitespace c =
      intersectingModifier c <|> case c of
        ' ' -> do
          appendInlineToStack Space
          next
        '~' -> do
          P.try (anyChar >> P.hspace >> newline)
          next
        '\n' -> do
          lNewline
          modify (delimitedActive .~ True)
          next
        '\r' -> do
          lNewline
          modify (delimitedActive .~ True)
          next
        _ -> fail "No newline or space"
      where
        next = do
          P.hspace
          withNextChar (\c' -> parWhitespace c' <|> attachedOpenings c' <|> word c')

paragraph :: Parser p Inline
paragraph = runInline $ do
  modify $ delimitedActive .~ False
  paragraph' $
    doubleNewline
      <|> P.try
        ( do
            gets (view delimitedActive) >>= guard
            isMarkupElement >>= guard
        )

singleLineParagraph :: Parser p Inline
singleLineParagraph = runInline $ do
  modify $ delimitedActive .~ False
  paragraph' (void newline)
