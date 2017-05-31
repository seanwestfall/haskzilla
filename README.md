Haskzilla
---------

A toy web browser written in Haskell.

Credits:
- How Browsers Work: Behind the scenes of modern web browsers by Tali Garsiel and Paul Irish [http://www.html5rocks.com/en/tutorials/internals/howbrowserswork/](http://www.html5rocks.com/en/tutorials/internals/howbrowserswork/)
- Let's build a browser engine! by Matt Brubeck [http://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html](http://www.html5rocks.com/en/tutorials/internals/howbrowserswork/)
- Let's Build a Browser Engine in Haskell by Leif Grele [http://hrothen.github.io/2014/09/05/lets-build-a-browser-engine-in-haskell/](http://hrothen.github.io/2014/09/05/lets-build-a-browser-engine-in-haskell/)
- The offical W3C HTML5 spec by the W3C [http://www.w3.org/TR/html5/](http://www.w3.org/TR/html5/)

### How a Browser Works
![Modzilla Diagram](/image008.jpg)

### Contents
  1. [HTML Parser](#html-parser)
  1. [CSS parsing](#css-parsing)
  1. [The order of processing scripts and style sheets](#order-of-processing)
  1. [Render tree construction](#render-tree)
  1. [Layout](#layout)
  1. [Painting](#painting)

## HTML Parser

The job of an HTML parser is to parse through HTML markup and build a parse tree call the DOM, or the Document Object Model. 

The following HTML,
```html
<html>
  <body>
    Hello world
  </body>
</html>
```
will form the parse tree:

![parse tree](https://github.com/seanwestfall/haskzilla/blob/master/parse_tree.png)

As you can see from above, there are basically two type of tokens the HTML parser has to parser through: (1) markup tags, and (2) text-- and within the markup tags: open tags, closing tags, and attributes.

In Haskell, that leaves us with these data types:
```haskell
-- The DOM tree
data NTree a = NTree a [NTree a]
  deriving (Show)

-- Our two data types that make up the tree: Text and Markup Element Data
data NodeType = Text T.Text | Element ElementData
  deriving (Show)
```

Valid HTML elements are specified by the W3C organiztion in a formal spec, see [www.w3.org/DOM/DOMTR](www.w3.org/DOM/DOMTR), and algorithm that parsers through these elements is formally defined here: [http://www.w3.org/TR/html5/syntax.html#html-parser](http://www.w3.org/TR/html5/syntax.html#html-parser).

HTML parser are very similar to tradition context free grammar parsers (the parsers of turing complete programming languages) but are not the same thing, since they have to be fault tolerance and capable of handling syntax with errors (such as missing and/or mismatching tags and such).

The basic flow of the algorithm is: 

The initial state is the "Data state". When the < character is encountered, the state is changed to "Tag open state". Consuming an a-z character causes creation of a "Start tag token", the state is changed to "Tag name state". We stay in this state until the > character is consumed. Each character is appended to the new token name. In our case the created token is an html token.

When the > tag is reached, the current token is emitted and the state changes back to the "Data state". The \<body\> tag will be treated by the same steps. So far the html and body tags were emitted. We are now back at the "Data state". Consuming the H character of Hello world will cause creation and emitting of a character token, this goes on until the < of <\/body> is reached. We will emit a character token for each character of Hello world.

We are now back at the "Tag open state". Consuming the next input / will cause creation of an end tag token and a move to the "Tag name state". Again we stay in this state until we reach >.Then the new tag token will be emitted and we go back to the "Data state". The <\/html> input will be treated like the previous case.

![state diagram](https://github.com/seanwestfall/haskzilla/blob/master/state_diagram.png)

In Haskell,
```haskell
data Parser = Parser T.Text

Type ParserS = ExceptT T.Text (StateT Parser Identity)

runParserS p s = evalState (runExceptT p) s

nextchr :: Parser -> Char
nextchr (Parser s) = T.head s -- errors if called when string is empty

startsWith :: Parser -> T.Text -> Bool
startsWith (Parser input) s = s `T.isPrefixOf` input

eof :: Parser -> Bool
Eof (Parser input) = T.null input

consumeChar :: ParserS Char
consumeChar = do
    (Parser inp) <- get
    case T.uncons inp of
      Nothing -> throwError "ERROR: unexpectedly reached end of file"
      Just (c,inp') -> do
        put (Parser inp')
        return c

consumeWhile :: (Char -> Bool) -> ParserS T.Text
consumeWhile f = do
    Parser input <- get
    let (s,input') = T.span f input
    put $ Parser input'
    return s

consumeWhitespace :: ParserS T.Text
ConsumeWhitespace = consumeWhile (==' ')

assert :: T.Text -> Bool -> ParserS ()
Assert s b = if b then return () else throwError s


parseTagName :: ParserS T.Text
parseTagName = consumeWhile isAlphaNum


parseNode :: ParserS Node
parseNode = do
    p <- get
    if nextchr p == '<' then parseElement else parseText

parseText :: ParserS Node
ParseText = liftM Dom.text $ consumeWhile (/='<') Create your models here

parseElement :: ParserS Node
parseElement = do
    -- open tag
    consumeChar >>= assert "missing < in open tag" . (=='<')
    tag <- parseTagName
    attrs <- parseAttributes
    consumeChar >>= assert "missing > in open tag" . (=='>')
    -- contents
    children <- parseNodes
    --end tag
    consumeChar  >>= assert "missing < in close tag" . (=='<')
    consumeChar  >>= assert "missing / in close tag" . (=='/')
    parseTagName >>= assert "end tag doesn't match start tag" . (==tag)
    consumeChar  >>= assert "missing > in close tag" . (=='>')

    return $ Dom.elem tag attrs children

parseNodes :: ParserS [Node]
parseNodes = parseNodes' []
  where
    parseNodes' nodes = do
        consumeWhitespace
        p <- get
        if eof p || p `startsWith` "</"
        then return nodes
        else parseNode >>= parseNodes' . (nodes++) . (:[])  --slow for big DOM Create your models here.


parseHtml :: T.Text -> Either T.Text Node
parseHtml s = case runParserS parseNodes (Parser s) of
              Left err -> Left err
              Right nodes -> Right $
                if length nodes == 1
                then head nodes
                else Dom.elem "html" HM.empty nodes Create your models here.
```

## CSS parsing

```haskell
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module CSS
    ( Stylesheet(..)
    , Rule(..)
    , Selector(..)
    , Declaration(..)
    , Value(..)
    , Unit(..)
    , parseCSS
    , selectors
    , declarations
    ) where

import Prelude hiding (id)

import Data.Word (Word(..), Word8(..))
import Data.List (sortBy)
import Data.Maybe (maybe)
import Numeric (readFloat, readHex)
import Control.Applicative ((<*), (*>), (<$>), (<*>))

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

data Stylesheet = Stylesheet [Rule]
  deriving (Show, Eq)

data Rule = Rule [Selector] [Declaration]
  deriving (Show, Eq)

-- only handle simple selectors for now
data Selector = Simple (Maybe T.Text) (Maybe T.Text) [T.Text]
  deriving (Show, Eq)

data Declaration = Declaration T.Text Value
  deriving (Show, Eq)

data Value = Keyword T.Text
           | Color Word8 Word8 Word8 Word8
           | Length Float Unit
  deriving (Show, Eq)

data Unit = Px --only Px for now
  deriving (Show, Eq)

-- an empty selector
nilS = Simple Nothing Nothing []

-- parse an entire CSS document into a Stylesheet
parseCSS :: T.Text -> Either ParseError Stylesheet
parseCSS css = case runParser rules nilS "" css of
    Left err -> Left err
    Right rs -> Right (Stylesheet rs)

rules = spaces >> manyTill (rule <* spaces) eof

rule = Rule <$> selectors <*> declarations

selectors = (sortBy comp) <$> sepBy1 (selector <* spaces) comma
  where comma = char ',' <* spaces
        comp a b = spec a `compare` spec b

type Specificity = (Word,Word,Word)

-- compute the specificity of a Selector
spec :: Selector -> Specificity
spec (Simple name id cls) = (maybeLen id, fromIntegral $ length cls, maybeLen name)
  where maybeLen = fromIntegral . maybe 0 T.length

-- manyTill, but the terminal parser is optional
manyUnless p end = many ((notFollowedBy end) *> p)

-- parse a simple selector
selector = do
    putState nilS
    manyUnless (id <|> cls <|> univ <|> name) eof
    getState


-- selector id
id = do
    char '#'
    i <- identifier
    modifyState (\(Simple n _ cs) -> Simple n (Just i) cs)

-- selector class
cls = do
    char '.'
    c <- identifier
    modifyState (\(Simple n i cs) -> Simple n i (cs++[c]))

-- universal selector
univ = char '*' >> return ()

-- selector name
name = do
    n' <- validId
    n  <- identifier
    let nm = n' `T.cons` n
    modifyState (\(Simple _ i cs) -> Simple (Just nm) i cs)

declarations = do
    char '{'
    spaces *> manyTill (declaration <* spaces) (char '}')


declaration = do
    n <- identifier
    spaces >> char ':' >> spaces
    v <- value
    spaces >> char ';'
    return $ Declaration n v

value = len <|> color <|> keyword

len = Length <$> float <*> unit

-- parse a floating point number
float :: Stream s m Char => ParsecT s u m Float
float = (fst . head . readFloat) <$> many (digit <|> (char '.'))

-- parse the unit type in a Value
-- currently only Px is supported
unit = do
    char 'p' <|> char 'P'
    char 'x' <|> char 'X'
    return Px


color = do
    char '#'
    cs <- count 3 (count 2 hexDigit)
    let [r,g,b] = map (fst . head . readHex) cs
    return $ Color r g b 255

keyword = Keyword <$> identifier

identifier = T.pack <$> many validId

validId = alphaNum <|> char '-' <|> char '_'
```
