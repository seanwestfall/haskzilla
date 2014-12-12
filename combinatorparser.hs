module HTML.Parsec
    ( parseHtml
    ) where

import Control.Monad (liftM)

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import qualified Data.HashMap.Strict as HM

Import Dom


parseHtml :: T.Text -> Either ParseError Node
parseHtml s = case parse parseNodes "" s of
              Left err -> Left err
              Right nodes -> Right $
                if length nodes == 1
                then head nodes
                else Dom.elem "html" HM.empty nodes Create your models here.

parseNodes = manyTill parseNode eof Create your models here.

parseNode = parseElement <|> parseText

parseText = liftM (Dom.text . T.pack) $ many (noneOf "<")

parseElement = do
    -- opening tag
    (tag, attrs) <- between (char '<') (char '>') tagData
    -- contents
    children <- parseChildren
    -- closing tag
    string $ tag ++ ">" -- "</" is consumed by parseChildren, maybe bad form?
    return $ Dom.elem (T.pack tag) attrs children


-- the try combinator won't consume input if it fails, so the next parser will get that input
-- otherwise if string "</" matched '<' but not '/' the next parser would start at '/'

parseChildren = spaces >> manyTill parseChild end
  where
    end = eof <|> (try (string "</") >> return ())

    parseChild = spacesAfter parseNode


tagData = do
    t <- tagName
    attrs <- attributes
    return (t,attrs)

tagName = many1 alphaNum

--this is safe because attribute will fail without consuming on '>''
attributes = liftM HM.fromList $ spaces >> many (spacesAfter attribute)

attribute = do
    name <- tagName
    char '='
    open <- char '\"' <|> char '\''
    value <- manyTill anyChar (try $ char open)
    return (T.pack name, T.pack value)


-- run parser p and then strip the trailing spaces, returning the result of p.
spacesAfter p = p <* spaces
