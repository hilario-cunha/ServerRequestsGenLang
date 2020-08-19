module Parsing
    ( parseReadOrThrow
    , TemplateSimpleGet()
    ) where

import Text.ParserCombinators.Parsec
import Data.Functor (void)
import Control.Applicative hiding ((<|>), optional, many)
import ParsingUtils
import TemplateSimpleGet

parseReadOrThrow :: String -> String -> Either String TemplateSimpleGet
parseReadOrThrow description = readOrThrow description parseTemplateSimpleGet

readOrThrow :: String -> Parser a -> String -> Either String a
readOrThrow description parser input = case parse parser description input of
    Left err  -> Left $ show err
    Right val -> Right val

parseTemplateSimpleGet :: Parser TemplateSimpleGet
parseTemplateSimpleGet = TemplateSimpleGet <$> parseExtraUsings <*> parseFunctionalityName <*> parseMethodsTryTo
        
parseExtraUsings :: Parser [String]
parseExtraUsings = return []

parseFunctionalityName :: Parser String
parseFunctionalityName = char 'f' *> (lexeme parseNames) <* spaces

parseMethodsTryTo :: Parser [MethodTryTo]
parseMethodsTryTo = do
    m <- parseMethodTryTo
    return [m]


searchField :: [MyField] -> String -> Maybe MyField
searchField fields fieldName = case filter (\f -> fieldName == extractNameFromMyField f) fields of
    [] -> Nothing
    (x:_) -> Just x

parseMethodTryTo :: Parser MethodTryTo
parseMethodTryTo = do
    methodInfo <- parseMethodInfo
    let fields = extractFieldsFromMethodInfo methodInfo
    urlBuilder <- parseUrlBuilder $ searchField fields
    return $ MethodTryToGet methodInfo urlBuilder
        
parseMethodInfo :: Parser MethodInfo
parseMethodInfo = do
    methodName <- char 'm' *> (lexeme parseNames)
    responseT <- lexeme parseResponseT
    myFields <- betweenBracketsSepByComma parseMyField
    spaces
    return $ MethodInfo methodName responseT myFields

parseMyField :: Parser MyField
parseMyField = do
    fieldType <- lexeme parseNames
    case fieldType of
        "String" -> StringField <$> (lexeme parseNames)
        _ -> CustomField fieldType <$> (lexeme parseNames)

parseResponseT :: Parser ResponseT
parseResponseT = (ResponseTArray <$> betweenBrackets parseNames) <|> (ResponseT <$> parseNames)

parseUrlBuilder :: (String -> Maybe MyField) -> Parser UrlBuilder
parseUrlBuilder search = UrlBuilder <$> lexeme parseUrlParts <*> (parseUrlQueryParts search) 

parseUrlParts :: Parser [UrlPart]
parseUrlParts = betweenBracketsSepByComma parseUrlPart

parseUrlPart :: Parser UrlPart
parseUrlPart = UrlPartLit <$> parseUrlPartName

parseUrlPartName :: Parser String
parseUrlPartName = parseNames

parseUrlQueryParts :: (String -> Maybe MyField) -> Parser [UrlQueryPart]
parseUrlQueryParts search = betweenBracketsSepByComma (parseUrlQueryPart search)

parseUrlQueryPart :: (String -> Maybe MyField) -> Parser UrlQueryPart
parseUrlQueryPart search = do
    n <- lexeme parseNames
    fieldName <- lexeme parseNames
    case search fieldName of
        Just field -> return $ mkUrlQueryPartVar n field
        Nothing -> return $ mkUrlQueryPartLiteral n fieldName


parseNames :: Parser String
parseNames = many1 (letter <|> digit <|> symbol)