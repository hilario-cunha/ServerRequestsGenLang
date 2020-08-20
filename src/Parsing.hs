module Parsing
    ( parseReadOrThrow
    , TemplateSimpleGet()
    ) where

import Text.ParserCombinators.Parsec
import Data.Functor (void)
import Control.Applicative hiding ((<|>), optional, many)
import ParsingUtils
import TemplateSimpleGet
import Data.List(elemIndex)

parseReadOrThrow :: String -> String -> Either String TemplateSimpleGet
parseReadOrThrow description = readOrThrow description parseTemplateSimpleGet

readOrThrow :: String -> Parser a -> String -> Either String a
readOrThrow description parser input = case parse parser description input of
    Left err  -> Left $ show err
    Right val -> Right val

parseTemplateSimpleGet :: Parser TemplateSimpleGet
parseTemplateSimpleGet = TemplateSimpleGet <$> parseExtraUsings <*> parseFunctionalityName <*> parseMethodsTryTo
        
parseExtraUsings :: Parser [String]
parseExtraUsings = (char 'u' *> ws *> betweenBracketsSepByComma (lexeme parseUsing) <* spaces) <|> (return [])

parseFunctionalityName :: Parser String
parseFunctionalityName = char 'f' *> (lexeme parseNames) <* spaces

parseMethodsTryTo :: Parser [MethodTryTo]
parseMethodsTryTo = many1 parseMethodTryTo

searchField :: [MyField] -> String -> Maybe MyField
searchField fields fieldName = case filter (\f -> fieldName == extractNameFromMyField f) fields of
    [] -> Nothing
    (x:_) -> Just x

parseMethodTryTo :: Parser MethodTryTo
parseMethodTryTo = do
    methodAction <- char 'm' <|> char 'p'
    case methodAction of
        'm' -> do
            methodInfo <- parseMethodInfo
            spaces
            let fields = extractFieldsFromMethodInfo methodInfo
            urlBuilder <- parseUrlBuilder $ searchField fields
            spaces
            return $ MethodTryToGet methodInfo urlBuilder
        'p' -> do
            methodInfo <- parseMethodInfo
            let fields = extractFieldsFromMethodInfo methodInfo
            dataT <- (lexeme parseNames)
            spaces
            urlBuilder <- parseUrlBuilder $ searchField fields
            spaces
            return $ MethodTryToPost methodInfo dataT urlBuilder
        _ -> fail $ "Unknow methodAction (" ++ [methodAction] ++ ")"
        
parseMethodInfo :: Parser MethodInfo
parseMethodInfo = do
    methodName <- (lexeme parseNames)
    responseT <- lexeme parseResponseT
    myFields <- betweenBracketsSepByComma parseMyField
    return $ MethodInfo methodName responseT myFields

parseMyField :: Parser MyField
parseMyField = do
    fieldType <- lexeme parseNames
    case fieldType of
        "String" -> StringField <$> (lexeme parseNames)
        "StringNotEmpty" -> StringNotEmptyField <$> (lexeme parseNames)
        "DateTime" -> DateTimeField <$> (lexeme parseNames)
        "Int" -> IntField <$> (lexeme parseNames)
        "StringNotEmptyArray" -> StringNotEmptyArrayField <$> (lexeme parseNames)
        "DateTimeNullable" -> DateTimeNullableField <$> (lexeme parseNames)
        _ -> CustomField fieldType <$> (lexeme parseNames)

parseResponseT :: Parser ResponseT
parseResponseT = (ResponseTArray <$> betweenBrackets parseNames) <|> (ResponseT <$> parseNames)

--TODO: implement new parsing model
-- resources/items/sku/p{itemId}s/expirations?store={retailStoreId}&b={b}
parseUrlBuilder :: (String -> Maybe MyField) -> Parser UrlBuilder
parseUrlBuilder search = UrlBuilder <$> lexeme (parseUrlParts search) <*> (parseUrlQueryParts search) 

parseUrlParts :: (String -> Maybe MyField) -> Parser [UrlPart]
parseUrlParts search = (sepBy (parseUrlPart search) (char '/'))

parseUrlPart :: (String -> Maybe MyField) -> Parser UrlPart
parseUrlPart search = do
    urlSection <- many parseUrlNameChar
    let beginPosMaybe = elemIndex '{' urlSection
    case beginPosMaybe of
        Just beginPos -> 
            do
                let endPosMaybe = elemIndex '}' urlSection
                case endPosMaybe of
                    Just endPos -> 
                        do
                            let prefix = take beginPos urlSection
                            let fieldName =  take (endPos - beginPos - 1) $ drop (beginPos+1) urlSection
                            let suffix = drop (endPos + 1)  urlSection
                            case search fieldName of
                                Just field -> 
                                    if((not $ null prefix) || (not $ null suffix))
                                        then return $ UrlPartVar $ updateMyFieldName field prefix suffix
                                        else return $ UrlPartVar field
                                    where
                                        addPrefix prefix = if(not $ null prefix) then "\"" ++ prefix ++ "\" + " else []
                                        addSuffix suffix = if(not $ null suffix) then " + \"" ++ suffix ++ "\"" else []
                                        updateMyFieldName (IntField n) prefix suffix = IntField $ addPrefix prefix ++ n ++ addSuffix suffix
                                        updateMyFieldName (StringField n) prefix suffix = StringField $ addPrefix prefix ++ n ++ addSuffix suffix
                                        updateMyFieldName (StringNotEmptyField n) prefix suffix = StringNotEmptyField $ addPrefix prefix ++ n ++ addSuffix suffix
                                        updateMyFieldName (StringNotEmptyArrayField n) prefix suffix = StringNotEmptyArrayField $ addPrefix prefix ++ n ++ addSuffix suffix
                                        updateMyFieldName (DateTimeField n) prefix suffix = DateTimeField $ addPrefix prefix ++ n ++ addSuffix suffix
                                        updateMyFieldName (DateTimeNullableField n) prefix suffix = DateTimeNullableField $ addPrefix prefix ++ n ++ addSuffix suffix
                                        updateMyFieldName (CustomField t n) prefix suffix = CustomField t $ addPrefix prefix ++ n ++ addSuffix suffix
                                Nothing -> fail $ "Unknow field with name (" ++ fieldName ++ ")"
                    Nothing -> fail $ "Is missing a ending }" 
        Nothing -> return $ UrlPartLit urlSection

parseUrlNameChar :: Parser Char
parseUrlNameChar = letter <|> digit <|> char '-' <|> char '{' <|> char '}'  <|> char ':'

parseUrlQueryParts :: (String -> Maybe MyField) -> Parser [UrlQueryPart]
parseUrlQueryParts search = (char '?' *> (sepBy (parseUrlQueryPart search) (char '&'))) <|> return []

parseUrlQueryPart :: (String -> Maybe MyField) -> Parser UrlQueryPart
parseUrlQueryPart search = do
    n <- lexeme parseQueryPartName
    char '='
    fieldName <- lexeme parseQueryPartName
    case search fieldName of
        Just field -> return $ mkUrlQueryPartVar n field
        Nothing -> return $ mkUrlQueryPartLiteral n fieldName

parseQueryPartName :: Parser String
parseQueryPartName = many1 (letter <|> digit <|> char '-' <|> char '_')

parseUsing :: Parser String
parseUsing = many1 (letter <|> digit <|> symbol <|> char '.')

parseNames :: Parser String
parseNames = many1 (letter <|> digit <|> symbol)

