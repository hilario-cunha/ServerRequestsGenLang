module Parsing
    ( parseReadOrThrow
    , TemplateSimpleGet()
    ) where

import Text.ParserCombinators.Parsec
import ParsingUtils
import TemplateSimpleGet
import Data.List(elemIndex)
import UrlBuilder

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
parseMethodsTryTo = manyTill parseMethodTryTo eof

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
            return $ MethodTryTo MethodActionGet methodInfo urlBuilder
        'p' -> do
            methodInfo <- parseMethodInfo
            let fields = extractFieldsFromMethodInfo methodInfo
            dataT <- (lexeme parseNames)
            spaces
            urlBuilder <- parseUrlBuilder $ searchField fields
            spaces
            return $ MethodTryTo (MethodActionPost dataT) methodInfo urlBuilder
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
parseResponseT = ((char '(') *> (char ')') *> return Response) <|> (ResponseTArray <$> betweenBrackets parseNames) <|> (ResponseT <$> parseNames)

parseUrlBuilder :: (String -> Maybe MyField) -> Parser UrlBuilder
parseUrlBuilder search = UrlBuilder <$> lexeme (parseUrlParts search) <*> (parseUrlQueryParts search) 

parseUrlParts :: (String -> Maybe MyField) -> Parser [UrlPart]
parseUrlParts search = (sepBy (parseUrlPart search) (char '/'))

parseUrlPart :: (String -> Maybe MyField) -> Parser UrlPart
parseUrlPart search = parseUrlSection search UrlPartLit fromMyFieldToUrlPartField UrlPartVar

fromMyFieldToUrlPartField :: MyField -> Parser UrlPartField
fromMyFieldToUrlPartField (IntField n) = return $ IntPartField n
fromMyFieldToUrlPartField (StringField n) = return $ StringPartField n
fromMyFieldToUrlPartField (StringNotEmptyField n) = return $ StringNotEmptyPartField n
fromMyFieldToUrlPartField (DateTimeField n) = return $ DateTimePartField n
fromMyFieldToUrlPartField (DateTimeNullableField n) = return $ DateTimeNullablePartField n
fromMyFieldToUrlPartField (StringNotEmptyArrayField _) = fail "StringNotEmptyArrayField Not supported in url part"
fromMyFieldToUrlPartField (CustomField n _) = fail $ "CustomField " ++ n ++ "not supported in url part"

parseUrlSection :: (String -> Maybe MyField) -> (String -> b) -> (MyField -> Parser a) -> (String -> a -> String -> b) -> Parser b
parseUrlSection search createLiteral fromMyField  createVar = do
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
                                    do
                                        p <- fromMyField field
                                        return $ createVar prefix p suffix
                                Nothing -> fail $ "Unknow field with name (" ++ fieldName ++ ")"
                    Nothing -> fail $ "Is missing a ending }" 
        Nothing -> return $ createLiteral urlSection

parseUrlNameChar :: Parser Char
parseUrlNameChar = letter <|> digit <|> char '-' <|> char '{' <|> char '}'  <|> char ':'

parseUrlQueryParts :: (String -> Maybe MyField) -> Parser [UrlQueryPart]
parseUrlQueryParts search = (char '?' *> (sepBy (parseUrlQueryPart search) (char '&'))) <|> return []

parseUrlQueryPart :: (String -> Maybe MyField) -> Parser UrlQueryPart
parseUrlQueryPart search = do
    n <- lexeme parseQueryPartName
    _ <- char '='
    parseUrlQueryPartName search n

parseUrlQueryPartName :: (String -> Maybe MyField) -> String -> Parser UrlQueryPart
parseUrlQueryPartName search n = parseUrlSection search (\v -> UrlQueryPart n $ UrlQueryPartLit v) fromMyFieldToUrlQueryPartField (\p f s-> UrlQueryPart n $ UrlQueryPartVar p f s) 

fromMyFieldToUrlQueryPartField :: MyField -> Parser UrlQueryPartField
fromMyFieldToUrlQueryPartField (IntField n) = return $ IntQueryPartField n
fromMyFieldToUrlQueryPartField (StringField n) = return $ StringQueryPartField n
fromMyFieldToUrlQueryPartField (StringNotEmptyField n) = return $ StringNotEmptyQueryPartField n
fromMyFieldToUrlQueryPartField (StringNotEmptyArrayField n) = return $ StringNotEmptyArrayQueryPartField n
fromMyFieldToUrlQueryPartField (DateTimeField n) = return $ DateTimeQueryPartField n
fromMyFieldToUrlQueryPartField (DateTimeNullableField n) = return $ DateTimeNullableQueryPartField n
fromMyFieldToUrlQueryPartField (CustomField n _) = fail $ "CustomField " ++ n ++ " not supported in url query part"

parseQueryPartName :: Parser String
parseQueryPartName = many1 (letter <|> digit <|> char '-' <|> char '_')

parseUsing :: Parser String
parseUsing = many1 (letter <|> digit <|> symbol <|> char '.')

parseNames :: Parser String
parseNames = many1 (letter <|> digit <|> symbol)

