module Main where

import Lib
import Language.CSharp.Parser
import Language.CSharp.Lexer
import Parsing
import TemplateSimpleGet

main :: IO ()
main = 
    generate
    -- someFunc
-- main = print $ parser "ex" l

l = lexer c
c= "using Tlantic.Server.Core;\
\namespace Tlantic.Server.Barcodes\
\{\
\    public class BarcodesServerRequests\
\    {\
\        ServerConfig serverConfig;\
\        internal BarcodesServerRequests(ServerConfig serverConfig)\
\        {\
\            this.serverConfig = serverConfig;\
\        }\
\        public IChoiceGetRequestWithRetry<Response<BarcodeRule[]>, NetworkError> TryToGetScanCodeRulesRequest(string retailStoreId)\
\        {\
\            var parts = new UrlParts(\"printers\");\
\            var queryParts = new UrlQueryParameters(new UrlQueryParameter(\"store\", retailStoreId));\
\            return serverConfig.TryToGet<PrinterEntryReponse[]>(new UrlBuilder(parts, queryParts));\
\        }\
\    }\
\}"



generate :: IO ()
generate =  do
    generateGeneric "BarcodesServerRequests.dsl"
    generateGeneric "ChecklistsServerRequests.dsl"
    generateGeneric "DocumentsServerRequests.dsl"
    print "end"

generateGeneric :: FilePath -> IO ()
generateGeneric inputFilename =  do
    toParse <- readFile inputFilename
    case parseReadOrThrow inputFilename toParse of
        Left err -> print err
        Right ast -> createAndWriteToFileTemplateSimpleGet ast
