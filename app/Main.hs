module Main where

import Lib
import Language.CSharp.Parser
import Language.CSharp.Lexer

main :: IO ()
main = someFunc
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

