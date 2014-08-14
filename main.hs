import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Network.Stream
import Network.HTTP
import Network.URI
import System.Environment
import Control.Concurrent.ParallelIO
import Data.List
import Data.List.Split

urlToCrawl :: String
urlToCrawl = "http://search.arxiv.org:8081/?query=%22big+data%22+OR+cloud+OR+%22machine+learning%22+OR+%22artificial+intelligence%22+OR+%22distributed+computing%22&qid=13871620873749a_nCnN_-288443966"

-- helper function for getting page content

getUrlResponseWithRedirect :: URI -> IO String
getUrlResponseWithRedirect u = do
    result <- simpleHTTP (mkRequest GET u) :: IO (Result (Response String))
    let Right resp = result
    let (Response respCode _ headers _) = resp
    if (respCode == (3,0,2)) then (
        let location = head $ [content | (Header name content) <- headers, name == HdrLocation] in
        let Just u = parseURI location in
        getUrlResponseWithRedirect u) else (getResponseBody result)

openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

paperAbsLinks tree = tree >>> css "table" >>> css "a" >>> hasAttrValue "class" (== "url") >>> deep isText >>> getText
pdfLink domain tree = tree >>> css "a" 
    >>> hasAttrValue "accesskey" (=="f") 
    >>> ( (deep (hasText (isPrefixOf "PDF"))) `guards` this) 
    >>> getAttrValue "href"
    >>> arr (\ relativePath -> domain ++ relativePath)

parseArgs = do
  args <- getArgs
  case args of
       (url:[]) -> return url
       otherwise -> error "usage: grabber [url]"

download absLink = do
  putStrLn $ "getting abs link " ++ absLink
  doc <- get absLink
  urls <- runX $ pdfLink "http://arxiv.org" doc
  let url = head urls
  putStrLn $ "downloading " ++ url
  let path = uriPath $ fromJust $ parseURI url 
  let name = (last (Data.List.Split.splitOn "/" path)) ++ ".pdf"
  putStrLn $name
  putStrLn $path

main = do
  doc <- get urlToCrawl 
  links <- runX . paperAbsLinks $ doc
  parallel_ $ map download links 
  stopGlobalPool
