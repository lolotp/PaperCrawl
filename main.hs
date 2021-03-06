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

baseUrlToCrawl :: String
baseUrlToCrawl = "http://search.arxiv.org:8081/?query=%22big+data%22+OR+cloud+OR+%22machine+learning%22+OR+%22artificial+intelligence%22+OR+%22distributed+computing%22&qid=13871620873749a_nCnN_-288443966"

indexStep :: Int
indexStep = 10

-- helper function for getting page content

getUrlResponseWithRedirect :: HStream t0 => URI -> IO t0 
getUrlResponseWithRedirect u = getUrlResponseWithRedirect_ u 3
    where  
        getUrlResponseWithRedirect_ u numRetry = do
            result <- simpleHTTP (mkRequest GET u) 
            case result of
                Right resp -> getResponseContent resp
                Left _ -> if (numRetry > 0) then (getUrlResponseWithRedirect u) else (getResponseBody result)
            where
                getResponseContent resp = let (Response respCode _ headers _ ) = resp in
                    if (respCode == (3,0,2)) then (
                        let location = head $ [content | (Header name content) <- headers, name == HdrLocation] in
                        let Just u = parseURI location in
                        getUrlResponseWithRedirect u) 
                    else (getResponseBody (Right resp))

openUrl :: HStream t0 => String -> MaybeT IO t0
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getUrlResponseWithRedirect u)--(getResponseBody =<< simpleHTTP (mkRequest GET u))

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT (openUrl url :: MaybeT IO String)
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

paperAbsLinks tree = tree >>> css "table" >>> css "a" >>> hasAttrValue "class" (== "url") >>> deep isText >>> getText
fileLink domain tree fileType = tree >>> css "a"
    >>> hasAttrValue "accesskey" (=="f")
    >>> ( (deep (hasText (isPrefixOf fileType))) `guards` this) 
    >>> getAttrValue "href"
    >>> arr (\ relativePath -> domain ++ relativePath)

parseArgs = do
  args <- getArgs
  case args of
       (url:[]) -> return url
       otherwise -> error "usage: grabber [url]"
getValidUrl :: IOSArrow XmlTree (NTree XNode) -> IO (String,String)
getValidUrl doc = do
  urls <- runX $ fileLink "http://arxiv.org" doc "PDF"
  if length urls == 0 then (
    do
      urls <-  runX $ fileLink "http://arxiv.org" doc "HTML"
      if length urls == 0 then (
        return ("",".html")) else (
        return ((head urls),".html")))
  else (do
    let url =  head urls
    return (url,".pdf"))
  
 
download absLink = do
  doc <- get absLink
  (url,extension) <- getValidUrl doc
  if url == "" then (do
    putStrLn $ show absLink) else (do
    putStrLn $ "downloading " ++ url)
  let path = uriPath $ fromJust $ parseURI url 
  let name = (last (Data.List.Split.splitOn "/" path)) ++ extension
  content <- runMaybeT (openUrl url :: MaybeT IO B.ByteString)
  case content of
     Nothing -> putStrLn $ "bad url: " ++ url
     Just _content -> do
        B.writeFile name _content


crawlPaperFromIndex :: Int -> IO [String] 
crawlPaperFromIndex index = do
  let urlToCrawl = if (index == 0) then (baseUrlToCrawl) else (baseUrlToCrawl ++ "&startat=" ++ (show index))
  doc <- get urlToCrawl 
  links <- runX . paperAbsLinks $ doc
  return links

getAllLinks :: [Int] -> IO [String]
getAllLinks [] = return []
getAllLinks indexList = do
    let index = head indexList
    putStrLn $ "crawling index : " ++ (show index)
    links <- crawlPaperFromIndex index
    links2 <- getAllLinks (tail indexList)
    return (links++links2) 

main = do
  links <- getAllLinks [x * 10 | x <- [0..46]]
  parallel_ $ map download links 
  stopGlobalPool
