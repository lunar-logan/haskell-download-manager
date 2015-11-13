module NetIO 
( nioReadHTTP
--, nioWriteHTTP
, mkRangeHeader
, nioGetRequest
, nioReadHTTP'
) where 

import System.IO
import Network.HTTP
import Network.URI
import Network.Stream
import Network.BufferType ( BufferOp(..), BufferType(..) )
import qualified Data.ByteString.Lazy as B


{-
	Given the byte range, this function creates the "range" header of the HTTP request
	According to IETF specs, range header is of the following format:

	Range: bytes=<fromByteIndex>-[<toByteIndex>]

	Provided the server supports partial content (206) in bytes. Fortuantely, most
	HTTP/1.1 complaint servers do accept ranges in bytes. 
	`toBytes` can be set to -1 to specify all the rest
-}
mkRangeHeader :: Int -> Int -> Header 
mkRangeHeader fromBytes toBytes =
	if toBytes == (-1) then
		mkHeader HdrRange ("bytes=" ++ (show fromBytes) ++ "-")
	else if toBytes < fromBytes then 
		error "Invalid byte range"
	else 
		mkHeader HdrRange ("bytes=" ++ (show fromBytes) ++ "-" ++ (show toBytes))


{- 
	Copied from the following URL:
	https://hackage.haskell.org/package/HTTP-4000.2.20/docs/src/Network-HTTP-Base.html#mkRequest

	I've no idea what it does and how it does whatever it does.
-}
toBufOps :: BufferType a => Request a -> BufferOp a
toBufOps _ = bufferOps


-- Returns a 'Request' object, given the url and a list of HTTP headers 
-- These headers must be of type 'Header' defined in the 'Network.HTTP' module.
-- see 'mkHeader' function for more information
-- Examples:
--
-- > nioGetRequest "http://blog.notespot.in" [] 
nioGetRequest :: BufferType ty => String -> [Header] -> Request ty
nioGetRequest urlString headers = 
	case parseURI urlString of
		Nothing -> error ("headRequest: Not a valid URL - " ++ urlString)
		Just url -> req
			where 
				req = 
					Request { rqURI		= url
							, rqBody	= empty
							, rqHeaders	= headers
							, rqMethod	= GET	 	
							}
				empty = buf_empty (toBufOps req)
		

-- Reads chunk of bytes from an HTTP/1.1 complaint server. 
-- Takes url and a list of HTTP headers as input, last param *must* always be 0
nioReadHTTP :: FilePath -> [Header] -> Int -> IO B.ByteString
nioReadHTTP urlString headers nBytes = do
	rsp <- Network.HTTP.simpleHTTP (nioGetRequest urlString headers)
	content <- getResponseBody rsp
	return content


-- Helper function to read byte ranges from HTTP servers. 
nioReadHTTP' :: String -> Int -> Int -> IO B.ByteString
nioReadHTTP' urlString fromBytes toBytes = do
	nioReadHTTP urlString headers 0
	where headers = [mkRangeHeader fromBytes toBytes]



--NetIO Data.ByteString.Lazy> let bs = nioReadHTTP' "http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-22.3-bin-i386.zip" 0 (-1)
{-
macs/windows/emacs-22.3-bin-i386.zip" 1831391 (-1)
*NetIO Data.ByteString.Lazy> bs >>= \bytes -> Data.ByteString.Lazy.writeFile "te
st.pyexe" bytes
Interrupted.

4831312
-}