module MD5Tag where

import Data.Byteable
import Crypto.Hash
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS(toStrict)

newtype MD5Tag = MD5Tag { rawMD5 :: BS.ByteString }
    deriving (Show, Eq, Ord)

md5Tag :: Bin.Put -> MD5Tag
md5Tag = MD5Tag . toBytes . (hash :: BS.ByteString -> Digest MD5) . BS.toStrict . Bin.runPut

emptyMD5Tag :: MD5Tag
emptyMD5Tag = md5Tag $ return ()
