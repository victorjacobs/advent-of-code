import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C

main = do
    content <- LB.readFile "data.txt"
    putStrLn $ show $ mine content

mine :: LB.ByteString -> Int
mine str = head [x | x <- [0..], take 5 (createHash x str) == "00000"]

createHash :: Int -> LB.ByteString -> String
createHash x str = show $ md5 $ LB.append str $ C.pack (show x :: String)