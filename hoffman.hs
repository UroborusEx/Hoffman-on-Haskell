import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function
import System.Environment
import System.IO
import Control.Exception
import System.IO.Error
type Codemap = M.Map Char [Char]

data HofTree  = Leaf Char Int
            | Fork HofTree HofTree Int
            deriving (Show)

weight :: HofTree -> Int
weight (Leaf _ w)    = w
weight (Fork _ _ w)  = w

main :: IO ()
main = tryRead  `catch`errorHandler 

tryRead :: IO ()
tryRead = do 
           (name:[]) <- getArgs
           line <- readFile name
           codelist <- return $ crtCodemap line
           writeCodeMap $ M.toList codelist
           putStrLn $ encode codelist line
           return ()

crtCodemap :: String -> Codemap
crtCodemap = buildCodemap . buildTree . crtHofList

crtHofList:: String -> [(Char, Int)]
crtHofList= M.toList . M.fromListWith (+) . map (flip (,) 1)

merge :: HofTree -> HofTree -> HofTree
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

buildTree :: [(Char, Int)] -> HofTree
buildTree = bld . map (uncurry Leaf) . sortBy (compare `on` snd)
    where  bld (t:[])    = t
           bld (a:b:cs)  = bld $ insertBy (compare `on` weight) (merge a b) cs
buildCodemap :: HofTree -> Codemap
buildCodemap = M.fromList . buildCodelist
    where  buildCodelist (Leaf c w)    = [(c, [])]
           buildCodelist (Fork l r w)  = map (addBit '0') (buildCodelist l) ++ map (addBit '1') (buildCodelist r)
             where addBit b = second (b :)

encode :: Codemap -> String -> [Char]
encode m = concat . map (m M.!)
  
writeCodeMap :: [(Char,[Char])] -> IO ()
writeCodeMap []= return ()
writeCodeMap ((letter,code):xs)= do
  putStrLn $ letter:' ':code
  writeCodeMap xs

errorHandler :: IOException -> IO ()
errorHandler e
  |isUserError e = putStrLn "Wrong argc"
  |isDoesNotExistError e = putStrLn "File does not exist"
  |otherwise = ioError e