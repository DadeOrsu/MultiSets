import MultiSet

import Data.Char (toLower)
import Data.List (sort, group)
import System.IO ( hPutStrLn, withFile, IOMode(WriteMode) )

{-
|Function to calculate the ciao of a string
-}
ciao :: String -> String
ciao = sort . map toLower

{- |Function to read a text file and create an MSet of ciaos with multiplicities
- @filename@ is the path of the file to read from
-}
readMSet :: FilePath -> IO (MSet String)
readMSet filename = do
  content <- readFile filename
  let wordsList = words content
      ciaoList = map ciao wordsList
      groupedCiaos = group $ sort ciaoList
      ciaoWithMultiplicity = map (\grouped -> (head grouped, length grouped)) groupedCiaos
  return $ MS ciaoWithMultiplicity



{- |Function to write MSet elements with multiplicities to a file
- @filename@ is the path of the file to write to
- @ms@ is the MSet to write to the file
-}
writeMSet :: Show a => FilePath -> MSet a -> IO ()
writeMSet filename (MS ms) = withFile filename WriteMode $ \handle -> do
  mapM_ (hPutStrLn handle . formatEntry) ms
  where
    formatEntry :: Show a => (a, Int) -> String
    formatEntry (elem, multiplicity) = show elem ++ " - " ++ show multiplicity

{- |Main function used to test the MultiSet module
-}
main :: IO ()
main = do
  -- a. Load files into corresponding multisets
  m1 <- readMSet "aux_files/anagram.txt"
  m2 <- readMSet "aux_files/anagram-s1.txt"
  m3 <- readMSet "aux_files/anagram-s2.txt"
  m4 <- readMSet "aux_files/margana2.txt"

  -- b. Check facts and print comments
  putStrLn "tests:"
  -- i. Multisets m1 and m4 are not equal, but they have the same elements
  putStrLn $ "i.a Multisets m1 and m4 are not equal: " ++ show (m1 /= m4)
  putStrLn $ "i.b Multisets m1 and m4 have the same elements: " ++ show (elems m1 == elems m4)
  -- ii. Multiset m1 is equal to the union of multisets m2 and m3
  let unionM2M3 = m2 `union` m3
  putStrLn $ "ii. Multiset m1 is equal to the union of multisets m2 and m3: " ++ show (m1 == unionM2M3)

  -- c. Write multisets m1 and m4 to files
  writeMSet "output/anag-out.txt" m1
  writeMSet "output/gana-out.txt" m4
  putStrLn "Multisets m1 and m4 written to files output/anag-out.txt and output/gana-out.txt."