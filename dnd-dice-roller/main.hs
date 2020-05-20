import System.Random
import System.IO

sumDice :: Int -> Int -> StdGen -> (Int, StdGen)
sumDice sides 0 g = (0, g') where (g', _) = split g
sumDice sides i g = ((roll + subTotal), g'')
                  where
                    (roll, g') = randomR (1, sides) g
                    (subTotal, g'') = sumDice sides (i - 1) g'

roll :: String -> StdGen -> (Int, StdGen)
roll s g = (rollTotal + adjustment, g')
  where
    (amountOfDice : s' : []) = splitOn (=='d') s
    (sides : adjustment : []) = map (\s -> read (filter (/=' ') s) :: Int) (splitOn (=='+') s')
    (rollTotal, g') = sumDice sides (read amountOfDice :: Int) g

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'

input :: String -> IO String
input prompt = do putStr prompt
                  hFlush stdout
                  getLine

main :: IO ()
main = do g <- newStdGen
          s <- input ">: "
          putStrLn . show . fst $ roll s g
          main