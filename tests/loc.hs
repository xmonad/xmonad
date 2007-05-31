import Control.Monad
import System.Exit

main = do foo <- getContents
          let actual_loc = filter (not.null) $ filter isntcomment $
                           map (dropWhile (==' ')) $ lines foo
              loc = length actual_loc
          putStrLn $ show loc
          -- uncomment the following to check for mistakes in isntcomment
          -- putStr $ unlines $ actual_loc

isntcomment "" = False
isntcomment ('-':'-':_) = False
isntcomment ('{':'-':_) = False -- pragmas
isntcomment _ = True
