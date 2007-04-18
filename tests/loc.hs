import Control.Monad
import System.Exit

main = do foo <- getContents
          let actual_loc = filter isntcomment $
                           map (dropWhile (==' ')) $ lines foo
              loc = length actual_loc
          putStrLn $ show loc
          -- uncomment the following to check for mistakes in isntcomment
          -- putStr $ unlines $ actual_loc
          when (loc > 400) $ fail "Too many lines of code!"

isntcomment "" = False
isntcomment ('-':'-':_) = False
isntcomment _ = True
