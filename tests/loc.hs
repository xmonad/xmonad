import Control.Monad
import System.Exit

main = do foo <- getContents
          let actual_loc = filter (not.null) $ filter isntcomment $
                           map (dropWhile (==' ')) $ lines foo
              loc = length actual_loc
          print loc
          -- uncomment the following to check for mistakes in isntcomment
          -- print actual_loc

isntcomment ('-':'-':_) = False
isntcomment ('{':'-':_) = False -- pragmas
isntcomment _ = True
