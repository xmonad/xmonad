--
-- Generates man/xmonad.1 from man/xmonad.1.in by filling the list of
-- keybindings with values scraped from Config.hs
--
-- Format for the docstrings in Config.hs takes the following form:
--
-- -- mod-x @@ Frob the whatsit
-- 
-- "Frob the whatsit" will be used as the description for keybinding "mod-x"
--
-- If the keybinding name is omitted, it will try to guess from the rest of the
-- line. For example:
--
-- [ ((modMask .|. shiftMask, xK_Return), spawn "xterm") -- @@ Launch an xterm
--
-- Here, mod-shift-return will be used as the keybinding name.
--
import Control.Monad
import Text.Regex.Posix
import Data.Char
import Data.List

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

guessKeys line = concat $ intersperse "-" (modifiers ++ [map toLower key])
    where modifiers = map (!!1) (line =~ "(mod|shift|control)Mask")
          (_, _, _, [key]) = line =~ "xK_(\\w+)" :: (String, String, String, [String])

binding :: [String] -> (String, String)
binding [ _, bindingLine, "", desc ] = (guessKeys bindingLine, desc)
binding [ _, _, keyCombo, desc ] = (keyCombo, desc)

allBindings :: String -> [(String, String)]
allBindings xs = map (binding . map trim) (xs =~ "(.*)--(.*)@@(.*)")

-- FIXME: What escaping should we be doing on these strings?
troff :: (String, String) -> String
troff (key, desc) = ".IP\n \\fB" ++ key ++ "\\fR\n" ++ desc ++ "\n"

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\a -> if a == x then y else a)

main = do
    troffBindings <- (concatMap troff . allBindings) `liftM` readFile "./Config.hs"
    let sed = unlines . replace "___KEYBINDINGS___" troffBindings . lines
    readFile "./man/xmonad.1.in" >>= return . sed >>= writeFile "./man/xmonad.1"
