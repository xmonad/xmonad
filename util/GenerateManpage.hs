#!/usr/bin/env runhaskell

-- Reads markdown (man/xmonad.1.markdown) from stdin, subtitutes
-- ___KEYBINDINGS___ for key-binding definitions generated from
-- src/XMonad/Config.hs, prints result to stdout.
--
-- Unlike the rest of xmonad, this file is released under the GNU General
-- Public License version 2 or later. (Historical reasons, used to link with
-- GPL-licensed pandoc.)

import Data.Char
import Data.List

main :: IO ()
main = do
    keybindings <- guessBindings
    interact $ unlines . replace "___KEYBINDINGS___" keybindings . lines

-- | The format for the docstrings in "Config.hs" takes the following form:
--
-- @
--   -- mod-x %! Frob the whatsit
-- @
--
-- "Frob the whatsit" will be used as the description for keybinding "mod-x".
-- If the name of the key binding is omitted, the function tries to guess it
-- from the rest of the line. For example:
--
-- @
--   [ ((modMask .|. shiftMask, xK_Return), spawn "xterm") -- %! Launch an xterm
-- @
--
-- Here, "mod-shift-return" will be used as the key binding name.

guessBindings :: IO String
guessBindings = do
    buf <- readFile "./src/XMonad/Config.hs"
    return (intercalate "\n\n" (map markdownDefn (allBindings buf)))

allBindings :: String -> [(String, String)]
allBindings = concatMap parseLine . lines
  where
    parseLine :: String -> [(String, String)]
    parseLine l
        | " -- " `isInfixOf` l
        , Just d <- parseDesc l = [(intercalate "-" (parseKeys l), d)]
        | otherwise = []

    parseDesc :: String -> Maybe String
    parseDesc = fmap (trim . drop 4) . find (" %! " `isPrefixOf`) . tails

    parseKeys :: String -> [String]
    parseKeys l = case lex l of
        [("", _)] -> []
        [("--", rest)] -> case words rest of
            k : "%!" : _ -> [k]
            _ -> []
        [(k, rest)] -> parseKey k ++ parseKeys rest

    parseKey :: String -> [String]
    parseKey k | "Mask" `isSuffixOf` k = [reverse (drop 4 (reverse k))]
               | "xK_"  `isPrefixOf` k = [map toLower (drop 3 k)]
               | otherwise             = []

-- FIXME: What escaping should we be doing on these strings?
markdownDefn :: (String, String) -> String
markdownDefn (key, desc) = key ++ "\n:     " ++ desc

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\a -> if a == x then y else a)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
