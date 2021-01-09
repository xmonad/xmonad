-- Reads "man/xmonad.1.markdown.in" and generates "man/xmonad.1.markdown" that
-- has the list of known key-bindings extracted automatically from
-- "src/XMonad/Config.hs". That document is then rendered with Pandoc as
-- "man/xmonad.1" and "man/xmonad.1.html".
--
-- Unlike the rest of xmonad, this file is released under the GNU General
-- Public License version 2 or later. (Used to be linked with pandoc and now
-- it's too much hassle to relicense.)

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Verbosity (normal)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.Text (display)
import System.Process
import Text.Regex.Posix

main :: IO ()
main = do
    releaseName <- display . package . packageDescription
        <$> readGenericPackageDescription normal "./xmonad.cabal"
    keybindings <- guessBindings

    markdownSource <- T.readFile "./man/xmonad.1.markdown.in"
    T.writeFile "./man/xmonad.1.markdown"
        . T.replace (T.pack "___VERSION___") (T.pack releaseName)
        . T.replace (T.pack "___KEYBINDINGS___") (T.pack keybindings)
        $ markdownSource

    callCommand . unwords $
        [ "pandoc"
        , "--from=markdown"
        , "--to=man"
        , "--standalone"
        , "--output=./man/xmonad.1"
        , "./man/xmonad.1.markdown"
        ]
    putStrLn "Documentation created: man/xmonad.1"

    callCommand . unwords $
        [ "pandoc"
        , "--from=markdown"
        , "--to=html"
        , "--standalone"
        , "--table-of-contents"
        , "--output=./man/xmonad.1.html"
        , "./man/xmonad.1.markdown"
        ]
    putStrLn "Documentation created: man/xmonad.1.html"

-- | The format for the docstrings in "Config.hs" takes the following form:
--
-- @
--   -- mod-x %! Frob the whatsit
-- @
--
-- "Frob the whatsit" will be used as the description for keybinding "mod-x".--
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
allBindings xs = map (binding . map trim) (xs =~ "(.*)--(.*)%!(.*)")

binding :: [String] -> (String, String)
binding [ _, bindingLine, "", desc ] = (guessKeys bindingLine, desc)
binding [ _, _, keyCombo, desc ] = (keyCombo, desc)
binding x = error ("binding: called with unexpected argument " ++ show x)

guessKeys :: String -> String
guessKeys line =
  case keys of
    [key] -> concat $ intersperse "-" (modifiers ++ [map toLower key])
    _     -> error ("guessKeys: unexpected number of keys " ++ show keys)
  where
    modifiers = map (!!1) (line =~ "(mod|shift|control)Mask")
    (_, _, _, keys) = line =~ "xK_([_[:alnum:]]+)" :: (String, String, String, [String])

-- FIXME: What escaping should we be doing on these strings?
markdownDefn :: (String, String) -> String
markdownDefn (key, desc) = key ++ "\n:     " ++ desc

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
