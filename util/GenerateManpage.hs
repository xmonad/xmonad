{-# LANGUAGE FlexibleContexts #-}
-- Unlike the rest of xmonad, this file is copyright under the terms of the
-- GPL.

--
-- Generates man/xmonad.1 from man/xmonad.1.in by filling the list of
-- keybindings with values scraped from Config.hs
--
-- Uses cabal to grab the xmonad version from xmonad.cabal
--
-- Uses pandoc to convert the "xmonad.1.markdown" to "xmonad.1"
--
-- Format for the docstrings in Config.hs takes the following form:
--
-- -- mod-x %! Frob the whatsit
--
-- "Frob the whatsit" will be used as the description for keybinding "mod-x"
--
-- If the keybinding name is omitted, it will try to guess from the rest of the
-- line. For example:
--
-- [ ((modMask .|. shiftMask, xK_Return), spawn "xterm") -- %! Launch an xterm
--
-- Here, mod-shift-return will be used as the keybinding name.
import Control.Monad
import Control.Applicative
import Text.Regex.Posix
import Data.Char
import Data.List

import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Text.PrettyPrint.HughesPJ
import Distribution.Text

import Text.Pandoc -- works with 1.15.x

releaseDate = "31 December 2012"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

guessKeys line = concat $ intersperse "-" (modifiers ++ [map toLower key])
    where modifiers = map (!!1) (line =~ "(mod|shift|control)Mask")
          (_, _, _, [key]) = line =~ "xK_([_[:alnum:]]+)" :: (String, String, String, [String])

binding :: [String] -> (String, String)
binding [ _, bindingLine, "", desc ] = (guessKeys bindingLine, desc)
binding [ _, _, keyCombo, desc ] = (keyCombo, desc)

allBindings :: String -> [(String, String)]
allBindings xs = map (binding . map trim) (xs =~ "(.*)--(.*)%!(.*)")

-- FIXME: What escaping should we be doing on these strings?
markdownDefn :: (String, String) -> String
markdownDefn (key, desc) = key ++ "\n:     " ++ desc

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\a -> if a == x then y else a)

-- rawSystem "pandoc" ["--read=markdown","--write=man","man/xmonad.1.markdown"]

main = do
    releaseName <- (show . disp . package . packageDescription)
                    `liftM`readPackageDescription normal "xmonad.cabal"
    keybindings <- (intercalate "\n\n" . map markdownDefn . allBindings)
                    `liftM` readFile "./src/XMonad/Config.hs"

    let manHeader = unwords [".TH xmonad 1","\""++releaseDate++"\"",releaseName,"\"xmonad manual\""]

    Right parsed <- readMarkdown def
        . unlines
        . replace "___KEYBINDINGS___" keybindings
        . lines
        <$> readFile "./man/xmonad.1.markdown"

    Right template <- getDefaultTemplate Nothing "man"
    writeFile "./man/xmonad.1"
        . (manHeader ++)
        . writeMan def{ writerStandalone = True, writerTemplate = template }
        $ parsed
    putStrLn "Documentation created: man/xmonad.1"

    Right template <- getDefaultTemplate Nothing "html"
    writeFile "./man/xmonad.1.html"
        . writeHtmlString def
            { writerVariables =
                        [("include-before"
                            ,"<h1>"++releaseName++"</h1>"++
                             "<p>Section: xmonad manual (1)<br/>"++
                             "Updated: "++releaseDate++"</p>"++
                             "<hr/>")]
            , writerStandalone = True
            , writerTemplate = template
            , writerTableOfContents = True }
        $ parsed
    putStrLn "Documentation created: man/xmonad.1.html"
