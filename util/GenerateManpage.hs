{-# LANGUAGE FlexibleContexts #-}
-- Unlike the rest of xmonad, this file is released under the GNU General
-- Public License version 2 or later.

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
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Text.PrettyPrint.HughesPJ
import Distribution.Text

import Text.Pandoc -- works with 2.1

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

    let manHeader = T.pack .
                    unwords
                    $ [".TH xmonad 1","\""++releaseDate++"\"",releaseName,"\"xmonad manual\""]

    markdownSource <- readFile "./man/xmonad.1.markdown"

    runIOorExplode $ do
        parsed <- readMarkdown def
            . T.pack
            . unlines
            . replace "___KEYBINDINGS___" keybindings
            . lines
            $ markdownSource

        manTemplate <- getDefaultTemplate "man"
        manBody <- writeMan def { writerTemplate = Just manTemplate } parsed
        liftIO $ TIO.writeFile "./man/xmonad.1" $ T.append manHeader manBody
        liftIO $ putStrLn "Documentation created: man/xmonad.1"

        htmltemplate <- getDefaultTemplate "html"
        htmlBody <- writeHtml5String def
                { writerVariables =
                            [("include-before"
                                ,"<h1>"++releaseName++"</h1>"++
                                 "<p>Section: xmonad manual (1)<br/>"++
                                 "Updated: "++releaseDate++"</p>"++
                                 "<hr/>")]
                , writerTemplate = Just htmltemplate
                , writerTableOfContents = True } parsed
        liftIO $ TIO.writeFile "./man/xmonad.1.html" htmlBody
        liftIO $ putStrLn "Documentation created: man/xmonad.1.html"
