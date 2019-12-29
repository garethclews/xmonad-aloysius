-- | Abstracting out the chosen theme to give it one place to edit
--   rather than across every file which imports the theme

module Theme.ChosenTheme where


-- > Change this to your colour preference of choice
--   NOTE: the polybar colour and screen lock are handled elsewhere
--         as is Xresources
-- import qualified Theme.Dracula   as T
-- import qualified Theme.Ephemeral as T
-- import qualified Theme.Laserwave as T
import qualified Theme.Nord as T


basebg = T.basebg
basefg = T.basefg
base00 = T.base00
base01 = T.base01
base02 = T.base02
base03 = T.base03
base04 = T.base04
base05 = T.base05
base06 = T.base06
base07 = T.base07
base08 = T.base08
base09 = T.base09
base10 = T.base10
base11 = T.base11
base12 = T.base12
base13 = T.base13
base14 = T.base14
base15 = T.base15

fontsize :: String
fontsize = T.fontsize

sansserif :: String
sansserif = T.sansserif

sansserif' :: String
sansserif' = T.sansserif'

monospace :: String
monospace = T.monospace
