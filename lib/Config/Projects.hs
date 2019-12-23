-- | Dynamic projects setup for import into C.Options

module Config.Projects where

import           XMonad
import           XMonad.Actions.DynamicProjects

import           App.Alias

-- Projects ---------------------------------------------------------------------
-- Workspace names, using symbols so like an alias
wsPlain = "1" -- https://fontawesome.com/icons/circle?style=solid
wsTerm = "2"
wsCode = "3" -- https://fontawesome.com/icons/envelope?style=solid
wsWeb = "4" -- https://fontawesome.com/icons/code?style=solid
wsMusic = "5" -- https://fontawesome.com/icons/music?style=solid
wsMail = "6" -- https://fontawesome.com/icons/terminal?style=solid
wsKeybase = "7"
wsScratch = "8" -- https://fontawesome.com/icons/globe-europe?style=solid

wsList =
  [wsScratch, wsTerm, wsWeb, wsCode, wsMusic, wsPlain, wsMail, wsKeybase]

projects :: [Project]
projects =
  [ Project { projectName      = wsPlain
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  , Project { projectName      = wsTerm
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn tty
            }
  , Project { projectName      = wsCode
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn code
            }
  , Project { projectName      = wsWeb
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn browser
            }
  , Project { projectName      = wsMusic
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn music
            }
  , Project { projectName      = wsMail
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn mail
            }
  , Project { projectName      = wsKeybase
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn keybase
            }
  , Project { projectName      = wsScratch
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  ]
