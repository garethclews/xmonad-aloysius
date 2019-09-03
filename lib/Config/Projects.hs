-- | Dynamic projects setup for import into C.Options

module Config.Projects where

import XMonad
import XMonad.Actions.DynamicProjects

import App.Alias

-- Projects ---------------------------------------------------------------------
-- Workspace names, using symbols so like an alias
wsTerm    = "1"
wsCode    = "2" -- https://fontawesome.com/icons/envelope?style=solid
wsWeb     = "3" -- https://fontawesome.com/icons/code?style=solid
wsMusic   = "4" -- https://fontawesome.com/icons/music?style=solid
wsMail    = "5" -- https://fontawesome.com/icons/terminal?style=solid
wsPlain   = "6" -- https://fontawesome.com/icons/circle?style=solid
wsScratch = "7" -- https://fontawesome.com/icons/globe-europe?style=solid

wsList = [ wsScratch
         , wsTerm
         , wsWeb
         , wsCode
         , wsMusic
         , wsPlain
         , wsMail
         ]

projects :: [Project]
projects =
  [ Project { projectName      = wsTerm
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
  , Project { projectName      = wsPlain
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  , Project { projectName      = wsScratch
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  ]
