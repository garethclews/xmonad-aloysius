-- | Dynamic projects setup for import into C.Options

module Config.Projects where

import XMonad
import XMonad.Actions.DynamicProjects

import App.Alias

-- Projects ---------------------------------------------------------------------
-- Workspace names, using symbols so like an alias
wsWeb     = "1" -- https://fontawesome.com/icons/code?style=solid
wsCode    = "2" -- https://fontawesome.com/icons/envelope?style=solid
wsMusic   = "3" -- https://fontawesome.com/icons/music?style=solid
wsPlain   = "4" -- https://fontawesome.com/icons/circle?style=solid
wsMail    = "5" -- https://fontawesome.com/icons/terminal?style=solid
wsScratch = "6" -- https://fontawesome.com/icons/globe-europe?style=solid

wsList = [ wsScratch
         , wsWeb
         , wsCode
         , wsMusic
         , wsPlain
         , wsMail
         ]

projects :: [Project]
projects =
  [ Project { projectName      = wsWeb
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn browser
            }
  , Project { projectName      = wsCode
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn code
            }
  , Project { projectName      = wsMusic
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn music
            }
  , Project { projectName      = wsPlain
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  , Project { projectName      = wsMail
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn mail
            }
  , Project { projectName      = wsScratch
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  ]
