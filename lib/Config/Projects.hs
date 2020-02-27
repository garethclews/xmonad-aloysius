-- | Dynamic projects setup for import into C.Options

module Config.Projects where

import           XMonad
import           XMonad.Actions.DynamicProjects

import           App.Alias

-- Projects ---------------------------------------------------------------------
-- Workspace names, using symbols so like an alias
wsPlain :: String
wsTerm :: String
wsCode :: String
wsWeb :: String
wsMusic :: String
wsMail :: String
wsScratch :: String

wsPlain = "1"
wsTerm = "2"
wsCode = "3"
wsWeb = "4"
wsMusic = "5"
wsMail = "6"
wsScratch = "7"

wsList :: [String]
wsList = [wsScratch, wsPlain, wsTerm, wsCode, wsWeb, wsMusic, wsMail]

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
  , Project { projectName      = wsScratch
            , projectDirectory = "~/"
            , projectStartHook = Just $ return ()
            }
  ]
