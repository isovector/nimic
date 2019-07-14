{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           System.Environment
import           Brick
import           Brick.AttrMap ( attrMap)
import qualified Brick.Main as M
import           Brick.Types (locationRowL, locationColumnL, Widget)
import qualified Brick.Types as T
import           Brick.Widgets.Border
import qualified Brick.Widgets.Border as B
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core (translateBy, str)
import           Control.Monad (void)
import           Control.Monad.State (runState)
import           Data.Generics.Product
import           Data.List.PointedList
import qualified Data.Text.IO as T
import           GHC.Generics
import qualified Graphics.Vty as V
import           Lens.Micro ((^.), (&), (%~))
import           Lib
import           Parser
import qualified Text.PrettyPrint.HughesPJ as PP
import           Types
import           Data.Attoparsec.Text

goPrev :: StepCtx -> StepCtx
goPrev a@(StepCtx prog) =
  case previous prog of
    Just z -> StepCtx z
    Nothing -> a

goNext :: StepCtx -> StepCtx
goNext (StepCtx prog) =
  case next prog of
    Just z -> StepCtx z
    Nothing -> StepCtx $ flip insertRight prog $ do
      let (t, c) = _focus prog
          (mt, c') = runState (step t) c
       in case mt of
            Just t' -> (t', c')
            Nothing -> error "stuck!"

data StepCtx = StepCtx
  { stepProgram :: PointedList (Term Void1, NimicCtx)
  } deriving Generic

data St = St
  { topLayerLocation :: T.Location
  , bottomLayerLocation :: T.Location
  } deriving Generic

drawUi :: StepCtx -> [Widget ()]
drawUi st =
    [ withBorderStyle unicode $
        borderWithLabel (str "Hello!") $
          leftPane st
    ]

leftPane :: StepCtx -> Widget ()
leftPane st = str $ PP.render $ ppr $ fst $ _focus $ stepProgram st

appEvent :: StepCtx -> T.BrickEvent () e -> T.EventM () (T.Next StepCtx)
appEvent st (T.VtyEvent (V.EvKey V.KLeft []))  =
    M.continue $ goPrev st
appEvent st (T.VtyEvent (V.EvKey V.KRight []))  =
    M.continue $ goNext st

-- appEvent st (T.VtyEvent (V.EvKey V.QKey [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

app :: M.App StepCtx e ()
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = do
  [filepath] <- getArgs
  prelcont <- T.readFile "examples/prelude.nim"
  progcont <- T.readFile filepath
  let res = do
        prelude <- parseOnly parseImplicitGroup prelcont
        program <- parseOnly parseImplicitGroup progcont
        pure $ flip runState (NimicCtx macros mempty) $ do
          ran_prelude <- force prelude
          pure $ Group [ ran_prelude, Sym ";",  program ]
  case res of
    Left err -> do
      putStrLn "nimic parse error:"
      putStrLn err
    Right z -> do
      void $ M.defaultMain app $ StepCtx $ singleton $ z
