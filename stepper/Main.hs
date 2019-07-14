{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Ansi
import           Brick hiding (App)
import           Brick.AttrMap (attrMap)
import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Control.Monad (void, join)
import           Control.Monad.State (runState, evalState)
import           Data.Attoparsec.Text
import           Data.List.PointedList
import           Data.Maybe (maybeToList)
import           Data.Monoid (Endo (..), First (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import qualified Graphics.Vty as V
import           Lib
import           Parser
import           System.Environment
import qualified Text.PrettyPrint.HughesPJ as PP
import           Types


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
            Just t' -> (fst t', c')
            Nothing -> error "stuck!"

bindingsToColors :: [Binding] -> String -> String
bindingsToColors bs = appEndo $ flip foldMap (zip bs bindingColors) $ \(Binding n _, color) ->
  Endo $ \v ->
    if T.unpack n == v
       then color ++ v ++ resetColor
       else v

bindingsToTreeColors :: [Binding] -> Term Void1 -> PP.Doc -> PP.Doc
bindingsToTreeColors bs t = appEndo $ flip foldMap (zip bs bindingColors) $ \(Binding _ p, color) ->
  Endo $ \d ->
    if t == p
       then PP.zeroWidthText color <> d <> PP.zeroWidthText resetColor
       else d

bindingColors :: [String]
bindingColors =
  [ "\x1b[31m"
  , "\x1b[33m"
  , "\x1b[32m"
  , "\x1b[36m"
  , "\x1b[34m"
  , "\x1b[35m"
  ]

resetColor :: String
resetColor = "\x1b[0m"

runApp :: StepCtx -> App a -> a
runApp ctx ma = evalState ma $ snd $ _focus $ stepProgram ctx

data StepCtx = StepCtx
  { stepProgram :: PointedList (Term Void1, NimicCtx)
  } deriving Generic

data St = St
  { topLayerLocation :: T.Location
  , bottomLayerLocation :: T.Location
  } deriving Generic

drawUi :: StepCtx -> [Widget ()]
drawUi st =
    [ let (d, macros) = macroPane st
       in withBorderStyle unicode $
            borderWithLabel (str "The Glorious Nimic Stepper") $
              termPane d st
              <+> vBorder
              <+> macros
    ]

termPane :: Maybe (Term Void1 -> Term Void1, (Macro, [Binding])) -> StepCtx -> Widget ()
termPane Nothing
  = padRight Max
  . padAll 2
  . str
  . PP.render
  . ppr
  . fst
  . _focus
  . stepProgram
termPane (Just (reassoc, bs))
  = padRight Max
  . padAll 2
  . raw
  . ansiImage
  . T.pack
  . PP.render
  . pprRaw (bindingsToTreeColors $ snd bs)
  . fst
  . _focus
  . stepProgram

isDefMacro :: Macro -> Bool
isDefMacro (Macro _ _) = True
isDefMacro _ = False

pprMacro :: (Macro -> String -> String) -> Macro -> Widget ()
pprMacro macroMatches m@(Macro a _)
  = raw . ansiImage . T.pack . PP.render $ pprId (macroMatches m . T.unpack) a
pprMacro macroMatches m@(Primitive a _)
  = raw . ansiImage . T.pack . PP.render $ pprId (macroMatches m . T.unpack) a

macroPane :: StepCtx -> (Maybe (Term Void1 -> Term Void1, (Macro, [Binding])), Widget ())
macroPane ctx@(StepCtx p) =
  let ms = ctxDefMacros . snd $ _focus p
      t = fst $ _focus p
      matched = runApp ctx $ doStep t
      macroMatches m =
        case matched of
          Nothing -> id
          Just (_, (matchedM, bs)) ->
            if matchedM == m
               then bindingsToColors bs
               else id
   in (matched, )
    . padAll 2
    . vBox
    . fmap (pprMacro macroMatches)
    $ ms

ansiImage :: T.Text -> V.Image
ansiImage = foldMap mkLine . map parseANSI . T.lines
  where
    mkLine ss =
      foldr (V.<|>) mempty [V.text' a s | Segment a s <- ss]

appEvent :: StepCtx -> T.BrickEvent () e -> T.EventM () (T.Next StepCtx)
appEvent st (T.VtyEvent (V.EvKey V.KLeft []))  =
    M.continue $ goPrev st
appEvent st (T.VtyEvent (V.EvKey V.KRight []))  =
    M.continue $ goNext st

appEvent st (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
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

