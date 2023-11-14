{- cabal:
default-language: GHC2021
ghc-options: -threaded
build-depends: base, vty, vty-crossplatform, text, transformers, mmorph
-}

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
module Main where

-- Additional base imports
import Data.Monoid (Endo (Endo), appEndo)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Data.Ord (clamp)

-- Data.Text import.
import Data.Text qualified as T

-- transformers and mmorph imports.
import Control.Monad.Trans.Reader
    ( ReaderT
    , Reader
    , runReaderT
    , ask
    )
import Control.Monad.Morph (generalize, hoist)

-- vty and vty-crossplatform imports.
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty
    ( Vty (update, nextEvent, shutdown)
    , defAttr
    , userConfig
    )
import Graphics.Vty.Picture
    ( Picture (..)
    , Cursor (Cursor)
    , picForImage
    )
import Graphics.Vty.Input.Events (Key (..), Event (EvKey))
import Graphics.Vty.Image (text', (<->), char)

type Env = (HorizontalUnits, VerticalUnits, Vty)
type HorizontalUnits = Int
type VerticalUnits = Int

main :: IO ()
main = initialize >>= runReaderT mainProg . toEnv

toEnv :: Vty -> Env
toEnv = (24,34,)

initialize :: IO Vty
initialize = mkVty =<< userConfig

mainProg :: ReaderT Env IO ()
mainProg = progLoop =<< hoist generalize pictureTemplate
  where
    progLoop picture = do
        (_,_,vtyHandle) <- ask
        liftIO $ update vtyHandle picture
        liftIO (nextEvent vtyHandle) >>= \case
           EvKey key _
               | key `elem` arrowKeys ->
                   progLoop =<< hoist generalize (updatePicture key picture)
           EvKey (KChar 'q') _ -> liftIO $ shutdown vtyHandle
           _ -> progLoop picture

arrowKeys :: [Key]
arrowKeys =
    [ KLeft  , KRight  , KUp      , KDown
    , KUpLeft, KUpRight, KDownLeft, KDownRight
    ]

type CursorUpdate = (Int -> Int, Int -> Int)
arrowObjs :: [(Key, CursorUpdate)]
arrowObjs = zip arrowKeys
    [ (subtract 1, id)
    , ((+1), id)
    , (id, subtract 1)
    , (id, (+1))
    , (subtract 1, subtract 1)
    , ((+1), subtract 1)
    , (subtract 1, (+1))
    , ((+1), (+1))
    ]

updatePicture :: Key -> Picture -> Reader Env Picture
updatePicture key picture = do
    newCursor <- getCursor
    pure $ picture {picCursor = newCursor}
  where
    getCursor = clampCursor
        . ($ cursorPosition)
        . uncurry bimap
        . fromMaybe (id, id)
        $ lookup key arrowObjs

    cursorPosition = case picCursor picture of
        Cursor x y -> (x, y)
	__________ -> (0, 0)

    clampCursor (x,y) = do
        (xRep, yRep, _) <- ask
        let xBound = 4 * xRep - 1
            yBound = 2 * yRep - 1
        pure $ Cursor
            do clamp (0, xBound) x
            do clamp (0, yBound) y

pictureTemplate :: Reader Env Picture
pictureTemplate = do
    (xReps, yReps, _) <- ask
    pure $ picForImage
        . ($ mempty)
        . appEndo
        . foldMap (Endo . (<->) . text' defAttr)
        $ hexagons xReps yReps

hexagons :: Int -> Int -> [T.Text]
hexagons xRepeat yRepeat = concat
      . replicate yRepeat
      $ T.replicate xRepeat
    <$> [ "/ \\_", "\\_/ "]
