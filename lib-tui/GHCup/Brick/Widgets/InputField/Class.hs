{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module GHCup.Brick.Widgets.InputField.Class where

import GHCup.Brick.Widgets.BaseWidget

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..),
      (<+>))
import qualified Brick
import           Control.Applicative            ( (<|>) )
import Data.Some
import qualified Data.Text                     as T
import Optics (lens, (%))

import GHC.Generics ((:*:)(..), K1(..), M1(..), from, to, Generic, Rep)

-- | An error message
type ErrorMessage = T.Text
type HelpMessage = T.Text

class (BaseWidget n a) => InputField n a | a -> n where
  drawInputField :: Bool -> (Widget n -> Widget n) -> a -> Widget n
  getLabel :: a -> (n, T.Text)

class GInputFields n a | a -> n where
  getLabels :: a p -> [(n, T.Text)]
  gDrawInputFields :: n -> (Bool -> Widget n -> Widget n) -> a p -> [Widget n]
  gHandleEvent :: n -> a p -> BrickEvent n () -> EventM n (a p) (a p, Maybe HandleEventResult)
  gHasOverlay :: a p -> Maybe (Some (IsSubWidget n (a p)))
  gCloseOverlay :: EventM n (a p) (a p)

instance (GInputFields n f, GInputFields n g, Eq n) => GInputFields n (f :*: g) where
  getLabels (x :*: y) = getLabels x ++ getLabels y
  gDrawInputFields n f (x :*: y) = gDrawInputFields n f x ++ gDrawInputFields n f y
  gHandleEvent n (x :*: y) ev = do
    (_, (x', res1)) <- Brick.nestEventM x $ gHandleEvent n x ev
    (_, (y', res2)) <- Brick.nestEventM y $ gHandleEvent n y ev
    pure ((x' :*: y'), res1 <|> res2)
  gHasOverlay (x :*: y) = case gHasOverlay x of
    Nothing -> case gHasOverlay y of
      Nothing -> Nothing
      Just (Some (IsSubWidget accessor)) -> Just (Some (IsSubWidget $ lens (\(_ :*: y) -> y) (\(x :*: _) y -> (x :*: y)) % accessor))
    Just (Some (IsSubWidget accessor)) -> Just (Some (IsSubWidget $ lens (\(x :*: _) -> x) (\(_ :*: y) x -> (x :*: y)) % accessor))
  gCloseOverlay = do
    (x :*: y) <- Brick.get
    (_, x') <- Brick.nestEventM x $ gCloseOverlay
    (_, y') <-  Brick.nestEventM y $ gCloseOverlay
    pure (x' :*: y')

instance (InputField n a, Eq n) => GInputFields n (K1 i a) where
  getLabels (K1 x) = [getLabel x]
  gDrawInputFields n f (K1 x) =
    let focused = n == fst (getLabel x)
    in [drawInputField focused (f focused) x]
  gHandleEvent n (K1 x) ev = if fst (getLabel x) == n
    then do
      (x', res) <- Brick.nestEventM x $ handleEvent ev
      pure (K1 x', res)
    else pure (K1 x, Nothing)
  gHasOverlay (K1 x) = case hasOverlay x of
    Nothing -> Nothing
    Just (Some (IsSubWidget accessor)) -> Just (Some (IsSubWidget $ lens (\(K1 x) -> x) (\_ x -> (K1 x)) % accessor))
  gCloseOverlay = do
    (K1 x) <- Brick.get
    (x', _) <-  Brick.nestEventM x $ closeOverlay
    pure (K1 x')

instance (GInputFields n a, Eq n) => GInputFields n (M1 i t a) where
  getLabels (M1 x) = getLabels x
  gDrawInputFields n f (M1 x) = gDrawInputFields n f x
  gHandleEvent n (M1 x) ev = do
    (_, (x', res)) <- Brick.nestEventM x $ gHandleEvent n x ev
    pure (M1 x', res)
  gHasOverlay (M1 x) = case gHasOverlay x of
    Nothing -> Nothing
    Just (Some (IsSubWidget accessor)) -> Just (Some (IsSubWidget $ lens (\(M1 x) -> x) (\_ x -> (M1 x)) % accessor))
  gCloseOverlay = do
    (M1 x) <- Brick.get
    (_, x') <-  Brick.nestEventM x $ gCloseOverlay
    pure (M1 x')
