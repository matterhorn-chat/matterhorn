{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draw.ListOverlay
  ( drawListOverlay
  , OverlayPosition(..)
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Foldable as F
import qualified Data.Text as T
import           Graphics.Vty ( imageWidth, translateX)
import           Lens.Micro.Platform ( (%~), (.~), to )

import           Themes
import           Types


hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

data OverlayPosition =
    OverlayCenter
    | OverlayUpperRight
    deriving (Eq, Show)

-- | Draw a ListOverlayState. This draws a bordered box with the
-- overlay's search input and results list inside the box. The provided
-- functions determine how to render the overlay in various states.
drawListOverlay :: ListOverlayState a b
                -- ^ The overlay state
                -> (b -> Widget Name)
                -- ^ The function to build the window title from the
                -- current search scope
                -> (b -> Widget Name)
                -- ^ The function to generate a message for the search
                -- scope indicating that no results were found
                -> (b -> Widget Name)
                -- ^ The function to generate the editor prompt for the
                -- search scope
                -> (Bool -> a -> Widget Name)
                -- ^ The function to render an item from the overlay's
                -- list
                -> Maybe (Widget Name)
                -- ^ The footer widget to render underneath the search
                -- results
                -> OverlayPosition
                -- ^ How to position the overlay layer
                -> Int
                -- ^ The maximum window width in columns
                -> Widget Name
drawListOverlay st scopeHeader scopeNoResults scopePrompt renderItem footer layerPos maxWinWidth =
  positionLayer $ hLimitWithPadding 10 $ vLimit 25 $
  hLimit maxWinWidth $
  borderWithLabel (withDefAttr clientEmphAttr $ scopeHeader scope) body
  where
      positionLayer = case layerPos of
          OverlayCenter -> centerLayer
          OverlayUpperRight -> upperRightLayer
      body = vBox [ (padRight (Pad 1) promptMsg) <+>
                    renderEditor (txt . T.unlines) True (st^.listOverlaySearchInput)
                  , cursorPositionBorder
                  , showResults
                  , fromMaybe emptyWidget footer
                  ]
      plural 1 = ""
      plural _ = "s"
      cursorPositionBorder =
          if st^.listOverlaySearching
          then hBorderWithLabel $ txt "[Searching...]"
          else case st^.listOverlaySearchResults.L.listSelectedL of
              Nothing -> hBorder
              Just _ ->
                  let msg = "Found " <> show numSearchResults <> " result" <> plural numSearchResults
                  in hBorderWithLabel $ str $ "[" <> msg <> "]"

      scope = st^.listOverlaySearchScope
      promptMsg = scopePrompt scope

      showMessage = center . withDefAttr clientEmphAttr

      showResults
        | numSearchResults == 0 = showMessage $ scopeNoResults scope
        | otherwise = renderedUserList

      renderedUserList = L.renderList renderItem True (st^.listOverlaySearchResults)
      numSearchResults = F.length $ st^.listOverlaySearchResults.L.listElementsL

upperRightLayer :: Widget a -> Widget a
upperRightLayer w =
    Widget (hSize w) (vSize w) $ do
        result <- render w
        c <- getContext
        let rWidth = result^.imageL.to imageWidth
            leftPaddingAmount = max 0 $ c^.availWidthL - rWidth
            paddedImage = translateX leftPaddingAmount $ result^.imageL
            off = Location (leftPaddingAmount, 0)
        if leftPaddingAmount == 0 then
            return result else
            return $ addResultOffset off
                   $ result & imageL .~ paddedImage
