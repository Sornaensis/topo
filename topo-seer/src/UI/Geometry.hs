module UI.Geometry
  ( WindowSize(..)
  , LogHeight(..)
  , SeedInputWidth(..)
  , LayoutInputs(..)
  , TopBarGeometry(..)
  , PanelTabs(..)
  , ConfigTabs(..)
  , LogFilterButtons(..)
  , OverlayViewGeometry(..)
  , LeftPanelGeometry(..)
  , ConfigPanelGeometry(..)
  , LogPanelGeometry(..)
  , EditorGeometry(..)
  , UiGeometry(..)
  , panelTabsToTuple
  , configTabsToTuple
  , logFilterButtonsToTuple
  , overlayViewGeometryToTuple
  , rectLeft
  , rectTop
  , rectWidth
  , rectHeight
  , rectRight
  , rectBottom
  , rectNonNegative
  , rectContainsRect
  , rectsSeparatedHorizontally
  ) where

import Linear (V2(..))
import UI.Widgets (Rect(..))

newtype WindowSize = WindowSize { unWindowSize :: V2 Int }
  deriving (Eq, Show)

newtype LogHeight = LogHeight { unLogHeight :: Int }
  deriving (Eq, Show)

newtype SeedInputWidth = SeedInputWidth { unSeedInputWidth :: Int }
  deriving (Eq, Show)

data LayoutInputs = LayoutInputs
  { layoutInputWindowSize :: !WindowSize
  , layoutInputLogHeight :: !LogHeight
  , layoutInputSeedWidth :: !SeedInputWidth
  } deriving (Eq, Show)

data TopBarGeometry = TopBarGeometry
  { topBarBounds :: !Rect
  } deriving (Eq, Show)

data PanelTabs = PanelTabs
  { panelTabPrimary :: !Rect
  , panelTabSecondary :: !Rect
  } deriving (Eq, Show)

data ConfigTabs = ConfigTabs
  { configTabOne :: !Rect
  , configTabTwo :: !Rect
  , configTabThree :: !Rect
  , configTabFour :: !Rect
  , configTabFive :: !Rect
  , configTabSix :: !Rect
  , configTabSeven :: !Rect
  , configTabEight :: !Rect
  } deriving (Eq, Show)

data LogFilterButtons = LogFilterButtons
  { logFilterDebugButton :: !Rect
  , logFilterInfoButton :: !Rect
  , logFilterWarnButton :: !Rect
  , logFilterErrorButton :: !Rect
  } deriving (Eq, Show)

data OverlayViewGeometry = OverlayViewGeometry
  { overlayPreviousButton :: !Rect
  , overlayNextButton :: !Rect
  , overlayFieldPreviousButton :: !Rect
  , overlayFieldNextButton :: !Rect
  } deriving (Eq, Show)

data LeftPanelGeometry = LeftPanelGeometry
  { leftPanelBounds :: !Rect
  , leftPanelToggleButton :: !Rect
  , leftPanelTabs :: !PanelTabs
  , leftPanelControlsTopY :: !Int
  , leftPanelGenerateButton :: !Rect
  , leftPanelChunkMinusButton :: !Rect
  , leftPanelChunkValueBounds :: !Rect
  , leftPanelChunkPlusButton :: !Rect
  , leftPanelSeedLabelBounds :: !Rect
  , leftPanelSeedRandomButton :: !Rect
  , leftPanelSeedValueBounds :: !Rect
  , leftPanelViewButtons :: ![Rect]
  , leftPanelOverlayButtons :: !OverlayViewGeometry
  , leftPanelDayNightToggleButton :: !Rect
  , leftPanelViewContentHeightPx :: !Int
  , leftPanelViewScrollMaxPx :: !Int
  } deriving (Eq, Show)

data ConfigPanelGeometry = ConfigPanelGeometry
  { configPanelBounds :: !Rect
  , configPanelToggleButton :: !Rect
  , configPanelTabs :: !ConfigTabs
  , configPresetSaveButton :: !Rect
  , configPresetLoadButton :: !Rect
  , configResetButton :: !Rect
  , configRevertButton :: !Rect
  , configScrollAreaBounds :: !Rect
  , configScrollBarBounds :: !Rect
  } deriving (Eq, Show)

data LogPanelGeometry = LogPanelGeometry
  { logPanelBounds :: !Rect
  , logHeaderBounds :: !Rect
  , logBodyBounds :: !Rect
  , logFilterButtons :: !LogFilterButtons
  } deriving (Eq, Show)

data EditorGeometry = EditorGeometry
  { editorToolbarBounds :: !Rect
  , editorToolButtonBounds :: ![Rect]
  , editorRadiusMinusButton :: !Rect
  , editorRadiusValueBounds :: !Rect
  , editorRadiusPlusButton :: !Rect
  , editorCloseButton :: !Rect
  , editorReopenButton :: !Rect
  , editorParamBarBounds :: !Rect
  } deriving (Eq, Show)

data UiGeometry = UiGeometry
  { uiGeometryInputs :: !LayoutInputs
  , uiTopBarGeometry :: !TopBarGeometry
  , uiLeftPanelGeometry :: !LeftPanelGeometry
  , uiConfigPanelGeometry :: !ConfigPanelGeometry
  , uiLogPanelGeometry :: !LogPanelGeometry
  , uiEditorGeometry :: !EditorGeometry
  } deriving (Eq, Show)

panelTabsToTuple :: PanelTabs -> (Rect, Rect)
panelTabsToTuple tabs =
  (panelTabPrimary tabs, panelTabSecondary tabs)

configTabsToTuple :: ConfigTabs -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect)
configTabsToTuple tabs =
  ( configTabOne tabs
  , configTabTwo tabs
  , configTabThree tabs
  , configTabFour tabs
  , configTabFive tabs
  , configTabSix tabs
  , configTabSeven tabs
  , configTabEight tabs
  )

logFilterButtonsToTuple :: LogFilterButtons -> (Rect, Rect, Rect, Rect)
logFilterButtonsToTuple buttons =
  ( logFilterDebugButton buttons
  , logFilterInfoButton buttons
  , logFilterWarnButton buttons
  , logFilterErrorButton buttons
  )

overlayViewGeometryToTuple :: OverlayViewGeometry -> (Rect, Rect, Rect, Rect)
overlayViewGeometryToTuple buttons =
  ( overlayPreviousButton buttons
  , overlayNextButton buttons
  , overlayFieldPreviousButton buttons
  , overlayFieldNextButton buttons
  )

rectLeft :: Rect -> Int
rectLeft (Rect (V2 x _, _)) = x

rectTop :: Rect -> Int
rectTop (Rect (V2 _ y, _)) = y

rectWidth :: Rect -> Int
rectWidth (Rect (_, V2 w _)) = w

rectHeight :: Rect -> Int
rectHeight (Rect (_, V2 _ h)) = h

rectRight :: Rect -> Int
rectRight rect = rectLeft rect + rectWidth rect

rectBottom :: Rect -> Int
rectBottom rect = rectTop rect + rectHeight rect

rectNonNegative :: Rect -> Bool
rectNonNegative rect = rectWidth rect >= 0 && rectHeight rect >= 0

rectContainsRect :: Rect -> Rect -> Bool
rectContainsRect outer inner =
  rectLeft outer <= rectLeft inner
    && rectTop outer <= rectTop inner
    && rectRight inner <= rectRight outer
    && rectBottom inner <= rectBottom outer

rectsSeparatedHorizontally :: Int -> Rect -> Rect -> Bool
rectsSeparatedHorizontally gap left right =
  rectRight left + gap <= rectLeft right
    || rectRight right + gap <= rectLeft left
