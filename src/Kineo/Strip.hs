-- | The strip: an ordered row of columns extending to the right. Each column
-- holds one or more windows stacked vertically and has a width expressed as
-- a fraction of the usable display width.
--
-- Everything here is pure and total. Operations that name a window which is
-- not in the strip leave the strip unchanged.
module Kineo.Strip
  ( WindowId
  , Column (..)
  , Strip
  , Dir (..)
  , column
  , empty
  , fromColumns
  , columns
  , windows
  , member
  , locate
  , columnOf
  , insertAt
  , insertAfter
  , remove
  , replace
  , adjustColumn
  , restrict
  , neighbor
  , firstWindow
  , lastWindow
  , moveColumn
  , moveInColumn
  , consume
  , expel
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word32)

-- | A CoreGraphics window number.
type WindowId = Word32

data Column = Column
  { stack :: NonEmpty WindowId
  -- ^ Top to bottom.
  , width :: Double
  -- ^ Fraction of the usable display width.
  , savedWidth :: Maybe Double
  -- ^ Width to return to when leaving full-width mode.
  }
  deriving stock (Eq, Show)

newtype Strip = Strip (Seq Column)
  deriving stock (Eq, Show)

data Dir = DirLeft | DirRight | DirUp | DirDown
  deriving stock (Eq, Show, Enum, Bounded)

-- | A single-window column.
column :: Double -> WindowId -> Column
column wd wid = Column {stack = wid :| [], width = wd, savedWidth = Nothing}

empty :: Strip
empty = Strip Seq.empty

fromColumns :: [Column] -> Strip
fromColumns = Strip . Seq.fromList

columns :: Strip -> [Column]
columns (Strip cs) = toList cs

-- | Every window, column by column, top to bottom.
windows :: Strip -> [WindowId]
windows s = concatMap (\c -> NE.toList c.stack) (columns s)

member :: WindowId -> Strip -> Bool
member wid s = wid `elem` windows s

-- | Column index and row index of a window.
locate :: WindowId -> Strip -> Maybe (Int, Int)
locate wid (Strip cs) = do
  ci <- Seq.findIndexL (\c -> wid `elem` c.stack) cs
  col <- Seq.lookup ci cs
  ri <- lookup wid (zip (NE.toList col.stack) [0 ..])
  pure (ci, ri)

columnOf :: WindowId -> Strip -> Maybe Column
columnOf wid (Strip cs) = do
  (ci, _) <- locate wid (Strip cs)
  Seq.lookup ci cs

-- | Insert a column at an index, clamped to the ends of the strip.
insertAt :: Int -> Column -> Strip -> Strip
insertAt i col (Strip cs) = Strip (Seq.insertAt (max 0 (min (Seq.length cs) i)) col cs)

-- | Insert a column immediately to the right of the column holding the
-- anchor window, or at the far right when there is no such column.
insertAfter :: Maybe WindowId -> Column -> Strip -> Strip
insertAfter anchor col (Strip cs) =
  case anchor >>= \a -> locate a (Strip cs) of
    Just (ci, _) -> Strip (Seq.insertAt (ci + 1) col cs)
    Nothing -> Strip (cs Seq.|> col)

-- | Remove a window, dropping its column if that leaves it empty.
remove :: WindowId -> Strip -> Strip
remove wid (Strip cs) = Strip (Seq.fromList (mapMaybe dropFrom (toList cs)))
  where
    dropFrom c = case NE.filter (/= wid) c.stack of
      [] -> Nothing
      (a : as) -> Just c {stack = a :| as}

-- | Put one window in another's place. Unchanged unless @old@ is in the
-- strip and @new@ is not.
replace :: WindowId -> WindowId -> Strip -> Strip
replace old new s@(Strip cs)
  | member old s && not (member new s) = Strip (fmap (\c -> c {stack = fmap swap c.stack}) cs)
  | otherwise = s
  where
    swap x = if x == old then new else x

-- | Modify the column holding a window.
adjustColumn :: WindowId -> (Column -> Column) -> Strip -> Strip
adjustColumn wid f (Strip cs) = case locate wid (Strip cs) of
  Just (ci, _) -> Strip (Seq.adjust' f ci cs)
  Nothing -> Strip cs

-- | Keep only the windows satisfying the predicate (for example the ones
-- that are not minimised), dropping columns that become empty.
restrict :: (WindowId -> Bool) -> Strip -> Strip
restrict keep (Strip cs) = Strip (Seq.fromList (mapMaybe go (toList cs)))
  where
    go c = case NE.filter keep c.stack of
      [] -> Nothing
      (a : as) -> Just c {stack = a :| as}

-- | The window one step away in the given direction. Moving sideways keeps
-- the row index where the target column is tall enough.
neighbor :: Dir -> WindowId -> Strip -> Maybe WindowId
neighbor dir wid (Strip cs) = do
  (ci, ri) <- locate wid (Strip cs)
  let rowIn c r = NE.toList c.stack !! min r (length c.stack - 1)
  case dir of
    DirLeft -> (`rowIn` ri) <$> Seq.lookup (ci - 1) cs
    DirRight -> (`rowIn` ri) <$> Seq.lookup (ci + 1) cs
    DirUp -> Seq.lookup ci cs >>= \c -> nth (ri - 1) (NE.toList c.stack)
    DirDown -> Seq.lookup ci cs >>= \c -> nth (ri + 1) (NE.toList c.stack)
  where
    nth i xs = if i < 0 then Nothing else listToMaybe (drop i xs)

firstWindow :: Strip -> Maybe WindowId
firstWindow s = case columns s of
  (c : _) -> Just (NE.head c.stack)
  [] -> Nothing

lastWindow :: Strip -> Maybe WindowId
lastWindow s = case reverse (columns s) of
  (c : _) -> Just (NE.head c.stack)
  [] -> Nothing

-- | Move the column holding a window one place left or right. Columns with
-- no visible window are stepped over, so the move is always visible.
moveColumn :: (WindowId -> Bool) -> Dir -> WindowId -> Strip -> Strip
moveColumn visible dir wid (Strip cs) = case locate wid (Strip cs) of
  Nothing -> Strip cs
  Just (ci, _) ->
    case target ci of
      Nothing -> Strip cs
      Just ti -> Strip (Seq.insertAt ti (Seq.index cs ci) (Seq.deleteAt ci cs))
  where
    shown c = any visible c.stack
    target ci = case dir of
      DirLeft -> listToMaybe [i | i <- [ci - 1, ci - 2 .. 0], shown (Seq.index cs i)]
      DirRight -> listToMaybe [i | i <- [ci + 1 .. Seq.length cs - 1], shown (Seq.index cs i)]
      _ -> Nothing

-- | Swap a window with its visible neighbour above or below in its column.
moveInColumn :: (WindowId -> Bool) -> Dir -> WindowId -> Strip -> Strip
moveInColumn visible dir wid s = adjustColumn wid swap s
  where
    swap c =
      let ws = NE.toList c.stack
          i = length (takeWhile (/= wid) ws)
          candidates = case dir of
            DirUp -> [j | j <- [i - 1, i - 2 .. 0], visible (ws !! j)]
            DirDown -> [j | j <- [i + 1 .. length ws - 1], visible (ws !! j)]
            _ -> []
       in case candidates of
            (j : _) -> c {stack = NE.fromList (swapAt i j ws)}
            [] -> c
    swapAt i j ws = [if k == i then ws !! j else if k == j then ws !! i else v | (k, v) <- zip [0 ..] ws]

-- | Pull a window out of its column and onto the bottom of the nearest
-- visible column to its left.
consume :: (WindowId -> Bool) -> WindowId -> Strip -> Strip
consume visible wid s@(Strip cs) = fromMaybe s $ do
  (ci, _) <- locate wid s
  ti <- listToMaybe [i | i <- [ci - 1, ci - 2 .. 0], any visible (Seq.index cs i).stack]
  -- Removing may drop the source column, but that is right of the host, so
  -- the host's index is unaffected.
  let Strip rest = remove wid s
      host = Seq.index rest ti
  pure (Strip (Seq.update ti host {stack = host.stack <> (wid :| [])} rest))

-- | Take a window out of a multi-window column into its own column directly
-- to the right, keeping the original column's width.
expel :: WindowId -> Strip -> Strip
expel wid s@(Strip cs) = case locate wid s of
  Just (ci, _)
    | c <- Seq.index cs ci
    , (a : as) <- NE.filter (/= wid) c.stack ->
        Strip
          ( Seq.insertAt (ci + 1) (column c.width wid)
              (Seq.update ci c {stack = a :| as} cs)
          )
  _ -> s
