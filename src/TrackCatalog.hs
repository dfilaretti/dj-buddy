module TrackCatalog
  ( module Track,
    TrackCatalog,
    sameKeySuccessors,
    keyJumpSuccessors,
    harmonicSuccessors,
  )
where

import qualified Data.Map.Strict as Map
import qualified Key as Key
import Track

----------------------------------
----- Track catalog --------------
----------------------------------

type TrackCatalog = Map.Map TrackId Track

empty :: TrackCatalog
empty = Map.fromList []

add :: TrackId -> Track -> TrackCatalog -> TrackCatalog
add = Map.insert

remove :: TrackId -> TrackCatalog -> TrackCatalog
remove = Map.delete

filter_ :: (Track -> Bool) -> TrackCatalog -> TrackCatalog
filter_ = Map.filter

getTracks :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTracks p = Map.keys . filter_ p

getTracksWithKey :: Key.Key -> TrackCatalog -> [TrackId]
getTracksWithKey k = getTracks ((== k) . key)

-- all the tracks with the same key as this one
sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors tId lib =
  case desiredKey of
    Just k -> filter (/= tId) $ getTracksWithKey k lib
    Nothing -> []
  where
    desiredKey = trackKey tId lib

-- all the tracks we can jump to based on the Camelot wheel, excluding same key!
keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors tId lib =
  case initialKey of
    Just k -> getTracksWithKeys (Key.compatibleKeys k) lib
    Nothing -> []
  where
    initialKey = trackKey tId lib

-- all the tracks we can jump to according to the Camelot wheel
-- TODO: this doesn't include weird jumps such as the "Armin Van Buuren transition" etc. Consider adding later.
harmonicSuccessors :: TrackId -> TrackCatalog -> [TrackId]
harmonicSuccessors tId lib =
  sameKeySuccessors tId lib ++ keyJumpSuccessors tId lib

-- TODO: refactor?
getTracksWithKeys :: [Key.Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys ks lib =
  concat $ fmap (\k -> getTracksWithKey k lib) ks

trackKey :: TrackId -> TrackCatalog -> Maybe Key.Key
trackKey n lib = key <$> Map.lookup n lib
