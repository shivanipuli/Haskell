
module Notes8 where

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just $ f x

mapListMaybe :: (a -> b) -> [Maybe a] -> [Maybe b]
mapListMaybe f = map (mapMaybe f)

applyMaybeFancy :: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybeFancy _ Nothing  = Nothing
applyMaybeFancy f (Just x) = f x

andThenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
andThenMaybe = flip applyMaybeFancy

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe Nothing = Nothing
joinMaybe (Just x) = andThenMaybe (x) (\i -> x)