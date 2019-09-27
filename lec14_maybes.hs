safeDiv :: Double -> Double -> Maybe Double
safeDiv x 0 = Nothing
safeDiv x y = Just (x / y)

increment :: Maybe Double -> Maybe Double
increment Nothing = Nothing
increment (Just x) = Just (x + 1)

identityForAdd :: Maybe Int -> Int
identityForAdd (Just x) = x
identityFOrAdd Nothing  = 0
