aitken :: (Double -> Double) -> [Double] -> Int -> Int -> Double
aitken f h n j
    -- | j >= n + k = Nothing
    | n == 0 = f $ h !! j
    | otherwise = let ajm1 = aitken f h (n-1) j
                      aj1m1 = aitken f h (n-1) (j+1)
                      hjm = h !! (j+n)
                      hj = h !! j
                  in ajm1 + (ajm1 - aj1m1) / ((hjm/hj) - 1.0)
