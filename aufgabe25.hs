aitken :: (Double -> Double) -> [Double] -> Int -> Int -> Double
aitken f h n k
    | k >= n + (length h) = -1
    | n == 0 = f $ h !! k
    | otherwise = let akm1 = aitken f h (n-1) k
                      ak1m1 = aitken f h (n-1) (k-1)
                      hkm = h !! (k+n)
                      hk = h !! k
                  in akm1 + (akm1 - ak1m1) / ((hkm/hk) - 1.0) 
