stammtisch = drop 143 $ zipWith3 (\w d m -> (wday w, 2000 + (m `div` 12), 1 + (m `mod` 12), d)) wdays days [0..]
        where 
          wday 1 = "Mon"
          wday 2 = "Tue"
          wday 3 = "Wed"
          wday 4 = "Thu"
          days = zipWith (+) months subs
          subs = map (\x -> ((x-1) `mod` 7) - 6) $ zipWith (-) wdays $ tail $
                 scanl (\b a -> (b + a) `mod` 7) 5 months
          wdays = cycle [1..4]
          months = map (\x -> g (x+1) - g x) [0..]
          g m =
              365*y' +
                (y' `div` 4) - (y' `div` 100) + (y' `div` 400) +
                ((m'*306 + 5) `div` 10) 
                  where m' = ((m `mod` 12)+10) `mod` 12
                        y' = (2000+(m `div` 12)) - (m' `div` 10)
