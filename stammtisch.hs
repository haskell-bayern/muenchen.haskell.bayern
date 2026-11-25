monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

stammtisch = map (\(w, y, m, d) -> (w ++ ", " ++ (show d) ++ " " ++ (monthNames !! (m - 1)) ++ " " ++ (show y))) $
             drop 143 $
             zipWith3 (\w d m -> (wday w, 2000 + (m `div` 12), 1 + (m `mod` 12), d)) wdays days [0..]
        where 
          wday 1 = "Mon"
          wday 2 = "Tue"
          wday 3 = "Wed"
          wday 4 = "Thu"
          days = zipWith (+) monthLens subs
          subs = map (\x -> ((x-1) `mod` 7) - 6) $ zipWith (-) wdays $ drop 1 $
                 scanl (\b a -> (b + a) `mod` 7) 5 monthLens
          wdays = cycle [1..4]
          monthLens = map (\x -> g (x+1) - g x) [0..]
          g m =
              365*y' +
                (y' `div` 4) - (y' `div` 100) + (y' `div` 400) +
                ((m'*306 + 5) `div` 10) 
                  where m' = ((m `mod` 12)+10) `mod` 12
                        y' = (2000+(m `div` 12)) - (m' `div` 10)
