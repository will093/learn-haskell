module TimeFormatter where

-- formatDuration :: (Integral i, Show i) => i -> String
-- formatDuration 0 = "now"
-- formatDuration n = formatResult . (reverse . (zip $ ["", " and "] ++ repeat ", ") . reverse ) $ filter ((>0) . snd) $ zip ["year", "day", "hour", "minute", "second"] (getDuration n)

-- formatResult :: (Integral i, Show i) => [(String, (String, i))] -> String
-- formatResult [] = ""
-- formatResult r = concat $ map (\(s, (u, v)) -> if (v > 1) then (show v ++ " " ++ u ++ "s" ++ s) else (show v ++ " " ++ u ++ s)) r

-- getDuration :: (Integral i) => i -> [i]
-- getDuration secs = fst $ foldl (\(norm, r) u -> (norm ++ [div r u] , mod r u)) ([], secs) [60 * 60 * 24 * 365, 60 * 60 * 24, 60 * 60, 60, 1]

import Data.Maybe(catMaybes)


formatDuration :: (Integral i) => i -> String
formatDuration n = show (toEnum (fromIntegral n) :: Time)

data Time = Time {
    year :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int
} deriving (Eq, Ord)

instance Enum Time where
    fromEnum Time { year=y, day=d, hour=h, minute=m, second=s }
        = 60 * (60 * (24 * (365 * y + d) + h) + m) + s
    toEnum ss = Time y d h m s
        where (ms,s) = ss `divMod` 60
              (hs, m) = ms `divMod` 60
              (ds, h) = hs `divMod` 24
              (y, d) = ds `divMod` 365

instance Show Time where
    show Time {year=y, day=d, hour=h, minute=m, second=s}
        = (toHumanReadable . formatList) [y, d, h, m, s]

toHumanReadable :: [String] -> String
toHumanReadable [] = ""
toHumanReadable [s] = s
toHumanReadable [s1, s2] = s1 ++ " and " ++ s2
toHumanReadable (str:strs) = str ++ ", " ++ toHumanReadable strs

formatList :: [Int] -> [String]
formatList [0, 0, 0, 0, 0] = ["now"]
formatList values = catMaybes $ zipWith showMaybeTimeUnit units values
    where units = ["year", "day", "hour", "minute", "second"]
          showMaybeTimeUnit :: String -> Int -> Maybe String
          showMaybeTimeUnit u v
            | v <= 0 = Nothing
            | v == 1 = Just (show v ++ " " ++ u)
            | otherwise = Just (show v ++ " " ++ u ++ "s")
