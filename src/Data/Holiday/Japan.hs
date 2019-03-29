{-# LANGUAGE MultiWayIf #-}
module Data.Holiday.Japan
  ( Holiday(..)
  , display
  , holiday
  , isHoliday
  ) where

import           Data.Maybe                  (isJust)
import           Data.Time.Calendar          (Day, fromGregorian, toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)

-- | Data type for Japanese holidays.
data Holiday
  -- | New Year's Day
  = D元日
  -- | Coming of Age Day
  | D成人の日
  -- | National Foundation Day
  | D建国記念の日
  -- | Vernal Equinox Day
  | D春分の日
  -- | Showa Day
  | D昭和の日
  -- | Constitution Memorial Day
  | D憲法記念日
  -- | Greenery Day
  | Dみどりの日
  -- | Children's Day
  | Dこどもの日
  -- | Marine Day
  | D海の日
  -- | Mountain Day
  | D山の日
  -- | Respect for the Aged Day
  | D敬老の日
  -- | Autumnal Equinox Day
  | D秋分の日
  -- | Health and Sports Day
  | D体育の日
  -- | Sports Day
  | Dスポーツの日
  -- | Culture Day
  | D文化の日
  -- | Labour Thanksgiving Day
  | D勤労感謝の日
  -- | Emperor's Birthday
  | D天皇誕生日
  -- | National Holiday
  | D国民の休日
  -- | Make Up Holiday
  | D振替休日
  -- | Ceremonial of Enthronement
  | D即位礼正殿の儀
  -- | Rites of Showa Emperor Funeral
  | D昭和天皇の大喪の礼
  -- | Ceremonial of Prince Akihito's Marriage
  | D皇太子明仁親王の結婚の儀
  -- | Ceremonial of Prince Naruhito's Marriage
  | D皇太子徳仁親王の結婚の儀
  -- | Enthronement Day
  | D即位の日
  deriving (Eq, Show)

-- | Remove prefix \"D\" and show 'Holiday' name.
--
-- >>> putStrLn $ display D元日
-- 元日
display :: Holiday -> String
display h =
  case show h of
    ('D' : name) -> name
    _            -> error "`Holiday` value is started by D"

-- | Identify if the day is a holiday or not.
isHoliday :: Day -> Bool
isHoliday = isJust . holiday

-- | Identify which holiday the day is if possible.
--
-- >>> holiday $ fromGregorian 2015 5 5
-- Just Dこどもの日
--
-- >>> holiday $ fromGregorian 2015 12 8
-- Nothing
holiday :: Day -> Maybe Holiday
holiday = makeUp standardHoliday

standardHoliday :: Day -> Maybe Holiday
standardHoliday day | day < enforcement = Nothing
standardHoliday day = case toGregorian day of
  (_, 1, 1) -> Just D元日
  (y, 1, _)
    | y >= 2000 && isNthMonday 2 day -> Just D成人の日
  (_, 1, 15) -> Just D成人の日
  (y, 2, 11)
    | y >= 1967 -> Just D建国記念の日
  (y, 2, 23)
    | y >= 2020 -> Just D天皇誕生日
  (1989, 2, 24) -> Just D昭和天皇の大喪の礼
  (y, 3, d)
    | d == vernalEquinox y -> Just D春分の日
  (1959, 4, 10) -> Just D皇太子明仁親王の結婚の儀
  (y, 4, 29)
    | y >= 2007 -> Just D昭和の日
    | y >= 1989 -> Just Dみどりの日
    | otherwise -> Just D天皇誕生日
  (2019, 4, 30) -> Just D国民の休日
  (2019, 5, 1) -> Just D即位の日
  (2019, 5, 2) -> Just D国民の休日
  (_, 5, 3) -> Just D憲法記念日
  (y, 5, 4)
    | y >= 2007 -> Just Dみどりの日
    | y >= 1986 && not (isSunday day) && not (isMonday day) -> Just D国民の休日
  (_, 5, 5) -> Just Dこどもの日
  (1993, 6, 9) -> Just D皇太子徳仁親王の結婚の儀
  (2020, 7, 23) -> Just D海の日
  (2020, 7, 24) -> Just Dスポーツの日
  (2020, 7, _) -> Nothing
  (y, 7, _)
    | y >= 2003 && isNthMonday 3 day -> Just D海の日
  (y, 7, 20)
    | y >= 1996 -> Just D海の日
  (2020, 8, 10) -> Just D山の日
  (2020, 8, _) -> Nothing
  (y, 8, 11)
    | y >= 2016 -> Just D山の日
  (y, 9, d) -> let equinox = autumnalEquinox y
               in if d == equinox
                  then Just D秋分の日
                  else if y >= 2003
                       then if isNthMonday 3 day
                            then Just D敬老の日
                            else if isTuesday day && d == equinox - 1
                                 then Just D国民の休日
                                 else Nothing
                       else if y >= 1966 && d == 15
                            then Just D敬老の日
                            else Nothing
  (2019, 10, 22) -> Just D即位礼正殿の儀
  (y, 10, _)
    | y >= 2000 && isNthMonday 2 day -> if
      | y == 2020 -> Nothing
      | y >= 2020 -> Just Dスポーツの日
      | otherwise -> Just D体育の日
  (y, 10, 10)
    | y >= 1966 -> Just D体育の日
  (_, 11, 3) -> Just D文化の日
  (_, 11, 23) -> Just D勤労感謝の日
  (1990, 11, 12) -> Just D即位礼正殿の儀
  (y, 12, 23)
    | y >= 1989 && y <= 2018 -> Just D天皇誕生日
  _ -> Nothing

-- |
-- [昭和48年法律第10号 国民の祝日に関する法律の一部を改正する法律](http://www.shugiin.go.jp/Internet/itdb_housei.nsf/html/houritsu/07119730412010.htm)
-- [平成17年法律第43号 国民の祝日に関する法律の一部を改正する法律](http://www.shugiin.go.jp/Internet/itdb_housei.nsf/html/housei/16220050520043.htm)
-- 平成17年改正後のルールはそれ以前のルールを包含する
makeUp :: (Day -> Maybe Holiday) -> Day -> Maybe Holiday
makeUp holiday' day
  | isHoliday' day                       = holiday' day
  | day < enforcementOfMakeUpDay         = Nothing
  | continuousPreviousSundayHolidayExist = Just D振替休日
  | otherwise                            = Nothing
  where
    isHoliday' = isJust . holiday'
    continuousPreviousSundayHolidayExist =
      any isSunday $ takeWhile isHoliday' $ iterate pred $ pred day

enforcement :: Day
enforcement = fromGregorian 1948 7 20

enforcementOfMakeUpDay :: Day
enforcementOfMakeUpDay = fromGregorian 1973 4 12

third :: (a, b, c) -> c
third (_, _, x) = x

isMonday, isTuesday, isSunday :: Day -> Bool
isMonday  = (== 1) . third . toWeekDate
isTuesday = (== 2) . third . toWeekDate
isSunday  = (== 7) . third . toWeekDate

isNthWeekOfMonth :: Int -> Int -> Bool
isNthWeekOfMonth n dayOfMonth = (dayOfMonth - 1) `div` 7 + 1 == n

isNthMonday :: Int -> Day -> Bool
isNthMonday n day = isMonday day && isNthWeekOfMonth n (third (toGregorian day))

vernalEquinox :: Integer -> Int
vernalEquinox year
  | year <= 1947 = error "before the Act on National Holidays"
  | year <= 1979 = calculateEquinox year 20.8357
  | year <= 2099 = calculateEquinox year 20.8431
  | year <= 2150 = calculateEquinox year 21.8510
  | otherwise    = error "unknown calculation after 2151"

autumnalEquinox :: Integer -> Int
autumnalEquinox year
  | year <= 1947 = error "before the Act on National Holidays"
  | year <= 1979 = calculateEquinox year 23.2588
  | year <= 2099 = calculateEquinox year 23.2488
  | year <= 2150 = calculateEquinox year 24.2488
  | otherwise    = error "unknown calculation after 2151"

calculateEquinox :: Integer -> Double -> Int
calculateEquinox year factor =
  floor $ factor + 0.242194 * fromIntegral year' - fromIntegral (year' `div` 4)
  where
    year' :: Integer
    year' = year - 1980
