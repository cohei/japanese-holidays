{-# LANGUAGE MultiWayIf #-}
module Data.Holiday.Japan
  ( Holiday(..)
  , display
  , holiday
  , isHoliday
  ) where

import           Control.Monad               (join)
import           Data.Maybe                  (isJust)
import           Data.Monoid                 (All (All, getAll),
                                              First (First, getFirst))
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
holiday d | d < enforcement = Nothing
holiday d = getFirst $ (standardHoliday <> makeUp) d

enforcement :: Day
enforcement = fromGregorian 1948 7 20

type Definition = Day -> First Holiday

-- |
-- [昭和48年法律第10号 国民の祝日に関する法律の一部を改正する法律](http://www.shugiin.go.jp/Internet/itdb_housei.nsf/html/houritsu/07119730412010.htm)
-- [平成17年法律第43号 国民の祝日に関する法律の一部を改正する法律](http://www.shugiin.go.jp/Internet/itdb_housei.nsf/html/housei/16220050520043.htm)
-- 平成17年改正後のルールはそれ以前のルールを包含する
makeUp :: Definition
makeUp = D振替休日 @@ sinceDay enforcementOfMakeUpDay <> continuousPreviousSundayHolidayExist
  where
    isStandardHoliday = isJust . getFirst . standardHoliday
    continuousPreviousSundayHolidayExist =
      All . any isSunday . takeWhile isStandardHoliday . iterate pred . pred

enforcementOfMakeUpDay :: Day
enforcementOfMakeUpDay = fromGregorian 1973 4 12

standardHoliday :: Definition
standardHoliday =
  mconcat
    [ D元日 @@ month 1 <> day 1
    , D成人の日 @@ since 2000 <> month 1 <> nth 2 monday
    , D成人の日 @@ month 1 <> day 15
    , D建国記念の日 @@ since 1967 <> month 2 <> day 11
    , D昭和天皇の大喪の礼 @@ year 1989 <> month 2 <> day 24
    , D天皇誕生日 @@ since 2020 <> month 2 <> day 23
    , D春分の日 @@ month 3 <> vernalEquinoxDay
    , D皇太子明仁親王の結婚の儀 @@ year 1959 <> month 4 <> day 10
    , D昭和の日 @@ since 2007 <> month 4 <> day 29
    , Dみどりの日 @@ since 1989 <> month 4 <> day 29
    , D天皇誕生日 @@ month 4 <> day 29
    , D国民の休日 @@ year 2019 <> month 4 <> day 30
    , D即位の日 @@ year 2019 <> month 5 <> day 1
    , D国民の休日 @@ year 2019 <> month 5 <> day 2
    , D憲法記念日 @@ month 5 <> day 3
    , Dみどりの日 @@ since 2007 <> month 5 <> day 4
    , D国民の休日 @@ since 1986 <> month 5 <> day 4 <> notP sunday <> notP monday
    , Dこどもの日 @@ month 5 <> day 5
    , D皇太子徳仁親王の結婚の儀 @@ year 1993 <> month 6 <> day 9
    , D海の日 @@ year 2020 <> month 7 <> day 23
    , D海の日 @@ since 2003 <> notP (year 2020) <> month 7 <> nth 3 monday
    , D海の日 @@ since 1996 <> notP (year 2020) <> month 7 <> day 20
    , Dスポーツの日 @@ year 2020 <> month 7 <> day 24
    , D山の日 @@ year 2020 <> month 8 <> day 10
    , D山の日 @@ since 2016 <> notP (year 2020) <> month 8 <> day 11
    , D秋分の日 @@ month 9 <> autumnalEquinoxDay
    , D敬老の日 @@ since 2003 <> month 9 <> nth 3 monday
    , D敬老の日 @@ since 1966 <> notP (since 2003) <> month 9 <> day 15
    , D国民の休日 @@ tuesday <> join (day . pred . autumnalEquinox . gregorianYear)
    , D即位礼正殿の儀 @@ year 2019 <> month 10 <> day 22
    , Dスポーツの日 @@ since 2021 <> month 10 <> nth 2 monday
    , D体育の日 @@ since 2000 <> notP (year 2020) <> month 10 <> nth 2 monday
    , D体育の日 @@ since 1966 <> month 10 <> day 10
    , D文化の日 @@ month 11 <> day 3
    , D勤労感謝の日 @@ month 11 <> day 23
    , D即位礼正殿の儀 @@ year 1990 <> month 11 <> day 12
    , D天皇誕生日 @@ since 1989 <> notP (since 2019) <> month 12 <> day 23
    ]

type Predicate = Day -> All

(@@) :: Holiday -> Predicate -> Definition
h @@ p = \d -> First $ toMaybe (getAll (p d)) h
infixr 5 @@

year :: Integer -> Predicate
year i = All . (i ==) . gregorianYear

month :: Int -> Predicate
month i = All . (i ==) . gregorianMonth

day :: Int -> Predicate
day i = All . (i ==) . gregorianDay

vernalEquinoxDay :: Predicate
vernalEquinoxDay = join $ day . vernalEquinox . gregorianYear

autumnalEquinoxDay :: Predicate
autumnalEquinoxDay = join $ day . autumnalEquinox . gregorianYear

since :: Integer -> Predicate
since y = All . (>= y) . gregorianYear

sinceDay :: Day -> Predicate
sinceDay d = All . (>= d)

notP :: Predicate -> Predicate
notP p = All . not . getAll . p

sunday, monday, tuesday :: Predicate
sunday = All . isSunday
monday = All . isMonday
tuesday = All . isTuesday

nth :: Int -> Predicate -> Predicate
nth n p = p <> All . isNthWeekOfMonth n . gregorianDay

gregorianYear :: Day -> Integer
gregorianYear = first3 . toGregorian

gregorianMonth :: Day -> Int
gregorianMonth = second3 . toGregorian

gregorianDay :: Day -> Int
gregorianDay = third3 . toGregorian

toMaybe :: Bool -> a -> Maybe a
toMaybe b x = if b then Just x else Nothing

first3 :: (a, b, c) -> a
first3 (x, _, _) = x

second3 :: (a, b, c) -> b
second3 (_, x, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

isMonday, isTuesday, isSunday :: Day -> Bool
isMonday  = (== 1) . third3 . toWeekDate
isTuesday = (== 2) . third3 . toWeekDate
isSunday  = (== 7) . third3 . toWeekDate

isNthWeekOfMonth :: Int -> Int -> Bool
isNthWeekOfMonth n dayOfMonth = (dayOfMonth - 1) `div` 7 + 1 == n

vernalEquinox :: Integer -> Int
vernalEquinox y
  | y <= 1947 = error "before the Act on National Holidays"
  | y <= 1979 = calculateEquinox y 20.8357
  | y <= 2099 = calculateEquinox y 20.8431
  | y <= 2150 = calculateEquinox y 21.8510
  | otherwise    = error "unknown calculation after 2151"

autumnalEquinox :: Integer -> Int
autumnalEquinox y
  | y <= 1947 = error "before the Act on National Holidays"
  | y <= 1979 = calculateEquinox y 23.2588
  | y <= 2099 = calculateEquinox y 23.2488
  | y <= 2150 = calculateEquinox y 24.2488
  | otherwise    = error "unknown calculation after 2151"

calculateEquinox :: Integer -> Double -> Int
calculateEquinox y factor =
  floor $ factor + 0.242194 * fromIntegral y' - fromIntegral (y' `div` 4)
  where
    y' :: Integer
    y' = y - 1980
