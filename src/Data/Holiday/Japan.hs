{-# LANGUAGE MultiWayIf #-}
module Data.Holiday.Japan
  ( Holiday(..)
  , toJapanese
  , holiday
  , isHoliday
  ) where

import           Data.Maybe                  (isJust)
import           Data.Time.Calendar          (Day, addDays, fromGregorian,
                                              toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)

-- | Data type for Japanese holidays.
data Holiday
  = NewYear'sDay
  | ComingOfAgeDay
  | NationalFoundationDay
  | VernalEquinoxDay
  | ShowaDay
  | ConstitutionMemorialDay
  | GreeneryDay
  | Children'sDay
  | MarineDay
  | MountainDay
  | RespectForTheAgedDay
  | AutumnalEquinoxDay
  | HealthAndSportsDay
  | SportsDay
  | CultureDay
  | LabourThanksgivingDay
  | Emperor'sBirthday
  | NationalHoliday
  | MakeUpHoliday
  | CeremonialOfEnthronement
  | RitesOfShowaEmperorFuneral
  | CeremonialOfPrinceAkihito'sMarriage
  | CeremonialOfPrinceNaruhito'sMarriage
  | EnthronementDay
  deriving (Eq, Show)

-- | Return Japanese name of @Holiday@s.
toJapanese :: Holiday -> String
toJapanese NewYear'sDay                         = "元日"
toJapanese ComingOfAgeDay                       = "成人の日"
toJapanese NationalFoundationDay                = "建国記念の日"
toJapanese VernalEquinoxDay                     = "春分の日"
toJapanese ShowaDay                             = "昭和の日"
toJapanese ConstitutionMemorialDay              = "憲法記念日"
toJapanese GreeneryDay                          = "みどりの日"
toJapanese Children'sDay                        = "こどもの日"
toJapanese MarineDay                            = "海の日"
toJapanese MountainDay                          = "山の日"
toJapanese RespectForTheAgedDay                 = "敬老の日"
toJapanese AutumnalEquinoxDay                   = "秋分の日"
toJapanese HealthAndSportsDay                   = "体育の日"
toJapanese SportsDay                            = "スポーツの日"
toJapanese CultureDay                           = "文化の日"
toJapanese LabourThanksgivingDay                = "勤労感謝の日"
toJapanese Emperor'sBirthday                    = "天皇誕生日"
toJapanese NationalHoliday                      = "国民の休日"
toJapanese MakeUpHoliday                        = "振替休日"
toJapanese CeremonialOfEnthronement             = "即位礼正殿の儀"
toJapanese RitesOfShowaEmperorFuneral           = "昭和天皇の大喪の礼"
toJapanese CeremonialOfPrinceAkihito'sMarriage  = "皇太子明仁親王の結婚の儀"
toJapanese CeremonialOfPrinceNaruhito'sMarriage = "皇太子徳仁親王の結婚の儀"
toJapanese EnthronementDay                      = "即位の日"

-- | Identify if the day is a holiday or not.
isHoliday :: Day -> Bool
isHoliday = isJust . holiday

-- | Identify which holiday the day is if possible.
--
-- >>> putStrLn $ toJapanese $ Data.Maybe.fromJust $ holiday $ fromGregorian 2015 5 5
-- こどもの日
--
-- >>> holiday $ fromGregorian 2015 12 8
-- Nothing
holiday :: Day -> Maybe Holiday
holiday day | day < enforcement = Nothing
holiday day = case toGregorian day of
  (_, 1, 1) -> Just NewYear'sDay
  (y, 1, _)
    | y >= 2000 && isNthMonday 2 day -> Just ComingOfAgeDay
  (_, 1, 15) -> Just ComingOfAgeDay
  (y, 2, 11)
    | y >= 1967 -> Just NationalFoundationDay
  (y, 2, 23)
    | y >= 2020 -> Just Emperor'sBirthday
  (1989, 2, 24) -> Just RitesOfShowaEmperorFuneral
  (y, 3, d)
    | d == vernalEquinox y -> Just VernalEquinoxDay
  (1959, 4, 10) -> Just CeremonialOfPrinceAkihito'sMarriage
  (y, 4, 29)
    | y >= 2007 -> Just ShowaDay
    | y >= 1989 -> Just GreeneryDay
    | otherwise -> Just Emperor'sBirthday
  (2019, 4, 30) -> Just NationalHoliday
  (2019, 5, 1) -> Just EnthronementDay
  (2019, 5, 2) -> Just NationalHoliday
  (_, 5, 3) -> Just ConstitutionMemorialDay
  (y, 5, 4)
    | y >= 2007 -> Just GreeneryDay
    | y >= 1986 && not (isSunday day) && not (isMonday day) -> Just NationalHoliday
  (_, 5, 5) -> Just Children'sDay
  (y, 5, 6)
    | y >= 2007 && (isTuesday day || isWednesday day) -> Just MakeUpHoliday
  (1993, 6, 9) -> Just CeremonialOfPrinceNaruhito'sMarriage
  (2020, 7, 23) -> Just MarineDay
  (2020, 7, 24) -> Just SportsDay
  (2020, 7, _) -> Nothing
  (y, 7, _)
    | y >= 2003 && isNthMonday 3 day -> Just MarineDay
  (y, 7, 20)
    | y >= 1996 -> Just MarineDay
  (2020, 8, 10) -> Just MountainDay
  (2020, 8, _) -> Nothing
  (y, 8, 11)
    | y >= 2016 -> Just MountainDay
  (y, 9, d) -> let equinox = autumnalEquinox y
               in if d == equinox
                  then Just AutumnalEquinoxDay
                  else if y >= 2003
                       then if isNthMonday 3 day
                            then Just RespectForTheAgedDay
                            else if isTuesday day && d == equinox - 1
                                 then Just NationalHoliday
                                 else Nothing
                       else if y >= 1966 && d == 15
                            then Just RespectForTheAgedDay
                            else Nothing
  (2019, 10, 22) -> Just CeremonialOfEnthronement
  (y, 10, _)
    | y >= 2000 && isNthMonday 2 day -> if
      | y == 2020 -> Nothing
      | y >= 2020 -> Just SportsDay
      | otherwise -> Just HealthAndSportsDay
  (y, 10, 10)
    | y >= 1966 -> Just HealthAndSportsDay
  (_, 11, 3) -> Just CultureDay
  (_, 11, 23) -> Just LabourThanksgivingDay
  (1990, 11, 12) -> Just CeremonialOfEnthronement
  (y, 12, 23)
    | y >= 1989 && y <= 2018 -> Just Emperor'sBirthday
  _ | isMonday day && isHoliday (addDays (-1) day) -> Just MakeUpHoliday
  _ -> Nothing

enforcement :: Day
enforcement = fromGregorian 1948 7 20

third :: (a, b, c) -> c
third (_, _, x) = x

isMonday, isTuesday, isWednesday, isSunday :: Day -> Bool
isMonday    = (== 1) . third . toWeekDate
isTuesday   = (== 2) . third . toWeekDate
isWednesday = (== 3) . third . toWeekDate
isSunday    = (== 7) . third . toWeekDate

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
