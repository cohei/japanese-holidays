{-# LANGUAGE MultiWayIf #-}

module Data.Time.Calendar.Holiday.Japan
  ( Holiday(..)
  , toJapanese
  , holiday
  , isHoliday
  ) where

import Data.Maybe (isJust)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

third :: (a, b, c) -> c
third (_, _, x) = x

isMonday, isTuesday, isWednesday :: Day -> Bool
isMonday    = (== 1) . third . toWeekDate
isTuesday   = (== 2) . third . toWeekDate
isWednesday = (== 3) . third . toWeekDate

vernalEquinox :: Integer -> Int
vernalEquinox year = if
  | year <= 1947 -> error "before the Act on National Holidays"
  | year <= 1979 -> calculateEquinox year 20.8357
  | year <= 2099 -> calculateEquinox year 20.8431
  | year <= 2150 -> calculateEquinox year 21.8510
  | otherwise    -> error "unknown calculation after 2151"

autumnalEquinox :: Integer -> Int
autumnalEquinox year = if
  | year <= 1947 -> error "before the Act on National Holidays"
  | year <= 1979 -> calculateEquinox year 23.2588
  | year <= 2099 -> calculateEquinox year 23.2488
  | year <= 2150 -> calculateEquinox year 24.2488
  | otherwise    -> error "unknown calculation after 2151"

calculateEquinox :: Integer -> Double -> Int
calculateEquinox year factor =
  floor $ factor + 0.242194 * fromIntegral year' - fromIntegral (year' `div` 4)
  where
    year' :: Integer
    year' = year - 1980

enforcement :: Day
enforcement = fromGregorian 1948 7 20

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
  | CultureDay
  | LabourThanksgivingDay
  | Emperor'sBirthday
  | NationalHoliday
  | MakeUpHoliday
  | CeremonialOfEnthronement
  | RitesOfShowaEmperorFuneral
  | CeremonialOfPrinceAkihito'sMarriage
  | CeremonialOfPrinceNaruhito'sMarriage
  deriving (Eq, Show)

-- | Returns Japanese name of holidays.
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
toJapanese CultureDay                           = "文化の日"
toJapanese LabourThanksgivingDay                = "勤労感謝の日"
toJapanese Emperor'sBirthday                    = "天皇誕生日"
toJapanese NationalHoliday                      = "国民の休日"
toJapanese MakeUpHoliday                        = "振替休日"
toJapanese CeremonialOfEnthronement             = "即位礼正殿の儀"
toJapanese RitesOfShowaEmperorFuneral           = "昭和天皇の大喪の礼"
toJapanese CeremonialOfPrinceAkihito'sMarriage  = "皇太子明仁親王の結婚の儀"
toJapanese CeremonialOfPrinceNaruhito'sMarriage = "皇太子徳仁親王の結婚の儀"

-- | Determine which holiday the day is if possible.
--
-- >>> putStrLn $ toJapanese $ Data.Maybe.fromJust $ holiday $ fromGregorian 2015 5 5
-- こどもの日
--
-- >>> holiday $ fromGregorian 2015 12 8
-- Nothing
holiday :: Day -> Maybe Holiday
holiday day | day < enforcement = Nothing
holiday day =
  let
    (y', m', d') = toGregorian day
    weekOfMonth = (d' - 1) `div` 7  -- zero-based
  in case (y', m', d') of
  (_, 1, 1) -> Just NewYear'sDay
  (y, 1, _)
    | y >= 2000 -> if weekOfMonth == 1 && isMonday day then Just ComingOfAgeDay else Nothing
  (_, 1, 15) -> Just ComingOfAgeDay
  (y, 2, 11)
    | y >= 1967 -> Just NationalFoundationDay
  (1989, 2, 24) -> Just RitesOfShowaEmperorFuneral
  (y, 3, d)
    | d == vernalEquinox y -> Just VernalEquinoxDay
  (y, 4, 29)
    | y >= 2007 -> Just ShowaDay
    | y >= 1989 -> Just GreeneryDay
    | otherwise -> Just Emperor'sBirthday
  (1959, 4, 10) -> Just CeremonialOfPrinceAkihito'sMarriage
  (_, 5, 3) -> Just ConstitutionMemorialDay
  (y, 5, 4)
    | y >= 2007 -> Just GreeneryDay
    | y >= 1986 && isMonday day -> Just NationalHoliday
  (_, 5, 5) -> Just Children'sDay
  (y, 5, 6)
    | y >= 2007 && (isTuesday day || isWednesday day) -> Just MakeUpHoliday
  (1993, 6, 9) -> Just CeremonialOfPrinceNaruhito'sMarriage
  (y, 7, _)
    | y >= 2003 -> if weekOfMonth == 2 && isMonday day then Just MarineDay else Nothing
  (y, 7, 20)
    | y >= 1996 -> Just MarineDay
  (y, 8, 11)
    | y >= 2016 -> Just MountainDay
  (y, 9, d) -> let equinox = autumnalEquinox y
               in if d == equinox
                  then Just AutumnalEquinoxDay
                  else if y >= 2003
                       then if weekOfMonth == 2 && isMonday day
                            then Just RespectForTheAgedDay
                            else if isTuesday day && d == equinox - 1
                                 then Just NationalHoliday
                                 else Nothing
                       else if y >= 1966 && d == 15
                            then Just RespectForTheAgedDay
                            else Nothing
  (y, 10, _)
    | y >= 2000 -> if weekOfMonth == 1 && isMonday day then Just HealthAndSportsDay else Nothing
  (y, 10, 10)
    | y >= 1966 -> Just HealthAndSportsDay
  (_, 11, 3) -> Just CultureDay
  (_, 11, 23) -> Just LabourThanksgivingDay
  (1990, 11, 12) -> Just CeremonialOfEnthronement
  (y, 12, 23)
    | y >= 1989 -> Just Emperor'sBirthday
  _ | isMonday day && isHoliday (addDays (-1) day) -> Just MakeUpHoliday
  _ -> Nothing

-- | The day is a holiday or not.
isHoliday :: Day -> Bool
isHoliday = isJust . holiday
