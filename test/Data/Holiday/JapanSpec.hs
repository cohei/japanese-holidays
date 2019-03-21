module Data.Holiday.JapanSpec (spec) where

import           Data.Foldable      (for_)
import           Data.List          (intercalate)
import           Data.Time.Calendar (fromGregorian)
import           Test.Hspec         (Spec, describe, it, parallel, shouldBe)

import           Data.Holiday.Japan (Holiday (AutumnalEquinoxDay, Children'sDay, ComingOfAgeDay, ConstitutionMemorialDay, CultureDay, Emperor'sBirthday, GreeneryDay, HealthAndSportsDay, LabourThanksgivingDay, MakeUpHoliday, MarineDay, MountainDay, NationalFoundationDay, NationalHoliday, NewYear'sDay, RespectForTheAgedDay, ShowaDay, SportsDay, VernalEquinoxDay),
                                     holiday)

spec :: Spec
spec = parallel $
  describe "holiday" $ do
    let
      holidays :: [(Integer, Int, Int, Holiday)]
      holidays =
        [ (2015,  1,  1, NewYear'sDay)
        , (2015,  1, 12, ComingOfAgeDay)
        , (2015,  2, 11, NationalFoundationDay)
        , (2015,  3, 21, VernalEquinoxDay)
        , (2015,  4, 29, ShowaDay)
        , (2015,  5,  3, ConstitutionMemorialDay)
        , (2015,  5,  4, GreeneryDay)
        , (2015,  5,  5, Children'sDay)
        , (2015,  7, 20, MarineDay)
        , (2015,  9, 21, RespectForTheAgedDay)
        , (2015,  9, 22, NationalHoliday)
        , (2015,  9, 23, AutumnalEquinoxDay)
        , (2015, 10, 12, HealthAndSportsDay)
        , (2015, 11,  3, CultureDay)
        , (2015, 11, 23, LabourThanksgivingDay)
        , (2015, 12, 23, Emperor'sBirthday)
        , (2009,  5,  6, MakeUpHoliday)
        , (2016,  8, 11, MountainDay)
        ]

    holidays `for_` \(year, month, day, holiday') ->
      let
        padZero s = if length s == 1 then "0" ++ s else s
        date = intercalate "-" [show year, padZero (show month), padZero (show day)]
      in
        it (concat ["returns ", show holiday', " for ", date]) $
          holiday (fromGregorian year month day) `shouldBe` Just holiday'

    it "returns Nothing for 2015-09-15" $
      holiday (fromGregorian 2015 9 15) `shouldBe` Nothing

    describe "New Emperor's Birthday" $ do
      it "2018-12-23 is Emperor's Birthday" $
        holiday (fromGregorian 2018 12 23) `shouldBe` Just Emperor'sBirthday
      it "2019-12-23 is not Emperor's Birthday" $
        holiday (fromGregorian 2019 12 23) `shouldBe` Nothing
      it "2019-02-23 is not Emperor's Birthday" $
        holiday (fromGregorian 2019 2 23) `shouldBe` Nothing
      it "2020-02-23 is Emperor's Birthday" $
        holiday (fromGregorian 2020 2 23) `shouldBe` Just Emperor'sBirthday

    describe "Health and Sports Day is turend into Sports Day from 2020" $ do
      it "2019-10-14 is Health and Sports Day" $
        holiday (fromGregorian 2019 10 14) `shouldBe` Just HealthAndSportsDay
      it "2021-10-11 is Sports Day" $
        holiday (fromGregorian 2021 10 11) `shouldBe` Just SportsDay

    describe "Tokyo Olympic" $ do
      it "2020-07-23 is Marine Day" $
        holiday (fromGregorian 2020 7 23) `shouldBe` Just MarineDay
      it "2020-07-20 is not Marine Day" $
        holiday (fromGregorian 2020 7 20) `shouldBe` Nothing

      it "2020-07-24 is Sports Day" $
        holiday (fromGregorian 2020 7 24) `shouldBe` Just SportsDay
      it "2020-10-12 is not Sports Day" $
        holiday (fromGregorian 2020 10 12) `shouldBe` Nothing

      it "2020-08-10 is Mountain Day" $
        holiday (fromGregorian 2020 8 10) `shouldBe` Just MountainDay
      it "2020-08-11 is not Mountain Day" $
        holiday (fromGregorian 2020 8 11) `shouldBe` Nothing

    describe "NationalHoliday" $ do
      it "1986-05-04 is not National Holiday" $
        holiday (fromGregorian 1986 5 4) `shouldBe` Nothing
      it "1987-05-04 is not National Holiday" $
        holiday (fromGregorian 1987 5 4) `shouldBe` Just MakeUpHoliday
      it "1988-05-04 is National Holiday" $
        holiday (fromGregorian 1988 5 4) `shouldBe` Just NationalHoliday
