{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Holiday.JapanSpec (spec) where

import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck

import Data.Holiday.Japan

deriving instance Arbitrary Day

spec :: Spec
spec = do
  describe "holiday" $ do
    it "returns `Maybe` values" $ do
      property $ (\h -> isJust h || isNothing h) . holiday

    let
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
      it (concat ["returns ", show holiday', " for ", show year, "/", show month, "/", show day]) $ do
        holiday (fromGregorian year month day) `shouldBe` Just holiday'

    it "returns Nothing for 2015/9/15" $ do
      holiday (fromGregorian 2015 9 15) `shouldBe` Nothing
