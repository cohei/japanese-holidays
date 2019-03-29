module Data.Holiday.JapanSpec (spec) where

import           Data.Foldable      (for_)
import           Data.List          (intercalate)
import           Data.Time.Calendar (fromGregorian)
import           Test.Hspec         (Spec, describe, it, parallel, shouldBe)

import           Data.Holiday.Japan (Holiday (Dこどもの日, Dみどりの日, Dスポーツの日, D体育の日, D元日, D勤労感謝の日, D即位の日, D国民の休日, D天皇誕生日, D山の日, D建国記念の日, D憲法記念日, D成人の日, D振替休日, D敬老の日, D文化の日, D春分の日, D昭和の日, D海の日, D秋分の日),
                                     holiday)

spec :: Spec
spec = parallel $
  describe "holiday" $ do
    let
      holidays :: [(Integer, Int, Int, Holiday)]
      holidays =
        [ (2015,  1,  1, D元日)
        , (2015,  1, 12, D成人の日)
        , (2015,  2, 11, D建国記念の日)
        , (2015,  3, 21, D春分の日)
        , (2015,  4, 29, D昭和の日)
        , (2015,  5,  3, D憲法記念日)
        , (2015,  5,  4, Dみどりの日)
        , (2015,  5,  5, Dこどもの日)
        , (2015,  7, 20, D海の日)
        , (2015,  9, 21, D敬老の日)
        , (2015,  9, 22, D国民の休日)
        , (2015,  9, 23, D秋分の日)
        , (2015, 10, 12, D体育の日)
        , (2015, 11,  3, D文化の日)
        , (2015, 11, 23, D勤労感謝の日)
        , (2015, 12, 23, D天皇誕生日)
        , (2009,  5,  6, D振替休日)
        , (2016,  8, 11, D山の日)
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
        holiday (fromGregorian 2018 12 23) `shouldBe` Just D天皇誕生日
      it "2019-12-23 is not Emperor's Birthday" $
        holiday (fromGregorian 2019 12 23) `shouldBe` Nothing
      it "2019-02-23 is not Emperor's Birthday" $
        holiday (fromGregorian 2019 2 23) `shouldBe` Nothing
      it "2020-02-23 is Emperor's Birthday" $
        holiday (fromGregorian 2020 2 23) `shouldBe` Just D天皇誕生日

    describe "Health and Sports Day is turend into Sports Day from 2020" $ do
      it "2019-10-14 is Health and Sports Day" $
        holiday (fromGregorian 2019 10 14) `shouldBe` Just D体育の日
      it "2021-10-11 is Sports Day" $
        holiday (fromGregorian 2021 10 11) `shouldBe` Just Dスポーツの日

    describe "Tokyo Olympic" $ do
      it "2020-07-23 is Marine Day" $
        holiday (fromGregorian 2020 7 23) `shouldBe` Just D海の日
      it "2020-07-20 is not Marine Day" $
        holiday (fromGregorian 2020 7 20) `shouldBe` Nothing

      it "2020-07-24 is Sports Day" $
        holiday (fromGregorian 2020 7 24) `shouldBe` Just Dスポーツの日
      it "2020-10-12 is not Sports Day" $
        holiday (fromGregorian 2020 10 12) `shouldBe` Nothing

      it "2020-08-10 is Mountain Day" $
        holiday (fromGregorian 2020 8 10) `shouldBe` Just D山の日
      it "2020-08-11 is not Mountain Day" $
        holiday (fromGregorian 2020 8 11) `shouldBe` Nothing

    describe "NationalHoliday" $ do
      it "1986-05-04 is not National Holiday" $
        holiday (fromGregorian 1986 5 4) `shouldBe` Nothing
      it "1987-05-04 is National Holiday" $
        holiday (fromGregorian 1987 5 4) `shouldBe` Just D振替休日
      it "1988-05-04 is National Holiday" $
        holiday (fromGregorian 1988 5 4) `shouldBe` Just D国民の休日

      it "2009-09-21 is Respect for the Aged Day" $
        holiday (fromGregorian 2009 9 21) `shouldBe` Just D敬老の日
      it "2009-09-22 is National Holiday" $
        holiday (fromGregorian 2009 9 22) `shouldBe` Just D国民の休日
      it "2009-09-23 is Autumnal Equinox" $
        holiday (fromGregorian 2009 9 23) `shouldBe` Just D秋分の日

    describe "Golden Week of 2019" $ do
      it "2019-04-28 is not a holiday" $
        holiday (fromGregorian 2019 4 28) `shouldBe` Nothing
      it "2019-04-29 is Showa Day" $
        holiday (fromGregorian 2019 4 29) `shouldBe` Just D昭和の日
      it "2019-04-30 is National Holiday" $
        holiday (fromGregorian 2019 4 30) `shouldBe` Just D国民の休日
      it "2019-05-01 is Enthronement Day" $
        holiday (fromGregorian 2019 5 1) `shouldBe` Just D即位の日
      it "2019-05-02 is National Holiday" $
        holiday (fromGregorian 2019 5 2) `shouldBe` Just D国民の休日
      it "2019-05-03 is Constitution Memorial Day" $
        holiday (fromGregorian 2019 5 3) `shouldBe` Just D憲法記念日
      it "2019-05-04 is Greenery Day" $
        holiday (fromGregorian 2019 5 4) `shouldBe` Just Dみどりの日
      it "2019-05-05 is Children's Day" $
        holiday (fromGregorian 2019 5 5) `shouldBe` Just Dこどもの日
      it "2019-05-06 is Make Up Holiday" $
        holiday (fromGregorian 2019 5 6) `shouldBe` Just D振替休日
      it "2019-05-07 is not a holiday" $
        holiday (fromGregorian 2019 5 7) `shouldBe` Nothing

    describe "Make Up Holiday" $ do
      it "1973-02-11 is National Foundation Day" $
        holiday (fromGregorian 1973 2 11) `shouldBe` Just D建国記念の日
      it "1973-02-12 is not Make Up Holiday" $
        holiday (fromGregorian 1973 2 12) `shouldBe` Nothing
      it "1973-04-29 is Emperor's Birthday" $
        holiday (fromGregorian 1973 4 29) `shouldBe` Just D天皇誕生日
      it "1973-04-30 is Make Up Holiday" $
        holiday (fromGregorian 1973 4 30) `shouldBe` Just D振替休日
      it "2008-05-06 is Make Up Holiday" $
        holiday (fromGregorian 2008 5 6) `shouldBe` Just D振替休日
