# Japanese Holidays

[![Hackage](https://img.shields.io/hackage/v/japanese-holidays.svg?style=flat-square)](https://hackage.haskell.org/package/japanese-holidays)
[![CircleCI](https://img.shields.io/circleci/project/github/cohei/japanese-holidays.svg?style=flat-square)](https://circleci.com/gh/cohei/japanese-holidays)

ある日付が、どの日本の祝日にあたるかを判定します。 [AddinBox / 祝日判定ロジック](http://addinbox.sakura.ne.jp/holiday_logic.htm) を Haskell へ移植しました。

This library, which is translated from [AddinBox / Holiday Decision Logic](http://addinbox.sakura.ne.jp/holiday_logic.htm) to Haskell, identifies Japanese holidays.

## 著作権表示 Copyright


```
_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
_/
_/  CopyRight(C) K.Tsunoda(AddinBox) 2001 All Rights Reserved.
_/  ( AddinBox  http://addinbox.sakura.ne.jp/index.htm )
_/  (  旧サイト  http://www.h3.dion.ne.jp/~sakatsu/index.htm )
_/
_/    この祝日マクロは『kt関数アドイン』で使用しているものです。
_/    このロジックは、レスポンスを第一義として、可能な限り少ない
_/    【条件判定の実行】で結果を出せるように設計してあります。
_/
_/    この関数では以下の祝日変更までサポートしています。
_/    ・２０１９年施行の「天皇誕生日の変更」 12/23⇒2/23 (補：2019年には[天皇誕生日]はありません)
_/    ・２０２０年施行の「体育の日の改名」⇒スポーツの日
_/    ・五輪特措法による２０２０年の「祝日移動」
_/       海の日：7/20(3rd Mon)⇒7/23, スポーツの日:10/12(2nd Mon)⇒7/24, 山の日：8/11⇒8/10
_/
_/    下記２つについては未だ法整備自体が行なわれていませんので未対応です。
_/    ・２０１９年の退位日(4/30)/即位日(5/1)
_/    ・２０１９年の「即位の礼　正殿の儀 (10/22) 」
_/
_/  (*1)このマクロを引用するに当たっては、必ずこのコメントも
_/      一緒に引用する事とします。
_/  (*2)他サイト上で本マクロを直接引用する事は、ご遠慮願います。
_/      【 http://addinbox.sakura.ne.jp/holiday_logic.htm 】
_/      へのリンクによる紹介で対応して下さい。
_/  (*3)[ktHolidayName]という関数名そのものは、各自の環境に
_/      おける命名規則に沿って変更しても構いません。
_/
_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
```
