{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MondoTest where

import Data.List (sort)
import Sound.Tidal.Control qualified as T
import Sound.Tidal.Core ((#), (|+), (|+|), (|-))
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.ParseBP ()
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Scales qualified as T
import Sound.Tidal.Show ()
import Sound.Tidal.UI qualified as T
import Test.Hspec

import Mondo

run :: Spec
run = describe "tidal-mondo" do
    -- Test cases from https://codeberg.org/uzu/strudel/src/branch/main/packages/mondo/test/mondo.test.mjs
    -- Copyright (C) 2022 Strudel contributors
    describe "s-expressions parser" do
        it "should parse an empty string" do
            parseTest ""
                `shouldBe` MList []
        it "should parse a single item" do
            parseTest "a"
                `shouldBe` MList [mp "a"]
        it "should parse an empty list" do
            parseTest "()"
                `shouldBe` MList []
        it "should parse a list with 1 item" do
            parseTest "(a)"
                `shouldBe` MList [mp "a"]
        it "should parse a list with 2 items" do
            parseTest "(a b)"
                `shouldBe` MList [mp "a", mp "b"]
        it "should parse a list with 2 items" do
            parseTest "(a (b c))"
                `shouldBe` MList [mp "a", MList [mp "b", mp "c"]]
        it "should parse numbers" do
            parseTest "(1 .2 1.2 10 22.3)"
                `shouldBe` MList [mv 1, mv 0.2, mv 1.2, mv 10, mv 22.3]
        it "should parse quotes" do
            parseTest "('it is plain' \"a double\")"
                `shouldBe` MList [mp "it is plain", MString (Positioned "a double" 0 0)]
        it "should parse comments" do
            parseTest "a // hello"
                `shouldBe` MList [mp "a"]

    describe "mondo sugar" do
        let desguar = showAst . parseTest
        it "should desugar []" do
            desguar "[a b c]"
                `shouldBe` "(square a b c)"
        it "should desugar []" do
            desguar "[a b c]"
                `shouldBe` "(square a b c)"
        it "should desugar [] nested" do
            desguar "[a [b c] d]"
                `shouldBe` "(square a (square b c) d)"
        it "should desugar <>" do
            desguar "<a b c>"
                `shouldBe` "(angle a b c)"
        it "should desugar <> nested" do
            desguar "<a <b c> d>"
                `shouldBe` "(angle a (angle b c) d)"
        it "should desugar mixed [] <>" do
            desguar "[a <b c>]"
                `shouldBe` "(square a (angle b c))"
        it "should desugar mixed <> []" do
            desguar "<a [b c]>"
                `shouldBe` "(angle a (square b c))"
        it "should desugar #" do
            desguar "s jazz # fast 2"
                `shouldBe` "(fast 2 (s jazz))"
        it "should desugar # square" do
            desguar "[bd cp # fast 2]"
                `shouldBe` "(fast 2 (square bd cp))"
        it "should desugar # twice" do
            desguar "s jazz # fast 2 # slow 2"
                `shouldBe` "(slow 2 (fast 2 (s jazz)))"
        it "should desugar # nested" do
            desguar "(s cp # fast 2)"
                `shouldBe` "(fast 2 (s cp))"
        it "should desugar # within []" do
            desguar "[bd cp # fast 2]"
                `shouldBe` "(fast 2 (square bd cp))"
        it "should desugar # within , within []" do
            desguar "[bd cp # fast 2, x]"
                `shouldBe` "(stack (fast 2 (square bd cp)) x)"
        it "should desugar , |" do
            desguar "[bd, hh | oh]"
                `shouldBe` "(stack bd (or hh oh))"
        it "should desugar , | of []" do
            desguar "[bd, hh | [oh rim]]"
                `shouldBe` "(stack bd (or hh (square oh rim)))"
        it "should desugar , square" do
            desguar "[bd, hh]"
                `shouldBe` "(stack bd hh)"
        it "should desugar , square 2" do
            desguar "[bd, hh oh]"
                `shouldBe` "(stack bd (square hh oh))"
        it "should desugar , square 3" do
            desguar "[bd cp, hh oh]"
                `shouldBe` "(stack (square bd cp) (square hh oh))"
        it "should desugar , angle" do
            desguar "<bd, hh, oh>"
                `shouldBe` "(stack bd hh oh)"
        it "should desugar , angle 2" do
            desguar "<bd, hh oh>"
                `shouldBe` "(stack bd (angle hh oh))"
        it "should desugar , angle 3" do
            desguar "<bd cp, hh oh>"
                `shouldBe` "(stack (angle bd cp) (angle hh oh))"
        it "should desugar , ()" do
            desguar "(s bd, s cp)"
                `shouldBe` "(stack (s bd) (s cp))"
        it "should desugar , () 2" do
            desguar "(bd, cp, ~ hh)"
                `shouldBe` "(stack bd cp (~ hh))"
        it "should desugar , () 3" do
            desguar "(bd, cp, ~ hh, oh)"
                `shouldBe` "(stack bd cp (~ hh) oh)"
        it "should desugar , () 4" do
            desguar "(1,   2,    3,  4, 5)"
                `shouldBe` "(stack 1 2 3 4 5)"
        it "should desugar * /" do
            desguar "[a b*2 c d/3 e]"
                `shouldBe` "(square a (* 2 b) c (/ 3 d) e)"
        it "should desugar []*x" do
            desguar "[a [b c]*3]"
                `shouldBe` "(square a (* 3 (square b c)))"
        it "should desugar []*<x y>" do
            desguar "[a b*<2 3> c]"
                `shouldBe` "(square a (* (angle 2 3) b) c)"
        it "should desugar bd*2" do
            desguar "bd*2"
                `shouldBe` "(* 2 bd)"
        it "should desugar x:y" do
            desguar "x:y"
                `shouldBe` "(: y x)"
        it "should desugar stack" do
            desguar "$ s bd\n$ s hh"
                `shouldBe` "(stack (s bd) (s hh))"
        it "should desugar def" do
            desguar "$ def melody [0 1 2 3]\n$ n melody # scale C:minor"
                `shouldBe` "(stack (def melody (square 0 1 2 3)) (scale (: minor C) (n melody)))"
        it "should desugar s fx" do
            desguar "s [bd (sd # lpf 50)]"
                `shouldBe` "(s (square bd (lpf 50 sd)))"
        it "should desugar README example" do
            desguar "s [bd hh*2 (cp # crush 4) <mt ht lt>] # speed .8"
                `shouldBe` "(speed .8 (s (square bd (* 2 hh) (crush 4 cp) (angle mt ht lt))))"
        it "should desugar call list" do
            desguar "s [bd sd] # lpf 50 42 # gain 1 2 3"
                `shouldBe` "(gain 1 2 3 (lpf 50 42 (s (square bd sd))))"
        {-
        it "should desugar x:y:z" do
            desguar "x:y:z"
                `shouldBe` "(: z (: y x))"
        -}
        it "should desugar x:y*x" do
            desguar "bd:0*2"
                `shouldBe` "(* 2 (: 0 bd))"
        it "should desugar a..b" do
            desguar "0..2"
                `shouldBe` "(.. 2 0)"
        it "should desugar (#)" do
            desguar "(#)"
                `shouldBe` "(fn (_) _)"
        it "should desugar lambda" do
            desguar "(# fast 2)"
                `shouldBe` "(fn (_) (fast 2 _))"
        it "should desugar lambda call" do
            desguar "((# mul 2) 2)"
                `shouldBe` "((fn (_) (mul 2 _)) 2)"
        it "should desugar lambda with pipe" do
            desguar "(# fast 2 # room 1)"
                `shouldBe` "(fn (_) (room 1 (fast 2 _)))"
        {-
        it "should desugar .(." do
            desguar "[jazz hh.(.fast 2)]"
                `shouldBe` "(square jazz (fast 2 hh))"
        -}
        it "should desugar scale" do
            desguar "n ([0 1 2] # scale minor)"
                `shouldBe` "(n (scale minor (square 0 1 2)))"
        it "should desugar chained function" do
            desguar "s [bd hh bd (cp # delay .6)] # bank tr909"
                `shouldBe` "(bank tr909 (s (square bd hh bd (delay .6 cp))))"

        it "should desugar sometimes" do
            desguar "s bd # sometimes (# lpf 42)"
                `shouldBe` "(sometimes (fn (_) (lpf 42 _)) (s bd))"
        it "should desugar brackets" do
            desguar "s [bd [sd hh]]"
                `shouldBe` "(s (square bd (square sd hh)))"

        it "should desugar !" do
            desguar "s bd!2"
                `shouldBe` "(s (! 2 bd))"
        it "should desugar @<>" do
            desguar "s [bd@<2 3> sd]"
                `shouldBe` "(s (square (@ (angle 2 3) bd) sd))"
        it "should desugar x%y:z" do
            desguar "bd&3:8"
                `shouldBe` "(& bd (: 8 3))"
        it "should desugar x?" do
            desguar "[bd? sd]"
                `shouldBe` "(square (? bd) sd)"
        it "should desugar x?1 y" do
            desguar "[bd?1 sd]"
                `shouldBe` "(square (? bd 1) sd)"
        it "should desugar slow sine" do
            desguar "s hh*8 # pan (sine # slow 3)"
                `shouldBe` "(pan (slow 3 sine) (s (* 8 hh)))"
        it "should desugar add" do
            desguar "n 1 # add (n 2)"
                `shouldBe` "(add (n 2) (n 1))"

        pure ()

    describe "mondo tidal" do
        let itEval mondo tidal = it ("should eval " <> mondo) $ compareP (play mondo) tidal
        itEval "(s [bd sd])" do
            T.sound "bd sd"
        itEval "(s bd*2)" do
            T.sound "bd*2"
        itEval "(s <bd sd>)" do
            T.sound "<bd sd>"
        itEval "(s [bd sd])" do
            T.sound "[bd sd]"
        itEval "(s [bd [sd hh]])" do
            T.sound "bd [sd hh]"
        itEval "(s [bd <sd [hh oh]>])" do
            T.sound "bd <sd [hh oh]>"
        itEval "(s bd # fast 2)" do
            T.fast 2 $ T.sound "bd"
        itEval "(s bd # slow 2)" do
            T.slow 2 $ T.sound "bd"
        itEval "s bd # fast 2 # lpf 50" do
            T.fast 2 $ T.sound "bd" # T.cutoff 50
        itEval "s bd # lpf 50" do
            T.sound "bd" # T.cutoff 50
        itEval "s bd*2 # lpf [50 100]" do
            T.sound "bd*2" # T.cutoff "50 100"
        itEval "s [bd (sd # lpf 50)]" do
            T.fastCat [T.sound "bd", T.sound "sd" # T.cutoff 50]
        itEval
            ( unlines
                [ "s <[bd sd] // love this"
                , "   [cp] // cool bit"
                , ">"
                ]
            )
            $ T.sound "<[bd sd] [cp]>"

        itEval "n [c2 c3]" do
            T.n "c2 c3"
        itEval "s sine # n [c2 c3]" do
            T.sound "sine" |+| T.n "c2 c3"
        itEval "$ s a $ s b $ s c # lpf 50" do
            T.stack [T.s "a", T.s "b", T.s "c" # T.cutoff 50]
        itEval "s bd:1" do
            T.sound "bd:1"
        itEval "s bd:<1 2>" do
            T.sound "<bd:1 bd:2>"
        itEval "n ([0 1 2] # scale minor)" do
            T.scale "minor" "0 1 2"
        itEval "n [0 1 2] # lpf 10 # scale minor" do
            T.scale "minor" "0 1 2" # T.cutoff 10
        itEval "s bd # sometimes (# lpf 42)" do
            T.sometimes (# T.cutoff 42) $ T.sound "bd"
        itEval "s [bd:<1 2> (sd # lpf 23)] # sometimes (# lpf 42)" do
            T.sometimes (# T.cutoff 42) $ (T.fastcat [T.sound "<bd:1 bd:2>", T.sound "sd" # T.cutoff 23])
        itEval "s [<bd (hh # hpf 23)>:[1 (2 # lpf 42)]]" do
            T.fastcat
                [ T.slow 2 $ T.timeCat [(1, T.sound "bd:1"), (1, T.sound "hh:1" # T.hcutoff 23)]
                , T.slow 2 $ T.timeCat [(1, T.sound "bd:2"), (1, T.sound "hh:2" # T.hcutoff 23)] # T.cutoff 42
                ]
        itEval "s breaks165 # splice 8 <0*8 0*2>" do
            T.splice 8 "<0*8 0*2>" $ T.sound "breaks165"
        itEval "s arpy*8 # pan sine" do
            T.sound "arpy*8" # T.pan T.sine
        itEval "s arpy*8 # mask [1 0 1]" do
            T.mask "[1 0 1]" $ T.sound "arpy*8"
        itEval "s [bd*2 sn] # euclid 3 8" do
            T.euclid 3 8 $ T.sound "bd*2 sn"

        -- add/sub tests
        itEval "n [1 2] # add (n 3) # pan 1" $ T.n "[1 2]" |+ T.n "3" # T.pan 1
        itEval "n 0..12 # sub (n <0 5>) # lpf 1" $ T.n "0..12" |- T.n "<0 5>" # T.cutoff 1

        -- timing tests
        itEval "s [bd ~]" $ T.sound "bd ~"
        itEval "s [bd!2 sd]" $ T.sound "[bd!2 sd]"
        itEval "s [bd@2 sd]" $ T.sound "[bd@2 sd]"
        itEval "s [bd@2 ~ sd]" $ T.sound "[bd _ ~ sd]"
        itEval "s bd&3:8" $ T.sound "bd(3,8)"
        itEval "s bd&3:8/2" $ T.sound "bd(3,8)/2"
        itEval "s [bd*2 sd/2]" $ T.sound "[bd*2 sd/2]"
        itEval "n 0..5" $ T.n "0..5"
        itEval "s bd&<3:8 11:16>" $ T.sound "<bd(3,8) bd(11,16)>"
        itEval "s bd?" $ T.sound "bd?"
        itEval "s [bd? sd?2]" $ T.fastcat [T.sound "bd?", T.degradeBy 2 (T.sound "sd")]

        -- full tests
        itEval "s hh*8 # pan (sine # slow <3 1>)" $ T.sound "hh*8" # T.pan (T.slow "<3 1>" T.sine)
        itEval "s sine*4 # dec(sine # slow 4 # range 0 2)" $ T.sound "sine*4" # T.decay (T.slow 4 $ T.range 0 2 T.sine)
        itEval "s [bd hh sn cp] # iter 4" $ T.iter 4 $ T.sound "bd hh sn cp"
        itEval "n <0 2 4 [3 1] 1>*4 # jux rev" $ T.jux T.rev $ T.n "<0 2 4 [3 1] 1>*4"
        pure ()
  where
    play :: String -> T.ControlPattern
    play = either (error . show) id . mondoToTidal

    parseTest :: String -> MondoExpr
    parseTest = clearLoc . either (error . show) id . mondoToExpr

    clearLoc v = case v of
        MList xs -> MList $ map clearLoc xs
        MPlain x -> mp x.value
        MValue x -> mv x.value
        MString x -> ms x.value
        x -> x
    mv = MValue . mkp
    mp = MPlain . mkp
    ms = MString . mkp
    mkp v = Positioned v 0 0

-- from tidal TestUtils
stripContext :: T.Pattern a -> T.Pattern a
stripContext = T.setContext $ T.Context []

-- | Compare the events of two patterns using the given arc
compareP :: (Ord a, Show a) => T.Pattern a -> T.Pattern a -> Expectation
compareP p q =
    sort (T.queryArc (stripContext p) a)
        `shouldBe` sort (T.queryArc (stripContext q) a)
  where
    a = T.Arc 0 4
