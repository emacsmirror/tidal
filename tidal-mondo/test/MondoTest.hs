{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MondoTest where

import Data.List (sort)
import Sound.Tidal.Control qualified as T
import Sound.Tidal.Core ((#), (|+), (|-))
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.ParseBP ()
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Scales qualified as T
import Sound.Tidal.Show ()
import Sound.Tidal.UI qualified as T
import Test.Hspec
import Text.Parsec qualified as P

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
            parseTest "(-42 1 .2 1.2 10 22.3)"
                `shouldBe` MList [mv (-42), mv 1, mv 0.2, mv 1.2, mv 10, mv 22.3]
        it "should parse quotes" do
            parseTest "('it is plain' \"a double\")"
                `shouldBe` MList [mp "it is plain", MString (Positioned "a double" 0 0 0)]
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
        it "should desugar sometime jux" do
            desguar "sometimes (# lpf 1 # jux rev)"
                `shouldBe` "(sometimes (fn (_) (jux rev (lpf 1 _))))"

        it "should desugar div" do
            desguar "s bd # stutter 4 1/16"
                `shouldBe` "(stutter 4 (/ 16 1) (s bd))"

        it "should desugar list" do
            desguar "smash 3 (list 2 3 4)"
                `shouldBe` "(smash 3 (list 2 3 4))"
        pure ()

    describe "mondo tidal" do
        let itEval mondo tidal = it ("should eval " <> mondo) $ comparePD (play mondo) tidal
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
        itEval "n [c2 c3] # s sine" do
            T.n "c2 c3" # T.sound "sine"
        itEval "$ s a $ s b $ s c # lpf 50" do
            T.stack [T.s "a", T.s "b", T.s "c" # T.cutoff 50]
        itEval "s bd:1" do
            T.sound "bd:1"
        itEval "s bd:<1 2>" do
            T.sound "<bd:1 bd:2>"
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
        itEval "pN n c2" $ T.pN "n" "c2"
        itEval "s bd # ghost" $ T.ghost $ T.s "bd"
        itEval "n (rand # segment 12 # range 0 24) # ribbon 23 1" $ T.ribbon 23 1 $ T.n (T.range 0 24 $ T.segment 12 T.rand)
        itEval "n (irand 5) # segment 4" $ T.segment 4 $ T.n (T.irand 5)
        itEval "s superhammond!12 # n (randrun 6)" $ T.s "superhammond!12" # T.n (fromIntegral <$> T.randrun 6)
        itEval "n (run <4 8>) # s amencutup" $ T.n (T.run "<4 8>") # T.sound "amencutup"
        itEval "n (scan 8) # s amencutup" $ T.n (T.scan 8) # T.sound "amencutup"
        itEval "n [0 1 [~ 2] 3] # every 3 (fast 2) # s arpy" $ T.every 3 (T.fast 2) $ T.n "0 1 [~ 2] 3" # T.sound "arpy"
        itEval "s [bd ~ sn cp] # ply [2 3]" $ T.ply "2 3" $ T.s "bd ~ sn cp"
        itEval "s [bd ~ sn cp] # every 3 (ply [2 3])" $ T.every 3 (T.ply "2 3") $ T.s "bd ~ sn cp"

        -- math tests
        itEval "n c2 # fast 3/4" $ T.fast (3 / 4) $ T.n "c2"

        -- scale tests
        itEval "note ([0 1 2] # scale minor)" do
            T.note (T.scale "minor" "0 1 2")
        itEval "note [0 3 2] # scale minor" do
            T.note (T.scale "minor" "0 3 2")
        itEval "note [0 1 2] # lpf 10 # scale minor" do
            T.note (T.scale "minor" "0 1 2") # T.cutoff 10
        itEval "note [0 1 2] # scale minor # lpf 10" do
            T.note (T.scale "minor" "0 1 2") # T.cutoff 10

        -- lambda tests
        itEval "n c'min # every 1 (arp up)" $ T.every 1 (T.arp "up") $ T.n "c'min"

        -- add/sub tests
        itEval "n [1 2] # ladd (n 3) # pan 1" $ T.n "[1 2]" |+ T.n "3" # T.pan 1
        itEval "n 0..12 # lsub (n <0 5>) # lpf 1" $ T.n "0..12" |- T.n "<0 5>" # T.cutoff 1

        -- stack tests
        itEval "s <piano,kawai>" $ T.s "<piano,kawai>"
        itEval "n <[0 2], 0 .. 12, 12>*2" $ T.n "<[0 2], 0 .. 12, 12>*2"

        -- : tests
        let vib = T.pF "vib"
        let vibmod = T.pF "vibmod"
        itEval "s piano # vib [1:<2 3>]" $ T.s "piano" # vib "1" # vibmod "<2 3>"
        itEval "s piano # vib 1:2" $ T.s "piano" # vib 1 # vibmod 2
        itEval "sound bd:<1 2>" $ T.s "<bd:1 bd:2>"
        itEval "s bd # distort 2:.5" $ T.s "bd" # T.distort "2" # T.pF "distortvol" 0.5

        -- timing tests
        itEval "s superhammond!12 # n 12" $ T.s "superhammond!12" # T.n 12
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
        itEval "s (bd # fast 4)" $ T.sound (T.fast 4 "bd")
        itEval "$ s bd $_ s sd # lpf 5" $ T.sound "bd"

        -- full tests
        itEval "s [drum*3 tabla:4 [arpy:2 ~ arpy] [can:2 can:3]] # spin 4 # slow 3" $ T.slow 3 $ T.spin 4 $ T.sound "drum*3 tabla:4 [arpy:2 ~ arpy] [can:2 can:3]"
        itEval "s bev # striate 128 # slow 8" $ T.slow 8 $ T.striate 128 $ T.s "bev"
        itEval "s bev # striateBy 32 1/16 # slow 32" $ T.slow 32 $ T.striateBy 32 (1 / 16) $ T.sound "bev"
        itEval "s [jvbass drum:4] # gap 16" $ T.gap 16 $ T.sound "[jvbass drum:4]"
        itEval "interlace (s [bd sn kurt]) (s [bd sn:2] # every 3 rev)" $ T.interlace (T.sound "bd sn kurt") (T.every 3 T.rev $ T.sound "bd sn:2")
        itEval "s bd*8 # sometimesBy 0.25 (density 2)" $ T.sometimesBy 0.25 (T.density 2) $ T.sound "bd*8"
        itEval "n [0 1 [~ 2] 3] # s arpy # someCyclesBy 0.5 (# crush 2)" $ T.someCyclesBy 0.5 (# T.crush 2) $ T.n "0 1 [~ 2] 3" # T.sound "arpy"
        itEval "s [bd sn] # fit' 1 2 [0 1] [1 0]" $ T.sound (T.fit' 1 2 "0 1" "1 0" "bd sn")
        itEval "s [bd sn:1] # juxBy .5 (fast 2)" $ T.juxBy 0.5 (T.fast 2) $ T.sound "bd sn:1"
        itEval "s bev # randslice 32 # fast 4" $ T.fast 4 $ T.randslice 32 $ T.sound "bev"
        itEval "s [bd cp] # stutter 4 1/16" $ T.stutter (4 :: Int) (1 / 16) $ T.s "bd cp"
        itEval "s [bd hh sn cp] # chunk 4 (# speed 2)" $ T.chunk 4 (# T.speed 2) $ T.sound "bd hh sn cp"
        itEval "s [bd sn:2 [~ bd] sn:2] # chunk 4 (hurry 2)" $ T.chunk 4 (T.hurry 2) $ T.sound "bd sn:2 [~ bd] sn:2"
        itEval "s superchip*8 # pN n (cosine # slow 8 # range -10 10 # quantise 1)" $ T.s "superchip*8" # T.n (T.quantise 1 $ T.range (-10) 10 $ T.slow 8 $ T.cosine)
        itEval "s bd*4 # pan (smooth [0 1 0.5 1] # slow 4)" $ T.sound "bd*4" # T.pan (T.slow 4 $ T.smooth "0 1 0.5 1")

        itEval "s [ho ho:2 ho:3 hc] # smash 3 (list 2 3 4)" $ T.smash 3 [2, 3, 4] $ T.sound "ho ho:2 ho:3 hc"
        itEval "weave 16 (pan sine) (list (s [bd sn cp]) (s hc*4))" $ T.weave 16 (T.pan T.sine) [T.s "bd sn cp", T.s "hc*4"]

        itEval "sound bev # chop 32 # rev # loopAt 8" $ T.loopAt 8 $ T.rev $ T.chop 32 $ T.sound "bev"
        itEval "sound [bd sn] # echo 4 .2 .5" $ T.echo 4 0.2 0.5 $ T.sound "bd sn"
        itEval "n [0 ~ 1 2 0 2 ~ 3*2] # rot <0 1> # s drum" $ T.rot "<0 1>" $ T.n "0 ~ 1 2 0 2 ~ 3*2" # T.sound "drum"
        itEval "s [bd sn [cp ht] hh] # superimpose (fast 2)" $ T.superimpose (T.fast 2) $ T.sound "bd sn [cp ht] hh"
        itEval "note [c2 c3] # s piano" $ T.note "[c2 c3]" # T.sound "piano"
        itEval "n c2 # off 0.125 (ladd (n 7))" $ T.off 0.125 (|+ T.n 7) $ T.n "c2"
        itEval "s hh*8 # pan (sine # slow <3 1>)" $ T.sound "hh*8" # T.pan (T.slow "<3 1>" T.sine)
        itEval "s sine*4 # dec(sine # slow 4 # range 0 2)" $ T.sound "sine*4" # T.decay (T.slow 4 $ T.range 0 2 T.sine)
        itEval "s [bd hh sn cp] # iter 4" $ T.iter 4 $ T.sound "bd hh sn cp"
        itEval "n <0 2 4 [3 1] -1>*4 # jux rev" $ T.jux T.rev $ T.n "<0 2 4 [3 1] -1>*4"
        itEval "s bd*2 # jux (# dec 1 # rev)" $ T.jux (T.rev . (# T.decay 1)) $ T.sound "bd * 2"
        itEval "n 0..7 # sometimes (jux rev)" $ T.sometimes (T.jux T.rev) $ T.n "0..7"
        itEval "n 0..7 # sometimes (# lpf 1 # jux rev)" $ T.sometimes (T.jux T.rev . (# T.cutoff 1)) $ T.n "0..7"
        itEval "n 0..7 # sometimes (# lpf 1 # dec 1)" $ T.sometimes ((# T.decay 1) . (# T.cutoff 1)) $ T.n "0..7"
        itEval "s sitar # lpf (sine/3 # range 120 400)" $ T.sound "sitar" # T.cutoff (T.range 120 400 $ T.slow 3 T.sine)
        itEval "n <a'm9'8 e'7sus4'8> # arp <up down>*2 # lsub (n <12 [12 5]>/2)" $ T.n $ T.arp "<up down>*2" "<a'm9'8 e'7sus4'8>" |- "<12 [12 5]>/2"

        itEval "$ def melody [0 1 2 3] $ n melody" $ T.n "[0 1 2 3]"

    describe "parse error location" do
        let itFail s expected =
                it ("should fail " <> s) $ case mondoToTidal s of
                    Right _ -> fail "mondo succeed"
                    Left err -> let pos = P.errorPos err in (P.sourceLine pos, P.sourceColumn pos) `shouldBe` expected

        itFail "<" (1, 1)
        itFail " <" (1, 2)
        itFail "s" (1, 1)
        itFail " s" (1, 2)
        itFail " \"" (1, 3)
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
    mkp v = Positioned v 0 0 0

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

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => T.Pattern a -> T.Pattern a -> Expectation
comparePD p p' =
    sort (T.defragParts $ T.queryArc (stripContext p) a)
        `shouldBe` sort (T.defragParts $ T.queryArc (stripContext p') a)
  where
    a = T.Arc 0 4
