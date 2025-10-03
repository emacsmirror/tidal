{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MondoTest where

import Data.List (sort)
import Sound.Tidal.Core ((#))
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.ParseBP ()
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Show ()
import Test.Hspec

import Mondo

-- Test cases from https://codeberg.org/uzu/strudel/src/branch/main/packages/mondo/test/mondo.test.mjs
-- Copyright (C) 2022 Strudel contributors

run :: Spec
run = describe "tidal-mondo" do
    describe "s-expressions parser" do
        it "should parse an empty string" $ parseTest "" `shouldBe` MList []
        it "should parse a single item" $ parseTest "a" `shouldBe` MList [mp "a"]
        it "should parse an empty list" $ parseTest "()" `shouldBe` MList []
        it "should parse a list with 1 item" $ parseTest "(a)" `shouldBe` MList [mp "a"]
        it "should parse a list with 2 items" $ parseTest "(a b)" `shouldBe` MList [mp "a", mp "b"]
        it "should parse a list with 2 items" $ parseTest "(a (b c))" `shouldBe` MList [mp "a", MList [mp "b", mp "c"]]
        it "should parse numbers" $ parseTest "(1 .2 1.2 10 22.3)" `shouldBe` MList [mv 1, mv 0.2, mv 1.2, mv 10, mv 22.3]
        it "should parse comments" $ parseTest "a // hello" `shouldBe` MList [mp "a"]

    describe "mondo sugar" do
        it "should desugar []" $ desguar "[a b c]" `shouldBe` "(square a b c)"
        it "should desugar []" $ desguar "[a b c]" `shouldBe` "(square a b c)"
        it "should desugar [] nested" $ desguar "[a [b c] d]" `shouldBe` "(square a (square b c) d)"
        it "should desugar <>" $ desguar "<a b c>" `shouldBe` "(angle a b c)"
        it "should desugar <> nested" $ desguar "<a <b c> d>" `shouldBe` "(angle a (angle b c) d)"
        it "should desugar mixed [] <>" $ desguar "[a <b c>]" `shouldBe` "(square a (angle b c))"
        it "should desugar mixed <> []" $ desguar "<a [b c]>" `shouldBe` "(angle a (square b c))"
        it "should desugar #" $ desguar "s jazz # fast 2" `shouldBe` "(fast 2 (s jazz))"
        it "should desugar # square" $ desguar "[bd cp # fast 2]" `shouldBe` "(fast 2 (square bd cp))"
        it "should desugar # twice" $ desguar "s jazz # fast 2 # slow 2" `shouldBe` "(slow 2 (fast 2 (s jazz)))"
        it "should desugar # nested" $ desguar "(s cp # fast 2)" `shouldBe` "(fast 2 (s cp))"
        it "should desugar # within []" $ desguar "[bd cp # fast 2]" `shouldBe` "(fast 2 (square bd cp))"
        it "should desugar # within , within []" $ desguar "[bd cp # fast 2, x]" `shouldBe` "(stack (fast 2 (square bd cp)) x)"
        it "should desugar , |" $ desguar "[bd, hh | oh]" `shouldBe` "(stack bd (or hh oh))"
        it "should desugar , | of []" $ desguar "[bd, hh | [oh rim]]" `shouldBe` "(stack bd (or hh (square oh rim)))"
        it "should desugar , square" $ desguar "[bd, hh]" `shouldBe` "(stack bd hh)"
        it "should desugar , square 2" $ desguar "[bd, hh oh]" `shouldBe` "(stack bd (square hh oh))"
        it "should desugar , square 3" $ desguar "[bd cp, hh oh]" `shouldBe` "(stack (square bd cp) (square hh oh))"
        it "should desugar , angle" $ desguar "<bd, hh>" `shouldBe` "(stack bd hh)"
        it "should desugar , angle 2" $ desguar "<bd, hh oh>" `shouldBe` "(stack bd (angle hh oh))"
        it "should desugar , angle 3" $ desguar "<bd cp, hh oh>" `shouldBe` "(stack (angle bd cp) (angle hh oh))"
        it "should desugar , ()" $ desguar "(s bd, s cp)" `shouldBe` "(stack (s bd) (s cp))"
        it "should desugar , () 2" $ desguar "(bd, cp, ~ hh)" `shouldBe` "(stack bd cp (~ hh))"
        it "should desugar , () 3" $ desguar "(bd, cp, ~ hh, oh)" `shouldBe` "(stack bd cp (~ hh) oh)"
        it "should desugar , () 4" $ desguar "(1,   2,    3,  4, 5)" `shouldBe` "(stack 1 2 3 4 5)"
        it "should desugar * /" $ desguar "[a b*2 c d/3 e]" `shouldBe` "(square a (* 2 b) c (/ 3 d) e)"
        it "should desugar []*x" $ desguar "[a [b c]*3]" `shouldBe` "(square a (* 3 (square b c)))"
        it "should desugar []*<x y>" $ desguar "[a b*<2 3> c]" `shouldBe` "(square a (* (angle 2 3) b) c)"
        it "should desugar bd*2" $ desguar "bd*2" `shouldBe` "(* 2 bd)"
        it "should desugar x:y" $ desguar "x:y" `shouldBe` "(: y x)"
        it "should desugar stack" $ desguar "$ s bd\n$ s hh" `shouldBe` "(stack (s bd) (s hh))"
        it "should desugar def" $ desguar "$ def melody [0 1 2 3]\n$ n melody # scale C:minor" `shouldBe` "(stack (def melody (square 0 1 2 3)) (scale (: minor C) (n melody)))"
        it "should desugar s fx" $ desguar "s bd (sd # lpf 50)" `shouldBe` "(s bd (lpf 50 sd))"
        it "should desugar README example" $ desguar "s [bd hh*2 (cp # crush 4) <mt ht lt>] # speed .8" `shouldBe` "(speed .8 (s (square bd (* 2 hh) (crush 4 cp) (angle mt ht lt))))"
        {-
        it "should desugar x:y:z" $ desguar "x:y:z" `shouldBe` "(: z (: y x))"
        it "should desugar x:y*x" $ desguar "bd:0*2" `shouldBe` "(* 2 (: 0 bd))"
        it "should desugar a..b" $ desguar "0..2" `shouldBe` "(.. 2 0)"
        -- it "should desugar x $ y" $ desguar "x $ y" `shouldBe` "(x y)"
        -- it "should desugar x $ y z" $ desguar "x $ y z" `shouldBe` "(x (y z))"
        -- it "should desugar x $ y . z" $ desguar "x $ y . z" `shouldBe` "(z (x y))"

        it "should desugar (#)" $ desguar "(#)" `shouldBe` "(fn (_) _)"
        it "should desugar lambda" $ desguar "(# fast 2)" `shouldBe` "(fn (_) (fast 2 _))"
        it "should desugar lambda call" $ desguar "((# mul 2) 2)" `shouldBe` "((fn (_) (mul 2 _)) 2)"
        it "should desugar lambda with pipe" $ desguar "(# fast 2 # room 1)" `shouldBe` "(fn (_) (room 1 (fast 2 _)))"
         -- it "should desugar .(." $ desguar "[jazz hh.(.fast 2)]" `shouldBe` "(square jazz (fast 2 hh))"
        -}
        pure ()

    describe "mondo tidal" do
        itEval "(s bd sd)" $ T.sound "bd sd"
        itEval "(s bd*2)" $ T.sound "bd*2"
        itEval "(s <bd sd>)" $ T.sound "<bd sd>"
        itEval "(s [bd sd])" $ T.sound "[bd sd]"
        itEval "(s bd <sd [hh oh]>)" $ T.sound "bd <sd [hh oh]>"
        itEval "(s bd # fast 2)" $ T.fast 2 $ T.sound "bd"
        itEval "(s bd # slow 2)" $ T.slow 2 $ T.sound "bd"
        itEval "s bd # lpf 50" $ T.sound "bd" # T.cutoff 50
        itEval "s bd (sd # lpf 50)" $ T.fastCat [T.sound "bd", T.sound "sd" # T.cutoff 50]
        itEval
            ( unlines
                [ "s <[bd sd] // love this"
                , "  [cp] // cool bit"
                , ">"
                ]
            )
            $ T.sound "<[bd sd] [cp]>"
  where
    parseTest = clearLoc . either (error . show) id . mondoToExpr
    clearLoc v = case v of
        MList xs -> MList $ map clearLoc xs
        MPlain x -> mp x.value
        MValue x -> mv x.value
        x -> x
    mv = MValue . mkp
    mp = MPlain . mkp
    mkp v = Positioned v 0 0

    desguar = showAst . parseTest

    play = either (error . show) id . mondoToTidal
    itEval mondo tidal = it ("should eval " <> mondo) $ compareP (play mondo) tidal

-- from tidal TestUtils
stripContext :: T.Pattern a -> T.Pattern a
stripContext = T.setContext $ T.Context []

-- | Compare the events of two patterns using the given arc
compareP :: (Ord a, Show a) => T.Pattern a -> T.Pattern a -> Expectation
compareP p q =
    sort (T.queryArc (stripContext p) a) `shouldBe` sort (T.queryArc (stripContext q) a)
  where
    a = T.Arc 0 4
