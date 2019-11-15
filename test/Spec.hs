import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Repl
import Lambda
import Parser

main :: IO ()
main = hspec $ do
  describe "Lambda Terms" $ do
    describe "Parser" $ do
      it "Parses var correctly" $ do
        doParseTerm "x" `shouldBe` Right  (TmVar (OrigName "x") (-1))
      it "Parses abstraction correctly" $ do
        doParseTerm "\\x.x" `shouldBe` Right (TmAbs Info "x" (TmVar (OrigName "x") 0))
      it "Parses application correctly" $ do
        doParseTerm "x x" `shouldBe` Right (TmApp Info (TmVar (OrigName "x") (-1)) (TmVar (OrigName "x") (-1)))
      it "Has left associativity for application" $ do
        doParseTerm "a b c" `shouldBe` doParseTerm "(a b) c"
      it "Stretches as far as possible for abstraction" $ do
        doParseTerm "\\a.\\b.\\c.a b c" `shouldBe` doParseTerm "\\a.(\\b.(\\c.(a b) c))"
      it "Parses compound correctly" $ do
        doParseTerm "\\x.\\y.(x y)" `shouldBe` Right (TmAbs Info "x" (TmAbs Info "y" (TmApp Info (TmVar (OrigName "x") 1) (TmVar (OrigName "y") 0))))
        doParseTerm "\\x.\\y.\\z.(x y z)" `shouldBe` Right (TmAbs Info "x" (TmAbs Info "y" (TmAbs Info "z" (TmApp Info (TmApp Info (TmVar (OrigName "x") 2) (TmVar (OrigName "y") 1)) (TmVar (OrigName "z") 0)))))
    describe "Shifting" $ do
      it "Shifts" $ do
        shift 1 (TmVar Info 0) `shouldBe` (TmVar Info 1)
    describe "Substitution" $ do
      it "Substitutes" $ do
        subs 0 (TmVar Info 5) (TmVar Info 0) `shouldBe` (TmVar Info 5)
      it "Substitutes in abstraction" $ do
        subs 0 (TmVar Info 5) (TmAbs Info "t" (TmVar Info 1)) `shouldBe` (TmAbs Info "t" (TmVar Info 6))
    describe "Evaluation" $ do
      it "Reduces variable correctly" $ do
        strEval "x"  `shouldBe` strEval "x"
      it "Reduces abstraction correctly" $ do
        eval (TmAbs Info "x" (TmVar Info 0))  `shouldBe` (TmAbs Info "x" (TmVar Info 0))
      it "Reduces application correctly" $ do
        strEval "(\\x.x) (\\y.y)"  `shouldBe` strEval "\\y.y"
    describe "Evaluation Substitution" $ do
      -- Not supported for this language, it will not reduce further than when substitution is defined
      it "substituting obeys shadowing" $ do
        strEval "(\\x.\\x.x) \\y.y" `shouldBe` strEval "\\x.x"
      it "substitutes nested correctly" $ do
        strEval "(\\x.(\\y.(\\z.x))) c" `shouldBe` strEval "\\y.\\z.c"
      it "substitutes does not capture variable" $ do
        strEval "(\\x.(\\y.(\\z.x)) t) y" `shouldBe` strEval "\\z.y"
    describe "Evaluation of small programs" $ do
      it "Value it returned directly" $ do
        eval (TmAbs Info "x" (TmVar Info 0))  `shouldBe` (TmAbs Info "x" (TmVar Info 0))
