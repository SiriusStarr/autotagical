{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module PredicateSpec
  ( spec,
  )
where

import Arbitrary ()
import Data.List (genericLength)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import GHC.Exts (IsList (..))
import Predicate (Bound (..), Predicate (..), checkPredicate)
import SafeType (NonEmptyList, makeNonEmptySet, toSet)
import Tag (TagGroup (..), TagValue (..), Tags (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (NonEmptyList)
import Utility (importFails, importSucceeds)

spec :: Spec
spec = do
  context "FromDhall instances" $ do
    describe "Bound" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Bound
      it "imports when AllPredicates" $
        importSucceeds d AllPredicates "bound-allPredicates.dhall"
      it "imports when a numeric bound" $
        importSucceeds d (Bound 7) "bound-numeric.dhall"
    describe "Predicate" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Predicate
      it "imports all" $
        importSucceeds
          d
          (Between AllPredicates AllPredicates [Always, Not Always])
          "predicate-all.dhall"
      it "imports always" $
        importSucceeds d Always "predicate-always.dhall"
      it "imports any" $
        importSucceeds d (Between (Bound 1) AllPredicates [Always, Not Always]) "predicate-any.dhall"
      it "imports atLeast" $
        importSucceeds d (Between (Bound 2) AllPredicates [Always, Not Always]) "predicate-atLeast.dhall"
      it "imports atMost" $
        importSucceeds d (Between (Bound 0) (Bound 1) [Always, Not Always]) "predicate-atMost.dhall"
      it "imports between" $
        importSucceeds d (Between (Bound 0) (Bound 2) [Always, Not Always]) "predicate-between.dhall"
      it "imports exactly" $
        importSucceeds d (Between (Bound 1) (Bound 1) [Always, Not Always]) "predicate-exactly.dhall"
      it "imports hasGroup" $
        importSucceeds
          d
          (HasGroup . TagGroup . makeNonEmptySet $ ["tag1", "tag2"])
          "predicate-hasGroup.dhall"
      it "imports hasTag" $
        importSucceeds d (HasTag "test" Nothing) "predicate-hasTag.dhall"
      it "imports hasTagWithValue" $
        importSucceeds
          d
          (HasTag "test" (Just . makeNonEmptySet $ ["val1", "val2"]))
          "predicate-hasTagWithValue.dhall"
      it "imports not" $
        importSucceeds d (Not Always) "predicate-not.dhall"
      it "fails when hasTagWithValue without values" $
        importFails d "predicate-hasTagWithValue-empty.dhall"
      it "fails when hasTagWithValue has an invalid value" $
        importFails d "predicate-hasTagWithValue-slash.dhall"
      it "fails with empty lists of predicates for between" $
        importFails d "predicate-between-empty.dhall"
      it "fails when lower bound is greater than upper bound" $
        importFails d "predicate-between-greaterLowerBound.dhall"
      it "fails when lower bound is AllPredicates and upper bound is numeric" $
        importFails d "predicate-between-lowerBoundAllPredicates.dhall"
      it "fails when a deeply nested predicate is invalid" $
        importFails d "predicate-nestedInvalid.dhall"
  parallel $ do
    describe "checkPredicate" $ do
      context "when predicate is always" $ do
        prop "is always True" $
          \ts -> checkPredicate ts Always `shouldBe` True
        it "is True even for no tags" $
          checkPredicate (Tags M.empty) Always `shouldBe` True
      context "when predicate is hasTag without values" $ do
        prop "is always True if the predicate tag is contained in the tags" $
          \t ts ->
            checkPredicate (Tags $ M.insert t Present ts) (HasTag t Nothing)
              `shouldBe` True
        prop "is always True if the predicate tag is contained in the tags with a value" $
          \t v ts ->
            checkPredicate (Tags $ M.insert t (Value v) ts) (HasTag t Nothing)
              `shouldBe` True
        prop "is always False for no tags" $
          \t -> checkPredicate (Tags M.empty) (HasTag t Nothing) `shouldBe` False
        prop "is never True if the predicate tag is not contained in the tags" $
          \t ts ->
            checkPredicate (Tags $ M.delete t ts) (HasTag t Nothing)
              `shouldBe` False
      context "when predicate is hasTagWithValue" $ do
        prop "is always True if the predicate tag is contained in the tags with the correct value" $
          \t v vs ts ->
            checkPredicate
              (Tags $ M.insert t (Value v) ts)
              (HasTag t . Just . makeNonEmptySet . fromList $ v : vs)
              `shouldBe` True
        prop "is never True if the predicate tag is contained in the tags but only with Present" $
          \t vs ts ->
            checkPredicate (Tags $ M.insert t Present ts) (HasTag t (Just vs))
              `shouldBe` False
        prop "is never True if the predicate tag is contained in the tags with a value other than the specified ones" $
          \t v vs ts ->
            not (v `S.member` toSet vs)
              ==> checkPredicate
                (Tags $ M.insert t (Value v) ts)
                (HasTag t (Just vs))
                `shouldBe` False
        prop "is always False for no tags" $
          \t vs ->
            checkPredicate (Tags M.empty) (HasTag t (Just vs))
              `shouldBe` False
        prop "is never True if the predicate tag is not contained in the tags" $
          \t vs ts ->
            checkPredicate (Tags $ M.delete t ts) (HasTag t (Just vs))
              `shouldBe` False
      context "when predicate is hasGroup" $ do
        prop "is always False if there are no tags" $
          \g -> checkPredicate (Tags M.empty) (HasGroup g) `shouldBe` False
        prop "is True if at least one tag is in the group and False otherwise" $
          \ts g ->
            checkPredicate ts (HasGroup g)
              `shouldBe` any (\t -> S.member t (toSet $ groupTags g)) (M.keys $ tagMap ts)
      context "when predicate is not" $ do
        prop "is always the opposite of the negated predicate" $
          \ts p -> checkPredicate ts p `shouldNotBe` checkPredicate ts (Not p)
        prop "is its own inverse" $
          \ts p -> checkPredicate ts p `shouldBe` checkPredicate ts (Not (Not p))
      context "when predicate is all" $ do
        prop "is always False if any predicate is Not Always" $
          -- Splice Not Always in between two lists of predicates, as this ensures it can comes at any position
          \ts ps1 ps2 ->
            checkPredicate
              ts
              ( Between AllPredicates AllPredicates $
                  fromList $
                    ps1 ++ Not Always : ps2
              )
              `shouldBe` False
        prop "is always False if any predicate evaluates to False" $
          \ts ps1 falseP ps2 ->
            not (checkPredicate ts falseP)
              ==> checkPredicate
                ts
                ( Between AllPredicates AllPredicates $
                    fromList $
                      ps1 ++ falseP : ps2
                )
              `shouldBe` False
        prop "is always True if all predicates evaluate to True" $
          \ts ps ->
            checkPredicate ts (Between AllPredicates AllPredicates ps)
              `shouldBe` all (checkPredicate ts) ps
      context "when predicate is any" $ do
        prop "is always True if any predicate is Always" $
          -- Splice Always in between two lists of predicates, as this ensures it can comes at any position
          \ts ps1 ps2 ->
            checkPredicate
              ts
              ( Between (Bound 1) AllPredicates $
                  fromList $
                    ps1 ++ Always : ps2
              )
              `shouldBe` True
        prop "is always True if any predicate evaluates to True" $
          \ts ps1 trueP ps2 ->
            checkPredicate ts trueP
              ==> checkPredicate
                ts
                ( Between (Bound 1) AllPredicates $
                    fromList $
                      ps1 ++ trueP : ps2
                )
        prop "is always False if all predicates evaluate to False and True otherwise" $
          \ts ps ->
            checkPredicate ts (Between (Bound 1) AllPredicates ps)
              `shouldBe` any (checkPredicate ts) ps
      context "when Predicate is atLeast" $
        prop "is always True if number of matches is greater than lower bound and false otherwise" $
          \ts ps n ->
            checkPredicate ts (Between (Bound n) AllPredicates ps)
              `shouldBe` countTrue ts ps >= n
      context "when Predicate is atMost" $
        prop "is always True if number of matches is less than or equal to upper bound" $
          \ts ps n ->
            checkPredicate ts (Between (Bound 0) (Bound n) ps)
              `shouldBe` countTrue ts ps <= n
      context "when Predicate is between" $ do
        prop "is always False if greater than upper bound" $
          \ts ps lb ->
            let len = countTrue ts ps
             in len >= 1
                  ==> checkPredicate ts (Between (Bound lb) (Bound (len - 1)) ps)
                  `shouldBe` False
        prop "is never True if lower bound is greater than upper bound" $
          \ts ps lB uB ->
            lB > uB
              ==> checkPredicate ts (Between (Bound lB) (Bound uB) ps)
                `shouldBe` False
        prop "is never True if upper bound is zero, unless lower bound is also" $
          \ts ps lB ->
            lB > 0
              ==> checkPredicate ts (Between (Bound lB) (Bound 0) ps)
              `shouldBe` False
        prop "is True if number of true predicates is between lower and upper bounds" $
          \ts ps lB uB ->
            lB < uB
              ==> let len = countTrue ts ps
                   in checkPredicate ts (Between (Bound lB) (Bound uB) ps)
                        `shouldBe` len >= lB && len <= uB
      context "when predicate is exactly" $
        prop "is True if number of true predicates is exactly the number" $
          \ts ps n ->
            checkPredicate ts (Between (Bound n) (Bound n) ps)
              `shouldBe` countTrue ts ps == n

countTrue :: (Num a) => Tags -> NonEmptyList Predicate -> a
countTrue ts = genericLength . filter (checkPredicate ts) . toList
