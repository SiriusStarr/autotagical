{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Predicate
  ( -- * Predicate
    checkPredicate,
    Predicate (..),

    -- * Internal For Testing
    Bound (..),
    validatePredicate,
  )
where

import Data.Bool (bool)
import Data.Either.Validation (Validation (..))
import qualified Data.Fix as Fix
import Data.Foldable (traverse_)
import qualified Data.Functor.Foldable as Foldable
import qualified Data.Functor.Foldable.TH as TH
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (Decoder (..), FromDhall, autoWith, extractError)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import SafeType (NonEmptyList, NonEmptySet, SafeText, toSet)
import Tag (Tag, TagGroup (..), TagValue (..), Tags (..))

-- * Predicates

-- | A bound for the `Between` predicate, which may be numeric or simply all.
data Bound
  = AllPredicates
  | Bound Natural
  deriving (Eq, FromDhall, Generic)

instance Show Bound where
  show AllPredicates = "All"
  show (Bound n) = show n

-- | A `Predicate` that evaluates to either `True` or `False` for any given
--   file (or, more accurately, map of tagged values); used to rename/move
--   files.
data Predicate
  = Always
  | Between
      { lowerBound :: Bound,
        upperBound :: Bound,
        predicates :: NonEmptyList Predicate
      }
  | HasGroup TagGroup
  | HasTag
      { tag :: Tag,
        withValue :: Maybe (NonEmptySet SafeText)
      }
  | Not Predicate
  deriving (Eq, Generic)

instance Show Predicate where
  show Always = "Always"
  show (Between AllPredicates AllPredicates ps) = "All of " <> show ps
  show (Between (Bound 1) AllPredicates ps) = "Any of " <> show ps
  show (Between (Bound lb) AllPredicates ps) =
    "At least " <> show lb <> " of " <> show ps
  show (Between (Bound 0) (Bound ub) ps) =
    "At most " <> show ub <> " of " <> show ps
  show (Between (Bound lb) (Bound ub) ps)
    | lb == ub = concat ["Exactly ", show lb, " of ", show ps]
    | otherwise = concat ["Between ", show lb, " and ", show ub, " of ", show ps]
  show p@(Between AllPredicates (Bound _) _) =
    error $ "Invalid Predicate encountered: " <> show p <> "  Please report this as a bug, because it should not be possible to decode from Dhall."
  show (HasGroup g) = "Has tag from " <> show g
  show (HasTag t Nothing) = "Has tag " <> show t
  show (HasTag t vs) = concat ["Has tag ", show t, " with a value ", show vs]
  show (Not p) = "Not " <> show p

TH.makeBaseFunctor ''Predicate

deriving instance Generic (PredicateF a)

deriving instance (FromDhall a) => FromDhall (PredicateF a)

-- | Ensure that `Between` bounds are valid.  Invalid ones shouldn't be
--   achievable through the "exposed" Dhall interface.
validatePredicate :: Predicate -> Validation Text ()
validatePredicate (Between AllPredicates (Bound _) _) =
  Failure "The Between predicate may not have a numeric upper bound with a lower bound of AllPredicates.  This should not be achievable using the normal Dhall interface, so if you weren't messing around with internal Dhall types, please report it as a bug."
validatePredicate (Between (Bound lb) (Bound ub) _)
  | lb > ub =
    Failure $
      T.concat
        [ "The Between predicate may not have a lower bound greater than its upper bound.  You might have swapped the two on accident, since you tried to use a lower bound of ",
          T.pack (show lb),
          " and an upper bound of ",
          T.pack (show ub),
          ".  Try fixing that and then try again!"
        ]
validatePredicate (Between _ _ ps) = traverse_ validatePredicate ps
validatePredicate (Not p') = validatePredicate p'
validatePredicate _ = Success ()

instance FromDhall Predicate where
  autoWith opts = validate $ Fix.cata Foldable.embed <$> autoWith opts
    where
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success p -> case validatePredicate p of
              Failure err -> extractError err
              Success () -> Success p
            Failure f -> Failure f

-- | Given a TagGroup and a Tag, determine if the Tag is in the TagGroup.
inGroup :: Tag -> TagGroup -> Bool
inGroup t = S.member t . toSet . groupTags

-- | Given a list of Tags, determine whether a Predicate is true or false
checkPredicate :: Tags -> Predicate -> Bool
checkPredicate _ Always = True
checkPredicate ts (HasTag t Nothing) = M.member t . tagMap $ ts
checkPredicate ts (HasTag t (Just vs)) = case M.lookup t . tagMap $ ts of
  Just (Value v) -> S.member v $ toSet vs
  _ -> False
checkPredicate ts (HasGroup g) = any (`inGroup` g) . M.keys . tagMap $ ts
checkPredicate ts (Not p) = not $ checkPredicate ts p
checkPredicate ts (Between AllPredicates AllPredicates ps) =
  all (checkPredicate ts) ps
checkPredicate _ p@(Between AllPredicates (Bound _) _) =
  error $ "Invalid Predicate encountered: " <> show p <> "  Please report this as a bug, because it should not be possible to decode from Dhall."
checkPredicate ts (Between (Bound lb) AllPredicates ps) = go 0 $ toList ps
  where
    go n []
      | n >= lb = True
      | otherwise = False
    go n (p' : ps')
      | n' >= lb = True
      | otherwise = go n' ps'
      where
        n' = n + bool 0 1 (checkPredicate ts p')
checkPredicate ts (Between (Bound lb) (Bound ub) ps) = go 0 $ toList ps
  where
    go n []
      | n >= lb && n <= ub = True
      | otherwise = False
    go n (p' : ps')
      | n' > ub = False
      | otherwise = go n' ps'
      where
        n' = n + bool 0 1 (checkPredicate ts p')
