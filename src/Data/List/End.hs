{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.List.End where

import Control.Applicative
import Control.Comonad (Comonad(..))
import Control.Foldl (Fold(..), FoldM(..), fold, foldM)
import Control.Foldl.Utils (extractM, feed, feedM)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Free (Free(..))
import Control.Monad.Trans.Free (FreeT(..))
import qualified Control.Monad.Trans.Free as F
import Control.Monad.Zip (MonadZip(..))
import Data.Biapplicative (Biapplicative(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Function (fix)
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import GHC.Generics
import Prelude hiding (repeat)

-- import Data.End (HasEnd(..))

-- (++) :: EndList e a -> EndList e a -> EndList e a
-- (++) = (<>)

-- head :: EndList e a -> a
-- head = _

-- last :: EndList e a -> a
-- last = _

-- tail :: EndList e a -> EndList e a
-- tail = _

-- init :: EndList e a -> EndList e a
-- init = _

-- uncons :: EndList e a -> Either e (a, EndList e a)
-- uncons = _

-- map :: (a -> b) -> EndList e a -> EndList e b
-- map = _

-- reverse :: EndList e a -> EndList e a
-- reverse = _


-- intersperse :: _
-- intersperse = _

-- intercalate :: _
-- intercalate = _

-- transpose :: _
-- transpose = _


-- subsequences :: _
-- subsequences = _

-- permutations :: _
-- permutations = _

-- concat :: _
-- concat = _

-- concatMap :: _
-- concatMap = _

-- scanl :: _
-- scanl = _

-- scanl' :: _
-- scanl' = _

-- scanl1 :: _
-- scanl1 = _

-- scanr :: _
-- scanr = _

-- scanr1 :: _
-- scanr1 = _


-- mapAccumL :: _
-- mapAccumL = _

-- mapAccumR :: _
-- mapAccumR = _


-- iterate :: _
-- iterate = _

-- iterate' :: _
-- iterate' = _

-- repeat :: _
-- repeat = _

-- replicate :: _
-- replicate = _

-- cycle :: _
-- cycle = _


-- unfoldr :: _
-- unfoldr = _


-- take :: _
-- take = _

-- drop :: _
-- drop = _

-- splitAt :: _
-- splitAt = _


-- takeWhile :: _
-- takeWhile = _

-- dropWhile :: _
-- dropWhile = _

-- dropWhileEnd :: _
-- dropWhileEnd = _

-- span :: _
-- span = _

-- break :: _
-- break = _


-- stripPrefix :: _
-- stripPrefix = _


-- group :: _
-- group = _


-- inits :: _
-- inits = _

-- tails :: _
-- tails = _


-- isPrefixOf :: _
-- isPrefixOf = _

-- isSuffixOf :: _
-- isSuffixOf = _

-- isInfixOf :: _
-- isInfixOf = _

-- isSubsequenceOf :: _
-- isSubsequenceOf = _


-- elem :: _
-- elem = _

-- notElem :: _
-- notElem = _

-- lookup :: _
-- lookup = _


-- find :: _
-- find = _

-- filter :: _
-- filter = _

-- partition :: _
-- partition = _


-- (!!) :: _
-- (!!) = _


-- elemIndex :: _
-- elemIndex = _

-- elemIndices :: _
-- elemIndices = _


-- findIndex :: _
-- findIndex = _

-- findIndices :: _
-- findIndices = _


-- zip :: _
-- zip = _

-- zip3 :: _
-- zip3 = _

-- zip4
-- zip5
-- zip6
-- zip7 :: _


-- zipWith :: _
-- zipWith = _

-- zipWith3 :: _
-- zipWith3 = _

-- zipWith4, zipWith5, zipWith6, zipWith7 :: _
-- zipWith4, zipWith5, zipWith6, zipWith7 = _


-- unzip :: _
-- unzip = _

-- unzip3 :: _
-- unzip3 = _

-- unzip4, unzip5, unzip6, unzip7 :: _
-- unzip4, unzip5, unzip6, unzip7 = _


-- lines :: _
-- lines = _

-- words :: _
-- words = _

-- unlines :: _
-- unlines = _

-- unwords :: _
-- unwords = _


-- nub :: _
-- nub = _


-- delete :: _
-- delete = _

-- (\\) :: _
-- (\\) = _


-- union :: _
-- union = _

-- intersect :: _
-- intersect = _


-- sort :: _
-- sort = _

-- sortOn :: _
-- sortOn = _

-- insert :: _
-- insert = _


-- nubBy :: _
-- nubBy = _

-- deleteBy :: _
-- deleteBy = _

-- deleteFirstsBy :: _
-- deleteFirstsBy = _

-- unionBy :: _
-- unionBy = _

-- intersectBy :: _
-- intersectBy = _

-- groupBy :: _
-- groupBy = _


-- sortBy :: _
-- sortBy = _

-- | Insert with a given `Ordering` and `Fold` to update the end
insertBy :: Fold a e -> (a -> a -> Ordering) -> a -> EndList e' a -> EndList e a
insertBy f _ x (End _) = End $ fold f (Identity x)
insertBy f cmp x ys@(~(y :. ys')) =
  case cmp x y of
    GT -> y :. insertBy (f `feed` y) cmp x ys'
    _ -> (x :. ys) `resetEndWith` f

-- | Insert with a given `Ordering` and `FoldM` to update the end
insertByM :: Monad m => FoldM m a e -> (a -> a -> Ordering) -> a -> EndList e' a -> m (EndList e a)
insertByM f _ x (End _) = End <$> foldM f (Identity x)
insertByM f cmp x ys@(~(y :. ys')) =
  case cmp x y of
    GT -> (y :.) <$> insertByM (f `feedM` y) cmp x ys'
    _  -> (x :. ys) `resetEndWithM` f

-- | Maximum using the given ordering
maximumBy :: (a -> a -> Ordering) -> EndList e a -> a
maximumBy _ (End _) = errorWithoutStackTrace "Data.List.End.maximumBy: Empty EndList"
maximumBy cmp xs = foldl1 maxBy xs
  where
    maxBy x y = case cmp x y of
                  GT -> x
                  _  -> y

-- | Minimum using the given ordering
minimumBy :: (a -> a -> Ordering) -> EndList e a -> a
minimumBy _ (End _) = errorWithoutStackTrace "Data.List.End.minimumBy: Empty EndList"
minimumBy cmp xs = foldl1 minBy xs
  where
    minBy x y = case cmp x y of
                  GT -> y
                  _  -> x




-- | Generic length, using the supplied function to calculate the length
-- of the end.
genericLength :: Num i => (e -> i) -> EndList e a -> i
{-# NOINLINE [1] genericLength #-}
genericLength f (End e) = f e
genericLength f ~(_ :. xs) = 1 + genericLength f xs

{-# RULES
"genericLengthInt" genericLength =
                   (strictGenericLength :: (e -> Int) -> EndList e a -> Int)
"genericLengthInteger" genericLength =
                       (strictGenericLength :: (e -> Integer) -> EndList e a -> Integer)
 #-}

-- | Strict version of `genericLength`
strictGenericLength :: Num i => (e -> i) -> EndList e a -> i
strictGenericLength f xs = loop xs 0
  where
    loop (End e) l = ((+) $! l) $! (f $! e)
    loop ~(_ :. ys) l = loop ys $! (l + 1)

-- | Take some number of elements and use the given `Fold`
-- to calculate the new end
genericTake :: Integral i => Fold a e -> i -> EndList e' a -> EndList e a
genericTake f n _ | n <= 0 = End $ extract f
genericTake f _ (End _) = End $ extract f
genericTake f n (x :. xs) = x :. genericTake (f `feed` x) (n - 1) xs

-- | `genericTake`, where a `FoldM` is provided to update the end
genericTakeM :: (Integral i, Monad m) => FoldM m a e -> i -> EndList e' a -> m (EndList e a)
genericTakeM f n _ | n <= 0 = End <$> extractM f
genericTakeM f _ (End _) = End <$> extractM f
genericTakeM f n (x :. xs) = (x :.) <$> genericTakeM (f `feedM` x) (n - 1) xs

-- | Drop the given number of elements from the front of an `EndList` or none
-- if negative.
genericDrop :: Integral i => i -> EndList e a -> EndList e a
genericDrop n xs | n <= 0 = xs
genericDrop _ (End e) = End e
genericDrop n (_ :. xs) = genericDrop (n - 1) xs

-- | Split at the given index, using the given `Fold` to provide the end of the prefix,
-- or split at @0@ if negative.
genericSplitAt :: Integral i => Fold a e' -> i -> EndList e a -> (EndList e' a, EndList e a)
genericSplitAt f n xs | n <= 0 = (End (extract f), xs)
genericSplitAt f _ (End e') = (End (extract f), End e')
genericSplitAt f n ~(x :. xs) = (x :. xs', xs'')
  where
    ~(xs', xs'') = genericSplitAt (f `feed` x) (n - 1) xs

-- | `genericSplitAt`, where a `FoldM` is provided to calculate the end of the prefix
genericSplitAtM :: (Integral i, Monad m) => FoldM m a e' -> i -> EndList e a -> m (EndList e' a, EndList e a)
genericSplitAtM f n xs | n <= 0 = (, xs) . End <$> extractM f
genericSplitAtM f _ (End e') = (, End e') . End <$> extractM f
genericSplitAtM f n ~(x :. xs) = do
  ~(xs', xs'') <- genericSplitAtM (f `feedM` x) (n - 1) xs
  return (x :. xs', xs'')

-- | The element of an `EndList` at the given index. Throws an error if given a negative argument
genericIndex :: Integral i => EndList e a -> i -> a
genericIndex (x :. _ ) 0 = x
genericIndex (_ :. xs) n
  | n > 0     = genericIndex xs (n - 1)
  | otherwise = errorWithoutStackTrace "Data.List.End:genericIndex: negative argument."

-- genericReplicate :: Integral i => Fold a e -> i -> a -> EndList e a
-- genericReplicate f n = _

-- genericReplicateM :: (Integral i, Monad m) => FoldM m a e -> i -> a -> m (EndList e a)
-- genericReplicateM f n = (>>= genericTakeM f n) . _


-- | A list of elements of type @a@ where the end of the list contains
-- a single element of type @e@
data EndList e a = End e
                 | (:.) a (EndList e a)
                 deriving (Eq, Ord, Functor, Generic, Generic1)

instance (Show e, Show a) => Show (EndList e a) where
  show (End x) = "End " <> show x
  show (x :. xs) = show x <> " :. " <> show xs

instance Bifunctor EndList where
  bimap f _ (End x) = End (f x)
  bimap f g ~(x :. xs) = g x :. bimap f g xs

instance Biapplicative EndList where
  bipure x y = y :. End x

  End f <<*>> End x = End $ f x
  End f <<*>> ~(_ :. xs) = End f <<*>> xs
  (_ :. fs) <<*>> End x = fs <<*>> End x
  ~(f :. fs) <<*>> ~(x :. xs) = f x :. (fs <<*>> xs)

instance Bifoldable EndList where
  bifoldr f _ x (End y) = f y x
  bifoldr f g x ~(y :. ys) = bifoldr f g (g y x) ys

instance Bitraversable EndList where
  bitraverse f _ (End x) = End <$> f x
  bitraverse f g ~(x :. xs) = (:.) <$> g x <*> bitraverse f g xs

instance Foldable (EndList e) where
  foldr _ x (End _) = x
  foldr f x ~(y :. ys) = foldr f (f y x) ys

instance Traversable (EndList e) where
  traverse _ (End x) = pure $ End x
  traverse f ~(x :. xs) = (:.) <$> f x <*> traverse f xs

instance Monoid e => Applicative (EndList e) where
  pure = (:. empty)

  End x <*> _ = End x
  _ <*> End x = End x
  (f :. fs) <*> (x :. xs) = f x :. (fs <*> xs)

instance Monoid e => Alternative (EndList e) where
  empty = End mempty

  End x <|> y = mappend x `first` y
  x <|> End y = mappend y `first` x
  ~(x :. xs) <|> ys = x :. (xs <|> ys)

instance Monoid e => Monad (EndList e) where
  fail _ = End mempty

  End x >>= _ = End x
  (x :. xs) >>= f = f x <|> (xs >>= f)

instance Monoid e => MonadZip (EndList e) where
  mzip (End x) (End y) = End $ mappend x y
  mzip (End x) _ = End x
  mzip _ (End y) = End y
  mzip ~(x :. xs) ~(y :. ys) = (x, y) :. mzip xs ys

  mzipWith _ (End x) (End y) = End $ mappend x y
  mzipWith _ (End x) _ = End x
  mzipWith _ _ (End y) = End y
  mzipWith f ~(x :. xs) ~(y :. ys) = f x y :. mzipWith f xs ys

instance Monoid e => MonadFix (EndList e) where
  mfix f = case fix (f . ehead) of
             End x -> End x
             ~(x :. _) -> x :. mfix (etail . f)







-- EndList utilities

-- | Convert a list and an end value to an `EndList`
fromListWithEnd :: e -> [a] -> EndList e a
fromListWithEnd x [] = End x
fromListWithEnd x ~(y:ys) = y :. fromListWithEnd x ys

-- | Convert a list and an end value calculated through a left scan to an `EndList`
fromListWithEndF :: (a -> e -> e) -> e -> [a] -> EndList e a
fromListWithEndF _ x [] = End x
fromListWithEndF f x ~(y:ys) = y :. fromListWithEndF f (f y x) ys

-- | `fromListWithEndF` with an `Applicative` state
fromListWithEndA :: Applicative f => (a -> f e -> f e) -> f e -> [a] -> f (EndList e a)
fromListWithEndA _ x [] = End <$> x
fromListWithEndA f x ~(y:ys) = (:.) <$> pure y <*> fromListWithEndA f (f y x) ys

-- | The first element of an `EndList`, lazily matched
ehead :: EndList e a -> a
{-# INLINE ehead #-}
ehead ~(x :. _) = x

-- | The tail of an `EndList`, lazily matched
etail :: EndList e a -> EndList e a
{-# INLINE etail #-}
etail (End x) = End x
etail ~(_ :. xs) = xs

-- | Concatenate `EndList`s, combining their ends with @(`<>`)@
econcat :: Semigroup e => EndList e (EndList e a) -> EndList e a
econcat = econcatWith (<>)

-- | Concatenate `EndList`s, combining their ends with the given method
econcatWith :: (e -> e -> e) -> EndList e (EndList e a) -> EndList e a
econcatWith _ (End x) = End x
econcatWith f ~(x :. xs) = case x of
                             End y -> f y `first` econcatWith f xs
                             ~(y :. ys) -> y :. econcatWith f (ys :. xs)

-- | Convert an `EndList` to a `Free` representation
endListToFree :: EndList e a -> Free ((,) a) e
endListToFree (End x) = Pure x
endListToFree ~(x :. xs) = Free (x, endListToFree xs)

-- | Convert a `Free` representation to an `EndList`
freeToEndList :: Free ((,) a) e -> EndList e a
freeToEndList (Pure x) = End x
freeToEndList ~(Free (x, xs)) = x :. freeToEndList xs

-- | Convert an `EndList` to a `Monad`ic `FreeT` representation
endListToFreeM :: Monad m => EndList e a -> FreeT ((,) a) m e
endListToFreeM (End x) = FreeT . return . F.Pure $ x
endListToFreeM ~(x :. xs) = FreeT . return . F.Free $ (x, endListToFreeM xs)

-- | Convert a `Monad`ic `FreeT` representation to an `EndList`
freeToEndListM :: Monad m => FreeT ((,) a) m e -> m (EndList e a)
freeToEndListM = (>>= go) . runFreeT
  where
    go (F.Pure x) = return . End $ x
    go ~(F.Free (x, xs)) = (x :.) <$> freeToEndListM xs

-- | Use a `Fold` to replace the end of an `EndList`
resetEndWith :: EndList e' a -> Fold a e -> EndList e a
resetEndWith (End _) f = End $ extract f
resetEndWith ~(x :. xs) f = x :. resetEndWith xs (f `feed` x)

-- | Use a `FoldM` to replace the end of an `EndList`
resetEndWithM :: Monad m => EndList e' a -> FoldM m a e -> m (EndList e a)
resetEndWithM (End _) f = End <$> extractM f
resetEndWithM ~(x :. xs) f = (x :.) <$> resetEndWithM xs (f `feedM` x)


-- | Example application of `EndList`: a list that's
-- all @b@'s, then all @a@'s, then all @b@'s, ..
newtype PartList a b = PartList { runPartList :: EndList (Maybe (PartList b a)) b } deriving (Eq, Ord, Show)

-- | Convert a list of `Either`'s to a `Either` `PartList` possibility
partEither :: [Either a b] -> Either (PartList b a) (PartList a b)
partEither [] = Right . PartList $ End Nothing
partEither ~(x:xs) = case x of
                       Left x' -> Left . PartList $ x' :. loopL xs
                       Right x' -> Right . PartList $ x' :. loopR xs
  where
    loopL [] = End Nothing
    loopL ~(y:ys) = case y of
                      Left y' -> y' :. loopL ys
                      ~(Right y') -> End . Just . PartList $ y' :. loopR ys

    loopR [] = End Nothing
    loopR ~(y:ys) = case y of
                      Right y' -> y' :. loopR ys
                      ~(Left y') -> End . Just . PartList $ y' :. loopL ys

-- | `Left` for even bits and `Right` for odd:
--
-- @
--  λ> enumEither 1265
--  [Right (),Left (),Left (),Left (),Right (),Right (),Right (),Right (),Left (),Left (),Right ()]--
-- @
--
enumEither :: Integral t => t -> [Either () ()]
enumEither 0 = [Left ()]
enumEither 1 = [Right ()]
enumEither n = case divMod n 2 of
                 (dv, 0) -> Left () : enumEither dv
                 ~(dv, _) -> Right () : enumEither dv

-- | Convert an `Integral` input to a list of `Either`s whose values are @[0..]@ and are `Left` or `Right` as in `enumEither`
--
-- @
--  λ> enumEithers 1265
--  [Right 0,Left 1,Left 2,Left 3,Right 4,Right 5,Right 6,Right 7,Left 8,Left 9,Right 10]
-- @
--
-- @
--  λ> partEither $ enumEithers 1265
--  Right
--    (PartList
--       { runPartList =
--           0 :.
--           End
--             Just
--             (PartList
--                { runPartList =
--                    1 :. 2 :. 3 :.
--                    End
--                      Just
--                      (PartList
--                         { runPartList =
--                             4 :. 5 :. 6 :. 7 :.
--                             End
--                               Just
--                               (PartList
--                                  { runPartList =
--                                      8 :. 9 :.
--                                      End
--                                        Just
--                                        (PartList
--                                           {runPartList = 10 :. End Nothing})
--                                  })
--                         })
--                })
--       })
-- @
--
enumEithers :: (Enum b, Num b, Integral t) => t -> [Either b b]
enumEithers x = zipWith (either (const $ Left) (const $ Right)) (enumEither x) [0..]

