# end-list

Lists, where the end (empty) list constructor is replaced with a constructor containing a single instance of an alternate type variable.

```haskell
data [] a = []
          | a : [a]

data EndList e a = End e
                 | (:.) a (EndList e a)

example :: EndList String Int
example = 1 :. 2 :. 3 :. End "Hi, I'm at the end!"
```

A few properties:
- The `e` parameter of an `EndList` is always inhabited by exactly one value
  + `EndList Void a -> Void`
- `EndList () a` is isomorphic to `[a]`
- A left fold over the `a`'s can end with storing the result in the `End` without an additional traversal
  + This can be used to build up a result in the `End` and evaluate it one step at a time by dropping elements


## Splitting lists

`EndList`s are a great way to lazily split lists.


### On predicates

- We can lazily split a list into two sublists by placing the second list in the end of an `EndList`:

```haskell
spanEnd :: (a -> Bool) -> [a] -> EndList [a] a
spanEnd _ [] = End []
spanEnd p (x:xs)
  | p x = x :. spanEnd p xs
  | otherwise = End (x : xs)

λ> spanEnd (< 4) [1..6]
1 :. 2 :. 3 :. End [4,5,6]
```


### On sums

`EndList`s may be used to provide a partition of a list according to a sum (`Either`):

```haskell
newtype PartList a b = PartList { runPartList :: EndList (Maybe (PartList b a)) b }

partEither :: [Either a b] -> Either (PartList b a) (PartList a b)
```

I haven't benchmarked, but I expect the `PartList` type to be
more memory efficient than `[Either a b]` when there are sufficiently long
subsequences that are `all isLeft` or `all isRight`.


## Note

The original plan was for `Data.List.End` to match the API provided in `Data.List`.

(There are a lot of commented type signatures for yet unimplemented functions.)

However, since then I've implemented enough methods to get an idea of what working
with them is like and some more intuition as to how they relate to regular lists.

Since I intend to apply the general method to a wide variety of data types,
the next plan is to use Template Haskell to lift methods on lists to methods on
`EndList`s, or in general, to lift methods on a data-type to methods on that
data type with empty constructors replaced in a similar way.

# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/end-list/)

