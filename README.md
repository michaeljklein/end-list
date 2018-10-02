# end-list

Lists, where the end list constructor is replaced with a term containing a single instance of an alternate type variable.

```haskell
data EndList e a = End e
                 | (:.) a (EndList e a)
```


`EndList`s may be used to provide a partition of a list according to a sum (`Either`):

```haskell
newtype PartList a b = PartList { runPartList :: EndList (Maybe (PartList b a)) b }

partEither :: [Either a b] -> Either (PartList b a) (PartList a b)
```

