# end-list

Lists, where the end list constructor is replaced with a term containing a single instance of an alternate type variable.

```haskell
data EndList e a = End e
                 | (:.) a (EndList e a)
```

