# FLP: Binary decision tree implementation in Haskell

- [Assignment](https://docs.google.com/document/d/1WChPfFOMtU3GBuqEZRUKFrNBsdJuaQ1pg1BIXiWPe98/edit?pli=1)
- [CART & Gini index
  explained](https://medium.com/geekculture/decision-trees-with-cart-algorithm-7e179acee8ff)
- [Folds explained](https://wiki.haskell.org/Foldr_Foldl_Foldl')

## Example usage

```sh
./flp-fun -1 tree.txt tree-examples.txt
./flp-fun -2 training-data.txt
```

## TODO

- [x] ~~Check that there are white space characters in the tree, not some random
  junk~~
- [x] Handle files with newlines at the end
  - Check this lovely line `let dataset = Prelude.map splitLine (Prelude.filter (not . Prelude.null) (lines contents))`
