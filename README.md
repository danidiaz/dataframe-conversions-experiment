```
$ cabal repl
ghci> frame <- createExampleFrame
ghci> frame & describe
"doubly" -> Double
"floaty" -> Float
"inty" -> Int
ghci> frame & "foo" .= "inty" +: "floaty"
ghci> describe frame
"doubly" -> Double
"floaty" -> Float
"foo" -> Float
"inty" -> Int
ghci> frame & printCol "foo"
2.0
4.0
6.0
ghci>
```