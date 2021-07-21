# Kattis

"https://open.kattis.com/problems/" ++ lowercase_module_name(in src/)

e.g. https://open.kattis.com/problems/reduction

# usage

*** important ***: rename module name to "Main" before submission

in `src/` folder, at the command line:
```sh
cat ../reduction.txt | runghc -package --ghc-arg=parsec Reduction.hs
```