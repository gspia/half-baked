
# hb5 - fileinput-reflex


How to implement with reflex approximately the same things as in 
[hb2](https://github.com/gspia/half-baked/tree/master/hb2-fileinputs)

That is, how use file open -dialog and select several files. After 
that we output each file name, its size and contents.


## Compilation

First, start the try-reflex and then cd to the hb5-directory.

```
ghcjs src/Main.hs
```

This is tested with reflex-platform (with try-reflex). My ghcjs version
is "The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.2.1 (GHC 8.0.2)".


## After compilation

Go to src/Main.jsexe-dir and open index.html with your browser. Then 
you should be able to select several files and see their names.

## Links

This is based on reflex examples , e.g.

  - [reflex-examples](https://github.com/reflex-frp/reflex-examples)


Other tutorials/docs

  - [reflex docs](http://docs.reflex-frp.org/en/latest/architecture.html)
  - [Quickref - reflex](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
  - [Quickref - reflex-dom](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)
  - [Real-world-reflex](https://github.com/mightybyte/real-world-reflex/blob/master/index.md)


