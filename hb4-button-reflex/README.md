
# hb4 - Button and getElementById


How to implement with reflex approximately the same things as in 
[hb1](https://github.com/gspia/half-baked/tree/master/hb1-button-ref)

That is, how to show conditional contents and how to see the properties 
of given dom element. This also contains trace-messages. And how to
find an element with given id.


## Compilation

First, start the try-reflex and then cd to the hb4-directory.

```
ghcjs src/Main.hs
```

This is tested with reflex-platform (with try-reflex). My ghcjs version
is "The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.2.1 (GHC 8.0.2)".


## After compilation

Go to src/Main.jsexe-dir and open index.html with your browser, look at the console messages
(e.g. ctr-shift-i) and try the button.


## Links

This is based on reflex examples , e.g.

  - [tutorial](https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md)
      (e.g. src/dom04.hs and other in that tutorial)
  - [reflex-examples](https://github.com/reflex-frp/reflex-examples)


Other tutorials/docs

  - [reflex docs](http://docs.reflex-frp.org/en/latest/architecture.html)
  - [Quickref - reflex](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
  - [Quickref - reflex-dom](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)
  - [Real-world-reflex](https://github.com/mightybyte/real-world-reflex/blob/master/index.md)


