
# hb6 - filesending-reflex


How to implement with reflex approximately the same things as in
[hb3](https://github.com/gspia/half-baked/tree/master/hb3-filesending)

That is, how use file open -dialog and select several files, and check
that the selected files were delivered to the server (into the output-directory).

Things TBD/questions:
- Does this work with massive amounts (size, number) of files? How to test?
- The ArrayBuffer can probably be used with bsFromMutableArrayBuffer e.g.
  (not done yet as the structure where this would be applied is probably not ok).
- How to do this with Generics and JSON messages (this would require server changes as well)?
- GHCJS.DOM.WebSockets were used, JSaddle one TBD.
- How to organize cabal-files in this kind of setting? (TBD)


## Compilation

First, start the try-reflex and then cd to the hb6-directory.

```
ghcjs src/Main.hs
```

This is tested with reflex-platform (with try-reflex). My ghcjs version
is "The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.2.1 (GHC 8.0.2)".

The server can be compiled with
```
cabal new-build
```

This is tested with ghc 8.0.2 and cabal 2.1.0.0 (cabal-install version 2.1.0.0).


## After compilation

Start the webserver. It can be found from dist-newstyle-directory. In my case
the full path is
./dist-newstyle/build/x86_64-linux/ghc-8.0.2/hb6-filesending-reflex-0.1/build/hb6-filesending-reflex/hb6-filesending-reflex

(Ensure that you have downloads-directory.)

Go to src/Main.jsexe-dir and open index.html with your browser. Then
you should be able to select several files and be able to send them to the server. Alternatively, 
you may copy the contents of src/Main.jsexe-dir to static-directory and let the 
server serve the file (point the browser to the localhost:8000)

## Links

  - [reflex-examples](https://github.com/reflex-frp/reflex-examples)
  - [hsnippet](https://github.com/mightybyte/hsnippet)
  - [reflex docs](http://docs.reflex-frp.org/en/latest/architecture.html)
  - [Quickref - reflex](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
  - [Quickref - reflex-dom](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)
  - [Real-world-reflex](https://github.com/mightybyte/real-world-reflex/blob/master/index.md)


