
# hb3 - filesending

## Idea shortly

On client, tie file input onChange event to an action, which in turn, launches the file
reading (in to the client) together with a listener waiting for
[loadend](https://developer.mozilla.org/en-US/docs/Web/Events/loadend) event.
Listener sends the files with websockets to the server, when the files have been
loaded.

This is based on [hb2](https://github.com/gspia/half-baked/tree/master/hb2-fileinputs).
This leads to somewhat long route to be able to load files and send them to
server. The client is written in [Purescript](http://www.purescript.org/) 
and [Halogen](https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen).
And to a lot of questions (see Main).

Server is written in [Haskell](https://www.haskell.org/) and
[snap-framework](http://snapframework.com/) and with its
[libs](http://hackage.haskell.org/packages/search?terms=snap).


Things TBD: 
- How to test different networking conditions/problems? 
- How about large file or large number of files? 
- Dom-libs expose number of other methods: some other more suitable for application? 
- Streaming and websockets (from halogen-client to snap-server)?


## Compilation

Purescript part
```
bower install
npm run build
```

This is tested with purescript 0.11.5. This copies the app.js to the static
directory, where the index.html is, too.


Haskell part
```
cabal new-build
```

This is tested with ghc 8.0.2 and cabal 2.1.0.0 (cabal-install version 2.1.0.0).


## After compilation

Start the webserver. It can be found from dist-newstyle-directory. In my case
the full path is 
./dist-newstyle/build/x86_64-linux/ghc-8.0.2/hb3-filesending-0.1/noopt/build/hb3-filesending/hb3-filesending.

(Ensure that you have downloads-directory.)

Then open your web browser and go to http://localhost:8000. The console (of
browser, ctrl-shift-i) shows information at the client side and similarly the
server writes information to the console. 

If you select a number of files, the downloads-dir should contain them: 
they should be same which can be checked with e.g. diff. 


## Links

- [Hayoo](http://hayoo.fh-wedel.de/)
- [Hackage](http://hackage.haskell.org/)
- [Pursuit](https://pursuit.purescript.org/) 
- [MDN](https://developer.mozilla.org/en-US/docs/Web)
- [Google web](https://developers.google.com/web/)

