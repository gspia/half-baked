
# hb7 - bootstrap-reflex

This time we want to apply bootstrap 4 (beta) and build a project
with cabal. We imitate barebones of the dashbore-example as seen at
the [bs4 dashboard example](http://getbootstrap.com/docs/4.0/examples/dashboard/)
or at [bs4 guide example](https://www.codeply.com/go/KrUO8QpyXP/bootstrap-4-dashboard)
(see [bootstrap4.guide](http://getbootstrap.com/docs/4.0/examples/)).



## Compilation

We need a cabal file (or a nix-file) to use work-on -script.

We start the nix-environment with the following:
```
~/<PATH>/reflex-platform/work-on ghcjs ./.
```

Then you can build like this

```
cabal install --ghcjs
```

or

```
cabal configure --ghcjs
cabal build
```

(See [reflex-dom-contrib](https://github.com/reflex-frp/reflex-dom-contrib).)


And the program (js-scripts) can be found in
dist/build/bs4reflex/bs4reflex.jsexe -directory.

If you update cabal-file, you probably should restart nix-shell with
the above work-on -command.  Otherwise, it may happen that some of the
packages are not included in the nix-shell and cabal cannot find them.



## After compilation

Make css and js dirs to dist/build/bs4reflex/bs4reflex.jsexe -dir and
copy scripts.js and styles.css there.  They are required for bs4 and
their styles to work correctly together. So you should have
```
dist/build/bs4reflex/bs4reflex.jsexe/css/styles.css
dist/build/bs4reflex/bs4reflex.jsexe/js/scripts.js
```

Bootstrap uses jQuery and Tether and some javascript of its own. I didn't
get them working by generating the script-tags programmatically but including
the script-tags directly in the index.html-file worked. So, either copy
```
html/index.html
```
to
```
dist/build/bs4reflex/bs4reflex.jsexe/index.html
```
or edit the file. (See [bootstrap 4](https://v4-alpha.getbootstrap.com/).)
The other stylesheet-links
and script-tag for scripts.js are generated in the Main.hs.  The
copied index.html contains links to the aforementioned styles.css and
scripts.js. 

After these changes, you can open the index.html with your browser.

Note that if you edit the index.html (at the
dist/build/bs4reflex/bs4reflex.jsexe -directory), it won't be replaced
by build process later. External css-libs should be put there, e.g.
bootstrap, jquery and popper, to get the bootstrap working.

(Note that with v4-beta the tether has changed to the popper-lib.)


The scripts.js handles submenu at the sidebar and styles.css is used to
position the sidebar relative to the top navigation bar.

## Next steps

Be sure to check

  - [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui)
  - [reflex-material](https://github.com/alasconnect/reflex-material)
  - [reflex-material-bootstrap](https://github.com/hexresearch/reflex-material-bootstrap)


## Links


Tutorials and examples
  - [An introduction to reflex](https://blog.qfpl.io/posts/reflex/basics/introduction/)
  - [A Beginner-friendly Step by Step Tutorial for Reflex-Dom](https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md)
  - [reflex-examples](https://github.com/reflex-frp/reflex-examples)
  - [Real-world-reflex](https://github.com/mightybyte/real-world-reflex/blob/master/index.md)

Other links

  - [reflex-dom-contrib](https://github.com/reflex-frp/reflex-dom-contrib)
  - [reflex docs](http://docs.reflex-frp.org/en/latest/)
  - [Quickref - reflex](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
  - [Quickref - reflex-dom](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)

