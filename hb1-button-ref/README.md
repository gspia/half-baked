
# hb1 - Button and RefLabels

Purescript
[Halogen](https://pursuit.purescript.org/packages/purescript-halogen/2.1.0) has
many examples and a guide.  If you are newcomer (like me, summer 2017), it can
be a bit difficult to study Purescript as a language, Halogen as a lib, and
javascript, html and css all at the same time.

Quite soon [Pursuit](https://pursuit.purescript.org/) and [MDN](https://developer.mozilla.org/en-US/docs/Web)
were open at the same time.


Using bootstrap css-lib together with Halogen left me wondering, how to check
the values of properties of given nodes or elements. 

What is the Halogen equivalent of
[getElementById](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById)
and how to read values of an element?


Maybe RefLabels? See the code at a
[discussion](https://github.com/slamdata/purescript-halogen/issues/423). If we
had getHTMLButtonRef, then we could read properties of the button.  (See the
comments on
[Main](https://github.com/gspia/half-baked/blob/master/hb1-button-ref/src/Main.purs).
[Halogen.Query.InputF](https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.Query.InputF) 
defines `RefLabel`. Its usage, however, wasn't instantly clear based on the docs.)

Note that
[web-audio-demo-player](https://github.com/justinwoo/purescript-web-audio-player-demo)
and associated
[blog-post](http://qiita.com/kimagure/items/653c52e77d7cd3567498) seem to
answer some of the questions posed in Main (-> update this readme & main TBD). 



## Compilation

```
bower install
npm run build
```

This is tested with purescript 0.11.5.


## After compilation

Go to dist dir and open index.html with your browser, look at the console messages
(e.g. ctr-shift-i) and see what's happening.

