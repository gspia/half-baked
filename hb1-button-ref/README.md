
Purescript Halogen has many examples and a guide, see
https://pursuit.purescript.org/packages/purescript-halogen/2.1.0


However, if you are newcomer (like me, summer 2017), it can be a bit difficult
to study Purescript as a language, Halogen as a lib, and javascript, html and
css along at the same time.

Quite soon pursuit and mdn where open at the same time:
- https://pursuit.purescript.org/
- https://developer.mozilla.org/en-US/docs/Web



Using bootstrap css-lib together with Halogen left me wondering, how to check
the values of properties of given nodes or elements. 

What is the Halogen equivalent of getElementById
(https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById) 
and how to read values of an element?


Maybe RefLabels? See
https://github.com/slamdata/purescript-halogen/issues/423
If we had getHTMLButtonRef, then we could read properties of the button.  (See
the comments on Main. Halogen.Query.InputF defines RefLabel. Its usage, however,
wasn't instantly clear based on the docs.)



Compilation

bower install
npm run build

This is tested with purescript 0.11.5.


After compilation

Go to dist dir and open index.html with your browser, look at the console messages
(e.g. ctr-shift-i) and see whats happening.

