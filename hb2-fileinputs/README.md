
# hb2 - Fileinput and file processing 

Purescript
[Halogen](https://pursuit.purescript.org/packages/purescript-halogen/2.1.0) has
many examples and a guide but file handling wasn't instantly obvious (summer 2017).


## Idea shortly

Tie file input onChange event to an action, which in turn, launches the file
reading (in to the client) together with a listener waiting for
[loadend](https://developer.mozilla.org/en-US/docs/Web/Events/loadend) event.
Listener launches another action that uses the files, when they have been
loaded.

To find out the files, we have to get a list of files from input element,
[see](https://pursuit.purescript.org/packages/purescript-dom/4.5.0/docs/DOM.HTML.HTMLInputElement#v:files).
But that needs the [HTMLInputElement](https://pursuit.purescript.org/packages/purescript-dom/4.5.0/docs/DOM.HTML.Types#t:HTMLInputElement).

The FileCh action looking for the onChange-event does not take inputs nor it
isn't clear how to check (or actually to get a handle), which html element/node
originated the action. We use RefLabels for this like in
[hb1](https://github.com/gspia/half-baked/tree/master/hb1-button-ref).  To make
things work, we need getHTMLInputRef (see Main) that mimicks
[getHTLMElementRef](https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.Query#v:getHTMLElementRef).

This leads to somewhat long route to be able to load files and use them. And to
a lot of questions (see Main).

Note that
[web-audio-demo-player](https://github.com/justinwoo/purescript-web-audio-player-demo)
and associated
[blog-post](http://qiita.com/kimagure/items/653c52e77d7cd3567498) seem to
answer some of the questions posed in Main (-> update this readme & main TBD).  
It uses, e.g. [createObjectURL](https://pursuit.purescript.org/packages/purescript-dom/4.5.0/docs/DOM.HTML.URL#v:createObjectURL)
that seams to lead much clearer structure (-> try it TBD).

Moreover,
[fileselector](https://github.com/jacereda/purescript-halogen-fileselector)
and [filepicker](https://github.com/zudov/purescript-halogen-filepicker)
seems to answer many of the questions. (-> try them TBD)



## Compilation

```
bower install
npm run build
```

This is tested with purescript 0.11.5.


## After compilation

Go to dist dir and open index.html with your browser, open select file input
dialog, select one or more files, and look at the console messages (e.g.
ctr-shift-i) and see what's happening.

## Other notes

While writing these notes, both [Pursuit](https://pursuit.purescript.org/) and
[MDN](https://developer.mozilla.org/en-US/docs/Web) were open at the same time.
