{-
  Idea shortly: apply boostrap (4, alpha6) and construct some barebones
  of the https://v4-alpha.getbootstrap.com/examples/dashboard/ -example.
  Or https://www.codeply.com/go/KrUO8QpyXP/bootstrap-4-dashboard -example
  (see http://bootstrap4.guide/examples.html).

  Moreover, this is build with cabal and thus uses the work-on -script
  that is delivered with the reflex-platform.

  Questions/problems:
  - How to make ghcid working?
  - This sample uses the classes directly from bootstrap css. Any idea in
    building a small lib from those? (See semui/material/contrib, links at readme.)
  - Now external javascript-libs (jquery, bs, tether) and scripts.js handle
    part of the ui. This results in ui-states that the haskell code is (possibly)
    not aware of. How the other ui-libs have solved this (better to really see
    semui/material/contrib this time, links are provided at the readme.)
-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecursiveDo, ScopedTypeVariables #-}
import Reflex.Dom
import Data.Map as Map
import Data.Monoid
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  elAttr "meta" ("charset" =: "utf-8") $ pure ()
  elAttr "meta" (
    ("name" =: "viewport") <>
    ("content" =: "width=device-width, initial-scale=1, shrink-to-fit=no")
                ) $ pure ()
  el "title" $ text "Decian platform"
  styleSheet "https://fonts.googleapis.com/css?family=Raleway"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css"
  styleSheet "./css/styles.css"
  -- elAttr "link" (
  --   ("rel" =: "stylesheet")
  --   <> ("href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css")
  --   <> ("integrity" =: "sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ")
  --   <> ("crossorigin" =: "anonymous")
  --               ) $ pure ()
  -- script "https://code.jquery.com/jquery-3.2.1.min.js"
  script "./js/scripts.js"
  where
    styleSheet lnk = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", lnk)
      ]) $ return ()
    script lnk = elAttr "script" ("src" =: lnk) $ return ()


bodyElement :: MonadWidget t m => m ()
bodyElement =  do
  navBar
  elAttr "div" (("class" =: "container-fluid") <> ("id" =: "main")) $ do
    divClass "row row-offcanvas row-offcanvas-left" $ do
      mi :: Dynamic t MenuItem <- sideBar
      uiPanel mi
  -- Other information not depending on the sideBar menu choice:
  divClass "row" $ do
    divClass "col-lg-2 col-md-3" $ blank
    divClass "col-lg-10 col-md-9" $ do
      el "h1" $ text "Other information"
  return ()

  -- elAttr "script" (
  --   ("src" =: "https://code.jquery.com/jquery-3.1.1.slim.min.js")
  --   <> ("integrity" =: "sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n")
  --   <> ("crossorigin" =: "anonymous")
  --             ) $ pure ()
  -- elAttr "script" (
  --   ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js")
  --   <> ("integrity" =: "sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb")
  --   <> ("crossorigin" =: "anonymous")
  --             ) $ pure ()
  -- elAttr "script" (
  --   ("src" =: "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js")
  --   <> ("integrity" =: "sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn")
  --   <> ("crossorigin" =: "anonymous")
  --             ) $ pure ()
  -- script "https://code.jquery.com/jquery-3.1.1.slim.min.js"
  -- script "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js"
  -- script "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js"
  -- where
  --   script lnk = elAttr "script" ("src" =: lnk) $ return ()


-- This is the top navbar (fixed) and very static at the moment. sideBar-uiPanel
-- below show one way, how to make content dynamic.
navBar :: MonadWidget t m => m ()
navBar = do
  elClass "nav" navBarC $ do
    elAttr "button" btnAttrs $ elClass "span" "navbar-toggler-icon" $ pure ()
    divClass "flex-row d-flex" $ do
      elAttr "a" ( ("class" =: "navbar-brand mb-1")
                 <> ("href" =: "#")
                 ) $ text "Brand"
      elAttr "button" ( ("class" =: "hidden-md-up navbar-toggler")
                      <> ("data-toggle" =: "offcanvas")
                      <> ("title" =: "Toggle left")
                      )
        $ elClass "span" "navbar-toggler-icon" $ pure ()
    elAttr "div" divAttrs $ do
      elClass "ul" "navbar-nav" $ do
        elClass "li" "nav-item active" $ do
          elAttr "a" ( ("class" =: "nav-link")
                     <> ("href" =: "#")
                     ) $ text "Home"
          elClass "span" "sr-only" $ text "Home"
        elClass "li" "nav-item" $
          elAttr "a" ( ("class" =: "nav-link")
                     <> ("href" =: "#")
                     ) $ text "Features"
        elClass "li" "nav-item" $
          elAttr "a" ( ("class" =: "nav-link")
                     <> ("href" =: "#myAlert")
                     <> ("data-toggle" =: "collapse")
                     ) $ text "Wow"
  where
    navBarC :: T.Text
    navBarC = "navbar navbar-inverse fixed-top navbar-toggleable-sm bg-primary mb-3"
    btnAttrs :: Map.Map T.Text T.Text
    btnAttrs =
      ("class" =: "navbar-toggler navbar-toggler-right") <>
      ("type" =: "button") <>
      ("data-toggle"=: "collapse") <>
      ("data-target"=: "#collapsingNavbar")
    divAttrs :: Map.Map T.Text T.Text
    divAttrs =
      ("class" =: "collapse navbar-collapse") <>
      ("id" =: "collapsingNavbar")


data MenuItem =
  Overview
  | SubItem1
  | SubItem2
  | Analytics
  | Export
  deriving (Eq,Show)


menuLink :: DomBuilder t m => T.Text -> m (Link t)
menuLink s = do
  (l,_) <- elAttr' "a" (("class" =: "nav-link") <> ("href" =: "#") ) $ text s
  return $ Link $ domEvent Click l

-- We use menuLink so that we can handle link click -events.
-- We return the dynamic MenuItem user selected (there has to be a default
-- MenuItem (it is Overview in this example).
-- The MenuItem is delevered to the uiPanel: this way the sideBar is used
-- to select the contents in the middle.
--
-- Note that the col-md's and col-lg's sum up to 12 to get the full width
-- correct (at sideBar and uiPanel).
sideBar :: MonadWidget t m => m (Dynamic t MenuItem)
sideBar = do
  elAttr "div" (("class" =: "col-md-3 col-lg-2 sidebar-offcanvas")
                <> ("id" =: "sidebar")
                <> ("role" =: "navigation")) $ do
    dm <- elClass "ul" "nav flex-column pl-1" $ do
      meO <- menuLink "Overview"
      (meS1, meS2)
        <- elClass "li" "nav-item" $ do
            elAttr "a" (("class" =: "nav-link")
                       <> ("href" =: "#submenu1")
                       <> ("data-toggle" =: "collapse")
                       <> ("data-target" =: "#submenu1")) $ text "Reports ▾"
            (meS1, meS2)
              <- elAttr "ul" (("class" =: "list-unstyled flex-column pl-3 collapse")
                        <> ("id" =: "submenu1")
                        <> ("aria-expanded" =: "false")) $ do
                meS1 <- menuLink "Sub item 1"
                meS2 <- menuLink "Sub item 2"
                return (meS1, meS2)
            return (meS1, meS2)
      meA <- menuLink "Analytics"
      meE <- menuLink "Export"
      dynLC <- holdDyn Overview $
        leftmost
          [ Overview <$ _link_clicked meO
          , SubItem1 <$ _link_clicked meS1
          , SubItem2 <$ _link_clicked meS2
          , Analytics <$ _link_clicked meA
          , Export <$ _link_clicked meE
          ]
      return dynLC
    return dm

-- Make some content based on the MenuItem into the middle part.
uiPanel :: MonadWidget t m => Dynamic t MenuItem -> m ()
uiPanel dmi = do
  divClass "col-md-9 col-lg-10" $ do
    elClass "h1" "hidden-xs-down" $ text "Some text"
    el "br" blank
    let dt = fmap (T.pack . show) dmi
    dynText dt


