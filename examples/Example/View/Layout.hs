{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Example.View.Layout where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Version (showVersion)
import Example.AppRoute
import Example.Colors (AppColor (..))
import Example.Style qualified as Style
import Example.View.Icon as Icon (hamburger)
import Paths_examples (version)
import Text.Casing (fromHumps, toWords)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

-- where
--  routeSource :: AppRoute -> Path
--  routeSource = \case
--    Simple -> "Example/Page/Simple.hs"
--    Contacts ContactsAll -> "Example/Page/Contacts.hs"
--    Contacts (Contact _) -> "Example/Page/Contact.hs"
--    Counter -> "Example/Page/Counter.hs"
--    Transitions -> "Example/Page/Transitions.hs"
--    FormSimple -> "Example/Page/FormSimple.hs"
--    FormValidation -> "Example/Page/FormValidation.hs"
--    Sessions -> "Example/Page/Sessions.hs"
--    LazyLoading -> "Example/Page/LazyLoading.hs"
--    Concurrent -> "Example/Page/Concurrent.hs"
--    Redirects -> "Example/Page/Redirects.hs"
--    Requests -> "Example/Page/Requests.hs"
--    Filter -> "Example/Page/Filter.hs"
--    Autocomplete -> "Example/Page/Autocomplete.hs"
--    Errors -> "Example/Page/Errors.hs"
--    RedirectNow -> "Main.hs"
--    Query -> "Main.hs"
--    Hello _ -> "Main.hs"
--    Main -> "Main.hs"
--    Examples -> "Example/View/Layout.hs"
--    Todos -> "Example/Page/Todo.hs"
--    DataTable -> "Example/Page/DataTable.hs"
--    Javascript -> "Example/Page/Javascript.hs"
--    ExternalCSS -> "Example/Page/ExternalCSS.hs"

exampleLayout :: AppRoute -> View c () -> View c ()
exampleLayout rt content =
  el ~ grow $ do
    navigation rt ~ position Fixed . onDesktop leftMenu . onMobile topMenu
    col ~ pad 25 . gap 30 . onDesktop horizontal . onMobile vertical $ do
      content
 where
  leftMenu = width menuWidth . left 0 . top 0 . bottom 0
  horizontal = margin (L menuWidth)
  vertical = margin (T menuHeight)

  topMenu = top 0 . right 0 . left 0

  menuWidth = 230
  menuHeight = 70

sourceLink :: Path -> View c ()
sourceLink p =
  link sourceUrl "View Source" ~ Style.link
 where
  sourceUrlBase = [uri|https://github.com/seanhess/hyperbole/blob/0.4/example/|]
  sourceUrl = sourceUrlBase ./. p

embed :: (Styleable h) => CSS h -> CSS h
embed =
  pad 20 . gap 10 . bg White . flexCol

example :: Text -> Path -> View c () -> View c ()
example t p cnt =
  col ~ gap 15 $ do
    row $ do
      el ~ bold . fontSize 24 $ text t
      space
      sourceLink p
    cnt

exampleMenu :: AppRoute -> View c ()
exampleMenu current = do
  exampleLink Intro
  exampleLink Requests
  exampleLink Concurrency
  exampleLink AtomicCSS
  exampleLink (State StateRoot)
  case current of
    State _ -> do
      exampleLink (State Effects) ~ sub
      exampleLink (State Query) ~ sub
      exampleLink (State Sessions) ~ sub
    _ -> none
  exampleLink (Data DataLists)
  case current of
    Data _ -> do
      exampleLink (Data SortableTable) ~ sub
      exampleLink (Data Autocomplete) ~ sub
      exampleLink (Data Filter) ~ sub
    _ -> none
  exampleLink Forms
  exampleLink Todos
  exampleLink (Contacts ContactsAll)
  exampleLink Javascript
 where
  -- example Errors

  -- link "/query?key=value" lnk "Query Params"
  sub = pad (TRBL 10 10 10 40)

  exampleLink rt =
    route rt ~ menuItem . selected rt $
      text $
        routeTitle rt
  menuItem =
    pad (XY 20 10) . color White . hover (bg DarkHighlight)
  selected rt =
    if rt == current then bg DarkHighlight else id

routeTitle :: AppRoute -> Text
routeTitle (Hello _) = "Hello World"
routeTitle (Contacts ContactsAll) = "Contacts (Advanced)"
routeTitle (State Effects) = "Effects"
routeTitle (State StateRoot) = "State"
routeTitle (State Query) = "Query"
routeTitle (State Sessions) = "Sessions"
routeTitle (Data DataLists) = "Data Lists"
routeTitle (Data SortableTable) = "Sortable Table"
routeTitle (Data Filter) = "Filters"
routeTitle (Data Autocomplete) = "Autocomplete"
routeTitle Todos = "TodoMVC"
routeTitle r = cs $ toWords $ fromHumps $ show r

pageDescription :: AppRoute -> View c ()
pageDescription = \case
  -- Simple -> do
  --   el "HyperViews update independently. In this example, two Message HyperViews are embedded into the same page with different ids."
  --   el "Try inspecting the page in the Chrome dev tools and watching both the DOM and messages"
  Todos ->
    row ~ (gap 5) $ do
      el "Implementation of "
      link [uri|https://todomvc.com/|] "TodoMVC" ~ Style.link
  Contacts _ -> do
    el "This complex example combines various features"
  _ -> none

navigation :: AppRoute -> View c ()
navigation rt = do
  nav ~ bg Dark . color White . flexCol . showMenuHover $ do
    row $ do
      link [uri|https://github.com/seanhess/hyperbole|] "HYPERBOLE" ~ bold . pad 20 . logo . width 220
      space
      menuButton
    col ~ cls "menu" . onMobile (display None) $ do
      exampleMenu rt
      space
      el ~ pad 10 . fontSize 12 $ do
        text "v"
        text $ cs $ showVersion version
 where
  menuButton =
    el ~ onDesktop (display None) . onMobile flexCol $ do
      el ~ pad 6 $ do
        el Icon.hamburger ~ color White . width 50 . height 50

  showMenuHover =
    css
      "show-menu"
      ".show-menu:hover > .menu"
      [ "display" :. "flex"
      ]

  -- https://www.fontspace.com/super-brigade-font-f96444
  logo =
    utility
      "logo"
      [ "background" :. "no-repeat center/90% url(/logo-robot.png)"
      , "color" :. "transparent"
      ]

onMobile :: (Styleable c) => (CSS c -> CSS c) -> CSS c -> CSS c
onMobile = media (MaxWidth 650)

onDesktop :: (Styleable c) => (CSS c -> CSS c) -> CSS c -> CSS c
onDesktop = media (MinWidth 650)
