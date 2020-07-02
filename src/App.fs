module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Browser.Navigator
open Card
open Elmish
open Elmish.React
open Fable.Core
open Fable.React
open Fable.React.Props
open Thoth.Fetch
open System.Text.RegularExpressions

// MODEL

type RoundInformation =
    { CardsToPlay: int
      InitialPlayerIndex: int }

type Model =
    { CurrentCard: Card.Type option
      Cards: Card.Type list
      InitialLoad: bool }

type Msg =
    | InitialLoad
    | ChangeActiveCard
    | AddCards of Card.RawType list
    | Reset
    | AdvanceTurn

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

let userLanguage =
    let lang = unwrapOr (navigator.language) "en-US"
    match Seq.head (lang.Split '-') with
    | "en" -> "en"
    | "de" -> "de"
    | _ -> "en"

let getCards dispatch =
    promise {
        let url = sprintf "https://raw.githubusercontent.com/torbencarstens/never-have-i-ever-cards/develop/tasks_%s.json" userLanguage
        let! res = Fetch.get (url)
        AddCards res |> dispatch
    }

let play id =
    ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement).play()

[<Emit("$0.currentTime = $2")>]
let assignCurrentTime element value = jsNative

let stop id =
    ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement).pause()
    assignCurrentTime ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement) "0.0"


type HtmlAttr =
    | [<CompiledName("aria-valuenow")>] AriaValueNow of string
    | [<CompiledName("aria-valuemin")>] AriaValueMin of string
    | [<CompiledName("aria-valuemax")>] AriaValueMax of string
    | [<CompiledName("data-toggle")>] DataToggle of string
    | [<CompiledName("data-target")>] DataTarget of string
    | [<CompiledName("data-dismiss")>] DataDismiss of string
    | [<CompiledName("type")>] InputType of string
    | [<CompiledName("for")>] For of string
    interface IHTMLProp

let init (): Model * Cmd<Msg> =
      { CurrentCard = None
        Cards = List.empty
        InitialLoad = true }, Cmd.Empty

let unwrapMapOrDefault (opt: 'b option) (m: 'b -> 't) (def: 't) =
    if opt.IsSome then m opt.Value else def

// UPDATE

let int_replacement_regex = new Regex("{int(:?:(\d+)-(\d+))?}")

let filterCardsForTurn cards model =
    let distinctCount = (Card.getDistinctCount cards)

    List.filter (fun card ->
        if distinctCount > 1 // TODO: this should be checked after everything else
                       then card.Id <> (unwrapMapOrDefault model.CurrentCard (fun c -> c.Id) -1)
                       else true) cards

let getNextCard cards model =
    let cards = filterCardsForTurn cards model
    if cards.Length = 0 then
        None
    else
        let card = cards.Item(System.Random().Next() % cards.Length)

        Some card

let update (msg: Msg) (model: Model) =
    match msg with
    | InitialLoad ->
        { model with InitialLoad = false }, Cmd.ofSub (fun dispatch -> getCards dispatch |> Promise.start)
    | AdvanceTurn ->
        model,
        Cmd.ofSub (fun dispatch ->
               dispatch ChangeActiveCard)
    | ChangeActiveCard ->
        let card = getNextCard model.Cards model

        let model =
            { model with
                  CurrentCard = card
                  Cards = List.filter (fun c -> card.IsNone || card.Value <> c) model.Cards }

        model, Cmd.Empty
    | AddCards cards ->
        { model with Cards = List.map Card.Into cards }, Cmd.Empty
    | Reset -> init ()

// VIEW (rendered with React)

let displayCurrentCard model dispatch =
    div
        [ ClassName "card d-flex col"
          Id "active-card" ]
        [ div [ ClassName "card-body flex-wrap" ]
              [ button
                  [ OnClick(fun _ -> dispatch AdvanceTurn)
                    ClassName "card-body card-title btn btn-dark w-100"
                    Style [ Height "95%" ]
                    Id "current-card-body" ]
                    [ span [ ClassName "h3" ]
                          [ str
                              (match model.CurrentCard with
                               | Some (card) -> card.Text
                               | None -> "Click to start") ] ] ] ]

let view (model: Model) dispatch =
    div
        [ Ref(fun element ->
            if not (isNull element) then
                if model.InitialLoad then dispatch InitialLoad)
          ClassName "container-fluid h-100" ] [
          div
              [ ClassName "row m-2"
                Style [ Height "98%" ] ]
              [ (displayCurrentCard model dispatch) ] ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
