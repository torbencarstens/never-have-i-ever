module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Card
open Elmish
open Elmish.React
open Browser
open Fable.Core
open Fable.React
open Fable.React.Props
open Player
open Thoth.Fetch
open Thoth.Json
open System
open System.Text.RegularExpressions

// MODEL

type Settings =
    { MinimumSips: int
      MaximumSips: int
      Remote: bool }

type RoundInformation =
    { CardsToPlay: int
      InitialPlayerIndex: int }

type Model =
    { Players: Player.Type list
      ActiveCards: (Card.Type * Player.Type option) list
      CurrentCard: Card.Type option
      Cards: Card.Type list
      CurrentPlayer: Player.Type option
      Counter: int
      DisplayPlayerNameDuplicateError: bool
      InitialLoad: bool
      Settings: Settings
      Round: int
      RoundInformation: RoundInformation }

type Msg =
    | InitialLoad
    | ChangeActiveCard
    | ChangeActivePlayer
    | IncrementCounter
    | AddCards of Card.RawType list
    | AddPlayer of Player.Type
    | RemovePlayer of Player.Type
    | TogglePlayerActivity of Player.Type
    | DisplayPlayerNameDuplicate
    | HidePlayerNameDuplicate
    | SaveSettings
    | ChangeRemoteSetting
    | Reset
    | AdvanceTurn
    | AdvanceRound
    | PlayAudio

let getCards dispatch =
    promise {
        let url = "https://raw.githubusercontent.com/torbencarstens/never-have-i-ever-cards/master/tasks.json"
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
    { Players = List.empty
      ActiveCards = List.empty
      CurrentCard = None
      Cards = List.empty
      CurrentPlayer = None
      Counter = 0
      DisplayPlayerNameDuplicateError = false
      InitialLoad = true
      Settings =
          { MinimumSips = 2
            MaximumSips = 10
            Remote = true }
      Round = 0
      RoundInformation =
          { CardsToPlay = 0
            InitialPlayerIndex = -1 } }, Cmd.Empty

let unwrapMapOrDefault (opt: 'b option) (m: 'b -> 't) (def: 't) =
    if opt.IsSome then m opt.Value else def

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

let rec findNextActivePlayer (playerList: Player.Type list) model =
    if playerList.Length = 0 then
        None
    else if playerList.Length = 1 then
        Some playerList.Head
    else
        match model.CurrentPlayer with
        | Some player ->
            match List.tryFindIndex ((=) player) playerList with
            | Some index ->
                let player = playerList.Item((index + 1) % playerList.Length)
                match player.Active with
                | false -> findNextActivePlayer (List.filter ((<>) player) playerList) model
                | true -> Some player
            | None -> Some model.Players.Head
        | None -> Some model.Players.Head

// UPDATE

let int_replacement_regex = new Regex("{int(:?:(\d+)-(\d+))?}")

let filterCardsForTurn cards model =
    let distinctCount = (Card.getDistinctCount cards)

    List.filter (fun card ->
        card.Count > 0 && if distinctCount > 1 // TODO: this should be checked after everything else
                       then card.Id <> (unwrapMapOrDefault model.CurrentCard (fun c -> c.Id) -1)
                       else true) cards

let getNextCard cards model =
    let cards = filterCardsForTurn cards model
    if cards.Length = 0 then
        None
    else
        let card = cards.Item(System.Random().Next() % cards.Length)

        let min = model.Settings.MinimumSips
        let max = model.Settings.MaximumSips

        let replacement_text =
            (Seq.map (fun w ->
                let m = int_replacement_regex.Match w
                match m.Success with
                | true -> (sprintf "%d" ((System.Random().Next()) % (max - min + 1) + min))
                | false -> w) (card.Text.Split ' '))
            |> String.concat " "
        Some { card with Text = replacement_text }

let explodeCards cards =
    (List.map (fun card -> ([ card ] |> Seq.collect (fun c -> List.replicate c.Count { c with Count = 1 }))) cards)
    |> Seq.reduce Seq.append
    |> List.ofSeq

let roundHasEnded model =
    model.RoundInformation.CardsToPlay <= 0 && model.Players.Length > 0

let getPlayerByIndex index (players: Player.Type list): Player.Type option =
    try
        Some(players.Item index)
    with _ -> None

let update (msg: Msg) (model: Model) =
    match msg with
    | InitialLoad ->
        { model with InitialLoad = false }, Cmd.ofSub (fun dispatch -> getCards dispatch |> Promise.start)
    | AdvanceTurn ->
        model,
        Cmd.ofSub (fun dispatch ->
            do dispatch IncrementCounter
               dispatch ChangeActivePlayer
               dispatch ChangeActiveCard
               dispatch PlayAudio
               if roundHasEnded model then dispatch AdvanceRound)
    | PlayAudio ->
        let audioId =
            (if roundHasEnded model then "nextround-audio" else "nextcard-audio")

        stop "nextcard-audio"
        stop "nextround-audio"
        play audioId

        model, Cmd.Empty
    | ChangeActiveCard ->
        let playerCards = Player.filterActiveCards (model.CurrentPlayer.Value) (model.ActiveCards)

        let cards =
            if model.CurrentPlayer.IsSome
            then List.filter (fun card -> not (List.exists (fun c -> c = card) playerCards)) model.Cards
            else model.Cards

        let card = getNextCard cards model

        let model =
            { model with
                  CurrentCard = card
                  Cards = Card.decreaseCount card model.Cards
                  RoundInformation =
                      { model.RoundInformation with CardsToPlay = max (model.RoundInformation.CardsToPlay - 1) 0 } }

        model, Cmd.Empty
    | ChangeActivePlayer ->
        let nextPlayer =
            findNextActivePlayer
                ((List.filter (fun p -> p.Active || (Player.compareOption (model.CurrentPlayer) (Some p)))
                      model.Players)) model

        (if model.Counter <> 0 then
            { model with
                  CurrentPlayer = nextPlayer
                  Players =
                      List.map (fun player ->
                          if (Player.compareOption (Some player) nextPlayer)
                          then { player with CardsPlayed = player.CardsPlayed + 1 }
                          else player) model.Players }
         else
             model), Cmd.Empty
    | IncrementCounter ->
        { model with Counter = model.Counter + 1 }, Cmd.Empty
    | AddCards cards ->
        { model with Cards = explodeCards (List.map Card.Into cards) }, Cmd.Empty
    | AddPlayer player ->
        { model with
              Players = model.Players @ [ player ]
              RoundInformation =
                  { model.RoundInformation with
                        CardsToPlay =
                            model.RoundInformation.CardsToPlay + 1 } },
        match model.CurrentPlayer with
        | Some _ -> Cmd.Empty
        | None -> Cmd.ofSub (fun dispatch -> dispatch ChangeActivePlayer)
    | RemovePlayer player ->
        let players = (List.filter (fun p -> p <> player) model.Players)
        let activeCards =
            List.filter (fun (card, p) -> not (Player.compareOption p (Some player))) model.ActiveCards

        { model with
              Players = players
              ActiveCards = activeCards
              RoundInformation = { model.RoundInformation with CardsToPlay = model.RoundInformation.CardsToPlay - 1 } },
        (if (Player.isCurrent player model.CurrentPlayer)
         then Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
         else Cmd.Empty)
    | TogglePlayerActivity player ->

        { model with
              Players =
                  (List.map (fun p ->
                      if p = player then { p with Active = not p.Active } else p) model.Players) },
        (if (Player.isCurrent player model.CurrentPlayer)
         then Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
         else Cmd.Empty)
    | DisplayPlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = true }, Cmd.Empty
    | HidePlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = false }, Cmd.Empty
    | ChangeRemoteSetting ->
        let remote =
            ((Browser.Dom.window.document.getElementById "remote") :?> Browser.Types.HTMLInputElement).``checked``

        { model with Settings = { model.Settings with Remote = remote } }, Cmd.Empty
    | SaveSettings ->
        let min =
            (match ((Browser.Dom.window.document.getElementById "minimum-sips") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.MinimumSips
             | value -> value |> int)

        let max =
            (match ((Browser.Dom.window.document.getElementById "maximum-sips") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.MaximumSips
             | value -> value |> int)

        { model with
              Settings =
                  { model.Settings with
                        MinimumSips = min
                        MaximumSips = max } }, Cmd.Empty
    | Reset -> init ()
    | AdvanceRound ->
        (if model.Players.Length > 0 then
            { model with
                  Round = model.Round + 1
                  RoundInformation =
                      { CardsToPlay = (Player.getActive model.Players).Length - 1
                        InitialPlayerIndex = (unwrapOr (Player.getIndex model.CurrentPlayer model.Players) 0) - 1 } }
         else
             model), Cmd.Empty

// VIEW (rendered with React)

let settings model dispatch =
    div
        [ ClassName "modal fade"
          Id "settings"
          TabIndex -1
          Role "dialog" ]
        [ div
            [ ClassName "modal-dialog"
              Role "document" ]
              [ div [ ClassName "modal-content" ]
                    [ div [ ClassName "modal-body" ]
                          [ div [ ClassName "form-group container" ]
                                [ div [ ClassName "row" ]
                                      [ label
                                          [ For "minimum-sips"
                                            ClassName "col" ] [ str "Minimum sips" ]
                                        input
                                            [ Name "minimum-sips"
                                              ClassName "m-1 w-100 col"
                                              Id "minimum-sips"
                                              Placeholder(sprintf "%d" (model.Settings.MinimumSips))
                                              MaxLength 2.
                                              InputType "text"
                                              Pattern "\d{1,2}" ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "maximum-sips"
                                            ClassName "col" ] [ str "Maximum sips" ]
                                        input
                                            [ Name "maximum-sips"
                                              ClassName "m-1 w-100 col"
                                              Id "maximum-sips"
                                              Placeholder(sprintf "%d" (model.Settings.MaximumSips))
                                              MaxLength 2.
                                              InputType "text"
                                              Pattern "\d{1,2}" ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "remote"
                                            ClassName "col" ] [ str "Remote" ]
                                        input
                                            [ Name "remote"
                                              OnClick(fun _ -> dispatch ChangeRemoteSetting)
                                              InputType "checkbox"
                                              ClassName "m-1 w-100 col"
                                              Id "remote"
                                              Checked(model.Settings.Remote) ] ] ] ]
                      div [ ClassName "modal-footer" ]
                          [ span [ ClassName "text-secondary" ] [ str "{{TAG}}" ]
                            button
                                [ ClassName "btn btn-primary"
                                  DataDismiss "modal"
                                  OnClick(fun _ -> dispatch SaveSettings) ] [ str "Save" ] ] ] ] ]

let addPlayer name model dispatch =
    match List.tryFind ((=) (Player.create name)) model.Players with
    | Some _ -> false
    | None ->
        dispatch (AddPlayer(Player.create name))
        HidePlayerNameDuplicate |> ignore
        true

let addPlayerFunction model dispatch =
    match ((Browser.Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value with
    | "" -> ()
    | value ->
        (match (addPlayer value model dispatch) with
         | true ->
             ((Browser.Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value <- ""
         | false ->
             DisplayPlayerNameDuplicate
             |> dispatch)

let displayPlayer player model dispatch =
    div
        [ ClassName
            ("flex-column mr-2 card " + (if model.CurrentPlayer.IsSome && model.CurrentPlayer.Value.Name = player.Name
                                         then "bg-primary"
                                         else if not player.Active
                                         then "border-warning"
                                         else "")) ]
        [ div [ ClassName "card-body" ]
              [ h5 [ ClassName "card-title text-center" ] [ str (sprintf "%s (%d)" player.Name player.CardsPlayed) ]
                div [ ClassName "d-flex justify-content-between" ]
                    [ button
                        [ ClassName "card btn btn-secondary toggle-button"
                          OnClick(fun _ -> TogglePlayerActivity player |> dispatch) ]
                          [ str (if player.Active then "Suspend" else "Unsuspend") ]
                      button
                          [ ClassName "card btn btn-secondary delete-button"
                            OnClick(fun _ -> RemovePlayer player |> dispatch) ] [ str "Delete" ] ] ] ]

let sidebar (model: Model) dispatch =
    div [ ClassName "col-md-2 sidebar col h-100" ]
        [ div [ ClassName "form-group" ]
              [ input
                  [ Name "add-player-field"
                    ClassName "form-control m-1 w-100"
                    Id "add-player-field"
                    OnKeyDown(fun x ->
                        if x.keyCode = 13. then (addPlayerFunction model dispatch))
                    MaxLength 20. ]
                button
                    [ ClassName "btn btn-primary m-1 w-100"
                      OnClick(fun _ -> addPlayerFunction model dispatch) ] [ str "Add player" ] ]
          hr []
          div
              [ ClassName "flex-row mb-4"
                Style
                    [ OverflowY "scroll"
                      Height "85%" ] ] (List.map (fun p -> displayPlayer p model dispatch) model.Players) ]

let displayCurrentCard model dispatch =
    div
        [ ClassName "card d-flex col"
          Id "active-card" ]
        [ div [ ClassName "card-body flex-wrap" ]
              [ button
                  [ OnClick(fun _ -> dispatch AdvanceTurn)
                    ClassName "card-body card-title btn btn-dark w-100"
                    Style [ Height "95%" ]
                    Id "current-card-body"
                    Disabled(model.CurrentCard.IsNone && model.Counter > 0) ]
                    [ span [ ClassName "h3" ]
                          [ str
                              (match model.CurrentCard with
                               | Some (card) -> card.Text
                               | None ->
                                   if model.Counter = 0 then "Click to start" else "No cards left") ] ] ] ]

let displayInformationHeader model dispatch =
    div
        [ Id "active-player-header"
          ClassName "text-center col text-truncate h3" ]
        [ span []
              [ str
                  (match model.CurrentPlayer with
                   | Some player -> player.Name
                   | None -> "No active player") ]
          span [] [ str " | " ]
          span [ Title "Number of cards played so far" ] [ str (sprintf "Cards played %d" model.Counter) ]
          span [] [ str (sprintf " | Round %d" model.Round) ]
          div [ ClassName "progress" ]
              [ div
                  [ ClassName "progress-bar"
                    Role "progressbar"
                    Style([ Width(sprintf "%d%%" ((model.Counter * 100 / model.Cards.Length))) ])
                    AriaValueNow(sprintf "%d" model.Counter)
                    AriaValueMin "0"
                    AriaValueMax(sprintf "%d" model.Cards.Length) ] [] ] ]

let view (model: Model) dispatch =
    div
        [ Ref(fun element ->
            if not (isNull element) then
                if model.InitialLoad then dispatch InitialLoad)
          ClassName "container-fluid h-100" ]
        [ div [ ClassName "row m-4" ]
              [ figure []
                    [ audio
                        [ Id "nextcard-audio"
                          Src "/nextcard.mp3" ] [] ]
                figure []
                    [ audio
                        [ Id "nextround-audio"
                          Src "/nextround.mp3" ] [] ]
                div [ ClassName "col-1" ]
                    [ button
                        [ ClassName "btn btn-primary m-1"
                          DataToggle "modal"
                          DataTarget "#settings" ] [ str "Settings" ]
                      button
                          [ ClassName "btn btn-primary ml-1"
                            OnClick(fun _ -> dispatch Reset) ] [ str "Reset" ] ]
                (displayInformationHeader model dispatch)
                span [ ClassName "text-secondary" ] [ str "Contact: saufautomat@carstens.tech" ] ]
          div
              [ ClassName "row m-2"
                Style [ Height "65%" ] ]
              [ (displayCurrentCard model dispatch)
                (sidebar model dispatch) ]
          (settings model dispatch) ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
