module App.Text

open Browser.Navigator

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

let userLanguage =
    let lang = unwrapOr (navigator.language) "en-US"
    match Seq.head (lang.Split '_') with
    | "en" -> "en"
    | "de" -> "de"
    | _ -> "en"

let english =
    Map.empty.
        Add("CLICK_TO_START", "Click me")

let german =
    Map.empty.
        Add("CLICK_TO_START", "Klick mich")

let getKey language key =
    match language with
    | "de" -> german.Item key
    | _ -> english.Item key
