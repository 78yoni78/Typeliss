module DiscordLib

// open System
// open System.Threading.Tasks

open DSharpPlus
open DSharpPlus.EventArgs
open DSharpPlus.Entities
open Typeliss
open Typeliss.Lib
open Discord
open Exporting

type ReflectionHelper() = inherit ReflectionHelperBase() 


type DiscordContext = { Conn: DiscordClient; E: MessageCreateEventArgs; Definitions: ResizeArray<string * Value> }

let reply (m: DiscordMessage) str = 
    match str with 
    | "" -> None
    | _ -> 
        m.RespondAsync str |> Async.AwaitTask |> Async.Ignore |> Async.RunSynchronously
        Some ()

let [<Export>] print ({E = e}: DiscordContext) =
    value { let! o = anyVal in return reply e.Message (string o) }

//let [<Export>] input ({Conn = discord; E = e}: DiscordContext as con) =
//    Function (fun value ->
//        apply (print con) value |> ignore
//        let e2 = 
//            waitEventOnce discord.add_MessageCreated discord.remove_MessageCreated
//            <| (fun e2 -> e2.Channel = e.Channel && e2.Author = e.Author)
//            |> Async.RunSynchronously
//        StringValue e2.Message.Content)

//let [<Export>] help (_: DiscordContext) =
//    Function (fun _ -> StringValue (IO.read "readme.txt"))

let [<Export>] read (_: DiscordContext) =
    value { let! () = monoVal in return IO.read "readwrite.txt" }

let [<Export>] write (_: DiscordContext) =
    value { let! str = stringVal in IO.write "readwrite.txt" str }

let [<Export>] define ({Definitions = defines}: DiscordContext) =
    value { let! name = undefinedErrorVal in let! value = anyVal in defines.Add (name, value) }

let [<Export>] undefine ({Definitions = defines}: DiscordContext) = 
    value { let! name = stringVal in defines.RemoveAll(fun (name',_) -> name' = name) |> ignore }

let [<Export>] ``undefine-all`` ({Definitions = defines}: DiscordContext) =
    value { let! () = monoVal in defines.Clear() }

let [<Export>] load (_: DiscordContext) =
    value { let! () = monoVal in return! IO.deserialize "saveload"}

let [<Export>] save (_: DiscordContext) =
    value { let! value = anyVal in IO.serialize "saveload" value }

let alternativeNames = Map.ofList [
    "print", ["הדפס"; "פלט"; "output"]
    "input", ["קלט"]
    "help", ["עזרה"]
    "read", ["קרא"]
    "write", ["כתוב"]
    "define", ["הגדר"; "יהי"]
    "load", ["טען"]
    "save", ["שמור"]
]

let vars (con: DiscordContext) = 
    addAlternatives
        (makeVars [con] (getModule<ReflectionHelper>))
        alternativeNames 