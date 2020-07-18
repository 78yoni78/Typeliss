// Learn more about F# at http://fsharp.org

open System.Threading.Tasks
open System.Text.RegularExpressions

open Typeliss
open Typeliss.Lib
open Parsing
open Discord
open DiscordLib


let codeRegex = Regex ("^`([^`]+)`$")

// [<EntryPoint>]
// let main _ =
//     printfn "DEBUG"

//     let context = startCon
//     let code = System.Console.ReadLine()

//     match tree code with
//     | Ok tree -> 
//         let value = eval context tree
//         printfn "%O\n\nResult: %O" tree value
//     | Error e ->
//         printfn "%O" e
    
//     ignore (System.Console.ReadKey())
//     0

let runCode context code = 
    async {
        return 
            match tree code with
            | Ok tree -> 
                let value = eval context tree
                string value
            | Error s ->
                sprintf "```%s```" s
    }

[<EntryPoint>]
let main _ =
    connection(fun discord -> async {
        let definitions = ResizeArray()
        let discordContext e = { Conn = discord; E = e; Definitions = definitions }
        
        addNonBlocking discord.add_MessageCreated (fun e -> async {
            let matchs = codeRegex.Match(e.Message.Content, 0) 
            if matchs.Success then
                let code = matchs.Groups.[1].Value

                let discordContext = discordContext e
                let context = 
                    startCon
                    |> addVars (vars discordContext)
                    |> addVars (definitions |> Map.ofSeq)

                let! resp = runCode context code

                do! e.Message.RespondAsync resp |> Async.AwaitTask |> Async.Ignore
        })
        
        do! Task.Delay -1 |> Async.AwaitTask
    }) |> Async.RunSynchronously
    0

// [<EntryPoint>]
// let main _ =
//     let mutable context = startCon

//     Discord.replyingBot (fun msg (write, read) ->
//         async {
//             let matchs = codeRegex.Match(msg, 0) 
//             if matchs.Success then
//                 let code = matchs.Groups.[1].Value
//                 match tree code with
//                 | Ok tree -> 
//                     Async.Start (async {
//                         let defineBuffer = ResizeArray()
//                         let mutable clear = false
//                         let context' = myAddVars (write, read, defineBuffer.Add, fun () -> (clear <- true)) context
//                         let value = eval context' tree
//                         do! write (string value)
//                         if clear then context <- startCon
//                         context <- addVars context (Map.ofSeq defineBuffer)
//                     })
//                 | Error s ->
//                     do! write (sprintf "```%s```" s)
//             return ()
//         }) |> Async.RunSynchronously
//     0

// [<EntryPoint>]
// let main _ = 
//     let m = Exporting.getModule<DiscordLib.ReflectionHelper>
//     let e = Exporting.exported m
//     printfn "%A" (List.ofSeq e)
//     ignore (System.Console.ReadKey false)
//     0
