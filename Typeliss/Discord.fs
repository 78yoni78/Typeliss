module Discord

open System.Threading.Tasks

open DSharpPlus


let connection(cont: DiscordClient -> Async<unit>) = 
    async {
        let conf = DiscordConfiguration ()
        conf.set_AutoReconnect true
        conf.set_UseInternalLogHandler true
        conf.set_LogLevel LogLevel.Debug
        conf.set_Token "NzMwMzExNTg0Mjk5MjIxMDM1.XwVqng.l6AtVg3o6nFlh-jpCVv55SvlUHo"
        conf.set_TokenType TokenType.Bot
        use discord = new DiscordClient(conf) 
        do! discord.ConnectAsync() |> Async.AwaitTask
        do! cont discord
    }

let addBlocking adder (asyncOperation: 'args -> Async<unit>): unit = 
    adder (AsyncEventHandler<_>(fun args -> asyncOperation args |> Async.StartAsTask :> Task))

let addNonBlocking adder (asyncOperation: 'args -> Async<unit>) = 
    addBlocking adder (fun args -> async { Async.Start (asyncOperation args) })

let waitEventOnce add remove predicate: Async<'args> =
    async {
        let event = Event<_> ()
        let rec handler = 
            AsyncEventHandler<'args>(fun args -> 
                async {
                    if predicate args then
                        remove handler
                        event.Trigger args
                } |> Async.StartAsTask :> Task)
        add handler
        return! Async.AwaitEvent event.Publish
    }

// let replyingBot (cont: string->_->Async<unit>): Async<unit> = 
//     async {
//         let conf = DiscordConfiguration ()
//         conf.set_AutoReconnect true
//         conf.set_UseInternalLogHandler true
//         conf.set_LogLevel LogLevel.Debug
//         conf.set_Token "NzMwMzExNTg0Mjk5MjIxMDM1.XwVqng.l6AtVg3o6nFlh-jpCVv55SvlUHo"
//         conf.set_TokenType TokenType.Bot
//         use discord = new DiscordClient(conf) 
//         discord.add_MessageCreated(fun (e:EventArgs.MessageCreateEventArgs) ->
//             let write message = 
//                 async { 
//                     let! _ = e.Message.RespondAsync message |> Async.AwaitTask 
//                     return () 
//                 }
//             let read (): Async<string> = 
//                 async {
//                     let inputEvent = Event<_>()
//                     let rec handler =
//                         //  Handler that removes itself and calls inputEvent 
//                         AsyncEventHandler<EventArgs.MessageCreateEventArgs>(fun e ->
//                             async {
//                                 discord.remove_MessageCreated handler
//                                 inputEvent.Trigger e
//                                 return ()
//                             } |> Async.StartAsTask :> Task)
//                     //  Add the inputEvent handler
//                     discord.add_MessageCreated handler
//                     //  Called when the handler is called by the bot
//                     let! e = Async.AwaitEvent inputEvent.Publish
//                     return e.Message.Content
//                 }
//             let msg = e.Message.Content
//             cont msg (write, read) |> Async.StartAsTask :> Task)
//         do! discord.ConnectAsync() |> Async.AwaitTask
//         do! Task.Delay(-1) |> Async.AwaitTask
//         return ()
//     }
