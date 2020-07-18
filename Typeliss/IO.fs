/// Simple IO wrappers to be used by DiscordLib
module IO

open System.IO
open System.Runtime.Serialization.Formatters.Binary


let fullPath path = sprintf "%s/%s" __SOURCE_DIRECTORY__ path


let read path = if File.Exists (fullPath path) then File.ReadAllText (fullPath path) else ""

let write path text = File.WriteAllText (fullPath path, text)


let serialize path object = 
    use file = File.OpenWrite (fullPath path)
    let br = BinaryFormatter()
    br.Serialize(file, object)

let deserialize path: 'a =
    use file = File.OpenRead (fullPath path)
    let br = BinaryFormatter()
    br.Deserialize file |> unbox
