/// Uses reflection to take functions and declarations from f#
/// modules.
module Exporting

open System
open System.Reflection


/// An f# module
type Module = Module of Type

/// Used to take the type of f# modules
type ReflectionHelperBase() = class end
/// Makes a Module object. 
/// 
/// 'ReflectionHelper should be a type defined in the module
/// like this: type ReflectionHelper() = inherit ReflectionHelperBase()
let getModule<'ReflectionHelper when 'ReflectionHelper :> ReflectionHelperBase> = 
    typeof<'ReflectionHelper>.DeclaringType |> Module

/// Marks a module function or module variable to be exported by 
/// the function 'exported'
type ExportAttribute(name: string option) = 
    inherit Attribute()
    member _.Name = name
    new () = ExportAttribute(None: string option)
    new (name) = ExportAttribute(Some name: string option)

/// An exported function or variable
type Exported = Exported of string * Type [] * Type * (obj array -> obj)

let private isExported (typeMember: MemberInfo) = 
    Attribute.IsDefined(typeMember, typeof<ExportAttribute>)

let private getExportedName (x: MemberInfo) = 
    x.GetCustomAttribute<ExportAttribute>().Name 
    |> Option.defaultValue x.Name

let private exportedField (field: FieldInfo): Exported = 
    let value = field.GetValue null
    Exported (getExportedName field, [||], field.FieldType, function [||] -> value | _ -> failwithf "No arguments to this value")

let private exportedProperty (property: PropertyInfo): Exported = 
    let value = property.GetValue null
    Exported (getExportedName property, [||], property.PropertyType, function [||] -> value | _ ->  failwithf "No arguments to this value")

let private exportedMethod (method: MethodInfo): Exported = 
    let value args = method.Invoke (null, args)
    Exported (getExportedName method, [|for p in method.GetParameters() -> p.ParameterType|], method.ReturnType, value)

/// Returns a sequence of exported values and functions marked with ExportAttribute
let exported (Module moduleType) = 
    Seq.collect id [
        (moduleType.GetFields ()        |> Seq.filter isExported    |> Seq.map exportedField)
        (moduleType.GetProperties ()    |> Seq.filter isExported    |> Seq.map exportedProperty)
        (moduleType.GetMethods ()       |> Seq.filter isExported    |> Seq.map exportedMethod)
    ]