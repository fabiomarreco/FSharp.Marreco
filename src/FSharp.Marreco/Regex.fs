namespace FSharp.Marreco


module Regex =
    open System.Text.RegularExpressions
    let (|Regex|_|) pattern input = 
        let options = RegexOptions.Singleline
        let m = Regex.Match(input,pattern, options)
        if (m.Success) then [for g in m.Groups -> g.Value] |> List.skip 1 |> Some
        else None


    let (|RegexWithOptions|_|) options pattern input = 
        let m = Regex.Match(input,pattern, options)
        if (m.Success) then [for g in m.Groups -> g.Value] |> List.skip 1 |> Some
        else None


    //open ProviderImplementation
    // open ProviderImplementation.ProvidedTypes
    // open Microsoft.FSharp.Core.CompilerServices
    // open System.Reflection

    // [<TypeProvider>]
    // type BasicProvider (config : TypeProviderConfig) as this =
    //     inherit TypeProviderForNamespaces (config)

    //     let ns = "StaticProperty.Provided"
    //     let asm = Assembly.GetExecutingAssembly()

    //     let createTypes () =
    //         let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
    //         let myProp = ProvidedProperty("MyProperty", typeof<string>, isStatic = true, getterCode = (fun args -> <@@ "Hello world" @@>))
    //         myType.AddMember(myProp)
    //         [myType]

    //     do
    //         this.AddNamespace(ns, createTypes())

    // [<assembly:TypeProviderAssembly>]
    // do ()        