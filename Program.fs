open FParsec
open System
open System.IO
open EDIParser.SyntaxP



[<EntryPoint>]
let main argv = 
    test pISA "ISA*00*          *" |> printfn "%A"
    0 //integer exit code
