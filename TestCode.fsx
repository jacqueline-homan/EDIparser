open System
open System.IO


let splitRecords (s:string) : string [] = s.Split('~')
let joinRecords (records:string [])= String.Join("~", records)

(*This was inside of main below the EntryPoint:

printfn "testInput length in characters: %d" input.Length
let rs = splitRecords input
printfn "Records (%d):\n" rs.Length
let srs = String.concat "\n\t" rs
printfn "%s\n" srs
printfn "%s" "\n"
let nrs = joinRecords rs
if nrs = input then printfn "SUCCESS: Original == rebuilt\n"
else printfn "FAILURE: %s \n\t!= \n%s\n" nrs input
*)
