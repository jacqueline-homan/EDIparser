(*
open FParsec
open System
open System.IO
open EDIParser.SyntaxP

//Preemptive avoiding of value restriction errors
type UserState = unit

type Parser<'t> = Parser<'t, UserState>

//Taking care of string constant
//type StringConstant = StringConstant of string * string
//EDI to F# data types for the ISAs
type ISA = 
    | ISA of string

type ISAsubField = 
    { I_02ToI_01 : ISA
      I_04ToI_03 : ISA
      I_05ToI_15 : ISA }

let test (p : Parser<_, _>) str = 
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str s = pstring s
// Handling whitespaces
let ws = spaces
let str_ws s = pstring s .>> ws

//To genericize and keep compiler from complaining about F# value restrictions
[<GeneralizableValue>]
let h<'T, 'u> = pstring "test"

run h "test" |> printfn "%A"

let stringLiteral<'T, 'u> = 
    let escape = 
        anyOf "\"\\/bfnrt" |>> function 
        | 'b' -> "\b"
        | 'f' -> "\u000C"
        | 'n' -> "\n"
        | 'r' -> "\r"
        | 't' -> "\t"
        | c -> string c // every other char is mapped to itself
    
    let unicodeEscape = 
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 -> 
                        (hex2int h3) * 4096 + (hex2int h2) * 256 
                        + (hex2int h1) * 16 + hex2int h0
                        |> char
                        |> string)
    
    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')
    between (str "\"") (str "\"") 
        (stringsSepBy normalCharSnippet escapedCharSnippet)

let isaString<'T, 'u> = stringLiteral |>> ISA
let listBetweenStrings sOpen sClose pElement f = 
    between (str sOpen) (str sClose) 
        (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
let path = @"Sample.EDI"
let file = File.ReadAllText(path)


[<EntryPoint>]
let main argv = 
    run stringLiteral file |> printfn "%A"
    0 //integer exit code

*)