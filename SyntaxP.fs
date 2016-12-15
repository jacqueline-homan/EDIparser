//
//  SyntaxP.fs
//
//  Author:
//       evan <>
//
//  Copyright (c) 2016 evan
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
module EDIParser.SyntaxP

open FParsec

// Parse a record like:
//
//     ISA*00*          *00*          *ZZ*MGCTLYST       *02*BLNJ           *160930*1453*U*00401*000000001*0*P*:
//
// Into a record type tag and then a sequence of fields
type Parser<'t> = Parser<'t, unit>

// The field separator
let pFSep : Parser<_> = skipChar '*'

// The record delimiter
let pRSep : Parser<_> = skipChar '~'

// The Authorization Qualifier
type AuthQual = AQNone

// We only have one of these anyway
let pAuthQual : Parser<AuthQual> = stringReturn "00" AQNone .>> pFSep

type AuthInfo = 
    | AuthInfo of string

let pAuthInfo : Parser<AuthInfo> = anyString 10 |>> AuthInfo .>> pFSep

// The Authorization Qualifier and Info
type Auth = 
    { authQual : AuthQual
      authInfo : AuthInfo }

let pAuth = 
    pipe2 pAuthQual pAuthInfo (fun q i -> 
        { authQual = q
          authInfo = i })

type ISA = 
    | ISA of Auth

let pISARec : Parser<_> = skipString "ISA" >>. pFSep
let pISA : Parser<ISA> = pISARec >>. pAuth |>> ISA



let test p str = 
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
