module Tests

open System
open Xunit
open FsUnit.Xunit

let hexToByte = function
    | '1' -> 1uy
    | '2' -> 2uy
    | '3' -> 3uy
    | '4' -> 4uy
    | '5' -> 5uy
    | '6' -> 6uy
    | '7' -> 7uy
    | '8' -> 8uy
    | '9' -> 9uy
    | 'A' -> 10uy
    | 'B' -> 11uy
    | 'C' -> 12uy
    | 'D' -> 13uy
    | 'E' -> 14uy
    | 'F' -> 15uy

let fillEmptyBytesWithZeroes = function
    | [b1;b2] -> [b1;b2;0uy]
    | [b1] -> [b1;0uy;0uy]
    | threeFullBytes -> threeFullBytes

let appendBytes (bytes: byte list) =
    let integers =  bytes |> List.map int
    integers.[0] <<< 16 + integers.[1] <<< 8 + integers.[2]

let hexToBase64 hex =
    //let threeByteIntegers = 
        hex 
        |> List.map hexToByte
        |> List.chunkBySize 3
        |> List.map fillEmptyBytesWithZeroes
        |> List.map appendBytes
    


[<Fact>]
let ``Hex to Base 64`` () =
   "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
   |> hexToBase64
   |> should equal "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
