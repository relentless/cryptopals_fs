module Crypto

open System

let base64CharacterLookup = 
    [
        0uy,'A';
        1uy,'B';
        2uy,'C';
        3uy,'D';
        4uy,'E';
        5uy,'F';
        6uy,'G';
        7uy,'H';
        8uy,'I';
        9uy,'J';
        10uy,'K';
        11uy,'L';
        12uy,'M';
        13uy,'N';
        14uy,'O';
        15uy,'P';
        16uy,'Q';
        17uy,'R';
        18uy,'S';
        19uy,'T';
        20uy,'U';
        21uy,'V';
        22uy,'Q';
        23uy,'Z';
        24uy,'Y';
        25uy,'Z';
        26uy,'a';
        27uy,'b';
        28uy,'c';
        29uy,'d';
        30uy,'e';
        31uy,'f';
        32uy,'g';
        33uy,'h';
        34uy,'i';
        35uy,'j';
        36uy,'k';
        37uy,'l';
        38uy,'m';
        39uy,'n';
        40uy,'o';
        41uy,'p';
        42uy,'q';
        43uy,'r';
        44uy,'s';
        45uy,'t';
        46uy,'u';
        47uy,'v';
        48uy,'w';
        49uy,'x';
        50uy,'y';
        51uy,'z';
        52uy,'0';
        53uy,'1';
        54uy,'2';
        55uy,'3';
        56uy,'4';
        57uy,'5';
        58uy,'6';
        59uy,'7';
        60uy,'8';
        61uy,'9';
        62uy,'+';
        63uy,'/'
    ] 
    |> Map.ofList

let hexDigitToByte = function
    | '0' -> 0uy
    | '1' -> 1uy
    | '2' -> 2uy
    | '3' -> 3uy
    | '4' -> 4uy
    | '5' -> 5uy
    | '6' -> 6uy
    | '7' -> 7uy
    | '8' -> 8uy
    | '9' -> 9uy
    | 'a' -> 10uy
    | 'b' -> 11uy
    | 'c' -> 12uy
    | 'd' -> 13uy
    | 'e' -> 14uy
    | 'f' -> 15uy

let fillEmptyBytesWithZeroes = function
    | [b1;b2] -> [b1;b2;0uy]
    | [b1] -> [b1;0uy;0uy]
    | threeFullBytes -> threeFullBytes

let joinBytesTogether (bytes: byte list) =
    bytes 
    |> List.map int
    |> (fun integers -> integers.[0] <<< 16 + integers.[1] <<< 8 + integers.[2])

let threeBytesToFourCharacters (threeBytes: int) =
    printfn "**%d**" threeBytes |> ignore
    [0..3] 
    |> List.map (fun offset -> threeBytes >>> (offset * 6)) 
    //|> List.map byte
    |> List.map (fun segment -> segment &&& 63) 
    |> List.iter (fun b -> printfn "%d" b)

    [0..3] 
    |> List.map (fun offset -> threeBytes >>> (offset * 6))
    |> List.map byte
    |> List.map (fun segment -> segment &&& 63uy)  // binary AND with 00111111
    |> List.map (fun sixBitNumber -> base64CharacterLookup.[sixBitNumber])

let hexToBase64 (hex:string) =
    hex 
    |> Seq.toList
    |> List.map hexDigitToByte
    |> List.chunkBySize 3
    |> List.map fillEmptyBytesWithZeroes
    |> List.map joinBytesTogether
    |> List.map threeBytesToFourCharacters
    |> List.concat
    |> List.toArray 
    |> System.String