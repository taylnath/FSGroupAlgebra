// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Microsoft.FSharp.Reflection

let toString (x:'a) =
    let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
    case.Name

// parse a string into a Cmd with optional argument
let fromString<'a> (s:string) =
    let name = (s.Split [|' '|]).[0]
    let arg = String.concat " " (s.Split [|' '|]).[1..]
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = name) with
    | [|case|] -> Some(FSharpValue.MakeUnion(case, [| if arg <> "" then box arg|]) :?> 'a)
    | _ -> None

// modulus helper
let inline (%!) a b = (a % b + b) % b

type Symbol = char * int

type Expression = Symbol list

let CharToDigit = Globalization.CharUnicodeInfo.GetDigitValue

let rec ParseSymbols (chars: char[]) : Expression option =
     let nonSpaceChars = chars |> Array.filter (fun elem -> elem <> ' ')
     match Array.toList nonSpaceChars with
     | [] -> Some []
     | x::xs when not (Char.IsLetter x) -> None
     | [x] -> Some [x,1]
     | [x;y] when Char.IsLetter y -> Some [x,1;y,1]
     | x::y::z::zs when y = '^' && Char.IsDigit z ->
         zs
         |> List.toArray
         |> ParseSymbols
         |> Option.defaultValue []
         |> List.append [x, CharToDigit z]
         |> Some
     | x::y::z::zs ->
         y::z::zs
         |> List.toArray
         |> ParseSymbols
         |> Option.defaultValue []
         |> List.append [x,1]
         |> Some
     | _ -> None

type Cmd =
    | Calculate of string
    | Subgroups
    | List
    | Quit
    with
    override this.ToString() = toString this
    static member fromString s = fromString<Cmd> s
    
let Evaluate (expr:Expression option) =
    let valCounts = Map<char, int>[]
    expr
    |> Option.map (fun e ->
        e
        |> List.fold (fun (valCounts:Map<char,int>) (x:Symbol) ->
            if valCounts.ContainsKey (fst x)
            then valCounts.Change(fst x, Option.map (fun x -> x + 1))
            else valCounts.Add(fst x, snd x)
            )
            Map.empty
        |> Map.fold (fun (outExpr:Expression) (key:char) (num:int) -> outExpr @ [key, num]) []
        )

let PrintExpression (expr: Expression option) =
    expr
    |> Option.map (fun e ->
        e
        |> List.iter (fun x -> Console.Write $"{fst x}^{snd x}")
        )
    |> ignore
    Console.WriteLine ""
    
let DisplayExpression (s:string) =
    s.ToCharArray()
    |> ParseSymbols
    |> Evaluate
    |> PrintExpression
    
let CalculateLetterAsModuloN (n:int) (input:string) =
    let mutable throwaway = 0
    input.ToCharArray()
    |> Array.filter (fun elem -> elem <> ' ')
    |> Array.fold (fun acc elem ->
            if elem = 'a'
            then Option.map (fun x -> x + 1) acc
            elif elem = '^' || Int32.TryParse(new string [|elem|], &throwaway)
            then acc
            else None)
        (Some 0)
    |> Option.map (fun x -> x %! n)
    |> Option.map (fun x ->
        match x with
        | 0 -> "1"
        | 1 -> "a"
        | x when x = n - 1 -> "a^{-1}"
        | x -> $"a^{x}"
        )
    |> (fun x ->
        match x with
        | Some y -> Console.WriteLine y
        | None -> Console.WriteLine "Unexpected character(s) in expression."
        )
 
let rec CalculationLoop n =
    Console.WriteLine $"Computing in cyclic group C{n} with generator 'a'.\nType an expression to evaluate. Type 'Quit' to quit, or 'List' to list all commands."
    let input = Console.ReadLine()
    let mutable continueMarker = true
    match Cmd.fromString input with
    | Some Subgroups -> Console.WriteLine "Subgroups found."
    | Some (Calculate s) ->
        Console.WriteLine $"Calculating {s}..."
        let b = ParseSymbols (s.ToCharArray())
        DisplayExpression s
        //CalculateLetterAsModuloN n s
    | Some List ->
        Console.WriteLine "*****\nCommands:"
        Array.iter (fun (x:UnionCaseInfo) -> Console.WriteLine x.Name) (FSharpType.GetUnionCases typeof<Cmd>)
        Console.WriteLine "*****"
    | Some Quit ->
        Console.WriteLine "Thanks for computing! Goodbye!"
        continueMarker <- false
    | _ -> Console.WriteLine "Command not understood."
    
    if continueMarker then CalculationLoop n

[<EntryPoint>]
let main argv =
    let b = FSharpValue.GetUnionFields (Calculate "hi", typeof<Cmd>)
    let c = FSharpValue.GetUnionFields (Subgroups, typeof<Cmd>)
    let d = FSharpType.GetUnionCases typeof<Cmd>
    Console.WriteLine "Enter a modulus"
    let n = Console.ReadLine() |> Int32.Parse
    CalculationLoop n
    0 // return an integer exit code