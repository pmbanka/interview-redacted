open System
open System.IO
open System.Collections.Generic

type MagazineName = string
type MaterialName = string
type MaterialId = string
type MaterialCount = int

type InputRecord = {
    MaterialName : MaterialName
    MaterialId : MaterialId
    Magazines : {| MagazineName : MagazineName; Count : MaterialCount |} list
}

type OutputRecord = {
    Materials: SortedDictionary<MaterialId,MaterialCount>
    MagazineName: MagazineName
    TotalCount: MaterialCount 
}

module Input = 

    let getFrom (i:TextReader) =
        Seq.initInfinite (fun _ -> i.ReadLine())
        |> Seq.takeWhile (not << isNull)

    let getFakeReader () =
        let input = """# Material inventory initial state as of Jan 01 2018
# New materials
Cherry Hardwood Arched Door - PS;COM-100001;WH-A,5|WH-B,10
Maple Dovetail Drawerbox;COM-124047;WH-A,15
Generic Wire Pull;COM-123906c;WH-A,10|WH-B,6|WH-C,2
Yankee Hardware 110 Deg. Hinge;COM-123908;WH-A,10|WH-B,11
# Existing materials, restocked
Hdw Accuride CB0115-CASSRC - Locking Handle Kit - Black;CB0115-CASSRC;WH-C,13|WH-B,5
Veneer - Charter Industries - 3M Adhesive Backed - Cherry 10mm - Paper Back;3M-Cherry-10mm;WH-A,10|WH-B,1
Veneer - Cherry Rotary 1 FSC;COM-123823;WH-C,10
MDF, CARB2, 1 1/8";COM-101734;WH-C,8
"""
        new StringReader(input)

    let toStructured (input:string seq) =
        input 
        |> Seq.where (fun l -> not (l.StartsWith '#'))
        |> Seq.map (fun l -> 
            let splitted = l.Split ';'  
            // we assume that the input rows are well formatted
            // so we use unsafe access to array elements
            let magazines = 
                splitted.[2].Split ('|', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun pair -> let s = pair.Split(',') in {| MagazineName = s.[0]; Count = int s.[1] |})
                |> List.ofArray
            { MaterialName = splitted.[0]; MaterialId = splitted.[1]; Magazines = magazines })

module Output =

    let writeTo (target: TextWriter) (records:OutputRecord list) =
        for record in records do
            target.WriteLine(sprintf "%s (total %d)" record.MagazineName record.TotalCount)
            for KeyValue(name, count) in record.Materials do
                target.WriteLine(sprintf "%s: %d" name count)
            target.WriteLine()
        target.Flush()

module Processing =

    let reshapeData (input:InputRecord seq) =
        // we first combine data per magazines
        let shape1 = Dictionary<MagazineName, SortedDictionary<MaterialId,MaterialCount>>()
        for inputRecord in input do
            for magazine in inputRecord.Magazines do
                match shape1.TryGetValue magazine.MagazineName with
                | true, materials -> 
                    materials.[inputRecord.MaterialId] <-
                        match materials.TryGetValue inputRecord.MaterialId with
                        | true, count -> count + magazine.Count
                        | _ -> magazine.Count
                | _  -> 
                    shape1.[magazine.MagazineName] <- SortedDictionary(dict [inputRecord.MaterialId, magazine.Count])
        // and then sort the magazines according to spec while keeping total count
        let shape2 = 
            [| 
                for KeyValue(magazineName, materials) in shape1 do
                   { MagazineName = magazineName
                     TotalCount = materials |> Seq.sumBy (fun x -> x.Value)
                     Materials = materials }
            |] 
            |> Array.sortByDescending(fun entry -> entry.TotalCount, entry.MagazineName)
            |> List.ofArray
        shape2

[<EntryPoint>]
let main argv =
    Input.getFrom Console.In // replace Console.In with (Input.getFakeReader ()) for quick testing
    |> Input.toStructured
    |> Processing.reshapeData
    |> Output.writeTo Console.Out
    0