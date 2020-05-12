open System
open System.Collections.Generic

type MagazineName = string
type MaterialName = string
type MaterialId = string
type MaterialCount = int

type InputRecord = {
    MaterialName : MaterialName
    MaterialId : MaterialId
    Stock : {| MagazineName : MagazineName; Count : MaterialCount |} list
}

[<EntryPoint>]
let main argv =
    let input = """
# Material inventory initial state as of Jan 01 2018
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

    let input = 
        input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        |> Array.where (fun l -> not (l.StartsWith '#'))
        |> Array.map (fun l -> 
            let splitted = l.Split ';'
            let matName = splitted.[0]
            let matId = splitted.[1]
            let stock = 
                splitted.[2].Split ('|', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun pair -> let s = pair.Split(',') in {| MagazineName = s.[0]; Count = int s.[1] |})
                |> List.ofArray
            { MaterialName = matName; MaterialId = matId; Stock = stock })
    
    let step1 = Dictionary<MagazineName, SortedDictionary<MaterialId,MaterialCount>>()
    for inputRecord in input do
        for stockRecord in inputRecord.Stock do
            match step1.TryGetValue stockRecord.MagazineName with
            | true, materials -> 
                materials.[inputRecord.MaterialId] <-
                    match materials.TryGetValue inputRecord.MaterialId with
                    | true, count -> count + stockRecord.Count
                    | false, _ -> stockRecord.Count
            | false, _  -> 
                step1.[stockRecord.MagazineName] <- SortedDictionary(dict [inputRecord.MaterialId,stockRecord.Count])
    let step2 =  
        [| 
            for entry in step1 do
                {|
                    MagazineName = entry.Key
                    TotalCount = entry.Value |> Seq.sumBy (fun x -> x.Value)
                    Entries = entry.Value
                |}
        |] |> Array.sortByDescending(fun entry -> entry.TotalCount, entry.MagazineName)

    for entry in step2 do
        printfn "%s (total %d)" entry.MagazineName entry.TotalCount
        for KeyValue(name, count) in entry.Entries do
            printfn "%s: %d" name count
        printfn ""
    

        // let magazines = 
        //     lines 
        //     |> Array.collect (fun l -> l.Stock |> Array.map (fun s -> s.Magazine))
        //     |> Array.distinct
        
    ()



    printfn "Hello World from F#!"
    0 // return an integer exit code
