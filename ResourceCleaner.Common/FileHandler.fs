namespace ResourceCleaner.Common
module FileHandler =
    open System.IO

    type Extension = 
        | Xaml 
        | CSharp

    let toExtension = 
        function
        | Xaml -> "*.xaml"
        | CSharp -> "*.cs"
        
    let fileNames extension path =
        let fileNames = Directory.GetFiles(path, extension |> toExtension, SearchOption.AllDirectories)
        fileNames |> List.ofSeq

    let relevantFiles path = [
            yield! (fileNames Xaml path)
            yield! (fileNames CSharp path) |> List.where (fun x -> not (x.EndsWith(".Designer.cs")))
        ]

    let fileContent path =
        let rec concatAllLines content list =
            match list with
            | head :: tail -> concatAllLines (content + " " + head) tail
            | _ -> content
            
        File.ReadAllLines(path) |> List.ofSeq |> concatAllLines " "

    let findIndecesOfKey (content : string) (key : string) : int list =
        let rec findNextIndex (startIndex : int) matches =
            match content.IndexOf(key, startIndex ) with
            | -1 -> matches
            | i -> i :: (findNextIndex (i + 1) matches)
        findNextIndex 0 []        

    type SearchResult = { 
        FileName : string 
        KeyWord : string
        StartingIndex : int}
        
    let checkFileForKeys (keys : string list) filePath  =
        let content = filePath |> fileContent
        keys 
        |> List.collect 
            (fun key ->
                findIndecesOfKey content key
                |> List.map (fun i -> { FileName = filePath; KeyWord = key; StartingIndex = i }))