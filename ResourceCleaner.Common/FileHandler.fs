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

    type FileContent = {
        FilePath : string
        Extension : Extension
        Lines : string list
    }

    let fileContent extention path = {
        FilePath = path
        Extension = extention
        Lines = File.ReadAllLines(path) |> List.ofSeq
    }   

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

    let cSharpFileContainsResource (fileContent : string list) =
        fileContent
        |> Seq.takeWhile (fun s -> s.StartsWith("using"))
        |> Seq.exists (fun s -> s.Contains(".Properties"))

    let xamlFileContainsResource (fileContent : string list) =
        fileContent
        |> Seq.map (fun x -> x.Trim())
        |> Seq.exists (fun x -> x.StartsWith("xmlns:") && x.Contains(".Properties"))

    let isRelevantFileForSearch (fileContent : FileContent) =
        match fileContent.Extension with
        | CSharp -> cSharpFileContainsResource fileContent.Lines
        | Xaml -> xamlFileContainsResource fileContent.Lines

    let checkFileForKeys (keys : string list) (file : FileContent) : SearchResult list =
        keys 
        |> List.collect 
            (fun key ->
                file.Lines
                |> Seq.collect (fun c -> findIndecesOfKey c key)
                |> List.ofSeq
                |> List.map (fun i -> { FileName = file.FilePath; KeyWord = key; StartingIndex = i }))