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

    type Line = {
        Index : int
        Content : string
    }

    type FileContent = {
        FilePath : string
        Extension : Extension
        Lines : Line list
    }

    let fileContentAsync extention path = async {
        let! read = Async.AwaitTask (File.ReadAllLinesAsync(path))
        return {
            FilePath = path
            Extension = extention
            Lines = read |> Seq.mapi (fun i x -> { Index = i; Content = x }) |> List.ofSeq 
        }
    }   

    type KeyPosition = {
        LineIndex : int
        StartIndex : int
    }

    let findIndecesOfKey (line : Line) (key : string) : KeyPosition list =
        let rec findNextIndex (startIndex : int) matches =
            match line.Content.IndexOf(key, startIndex ) with
            | -1 -> matches
            | i -> { LineIndex = line.Index; StartIndex = i } :: (findNextIndex (i + 1) matches)
        findNextIndex 0 []        

    type SearchResult = { 
        FileName : string 
        KeyWord : string
        LineIndex : int
        StartIndex : int}

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
        | CSharp ->  fileContent.Lines |> List.map (fun x -> x.Content) |> cSharpFileContainsResource
        | Xaml ->  fileContent.Lines |> List.map (fun x -> x.Content) |> xamlFileContainsResource

    let checkFileForKeys (keys : string list) (file : FileContent) : SearchResult list =

        let searchResultsForKey key =
            file.Lines
            |> Seq.collect (fun c -> findIndecesOfKey c key)
            |> List.ofSeq
            |> List.map (fun i -> { FileName = file.FilePath; KeyWord = key; LineIndex = i.LineIndex; StartIndex = i.StartIndex })

        keys |> List.collect searchResultsForKey