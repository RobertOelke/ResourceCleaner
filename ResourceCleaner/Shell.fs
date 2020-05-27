namespace ResourceCleaner


module Shell =
    
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Components
    open ResourceCleaner.Common
    open FSharp.Collections.ParallelSeq
    
    type ResourceEntry = {
        Key : string
        Value : string option
        ReferenceCount : int
    }

    type ProcessingState = 
    | Loading
    | Ready

    type State = {
        ProcessingState : ProcessingState
        SelectedFile : string option        
        SelectedDirectory : string option
        Resources : ResourceEntry list
        SourceResourceFile : ResourceParser.Root option
    }

    let initState = {
        ProcessingState = Ready
        SelectedFile = None
        SelectedDirectory = None
        Resources = []
        SourceResourceFile = None
    }

    let init () =
        initState, Cmd.none

    type Msg = 
    | SelectFile
    | FilesSelected of string list
    | SelectDirectory
    | DirectorySelected of string
    | Reset
    | ProcessResources
    | ResourcesProcessed of ResourceEntry list
    | RemoveUnusedResources

    let createFileDialog () =
        let dialog = Avalonia.Controls.OpenFileDialog()
        dialog.AllowMultiple <- false

        let filter = new FileDialogFilter()
        filter.Name <- "Resource"
        let extentions = new System.Collections.Generic.List<string>()
        extentions.Add("resx")
        filter.Extensions <- extentions

        let filters = new System.Collections.Generic.List<FileDialogFilter>()
        filters.Add(filter)

        dialog.Filters <- filters
        dialog

    let createFolderDialog () =
        Avalonia.Controls.OpenFolderDialog()

    let parseContent (fileName : string) =
        Common.ResourceParser.ResourceFile.Load fileName

    let parseSourceFile (content : ResourceParser.Root) =
        content.Datas 
        |> Seq.map (fun x -> { Key = x.Name; Value = x.Value; ReferenceCount = 0 })
        |> List.ofSeq

    let update (msg: Msg) (state: State) : (State*Cmd<Msg>) =
        match msg with
        | SelectFile ->
            let dialog = createFileDialog ()
            let handleResult x = x :> seq<string> |> List.ofSeq |> FilesSelected 

            let cmd = Cmd.OfTask.perform dialog.ShowAsync Shared.mainWindow handleResult

            state, cmd

        | FilesSelected lst ->
            match lst with
            | h::_ ->
                let sourceResource = parseContent h
                { state with
                        SelectedFile = Some h
                        Resources = parseSourceFile sourceResource
                        SourceResourceFile = Some sourceResource 
                }, Cmd.none
            | _ -> state, Cmd.none

        | SelectDirectory ->
            let dialog = createFolderDialog ()
            let handleResult x = x |> DirectorySelected

            let cmd = Cmd.OfTask.perform dialog.ShowAsync Shared.mainWindow handleResult

            state, cmd

        | DirectorySelected path ->
            { state with SelectedDirectory = Some path}, Cmd.none

        | Reset -> initState, Cmd.none
        | ProcessResources ->
            let processFiles () = async {
                let keys = state.Resources |> List.map (fun x -> x.Key)
                
                let! allCSharpFiles =
                    state.SelectedDirectory.Value 
                    |> FileHandler.fileNames FileHandler.Extension.CSharp
                    |> List.where (fun x -> not (x.EndsWith(".Designer.cs")))
                    |> List.map (FileHandler.fileContentAsync FileHandler.Extension.CSharp)
                    |> Async.Parallel

                let cSharpFiles = 
                    allCSharpFiles
                    |> Seq.filter FileHandler.isRelevantFileForSearch
                    |> List.ofSeq

                let! allXamlFiles =
                    state.SelectedDirectory.Value 
                    |> FileHandler.fileNames FileHandler.Extension.Xaml
                    |> List.map (FileHandler.fileContentAsync FileHandler.Extension.Xaml)
                    |> Async.Parallel

                let xamlFiles =
                    allXamlFiles
                    |> Seq.filter FileHandler.isRelevantFileForSearch
                    |> List.ofSeq

                let getResultsForPath = FileHandler.checkFileForKeys keys

                let res = (cSharpFiles@xamlFiles) |> PSeq.map getResultsForPath |> Seq.collect id |> List.ofSeq
                let countKeywords resource = res |> List.sumBy (fun y -> if resource.Key = y.KeyWord then 1 else 0 )

                let newResources = state.Resources |> List.map (fun x -> { x with ReferenceCount = countKeywords x})
                return newResources
            }

            let nextCommand = Cmd.OfAsync.perform processFiles () ResourcesProcessed

            { state with ProcessingState = Loading }, nextCommand

        | ResourcesProcessed resources -> { state with ProcessingState = Ready; Resources = resources }, Cmd.none

        | RemoveUnusedResources ->
            let keysToRemove = state.Resources |> List.filter (fun x -> x.ReferenceCount = 0) |> List.map (fun x -> x.Key)

            let datasToRemove =
                state.SourceResourceFile.Value.Datas
                |> Seq.filter (fun x -> keysToRemove |> List.contains x.Name)
                |> List.ofSeq

            for dataNode in datasToRemove do
                dataNode.XElement.Remove()

            state.SourceResourceFile.Value.XElement.Save(state.SelectedFile.Value)

            initState, Cmd.none
    
    let createHeader row (state : State) dispatch =
        let createFilePathElement row col text =
            TextBlock.create [
                Grid.row row
                Grid.column col
                TextBlock.text text]

        DockPanel.create [
            Grid.row row

            DockPanel.margin 10.

            DockPanel.lastChildFill false
            DockPanel.children [
                
                Grid.create [
                    DockPanel.dock Dock.Left
                    Grid.rowDefinitions "auto,auto"
                    Grid.columnDefinitions "auto,10,auto"

                    Grid.children [
                        createFilePathElement 0 0 "Resource-Datei"
                        createFilePathElement 0 2 (state.SelectedFile |> Option.defaultValue "<empty>")
                        createFilePathElement 1 0 "Ordner"
                        createFilePathElement 1 2 (state.SelectedDirectory |> Option.defaultValue "<empty>")
                    ]
                ]

                Button.create [
                    DockPanel.dock Dock.Right
                    Button.content "Resource-Datei auswählen."
                    Button.onClick (fun _ -> dispatch SelectFile)
                ]
                Button.create [
                    DockPanel.dock Dock.Right
                    Button.content "Ordner auswählen."
                    Button.onClick (fun _ -> dispatch SelectDirectory)
                ]
                Button.create [
                    Button.isEnabled (state.SelectedDirectory.IsSome && state.SelectedFile.IsSome )
                    DockPanel.dock Dock.Right
                    Button.content "Start."
                    Button.onClick (fun _ -> dispatch ProcessResources)
                ]
                Button.create [
                    Button.isEnabled (state.SourceResourceFile.IsSome)
                    DockPanel.dock Dock.Right
                    Button.content "RemoveNodes"
                    Button.onClick (fun _ -> dispatch RemoveUnusedResources)
                ]
            ]
        ]

    let createListBox row state dispatch =
        ListBox.create [
            Grid.row row
            ListBox.dataItems (state.Resources |> Seq.sortBy (fun x -> x.ReferenceCount))

            ListBox.margin 10.

            ListBox.itemTemplate (DataTemplateView<ResourceEntry>.create(fun res ->
                Grid.create [
                    Grid.columnDefinitions "100,1*,2*"
                    Grid.children [
                        TextBlock.create [
                            Grid.column 0
                            TextBlock.text (res.ReferenceCount |> string)
                        ]
                        TextBlock.create [
                            Grid.column 1
                            TextBlock.text res.Key
                        ]
                        TextBlock.create [
                            yield Grid.column 2
                            match res.Value with
                            | Some v -> 
                                yield TextBlock.text v
                            | None -> ()
                        ]
                    ]
                ]))
        ]

    let view (state : State) (dispatch) =
        Grid.create [
            Grid.isEnabled (state.ProcessingState = Ready)
            Grid.rowDefinitions "auto,1*"
            Grid.columnDefinitions "1*"
        
            Grid.children [
                createHeader 0 state dispatch
                createListBox 1 state dispatch
            ]
        ]