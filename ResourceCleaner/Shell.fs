namespace ResourceCleaner


module Shell =
    
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Components
    open Avalonia.Layout
    
    type ResourceEntry = {
        Key : string
        Value : string option
    }

    type State = {
        SelectedFile : string option
        SelectedDirectory : string option
        Resources : ResourceEntry list
    }

    let initState = {
        SelectedFile = None
        SelectedDirectory = None
        Resources = []
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
        let content = Common.ResourceParser.ResourceFile.Load fileName
        content.Datas 
        |> Seq.map (fun x -> { Key = x.Name; Value = x.Value })
        |> List.ofSeq

    let update (msg: Msg) (state: State) : (State*Cmd<Msg>) =
        match msg with
        |SelectFile ->
            let dialog = createFileDialog ()
            let handleResult x = x :> seq<string> |> List.ofSeq |> FilesSelected 

            let cmd = Cmd.OfTask.perform dialog.ShowAsync Shared.mainWindow handleResult

            state, cmd

        | FilesSelected lst ->
            match lst with
            | h::_ -> { state with SelectedFile = Some h; Resources = parseContent h }, Cmd.none
            | _ -> state, Cmd.none

        | SelectDirectory ->
            let dialog = createFolderDialog ()
            let handleResult x = x |> DirectorySelected

            let cmd = Cmd.OfTask.perform dialog.ShowAsync Shared.mainWindow handleResult

            state, cmd

        | DirectorySelected path ->
            { state with SelectedDirectory = Some path}, Cmd.none

        | Reset -> initState, Cmd.none
        | ProcessResources -> state, Cmd.none
    
    let createHeader row (state : State) dispatch =
        let createFilePathElement row col text =
            TextBlock.create [
                Grid.row row
                Grid.column col
                TextBlock.text text]

        DockPanel.create [
            Grid.row row
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
                    Button.content "Start"
                    Button.onClick (fun _ -> dispatch ProcessResources)
                ]
            ]
        ]

    let createListBox row state dispatch =
        ListBox.create [
            Grid.row row
            ListBox.dataItems state.Resources

            ListBox.itemTemplate (DataTemplateView<ResourceEntry>.create(fun res ->
                Grid.create [
                    Grid.columnDefinitions "1*,2*"
                    Grid.children [
                        TextBlock.create [
                            Grid.column 0
                            TextBlock.text res.Key
                        ]
                        TextBlock.create [
                            yield Grid.column 1
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
            Grid.rowDefinitions "auto,1*"
            Grid.columnDefinitions "1*"
        
            Grid.children [
                createHeader 0 state dispatch
                createListBox 1 state dispatch
            ]
        ]