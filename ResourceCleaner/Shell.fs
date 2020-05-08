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
        Resources : ResourceEntry list
    }

    let initState = {
        SelectedFile = None
        Resources = []
    }

    let init () =
        initState, Cmd.none

    type Msg = 
    | SelectFile
    | FilesSelected of string list
    | Reset

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

        | Reset -> initState, Cmd.none
    
    let createHeader row (state : State) dispatch =
        DockPanel.create [
            Grid.row row
            DockPanel.lastChildFill false
            DockPanel.children [
                TextBlock.create [
                    DockPanel.dock Dock.Left
                    TextBlock.text "Resource-Datei"
                ]
                TextBlock.create [
                    DockPanel.dock Dock.Left
                    TextBlock.margin (10., 0.)
                    TextBlock.text (state.SelectedFile |> Option.defaultValue "<empty>")
                ]
                Button.create [
                    DockPanel.dock Dock.Right
                    Button.content "Resource-Datei auswählen."
                    Button.onClick (fun _ -> dispatch SelectFile)
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