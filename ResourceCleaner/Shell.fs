namespace ResourceCleaner

module Shell =
    
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    type State = {
        SomeContent : string
        SelectedFile : string option
    }

    let initState = {
        SomeContent = "Content" 
        SelectedFile = None
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


    let update (msg: Msg) (state: State) : (State*Cmd<Msg>) =
        match msg with
        |SelectFile ->
            let dialog = createFileDialog ()
            let handleResult x = x :> seq<string> |> List.ofSeq |> FilesSelected 

            let cmd = Cmd.OfTask.perform dialog.ShowAsync Shared.mainWindow handleResult

            state, cmd

        | FilesSelected lst ->
            match lst with
            | h::_ -> { state with SelectedFile = Some h }, Cmd.none
            | _ -> state, Cmd.none

        | Reset -> initState, Cmd.none
    
    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [
                    DockPanel.dock Dock.Top
                    TextBlock.text state.SomeContent
                ]

                Button.create [
                    DockPanel.dock Dock.Top
                    Button.content "Resource-Datei auswählen."
                    Button.onClick (fun _ -> dispatch SelectFile)
                ]
                TextBlock.create [
                    DockPanel.dock Dock.Top
                    TextBlock.text (state.SelectedFile |> Option.defaultValue "<empty>")
                ]
            ]
        ]