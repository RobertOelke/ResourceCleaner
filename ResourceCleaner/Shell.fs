namespace ResourceCleaner

module Shell =
    
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    type State = {
        SomeContent : string
    }

    let initState = { SomeContent = "Content" }

    let init () =
        initState, Cmd.none

    type Msg = 
    | Reset

    let update (msg: Msg) (state: State) : (State*Cmd<Msg>) =
        match msg with
        | Reset -> initState, Cmd.none
    
    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [
                    TextBlock.text state.SomeContent
                ]
            ]
        ]