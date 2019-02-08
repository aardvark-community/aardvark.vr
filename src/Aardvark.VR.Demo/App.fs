namespace Demo

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.Text
open Aardvark.Vr
open Aardvark.UI
open Aardvark.UI.Generic

type Message =
    | SetText of string 
    | StartVR
    | StopVR

module Demo =
    
    let initial = 
        {
            text = "some text"
        }
        
        
    let update (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        match msg with
        | SetText t -> 
            { model with text = t }
        | StartVR ->
            vr.start()
            model
        | StopVR ->
            vr.stop()
            model

    let threads (model : Model) =
        ThreadPool.empty
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(_,1) ->
            [StopVR]
        | _ -> 
            []

    let ui (m : MModel) =
        div [ style "width: 100%; height: 100%" ] [
            textarea [ onChange SetText ] m.text
        ]

    let vr (info : VrSystemInfo) (m : MModel) =
        Sg.textWithConfig TextConfig.Default m.text
        |> Sg.noEvents

    let app =
        {
            unpersist = Unpersist.instance
            initial = initial
            update = update
            threads = threads
            input = input
            ui = ui
            vr = vr
        }
