﻿namespace Demo

open Aardvark.Base
open Aardvark.Rendering.Text
open Aardvark.Vr
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Generic
open Aardvark.Application.OpenVR

open FSharp.Data.Adaptive

type Message =
    | SetText of string 
    | ToggleVR
    | UpdatePose

module Demo =
    open Aardvark.Rendering
    
    let show  (att : list<string * AttributeValue<_>>) (sg : ISg<_>) =

        let view (m : AdaptiveCameraControllerState) =
            let frustum = Frustum.perspective 60.0 0.1 1000.0 1.0 |> AVal.constant
            FreeFlyController.controlledControl m id frustum (AttributeMap.ofList att) sg

        let app =
            {
                initial = FreeFlyController.initial
                update = FreeFlyController.update
                view = view
                threads = FreeFlyController.threads
                unpersist = Unpersist.instance
            }

        subApp app

    let initial = 
        {
            text = "some text"
            vr = false
        }
        
        
    let update (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        match msg with
        | UpdatePose -> model
        | SetText t -> 
            { model with text = t }
        | ToggleVR ->
            if model.vr then vr.stop()
            else vr.start()
            { model with vr = not model.vr }

    let threads (model : Model) =
        ThreadPool.empty
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(_,1) ->
            [ToggleVR]
        | VrMessage.UpdatePose(_,_) ->  
            [UpdatePose]
        | _ -> 
            []

    let ui (info : VrSystemInfo) (m : AdaptiveModel) =
        let text = m.vr |> AVal.map (function true -> "Stop VR" | false -> "Start VR")


        let hmd =
            m.vr |> AVal.bind (fun vr ->
                if vr then
                    AVal.map2 (Array.map2 (fun (v : Trafo3d) (p : Trafo3d) -> (v * p).Inverse)) info.render.viewTrafos info.render.projTrafos
                else
                    AVal.constant [|Trafo3d.Translation(100000.0,10000.0,1000.0)|]
            )

        let hmdSg =
            List.init 2 (fun i ->
                Sg.wireBox (AVal.constant C4b.Yellow) (AVal.constant (Box3d(V3d(-1,-1,-1000), V3d(1.0,1.0,-0.9))))
                |> Sg.noEvents
                |> Sg.trafo (hmd |> AVal.map (fun t -> if i < t.Length then t.[i] else Trafo3d.Translation(100000.0,10000.0,1000.0)))
            )
            |> Sg.ofList
            

        let chap =
            match info.bounds with
            | Some bounds ->
                let arr = bounds.EdgeLines |> Seq.toArray
                Sg.lines (AVal.constant C4b.Red) (AVal.constant arr)
                |> Sg.noEvents
                |> Sg.transform (Trafo3d.FromBasis(V3d.IOO, V3d.OOI, -V3d.OIO, V3d.Zero))
            | _ ->
                Sg.empty


        let stuff =
            Sg.ofList [hmdSg; chap]
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
            }

        div [ style "width: 100%; height: 100%" ] [
            show [ style "width: 100%; height: 100%" ] (
                Sg.textWithConfig TextConfig.Default m.text
                |> Sg.noEvents
                |> Sg.andAlso stuff
            )
            textarea [ style "position: fixed; top: 5px; left: 5px"; onChange SetText ] m.text
            button [ style "position: fixed; bottom: 5px; right: 5px"; onClick (fun () -> ToggleVR) ] text


        ]

    let vr (info : VrSystemInfo) (m : AdaptiveModel) =
    
        let deviceSgs = 
            info.state.devices |> AMap.toASet |> ASet.chooseA (fun (_,d) ->
                d.model |> AVal.map (fun m ->
                    match m.Value with
                    | Some sg -> 
                        sg 
                        |> Sg.noEvents 
                        |> Sg.trafo d.pose.deviceToWorld
                        |> Sg.onOff d.pose.isValid
                        |> Some
                    | None -> 
                        None 
                )
            )
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
            }

        Sg.textWithConfig TextConfig.Default m.text
        |> Sg.noEvents
        |> Sg.andAlso deviceSgs

        
    let pause (info : VrSystemInfo) (m : AdaptiveModel) =
        Sg.box' C4b.Red Box3d.Unit
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

    let app =
        {
            unpersist = Unpersist.instance
            initial = initial
            update = update
            threads = threads
            input = input
            ui = ui
            vr = vr
            pauseScene = Some pause
        }
