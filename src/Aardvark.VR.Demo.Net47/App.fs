namespace Demo

open Aardvark.Base
open Aardvark.Rendering.Text
open Aardvark.Vr
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Generic
open Aardvark.Application.OpenVR

open FSharp.Data.Adaptive
open Aardvark.Cef

type Message =
    | SetText of string 
    | ToggleVR

module Shader =
    open Aardvark.Base.Rendering
    open FShade

    let browserSampler =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinMagPoint
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    let fullScreen (v : Effects.Vertex) =
        fragment {
            let coord = V2d(0.5 + 0.5 * v.pos.X, 0.5 - 0.5 * v.pos.Y)
            let pixel = V2d uniform.ViewportSize * coord |> V2i
            let textureSize = browserSampler.Size

            if pixel.X < textureSize.X && pixel.Y < textureSize.Y then
                let color = browserSampler.[pixel]
                return color
            else
                return V4d(0.0,0.0,0.0,0.0)
        }

module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    
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

    let vr (runtime : IRuntime) (info : VrSystemInfo) (m : AdaptiveModel) =



    
        let client = new Browser(null,AVal.constant System.DateTime.Now,runtime, true, AVal.constant (V2i(1024,1024)))
        let res = client.LoadUrl "https://www.youtube.com/watch?v=SG-JqnKxzxI&list=RDSG-JqnKxzxI&start_radio=1"
        printfn "%A" res

        let test =
            adaptive {
                let! (size,version) = AVal.map2 (fun a b -> a,b) client.Size client.Version
                let vp = client.GetViewport "newsLogo"
                do
                    match vp with
                        | Some vp ->
                            let bounds = Box2d(V2d vp.Min / V2d size, V2d vp.Max / V2d size)

                            let visible = bounds.Intersection(Box2d.Unit)
                            if not visible.IsEmpty then
                                Log.line "visible: %A" visible
                        | None ->
                            ()

                return size
            }

        let sg =
            Sg.fullScreenQuad
                |> Sg.noEvents
                |> Sg.diffuseTexture client.Texture 
                |> Sg.shader {
                     do! DefaultSurfaces.trafo
                     do! DefaultSurfaces.diffuseTexture
                   }

    
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

        let s = 
            Sg.textWithConfig TextConfig.Default m.text
            |> Sg.noEvents
            |> Sg.andAlso deviceSgs

        Sg.andAlso s sg

        
    let pause (info : VrSystemInfo) (m : AdaptiveModel) =
        Sg.box' C4b.Red Box3d.Unit
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

    let app runtime =
        {
            unpersist = Unpersist.instance
            initial = initial
            update = update
            threads = threads
            input = input
            ui = ui
            vr = vr runtime
            pauseScene = Some pause
        }
