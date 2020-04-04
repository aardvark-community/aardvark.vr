namespace Aardvark.Vr
//
//module private DemoApp = 
//
//    open System
//    open Aardvark.Base
//    open Aardvark.Base.Ag
//    open Aardvark.Base.Incremental
//    open Aardvark.Base.Rendering
//    open Aardvark.SceneGraph
//    open Aardvark.SceneGraph.Semantics
//    open Aardvark.Rendering.Text
//
//    open Aardvark.Vr
//
//    module TestApp =
//        open FShade
//
//        type Message =
//            | VrMessage of VrMessage
//
//        type Model =
//            {
//                log : plist<string>
//                value : int
//            }
//
//        type MModel(m : Model) =
//            let _current = AVal.init m
//            let _value = AVal.init m.value
//            let _log = Aardvark.Base.Incremental.MList<string>(m.log)
//
//            member x.Update(m : Model) =
//                _current.Value <- m
//                _value.Value <- m.value
//                _log.Update(m.log)
//            
//            member x.log = _log :> alist<_>
//            member x.value = _value :> aval<_>
//            member x.Current = _current :> aval<_>
//
//
//            static member Create(m : Model) = MModel(m)
//            static member Update(mm : MModel, m : Model) = mm.Update m
//
//        let hsv2rgb (h : float) (s : float) (v : float) =
//            let s = clamp 0.0 1.0 s
//            let v = clamp 0.0 1.0 v
//
//            let h = h % 1.0
//            let h = if h < 0.0 then h + 1.0 else h
//            let hi = floor ( h * 6.0 ) |> int
//            let f = h * 6.0 - float hi
//            let p = v * (1.0 - s)
//            let q = v * (1.0 - s * f)
//            let t = v * (1.0 - s * ( 1.0 - f ))
//            match hi with
//                | 1 -> V4d(q,v,p, 0.3)
//                | 2 -> V4d(p,v,t, 0.3)
//                | 3 -> V4d(p,q,v, 0.3)
//                | 4 -> V4d(t,p,v, 0.3)
//                | 5 -> V4d(v,p,q, 0.3)
//                | _ -> V4d(v,t,p, 0.3)
//
//        let view (m : MModel) (state : MVrState) (runtime : IRuntime) (uniforms : IUniformProvider) (stencil : StencilMode) =
//
//            let wrap (sg : ISg) =
//                let sg = 
//                    Sg.UniformApplicator(uniforms, sg)
//                        |> Sg.stencilMode (Mod.constant stencil)
//                sg?Runtime <- runtime
//                sg
//
//    //
//    //        let allText =
//    //            m.log |> AList.toMod |> Mod.map (fun lines -> String.concat "\r\n" lines)
//    //
//    //        Sg.text
//
//            let font = Font("Consolas")
//            state.runtime.PrepareGlyphs(font, [0..127] |> Seq.map char)
//            state.runtime.PrepareTextShaders(font, state.signature)
//
//        
//            let controllers = 
//                state.devices |> AMap.toASet |> ASet.choose (fun (id, d) ->
//                    let rot = d.axis.[1].value |> Mod.map (fun v -> Trafo3d.Scale(v.X + 1.0))
//                    //let color = d.axis.[0].value |> Mod.map (fun v -> hsv2rgb v.X 1.0 1.0)
//                    //let scale = d.axis.[0].value |> Mod.map (fun v -> Trafo3d.Scale(v.Y * 0.5 + 1.0))
//
//                    let color = d.buttons.[2].pressed |> Mod.map (function true -> V4d(1.0, 0.0, 0.0, 0.2) | false -> V4d.Zero)
//
//
//                    let overlay = 
//                        let text = d.axis.[0].touched |> Mod.bind (function true -> state.fps  |> Mod.map (sprintf "%.2ffps") | false -> Mod.constant "fps")
//                        let shape = text |> Mod.map (fun text -> Text.Layout(font, C4b.White, TextAlignment.Center, Box2d(-10.0, 0.0, 10.0, 1.0), text))
//                
//                        Sg.shape shape 
//                            |> Sg.scale 0.04
//                            |> Sg.translate 0.0 0.0 0.02
//                            |> Sg.onOff d.axis.[0].touched
//
//                    if d.kind = VrDeviceKind.Controller then
//                        //Sg.box color (Mod.constant (Box3d(-V3d.III * 0.03, V3d.III * 0.03)))
//                        d.Model |> Option.map (fun sg ->
//                            sg
//                            |> Sg.uniform "OverlayColor" color
//                            |> Sg.diffuseTexture DefaultTextures.checkerboard
//                            //|> Sg.trafo scale
//                            |> Sg.trafo rot
//
//                            |> Sg.andAlso overlay
//
//                            |> Sg.trafo d.pose.deviceToWorld
//                            |> Sg.onOff d.pose.isValid
//                        )
//                    else
//                        None
//                )
//                |> Sg.set
//                |> Sg.shader {
//                    do! DefaultSurfaces.trafo
//                    do! DefaultSurfaces.diffuseTexture
//                    do! fun (v : Effects.Vertex) -> fragment { let c : V4d = uniform?OverlayColor in return V4d(v.c.XYZ * (1.0 - c.W) + c.XYZ * c.W, 1.0) }
//                    do! DefaultSurfaces.simpleLighting
//                }
//
//        
//            Sg.box' C4b.Red (Box3d(V3d(-1.0, -1.0, 0.0), V3d(1.0, 1.0, 0.2)))
//                |> Sg.translate 0.0 0.0 0.72
//                |> Sg.diffuseTexture DefaultTextures.checkerboard
//                |> Sg.andAlso controllers
//                |> Sg.shader {
//                    do! DefaultSurfaces.trafo
//                    do! DefaultSurfaces.diffuseTexture
//                    do! DefaultSurfaces.simpleLighting
//                }
//                |> wrap
//                |> Semantic.renderObjects
//                |> RuntimeCommand.Render
//
//        let update (m : Model) (s : VrState) (msg : Message) =
//            m
//    //        let (VrMessage msg) = msg
//    //        let str = 
//    //            match msg with
//    //                | VrMessage.ControllerConnected id -> sprintf "connected %d" id
//    //                | VrMessage.ControllerDisconnected id -> sprintf "disconnected %d" id
//    //                | VrMessage.Touch (c,a) -> sprintf "touch %d %d" c a
//    //                | VrMessage.Untouch (c,a) -> sprintf "untouch %d %d" c a
//    //                | VrMessage.Press (c,a) -> sprintf "press %d %d" c a
//    //                | VrMessage.Unpress (c,a) -> sprintf "unpress %d %d" c a
//    //                | VrMessage.ValueChange(c,a,v) -> sprintf "value %d %d = %A" c a v
//    //                | VrMessage.PressButton (c,b) -> 
//    //                    if b = 2 then s.devices.[c].startVibrate (MicroTime(TimeSpan.FromDays 10.0))
//    //                    sprintf "press button %d %d" c b
//    //
//    //                | VrMessage.UnpressButton (c,b) ->
//    //                    if b = 2 then s.devices.[c].stopVibrate ()
//    //                    sprintf "unpress button %d %d" c b
//    //
//    //                | UpdatePose (c, p) ->
//    //                    let pos = p.deviceToWorld.Forward.TransformPos V3d.Zero
//    //                    sprintf "pose %d %A" c pos
//    //
//    //        Log.warn "%s" str
//    //        { m with log = PList.append str m.log }
//
//        let app =
//            {
//                initial = { value = 0; log = PList.empty }
//                update = update
//                view = view
//                input = fun m -> [VrMessage m]
//                unpersist = Unpersist.instance<Model, MModel>
//            }
//
//
//    //[<EntryPoint;STAThread>]
//    let main argv = 
//        Ag.initialize()
//        Aardvark.Init()
//
//        VrApp.run 8 false TestApp.app
//        0
