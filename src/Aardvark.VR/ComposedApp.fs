namespace Aardvark.Vr

open System
open Aardvark.Base
open Aardvark.UI
open FSharp.Data.Adaptive


type VrActions =
    {
        kind    : VRDisplayKind
        start   : unit -> unit
        stop    : unit -> unit
    }

module VrActions =
    let nop = { kind = VRDisplayKind.None; start = id; stop = id }

type ComposedApp<'model, 'mmodel, 'msg> =
    {
        unpersist   : Unpersist<'model, 'mmodel>
        initial     : 'model
        update      : VrState -> VrActions -> 'model -> 'msg -> 'model
        threads     : 'model -> ThreadPool<'msg>
        input       : VrMessage -> list<'msg>
        ui          : VrSystemInfo -> 'mmodel -> DomNode<'msg>
        vr          : VrSystemInfo -> 'mmodel -> ISg<'msg>

        pauseScene  : Option<VrSystemInfo -> 'mmodel -> ISg<'msg>>

    }

module ComposedApp =
    open Aardvark.Base.Ag
    open Aardvark.Rendering.Text
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics
    open Aardvark.Base.Rendering


    let start' (vrapp : IVrApplication) (startRunning : bool) (capp : ComposedApp<'model, 'mmodel, 'msg>) : MutableApp<'model,'msg> =
        let mutable vr = { new IDisposable with member x.Dispose() = () }
        let mutable start = id
        let mutable stop = id
        let info = { kind = vrapp.Kind; start = (fun () -> start()); stop = (fun () -> stop()) }

        let app =
            {
                initial = capp.initial
                update = fun m msg -> capp.update vrapp.SystemInfo.state.Current info m msg
                view = capp.ui vrapp.SystemInfo
                threads = capp.threads
                unpersist = capp.unpersist
            }
        let mmodel, mapp = app.startAndGetState()

        let running = AVal.init false

        let emptyScene =
            match capp.pauseScene with
            | Some scene -> 
                scene vrapp.SystemInfo mmodel :> ISg
            | None -> 
                Sg.textWithConfig TextConfig.Default (AVal.constant "paused...")
                |> Sg.scale 0.3333
                |> Sg.transform (Trafo3d.FromBasis(V3d.IOO, V3d.OOI, -V3d.OIO, V3d(0.0, 0.0, 1.0)))
                

        let realScene = 
            capp.vr vrapp.SystemInfo mmodel :> ISg

        let input (msg : VrMessage) =
            if running.Value then
                capp.input msg
            else
                []


        let scene = 
            running 
            |> AVal.map (function true -> realScene | false -> emptyScene) 
            |> Sg.dynamic
            |> vrapp.SystemInfo.wrapSg

        scene?Runtime <- vrapp.Runtime
        scene?ViewportSize <- vrapp.Size
        let objects = scene.RenderObjects(Ag.Scope.Root)
        //let kill = 
        //    lazy (
        //        vrapp.Start {
        //            new IMutableVrApp with
        //                member x.Scene = RuntimeCommand.Render(objects)
        //                member x.Stop() = ()
        //                member x.Input msg = mapp.update Guid.Empty (input msg :> seq<_>)
        //        } 
        //    )

        let mutable vr = { new IDisposable with member x.Dispose() = () }

        start <- fun () ->
            //kill.Value |> ignore
            vr.Dispose()
            transact (fun () -> running.Value <- true)
            vr <- 
                vrapp.Start {
                    new IMutableVrApp with
                        member x.Scene = RuntimeCommand.Render(objects)
                        member x.Stop() = ()
                        member x.Input msg = mapp.update Guid.Empty (capp.input msg :> seq<_>)
                }

        stop <- fun () ->
            transact (fun () -> running.Value <- false)
            vr.Dispose()
            vr <- { new IDisposable with member x.Dispose() = () }

        if startRunning then start ()
            
        let shutdown() =
            vr.Dispose()
            mapp.shutdown()

        { mapp with shutdown = shutdown }

    let start (vrapp : IVrApplication) (capp : ComposedApp<'model, 'mmodel, 'msg>) : MutableApp<'model,'msg> =
        start' vrapp false capp 