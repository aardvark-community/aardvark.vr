namespace Aardvark.Vr

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

[<AutoOpen>]
module MissingMediaFeatures =
    open System.Threading
    open System.Collections.Generic
    open System.Reactive.Subjects

    type private Message<'msg> = { msgs : seq<'msg>; processed : Option<System.Threading.ManualResetEventSlim> }

    type App<'model, 'mmodel, 'msg> with
        member app.StartAndGetMModel() =
            let l = obj()
            let initial = app.initial
            let state = Mod.init initial
            let mstate = app.unpersist.create initial
            let initialThreads = app.threads initial
            let node = app.view mstate

            let mutable running = true
            let messageQueue = List<Message<'msg>>(128)
            let subject = new Subject<'msg>()

            let mutable currentThreads = ThreadPool.empty
        

            let update (source : Guid) (msgs : seq<'msg>) =
                //use mri = new System.Threading.ManualResetEventSlim()
                lock messageQueue (fun () ->
                    messageQueue.Add { msgs = msgs; processed = None }
                    Monitor.Pulse messageQueue
                )
              //  mri.Wait()

            let rec updateSync (source : Guid) (msgs : seq<'msg>) =
                doit [{ msgs = msgs; processed = None }] // TODO: gh, what can we do about this deadlock problem agains render service thread.

            and adjustThreads (newThreads : ThreadPool<'msg>) =
                let merge (id : string) (oldThread : Option<Command<'msg>>) (newThread : Option<Command<'msg>>) : Option<Command<'msg>> =
                    match oldThread, newThread with
                        | Some o, None ->
                            o.Stop()
                            newThread
                        | None, Some n -> 
                            n.Start(emit)
                            newThread
                        | Some o, Some n ->
                            oldThread
                        | None, None -> 
                            None
            
                currentThreads <- ThreadPool<'msg>(HMap.choose2 merge currentThreads.store newThreads.store)


            and doit(msgs : list<Message<'msg>>) =
                lock l (fun () ->
                    if Config.shouldTimeUnpersistCalls then Log.startTimed "[Aardvark.UI] update/adjustThreads/unpersist"
                    for msg in msgs do
                        for msg in msg.msgs do
                            let newState = app.update state.Value msg
                            let newThreads = app.threads newState
                            adjustThreads newThreads
                            transact (fun () ->
                                state.Value <- newState
                                app.unpersist.update mstate newState
                            )
                        // if somebody awaits message processing, trigger it
                        msg.processed |> Option.iter (fun mri -> mri.Set())
                    if Config.shouldTimeUnpersistCalls then Log.stop ()
                )
                for m in msgs do 
                    for m in m.msgs do subject.OnNext(m)

            and emit (msg : 'msg) =
                lock messageQueue (fun () ->
                    messageQueue.Add { msgs = Seq.singleton msg; processed = None }
                    Monitor.Pulse messageQueue
                )


            // start initial threads
            adjustThreads initialThreads

            let updateThread =
                let update () = 
                    while running do
                        Monitor.Enter(messageQueue)
                        while running && messageQueue.Count = 0 do
                            Monitor.Wait(messageQueue) |> ignore
                    
                        let messages = 
                            if running then 
                                let messages = messageQueue |> CSharpList.toList
                                messages
                            else      
                                []      
                            
                        messageQueue.Clear()                 
                            
                        Monitor.Exit(messageQueue)

                        match messages with
                            | [] -> ()
                            | _ -> doit messages

                Thread(ThreadStart update)

            updateThread.Name <- "[Aardvark.Media.App] updateThread"
            updateThread.IsBackground <- true
            updateThread.Start()

            let shutdown () =
                running <- false
                lock messageQueue (fun () -> Monitor.PulseAll messageQueue)
                updateThread.Join()
                subject.OnCompleted()
                subject.Dispose()

            mstate, {
                lock = l
                model = state
                ui = node
                update = update
                updateSync = updateSync
                shutdown = shutdown
                messages = subject
            }


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

    //let ofApp (app : App<'model, 'mmodel, 'msg>) =
    //    {
    //        initial = app.initial
    //        update = fun _ _ model msg -> app.update model msg
    //        threads = app.threads
    //        unpersist = app.unpersist
    //        input = fun _ -> []
    //        ui = app.view
    //        vr = fun _ _ -> Sg.noEvents Sg.empty
    //    }
        
        
    //let toApp (app : ComposedApp<'model, 'mmodel, 'msg>) : App<'model, 'mmodel, 'msg> =
    //    let emptyState =
    //        {
    //            VrState.devices = HMap.empty
    //            VrState.display = { name = "none"; pose = Pose.none }
    //            VrState.renderTargetSize = V2i(0,0)
    //        }
    //    {
    //        initial = app.initial
    //        update = fun  model msg -> app.update emptyState VrActions.nop model msg
    //        threads = app.threads
    //        unpersist = app.unpersist
    //        view = app.ui
    //    }

    let start (vrapp : IVrApplication) (capp : ComposedApp<'model, 'mmodel, 'msg>) =
        let mutable vr = { new IDisposable with member x.Dispose() = () }
        let mutable start = id
        let mutable stop = id
        let info = { kind = vrapp.Kind; start = (fun () -> start()); stop = (fun () -> stop()) }

        let app =
            {
                initial = capp.initial
                update = capp.update (vrapp.SystemInfo.getState()) info
                view = capp.ui vrapp.SystemInfo
                threads = capp.threads
                unpersist = capp.unpersist
            }
        let mmodel, mapp = app.StartAndGetMModel()

        let running = Mod.init false

        let emptyScene =
            match capp.pauseScene with
            | Some scene -> 
                scene vrapp.SystemInfo mmodel :> ISg
            | None -> 
                Sg.textWithConfig TextConfig.Default (Mod.constant "paused...")
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
            |> Mod.map (function true -> realScene | false -> emptyScene) 
            |> Sg.dynamic
            |> vrapp.SystemInfo.wrapSg

        scene?Runtime <- vrapp.Runtime
        scene?ViewportSize <- vrapp.Size
        let objects = scene.RenderObjects()
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
            
        let shutdown() =
            vr.Dispose()
            mapp.shutdown()

        { mapp with shutdown = shutdown }
