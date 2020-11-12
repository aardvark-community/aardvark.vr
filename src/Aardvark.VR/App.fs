namespace Aardvark.Vr

open Valve.VR

open Aardvark.UI
open Aardvark.Base
open Aardvark.SceneGraph
open Aardvark.Application.OpenVR
open Aardvark.Base.Rendering
open Aardvark.SceneGraph.Semantics
open Aardvark.Base.Ag
open System.Threading
open System
open System.Runtime.CompilerServices
open Aardvark.Rendering.Vulkan
open Aardvark.Application.Slim
open Aardvark.Application

open FSharp.Data.Adaptive
open Adaptify


open Aardvark.Vr


type IAdaptiveVrState =
    abstract member Current : VrState
    abstract member display : Hmd
    abstract member devices : amap<int, AdaptiveVrDevice>
    abstract member renderTargetSize : V2i
    abstract member signature : IFramebufferSignature
    abstract member runtime : IRuntime
    abstract member frameTime : aval<MicroTime>
    abstract member fps : aval<float>


module AdaptiveVrStateExtensions = 
    let toAbstract (m : AdaptiveVrState) (frameTime : aval<MicroTime>) (fps : aval<float>) = 
        { new IAdaptiveVrState with
            member x.Current = m.Current.GetValue() // TODO
            member x.display = m.display.Current.GetValue()
            member x.devices = m.devices
            member x.renderTargetSize = m.renderTargetSize
            member x.signature = m.framebuffer
            member x.runtime = m.runtime
            member x.frameTime = frameTime
            member x.fps = fps
        }


type VrApp<'model, 'mmodel, 'msg> =
    {
        input       : VrMessage -> list<'msg>
        view        : 'mmodel -> IAdaptiveVrState -> IRuntime -> IUniformProvider -> StencilMode -> RuntimeCommand
        update      : 'model -> VrState -> 'msg -> 'model
        initial     : 'model
        unpersist   : Unpersist<'model,'mmodel>
        threads     : 'model -> ThreadPool<'msg>
    }

type IMutableVrApp =
    abstract member Scene : RuntimeCommand
    abstract member Input : VrMessage -> unit
    abstract member Stop : unit -> unit

type MutableVrApp<'model, 'mmodel, 'msg> =
    {
        updateLock  : obj
        input       : VrMessage -> unit
        update      : 'msg -> unit
        mmodel      : 'mmodel
        model       : aval<'model>
        scene       : RuntimeCommand
        stop        : unit -> unit
    }

    interface IMutableVrApp with
        member x.Scene = x.scene
        member x.Input msg = x.input msg
        member x.Stop() = x.stop()

type PoseInfo =
    {
        Pose                : aval<Trafo3d>
        Velocity            : aval<V3d>
        AngularVelocity     : aval<V3d>
        IsValid             : aval<bool>
    }

module PoseInfo =
    let ofMotionState (m : MotionState) =
        {
            Pose = m.Pose
            Velocity = m.Velocity
            AngularVelocity = m.AngularVelocity
            IsValid = m.IsValid
        }

type VrSystemInfo =
    {
        signature   : IFramebufferSignature
        hmd         : PoseInfo
        render      : VrRenderInfo
        bounds      : Option<Polygon3d>
        state       : IAdaptiveVrState
    }
    member x.wrapSg (sg : ISg) =
        let hmdLocation = x.hmd.Pose |> AVal.map (fun t -> t.Forward.C3.XYZ)

        let uniforms =
            UniformProvider.ofList [
                "ViewTrafo", x.render.viewTrafos :> IAdaptiveValue
                "ProjTrafo", x.render.projTrafos :> IAdaptiveValue
                "CameraLocation", hmdLocation :> IAdaptiveValue
                "LightLocation", hmdLocation :> IAdaptiveValue
            ]

        let stencilTest =
            StencilMode(
                StencilOperation(
                    StencilOperationFunction.Keep,
                    StencilOperationFunction.Keep,
                    StencilOperationFunction.Keep
                ),
                StencilFunction(
                    StencilCompareFunction.Equal,
                    0,
                    0xFFFFFFFFu
                )
            )

        Sg.UniformApplicator(uniforms, AVal.constant sg)
        |> Sg.stencilMode (AVal.constant stencilTest)


module MutableVrApp =

    let empty =
        { new IMutableVrApp with
            member x.Scene = RuntimeCommand.Empty
            member x.Input _ = ()
            member x.Stop() = ()
        }

    let start (app : VrApp<'model, 'mmodel, 'msg>) (info : VrSystemInfo) =
        let mutable model = app.initial
        let mmodel = app.unpersist.init app.initial
        let updateLock = obj()

        
        let initialThreads = app.threads model
        let mutable currentThreads = ThreadPool.empty

        let rec emit (msg : 'msg) =
            lock updateLock (fun _ -> 
                let m =  app.update model (info.state.Current) msg
                model <- m
                transact (fun () -> 
                    app.unpersist.update mmodel m
                )
            )

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
            
            currentThreads <- ThreadPool<'msg>(HashMap.choose2 merge currentThreads.store newThreads.store)

        let input (msg : VrMessage) =
            app.input msg |> Seq.iter emit
            
        
        let hmdLocation = info.hmd.Pose |> AVal.map (fun t -> t.Forward.C3.XYZ)

        let uniforms =
            UniformProvider.ofList [
                "ViewTrafo", info.render.viewTrafos :> IAdaptiveValue
                "ProjTrafo", info.render.projTrafos :> IAdaptiveValue
                "CameraLocation", hmdLocation :> IAdaptiveValue
                "LightLocation", hmdLocation :> IAdaptiveValue
            ]

        let stencilTest =
            StencilMode(
                StencilOperation(
                    StencilOperationFunction.Keep,
                    StencilOperationFunction.Keep,
                    StencilOperationFunction.Keep
                ),
                StencilFunction(
                    StencilCompareFunction.Equal,
                    0,
                    0xFFFFFFFFu
                )
            )

        let scene = app.view mmodel info.state info.signature.Runtime uniforms stencilTest

        adjustThreads initialThreads

        let stop() =
            adjustThreads ThreadPool.empty


        {
            updateLock = updateLock
            input = input
            update = emit
            mmodel = mmodel
            model = mmodel :> aval<_>
            scene = scene
            stop = stop
        }


[<RequireQualifiedAccess>]
type VRDisplayKind =
   | None
   | Fake
   | OpenVR

type IVrApplication =
    inherit IDisposable
    abstract member Runtime : IRuntime
    abstract member Size : aval<V2i>
    abstract member Start : IMutableVrApp -> IDisposable
    abstract member SystemInfo : VrSystemInfo
    abstract member Kind : VRDisplayKind
    abstract member Statistics : IEvent<VrSystemStats>

[<AbstractClass; Sealed; Extension>]
type IVrApplicationExtensions private() =
    [<Extension>]
    static member Start(this : IVrApplication, app : VrApp<'model, 'mmodel, 'msg>) =
        let mapp = MutableVrApp.start app this.SystemInfo
        this.Start mapp

        
type VulkanVRApplication(samples : int, debug : bool, adjustSize : V2i -> V2i) as this =
    inherit VulkanVRApplicationLayered(samples, debug, adjustSize)
    
    let mutable currentApp = MutableVrApp.empty
        
    static let modelCache = System.Collections.Concurrent.ConcurrentDictionary<string, Option<ISg>>()

    static let getModel(name : string) =
        modelCache.GetOrAdd(name, fun name ->
            OpenVR.RenderModels.load name
        )

    static let sw = System.Diagnostics.Stopwatch.StartNew()
   
    static let toDeviceKind (clazz : ETrackedDeviceClass) =
        match clazz with
            | ETrackedDeviceClass.Invalid -> VrDeviceKind.Invalid
            | ETrackedDeviceClass.HMD -> VrDeviceKind.Hmd
            | ETrackedDeviceClass.Controller -> VrDeviceKind.Controller
            | ETrackedDeviceClass.TrackingReference -> VrDeviceKind.BaseStation
            | ETrackedDeviceClass.GenericTracker -> VrDeviceKind.Tracker
            | _ -> VrDeviceKind.Unknown

    static let toAxisKind (t : EVRControllerAxisType) =
        match t with
            | EVRControllerAxisType.k_eControllerAxis_Trigger -> VrAxisKind.Trigger
            | EVRControllerAxisType.k_eControllerAxis_TrackPad -> VrAxisKind.Trackpad
            | EVRControllerAxisType.k_eControllerAxis_Joystick -> VrAxisKind.Joystick
            | _ -> VrAxisKind.None

    static let getDevice (system : CVRSystem) (pulses : Dict<int, MicroTime>) (i : int) =
        let deviceType = system.GetTrackedDeviceClass (uint32 i)
        let kind = toDeviceKind deviceType

        let model =
            lazy (
                let name = Text.StringBuilder(4096)
                let mutable err = Unchecked.defaultof<_>
                let len = system.GetStringTrackedDeviceProperty(uint32 i, ETrackedDeviceProperty.Prop_RenderModelName_String, name, 4096u, &err)
                if len > 0u then
                    let name = name.ToString()
                    getModel name
                else
                    None
            )

        let startVibrate(dur : MicroTime) =
            let until = sw.MicroTime + dur

            match pulses.TryGetValue (int i) with
                | (true, o) -> pulses.[int i] <- max o until
                | _ -> pulses.[int i] <- until


        let stopVibrate() =
            pulses.Remove (int i) |> ignore


        { 
            id = int i
            kind = kind
            pose = { deviceToWorld = Trafo3d.Identity; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = false }

            model = model

            axis = 
                HashMap.ofList [
                    let axis0 = ETrackedDeviceProperty.Prop_Axis0Type_Int32
                    for ai in 0 .. int OpenVR.k_unControllerStateAxisCount - 1 do
                        let at = int axis0 + ai |> unbox<ETrackedDeviceProperty>

                        let mutable err = ETrackedPropertyError.TrackedProp_Success
                        let axisKind = system.GetInt32TrackedDeviceProperty(uint32 i, at, &err) |> unbox<EVRControllerAxisType> |> toAxisKind

                        if err = ETrackedPropertyError.TrackedProp_Success then
                            if axisKind <> VrAxisKind.None then
                                yield ai, { id = ai; kind = axisKind; value = V2d.Zero; touched = false; pressed = false }
                ]
            buttons =
                HashMap.ofList [
                    let mutable err = ETrackedPropertyError.TrackedProp_Success
                    let supported = system.GetUint64TrackedDeviceProperty(uint32 i, ETrackedDeviceProperty.Prop_SupportedButtons_Uint64, &err)
                                
                    if err = ETrackedPropertyError.TrackedProp_Success then
                        let mutable mask = 1UL
                        for i in 0 .. 63 do
                            if mask &&& supported <> 0UL then
                                if i < int EVRButtonId.k_EButton_Axis0 || i > int EVRButtonId.k_EButton_Axis4 then
                                    let kind = unbox<EVRButtonId> i
                                    yield i, { id = i; kind = kind; pressed = false }
                            mask <- mask <<< 1
                                        
                        ()

                ]

            startVibrate = startVibrate
            stopVibrate = stopVibrate
        }

    let allModels =
        lazy (
            Map.ofList [
                let cnt = OpenVR.RenderModels.GetRenderModelCount() |> int

                for i in 0 .. cnt - 1 do
                    let name = Text.StringBuilder(4096)
                    let len = OpenVR.RenderModels.GetRenderModelName(uint32 i, name, 4096u)
                    if len > 0u then
                        let name = name.ToString()
                        let model = OpenVR.RenderModels.load name
                        match model with
                        | Some model -> yield name, model
                        | _ -> ()
            ]
        )

    let pulses = Dict<int, MicroTime>()
    let mutable numFrames = 0
    let mutable totalTime = MicroTime.Zero
    let system = base.System.System

    let deviceCache =
        [|
            for i in 0 .. int OpenVR.k_unMaxTrackedDeviceCount-1 do
                yield getDevice system pulses i
        |]


    let removeDevice (i : int) =
        if i >= 0 && i < deviceCache.Length then
            lock deviceCache (fun () ->
                let d = deviceCache.[i]
                if d.kind <> VrDeviceKind.Invalid then
                    Log.warn "disconnect %A"  d.kind
                    deviceCache.[i] <- { d with kind = VrDeviceKind.Invalid }
                    d.kind
                else
                    VrDeviceKind.Invalid
            )
        else
            VrDeviceKind.Invalid

    let rebuildDevice (i : int) =
        if i >= 0 && i < deviceCache.Length then
            lock deviceCache (fun () ->
                let o = 
                    let o = deviceCache.[i]
                    if o.kind <> VrDeviceKind.Invalid then Some o
                    else None

                let d = getDevice system pulses i
                deviceCache.[i] <- d
                if d.kind = VrDeviceKind.Invalid then
                    Log.warn "disconnect %A %A" (o |> Option.map (fun d -> d.kind)) d.kind
                    o, None
                else
                    Log.warn "rebuild %A %A" (o |> Option.map (fun d -> d.kind)) d.kind
                    o, Some d
            )
        else
            None, None

    let updateDevice (i : int) (f : VrDevice -> VrDevice) =
        if i >= 0 && i < deviceCache.Length then
            lock deviceCache (fun () ->
                let o = deviceCache.[i]
                let device = 
                    if o.kind = VrDeviceKind.Invalid then
                        let d = getDevice system pulses i
                        deviceCache.[i] <- d
                        if d.kind = VrDeviceKind.Invalid then
                            None
                        else
                            Log.warn "connect %A"  d.kind
                            Some d
                    else
                        Some o
                
                match device with
                | Some d ->
                    deviceCache.[i] <- f d
                | None ->
                    ()
            )

    let updateAxis (ci : int) (ai : int) (f : Axis -> Axis) =
        updateDevice ci (fun d ->
            { d with 
                axis = d.axis |> HashMap.alter ai (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )      
            }
        )

    let updateButton (ci : int) (bi : int) (f : Button -> Button) =
        updateDevice ci (fun d ->
            { d with 
                buttons = d.buttons |> HashMap.alter bi (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )      
            }
        )

    let getAxisIndex (b : VREvent_Controller_t) =
        let d = int b.button - int EVRButtonId.k_EButton_Axis0
        if d >= 0 && d < int OpenVR.k_unControllerStateAxisCount then
            Some d 
        else
            None

    let getState() =
        lock deviceCache (fun () ->
            let hmd = deviceCache |> Array.find (fun d -> d.kind = VrDeviceKind.Hmd)
            let mutable w = 0u
            let mutable h = 0u
            system.GetRecommendedRenderTargetSize(&w,&h)
            let size = adjustSize (V2i(int w, int h))

            { 
                runtime = this.Runtime
                framebuffer = this.FramebufferSignature
                display = { name = "HMD"; pose = hmd.pose }
                devices = 
                    deviceCache |> Array.choose (fun d -> 
                        if d.kind <> VrDeviceKind.Invalid && d.kind <> VrDeviceKind.Hmd then
                            Some (d.id, d)      
                        else
                            None 
                    )
                    |> HashMap.ofArray
                renderTargetSize = size
            }
        )

    let mutable loaded = false
    let mutable state = { display = { name = "HMD"; pose = Pose.none }; devices = HashMap.empty; renderTargetSize = V2i.Zero; runtime = base.Runtime; framebuffer = base.FramebufferSignature }
    let mstate = AdaptiveVrState.Create state
    let sem = new SemaphoreSlim(1)
    let _frameTime = AVal.init MicroTime.Zero
    let _fps = AVal.init 0.0


    let vrSystem =
        {
            signature   = base.FramebufferSignature
            hmd         = PoseInfo.ofMotionState base.Hmd.MotionState
            render      = base.Info
            bounds      = base.Chaperone
            state       = AdaptiveVrStateExtensions.toAbstract mstate _frameTime _fps
        }

    member x.UpdateFrameTime(time : MicroTime) =
        _frameTime.Value <- time
        _fps.Value <- (if time.TotalSeconds > 0.0 then 1.0 / time.TotalSeconds else 0.0)

    member x.Statistics = base.Statistics

    member x.SystemInfo = vrSystem

    member x.SetApp(app : IMutableVrApp) =
        lock x (fun () ->
            currentApp <- app
            x.RenderTask <- app.Scene
        )
        
    member x.State = getState()
    member x.AllModels = allModels.Value


    override x.Render() =
        let start = sw.MicroTime

        base.Render()

        let now = sw.MicroTime

        let dt = now - start
        numFrames <- numFrames + 1
        totalTime <- totalTime + dt

        x.UpdateFrameTime(dt)
        
        if numFrames >= 30 then
            numFrames <- 0
            totalTime <- MicroTime.Zero


        for (KeyValue(c, time)) in Seq.toList pulses do
            if time > now then
                system.TriggerHapticPulse(uint32 c, 0u, char 65535us)
            else
                pulses.Remove c |> ignore

    override x.OnLoad(info) =
        let res = base.OnLoad(info)
        state <- getState()
        transact (fun () -> mstate.Update(state))
        loaded <- true
        res

    override x.UpdatePoses(poses : TrackedDevicePose_t[]) =
        if loaded then
            
            transact (fun () ->
                let changes = System.Collections.Generic.List<VrMessage>()
                for i in 0 .. poses.Length - 1 do
                    let mutable state = Unchecked.defaultof<VRControllerState_t>

                    updateDevice i (fun d ->
                        let mutable d = d
                        if system.GetControllerState(uint32 i, &state, uint32 sizeof<VRControllerState_t>) then
                            let value (ai : int) =
                                match ai with
                                    | 0 -> V2d(state.rAxis0.x, state.rAxis0.y)
                                    | 1 -> V2d(state.rAxis1.x, state.rAxis1.y)
                                    | 2 -> V2d(state.rAxis2.x, state.rAxis2.y)
                                    | 3 -> V2d(state.rAxis3.x, state.rAxis3.y)
                                    | 4 -> V2d(state.rAxis4.x, state.rAxis4.y)
                                    | _ -> V2d.Zero

                            d <- {
                                d with
                                    axis = d.axis |> HashMap.map (fun ai a -> 
                                        let n = value ai
                                        if a.value <> n then
                                            changes.Add(ValueChange(i, ai, n))
                                            { a with value = n }
                                        else
                                            a
                                    ) 
                            }
                        let pose = Pose.ofOpenVR poses.[i]
                        changes.Add(UpdatePose(i, pose))
                        { d with pose = pose}
                    )
                    
                if changes.Count > 0 then 
                    changes |> Seq.iter currentApp.Input
                state <- getState()
                mstate.Update(state)
            )

    override x.ProcessEvent(evt : VREvent_t) =
        if loaded then
            
            let eventType = evt.eventType |> int |> unbox<EVREventType>
            match eventType with
                | EVREventType.VREvent_TrackedDeviceRoleChanged | EVREventType.VREvent_TrackedDeviceUpdated ->
                    let id = int evt.trackedDeviceIndex
                    match rebuildDevice id with
                    | None, Some n when n.kind = VrDeviceKind.Controller -> currentApp.Input (ControllerConnected id)
                    | Some o, None when o.kind = VrDeviceKind.Controller -> currentApp.Input (ControllerDisconnected id)
                    | _ -> ()
                        
                | EVREventType.VREvent_TrackedDeviceActivated ->
                    let id = int evt.trackedDeviceIndex
                    let mutable kind = VrDeviceKind.Invalid
                    updateDevice id (fun d -> kind <- d.kind; d)
                    if kind = VrDeviceKind.Controller then
                        currentApp.Input (ControllerConnected id)

                | EVREventType.VREvent_TrackedDeviceDeactivated ->
                    let id = int evt.trackedDeviceIndex
                    let oldKind = removeDevice id
                    if oldKind = VrDeviceKind.Controller then
                        currentApp.Input (ControllerDisconnected id)


                | EVREventType.VREvent_ButtonTouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = true })
                            //Log.line "[Aardvark.VR.App] Touching Axis %A" ai
                            currentApp.Input (Touch(ci, ai))
                        | _ -> ()

                | EVREventType.VREvent_ButtonUntouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = false })
                            currentApp.Input (Untouch(ci, ai))
                        | _ -> ()
                    
                | EVREventType.VREvent_ButtonPress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = true })
                            //Log.info "[Aardvark.VR.App] Pressing Axis %A" ai
                            currentApp.Input (Press(ci, ai))
                        | _ ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = true })
                            currentApp.Input (PressButton(ci, bi))
                            ()
                    
                | EVREventType.VREvent_ButtonUnpress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = false })
                            currentApp.Input (Unpress(ci, ai))
                        | _ ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = false })
                            currentApp.Input (UnpressButton(ci, bi))

                | _ ->
                    ()

            transact (fun () ->
                state <- getState()
                mstate.Update(state)
            )
        
    member x.Start(mapp : IMutableVrApp) =
        sem.Wait()
        x.SetApp mapp
        let thread = 
            startThread (fun () ->
                x.Run()
            )

        { new System.IDisposable with
            member __.Dispose() =
                mapp.Stop()
                x.Shutdown()
                thread.Join()
                sem.Release() |> ignore
        }

    member x.Start(app : VrApp<'model, 'mmodel, 'msg>) =
        let mapp = MutableVrApp.start app x.SystemInfo
        x.Start(mapp)

    interface IVrApplication with
        member x.Start m = x.Start m
        member x.SystemInfo = x.SystemInfo
        member x.Runtime = x.Runtime :> IRuntime
        member x.Size = x.Sizes 
        member x.Kind = VRDisplayKind.OpenVR
        member x.Statistics = x.Statistics


type GLVRApplication(samples : int, debug : bool, adjustSize : V2i -> V2i) as this =
    inherit OpenGlVRApplicationLayered(samples, debug, adjustSize)
    
    let mutable currentApp = MutableVrApp.empty
        
    static let modelCache = System.Collections.Concurrent.ConcurrentDictionary<string, Option<ISg>>()

    static let getModel(name : string) =
        modelCache.GetOrAdd(name, fun name ->
            OpenVR.RenderModels.load name
        )

    static let sw = System.Diagnostics.Stopwatch.StartNew()
   
    static let toDeviceKind (clazz : ETrackedDeviceClass) =
        match clazz with
            | ETrackedDeviceClass.Invalid -> VrDeviceKind.Invalid
            | ETrackedDeviceClass.HMD -> VrDeviceKind.Hmd
            | ETrackedDeviceClass.Controller -> VrDeviceKind.Controller
            | ETrackedDeviceClass.TrackingReference -> VrDeviceKind.BaseStation
            | ETrackedDeviceClass.GenericTracker -> VrDeviceKind.Tracker
            | _ -> VrDeviceKind.Unknown

    static let toAxisKind (t : EVRControllerAxisType) =
        match t with
            | EVRControllerAxisType.k_eControllerAxis_Trigger -> VrAxisKind.Trigger
            | EVRControllerAxisType.k_eControllerAxis_TrackPad -> VrAxisKind.Trackpad
            | EVRControllerAxisType.k_eControllerAxis_Joystick -> VrAxisKind.Joystick
            | _ -> VrAxisKind.None

    static let getDevice (system : CVRSystem) (pulses : Dict<int, MicroTime>) (i : int) =
        let deviceType = system.GetTrackedDeviceClass (uint32 i)
        let kind = toDeviceKind deviceType

        let model =
            lazy (
                let name = Text.StringBuilder(4096)
                let mutable err = Unchecked.defaultof<_>
                let len = system.GetStringTrackedDeviceProperty(uint32 i, ETrackedDeviceProperty.Prop_RenderModelName_String, name, 4096u, &err)
                if len > 0u then
                    let name = name.ToString()
                    getModel name
                else
                    None
            )

        let startVibrate(dur : MicroTime) =
            let until = sw.MicroTime + dur

            match pulses.TryGetValue (int i) with
                | (true, o) -> pulses.[int i] <- max o until
                | _ -> pulses.[int i] <- until


        let stopVibrate() =
            pulses.Remove (int i) |> ignore


        { 
            id = int i
            kind = kind
            pose = { deviceToWorld = Trafo3d.Identity; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = false }

            model = model

            axis = 
                HashMap.ofList [
                    let axis0 = ETrackedDeviceProperty.Prop_Axis0Type_Int32
                    for ai in 0 .. int OpenVR.k_unControllerStateAxisCount - 1 do
                        let at = int axis0 + ai |> unbox<ETrackedDeviceProperty>

                        let mutable err = ETrackedPropertyError.TrackedProp_Success
                        let axisKind = system.GetInt32TrackedDeviceProperty(uint32 i, at, &err) |> unbox<EVRControllerAxisType> |> toAxisKind

                        if err = ETrackedPropertyError.TrackedProp_Success then
                            if axisKind <> VrAxisKind.None then
                                yield ai, { id = ai; kind = axisKind; value = V2d.Zero; touched = false; pressed = false }
                ]
            buttons =
                HashMap.ofList [
                    let mutable err = ETrackedPropertyError.TrackedProp_Success
                    let supported = system.GetUint64TrackedDeviceProperty(uint32 i, ETrackedDeviceProperty.Prop_SupportedButtons_Uint64, &err)
                                
                    if err = ETrackedPropertyError.TrackedProp_Success then
                        let mutable mask = 1UL
                        for i in 0 .. 63 do
                            if mask &&& supported <> 0UL then
                                if i < int EVRButtonId.k_EButton_Axis0 || i > int EVRButtonId.k_EButton_Axis4 then
                                    let kind = unbox<EVRButtonId> i
                                    yield i, { id = i; kind = kind; pressed = false }
                            mask <- mask <<< 1
                                        
                        ()

                ]

            startVibrate = startVibrate
            stopVibrate = stopVibrate
        }

    let allModels =
        lazy (
            Map.ofList [
                let cnt = OpenVR.RenderModels.GetRenderModelCount() |> int

                for i in 0 .. cnt - 1 do
                    let name = Text.StringBuilder(4096)
                    let len = OpenVR.RenderModels.GetRenderModelName(uint32 i, name, 4096u)
                    if len > 0u then
                        let name = name.ToString()
                        let model = OpenVR.RenderModels.load name
                        match model with
                        | Some model -> yield name, model
                        | _ -> ()
            ]
        )

    let pulses = Dict<int, MicroTime>()
    let mutable numFrames = 0
    let mutable totalTime = MicroTime.Zero
    let system = base.System.System

    let deviceCache =
        [|
            for i in 0 .. int OpenVR.k_unMaxTrackedDeviceCount-1 do
                yield getDevice system pulses i
        |]


    let removeDevice (i : int) =
        if i >= 0 && i < deviceCache.Length then
            lock deviceCache (fun () ->
                let d = deviceCache.[i]
                if d.kind <> VrDeviceKind.Invalid then
                    Log.warn "disconnect %A"  d.kind
                    deviceCache.[i] <- { d with kind = VrDeviceKind.Invalid }
                    d.kind
                else
                    VrDeviceKind.Invalid
            )
        else
            VrDeviceKind.Invalid

    let rebuildDevice (i : int) =
        if i >= 0 && i < deviceCache.Length then
            lock deviceCache (fun () ->
                let o = 
                    let o = deviceCache.[i]
                    if o.kind <> VrDeviceKind.Invalid then Some o
                    else None

                let d = getDevice system pulses i
                deviceCache.[i] <- d
                if d.kind = VrDeviceKind.Invalid then
                    Log.warn "disconnect %A %A" (o |> Option.map (fun d -> d.kind)) d.kind
                    o, None
                else
                    Log.warn "rebuild %A %A" (o |> Option.map (fun d -> d.kind)) d.kind
                    o, Some d
            )
        else
            None, None

    let updateDevice (i : int) (f : VrDevice -> VrDevice) =
        if i >= 0 && i < deviceCache.Length then
            lock deviceCache (fun () ->
                let o = deviceCache.[i]
                let device = 
                    if o.kind = VrDeviceKind.Invalid then
                        let d = getDevice system pulses i
                        deviceCache.[i] <- d
                        if d.kind = VrDeviceKind.Invalid then
                            None
                        else
                            Log.warn "connect %A"  d.kind
                            Some d
                    else
                        Some o
                
                match device with
                | Some d ->
                    deviceCache.[i] <- f d
                | None ->
                    ()
            )

    let updateAxis (ci : int) (ai : int) (f : Axis -> Axis) =
        updateDevice ci (fun d ->
            { d with 
                axis = d.axis |> HashMap.alter ai (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )      
            }
        )

    let updateButton (ci : int) (bi : int) (f : Button -> Button) =
        updateDevice ci (fun d ->
            { d with 
                buttons = d.buttons |> HashMap.alter bi (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )      
            }
        )

    let getAxisIndex (b : VREvent_Controller_t) =
        let d = int b.button - int EVRButtonId.k_EButton_Axis0
        if d >= 0 && d < int OpenVR.k_unControllerStateAxisCount then
            Some d 
        else
            None

    let getState() =
        lock deviceCache (fun () ->
            let hmd = deviceCache |> Array.find (fun d -> d.kind = VrDeviceKind.Hmd)
            let mutable w = 0u
            let mutable h = 0u
            system.GetRecommendedRenderTargetSize(&w,&h)
            let size = adjustSize (V2i(int w, int h))

            { 
                runtime = this.Runtime
                framebuffer = this.FramebufferSignature
                display = { name = "HMD"; pose = hmd.pose }
                devices = 
                    deviceCache |> Array.choose (fun d -> 
                        if d.kind <> VrDeviceKind.Invalid && d.kind <> VrDeviceKind.Hmd then
                            Some (d.id, d)      
                        else
                            None 
                    )
                    |> HashMap.ofArray
                renderTargetSize = size
            }
        )

    let mutable loaded = false
    let mutable state = { display = { name = "HMD"; pose = Pose.none }; devices = HashMap.empty; renderTargetSize = V2i.Zero; runtime = base.Runtime; framebuffer = base.FramebufferSignature }
    let mstate = AdaptiveVrState.Create state
    let sem = new SemaphoreSlim(1)
    let _frameTime = AVal.init MicroTime.Zero
    let _fps = AVal.init 0.0


    let vrSystem =
        {
            signature   = base.FramebufferSignature
            hmd         = PoseInfo.ofMotionState base.Hmd.MotionState
            render      = base.Info
            bounds      = base.Chaperone
            state       = AdaptiveVrStateExtensions.toAbstract mstate _frameTime _fps
        }

    member x.UpdateFrameTime(time : MicroTime) =
        _frameTime.Value <- time
        _fps.Value <- (if time.TotalSeconds > 0.0 then 1.0 / time.TotalSeconds else 0.0)

    member x.Statistics = base.Statistics

    member x.SystemInfo = vrSystem

    member x.SetApp(app : IMutableVrApp) =
        lock x (fun () ->
            currentApp <- app
            match app.Scene with
            | RuntimeCommand.RenderCmd objs-> 
                x.RenderTask <- new Aardvark.Rendering.GL.RenderTasks.RenderTask(x.Runtime.ResourceManager, x.FramebufferSignature, objs, AVal.constant BackendConfiguration.Default, true, true)
            | _ -> failwith "https://github.com/aardvark-platform/aardvark.rendering/issues/67"
        )
        
    member x.State = getState()
    member x.AllModels = allModels.Value


    override x.Render() =
        let start = sw.MicroTime

        base.Render()

        let now = sw.MicroTime

        let dt = now - start
        numFrames <- numFrames + 1
        totalTime <- totalTime + dt

        x.UpdateFrameTime(dt)
        
        if numFrames >= 30 then
            numFrames <- 0
            totalTime <- MicroTime.Zero


        for (KeyValue(c, time)) in Seq.toList pulses do
            if time > now then
                system.TriggerHapticPulse(uint32 c, 0u, char 65535us)
            else
                pulses.Remove c |> ignore

    override x.OnLoad(info) =
        let res = base.OnLoad(info)
        state <- getState()
        transact (fun () -> mstate.Update(state))
        loaded <- true
        res

    override x.UpdatePoses(poses : TrackedDevicePose_t[]) =
        if loaded then
            
            transact (fun () ->
                let changes = System.Collections.Generic.List<VrMessage>()
                for i in 0 .. poses.Length - 1 do
                    let mutable state = Unchecked.defaultof<VRControllerState_t>

                    updateDevice i (fun d ->
                        let mutable d = d
                        if system.GetControllerState(uint32 i, &state, uint32 sizeof<VRControllerState_t>) then
                            let value (ai : int) =
                                match ai with
                                    | 0 -> V2d(state.rAxis0.x, state.rAxis0.y)
                                    | 1 -> V2d(state.rAxis1.x, state.rAxis1.y)
                                    | 2 -> V2d(state.rAxis2.x, state.rAxis2.y)
                                    | 3 -> V2d(state.rAxis3.x, state.rAxis3.y)
                                    | 4 -> V2d(state.rAxis4.x, state.rAxis4.y)
                                    | _ -> V2d.Zero

                            d <- {
                                d with
                                    axis = d.axis |> HashMap.map (fun ai a -> 
                                        let n = value ai
                                        if a.value <> n then
                                            changes.Add(ValueChange(i, ai, n))
                                            { a with value = n }
                                        else
                                            a
                                    ) 
                            }
                        let pose = Pose.ofOpenVR poses.[i]
                        changes.Add(UpdatePose(i, pose))
                        { d with pose = pose}
                    )
                    
                if changes.Count > 0 then 
                    changes |> Seq.iter currentApp.Input
                state <- getState()
                mstate.Update(state)
            )

    override x.ProcessEvent(evt : VREvent_t) =
        if loaded then
            
            let eventType = evt.eventType |> int |> unbox<EVREventType>
            match eventType with
                | EVREventType.VREvent_TrackedDeviceRoleChanged | EVREventType.VREvent_TrackedDeviceUpdated ->
                    let id = int evt.trackedDeviceIndex
                    match rebuildDevice id with
                    | None, Some n when n.kind = VrDeviceKind.Controller -> currentApp.Input (ControllerConnected id)
                    | Some o, None when o.kind = VrDeviceKind.Controller -> currentApp.Input (ControllerDisconnected id)
                    | _ -> ()
                        
                | EVREventType.VREvent_TrackedDeviceActivated ->
                    let id = int evt.trackedDeviceIndex
                    let mutable kind = VrDeviceKind.Invalid
                    updateDevice id (fun d -> kind <- d.kind; d)
                    if kind = VrDeviceKind.Controller then
                        currentApp.Input (ControllerConnected id)

                | EVREventType.VREvent_TrackedDeviceDeactivated ->
                    let id = int evt.trackedDeviceIndex
                    let oldKind = removeDevice id
                    if oldKind = VrDeviceKind.Controller then
                        currentApp.Input (ControllerDisconnected id)


                | EVREventType.VREvent_ButtonTouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = true })
                            //Log.line "[Aardvark.VR.App] Touching Axis %A" ai
                            currentApp.Input (Touch(ci, ai))
                        | _ -> ()

                | EVREventType.VREvent_ButtonUntouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = false })
                            currentApp.Input (Untouch(ci, ai))
                        | _ -> ()
                    
                | EVREventType.VREvent_ButtonPress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = true })
                            //Log.info "[Aardvark.VR.App] Pressing Axis %A" ai
                            currentApp.Input (Press(ci, ai))
                        | _ ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = true })
                            currentApp.Input (PressButton(ci, bi))
                            ()
                    
                | EVREventType.VREvent_ButtonUnpress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = false })
                            currentApp.Input (Unpress(ci, ai))
                        | _ ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = false })
                            currentApp.Input (UnpressButton(ci, bi))

                | _ ->
                    ()

            transact (fun () ->
                state <- getState()
                mstate.Update(state)
            )
        
    member x.Start(mapp : IMutableVrApp) =
        sem.Wait()
        x.SetApp mapp
        let thread = 
            startThread (fun () ->
                x.Run()
            )

        { new System.IDisposable with
            member __.Dispose() =
                mapp.Stop()
                x.Shutdown()
                thread.Join()
                sem.Release() |> ignore
        }

    member x.Start(app : VrApp<'model, 'mmodel, 'msg>) =
        let mapp = MutableVrApp.start app x.SystemInfo
        x.Start(mapp)

    interface IVrApplication with
        member x.Start m = x.Start m
        member x.SystemInfo = x.SystemInfo
        member x.Runtime = x.Runtime :> IRuntime
        member x.Size = x.Sizes 
        member x.Kind = VRDisplayKind.OpenVR
        member x.Statistics = x.Statistics

open Aardvark.Application.Utilities
type VulkanFakeVrApplication(samples : int, debug : bool) =
    let vulkanApp = new VulkanApplication(debug)

    let boot = new SemaphoreSlim(0)

    let win : ChangeableValue<Option<ISimpleRenderWindow>> = AVal.init None
    let view = win |> AVal.bind (function Some w -> w.View | None -> AVal.constant [|Trafo3d.Identity; Trafo3d.Identity|])
    let proj = win |> AVal.bind (function Some w -> w.Proj | None -> AVal.constant [|Trafo3d.Identity; Trafo3d.Identity|])
    let size = win |> AVal.bind (function Some w -> w.Sizes | None -> AVal.constant V2i.II)

    let tasks = new System.Collections.Concurrent.BlockingCollection<RuntimeCommand>(1)

    let command = AVal.init RuntimeCommand.Empty

    let evtStatistics = EventSource<VrSystemStats>()


    let run() =
        let w = 
            let s = samples
            let d = debug
            
            window {
                app vulkanApp
                backend Backend.Vulkan
                display Display.Stereo
                debug d
                samples s
            }
            
        let obj = 
            command |> AVal.map (fun c ->
                [CommandRenderObject(RenderPass.main, Ag.Scope.Root, c) :> IRenderObject]
            )
        let sg = 
            Sg.RenderObjectNode(ASet.ofAVal obj) :> ISg
            
        w.Scene <- sg
        transact (fun () -> win.Value <- Some w)
        boot.Release() |> ignore
        
        w.Run()

        w.Dispose()

    let uiThread = 
        lazy (
            let t = new Thread(ThreadStart(run), IsBackground = true, ApartmentState = ApartmentState.STA)
            t.Start()
            boot.Wait()
            boot.Dispose()
            t
        )
    




    let info =
        let mstate =
            { new IAdaptiveVrState with
                member x.Current = { VrState.empty with display = { VrState.empty.display with pose = { deviceToWorld = view.GetValue().[0].Inverse; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = true } } }
                member x.devices = AMap.empty
                member x.display = { VrState.empty.display with pose = { deviceToWorld = view.GetValue().[0].Inverse; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = true } }
                member x.renderTargetSize = size.GetValue()
                member x.frameTime = AVal.constant (MicroTime.FromMilliseconds 10.0)
                member x.fps = AVal.constant 100.0
                member x.runtime = vulkanApp.Runtime :> IRuntime
                member x.signature = Unchecked.defaultof<_>
            }
        let size = V2i(1024, 768)
        {
            VrSystemInfo.signature = Unchecked.defaultof<_>
            VrSystemInfo.hmd =
                {
                    Pose = view |> AVal.map (fun arr -> arr.[0].Inverse)
                    Velocity = AVal.constant V3d.Zero
                    AngularVelocity = AVal.constant V3d.Zero
                    IsValid = AVal.constant true
                }
            VrSystemInfo.render =
                {
                    framebufferSize = size
                    viewTrafos = view
                    projTrafos = proj
                }
            VrSystemInfo.state = mstate
            VrSystemInfo.bounds = None
        }

    member x.SystemInfo = info

    member x.Statistics = evtStatistics :> IEvent<_>

    member x.Start(mapp : IMutableVrApp) =
        uiThread.Value |> ignore
        transact (fun () -> command.Value <- mapp.Scene)
        { new IDisposable with
            member x.Dispose() =
                transact (fun () -> command.Value <- RuntimeCommand.Empty)
        }


    member x.Dispose() =
        tasks.CompleteAdding()
        if uiThread.IsValueCreated then
            uiThread.Value.Join()

    member x.Runtime = vulkanApp.Runtime :> IRuntime
    member x.Size = size

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IVrApplication with
        member x.SystemInfo = x.SystemInfo
        member x.Start m = x.Start m
        member x.Runtime = vulkanApp.Runtime :> IRuntime
        member x.Size = size
        member x.Kind = VRDisplayKind.Fake
        member x.Statistics = x.Statistics 

type VulkanNoVrApplication(debug : bool) as this =
    inherit HeadlessVulkanApplication(debug)

    let hmd : PoseInfo =
        {
            Pose = AVal.constant Trafo3d.Identity
            Velocity = AVal.constant V3d.Zero
            AngularVelocity = AVal.constant V3d.Zero
            IsValid = AVal.constant false
        }
        

    let info =
        let size = V2i(1024, 768)
        let mstate =
            { new IAdaptiveVrState with
                member x.Current = VrState.empty
                member x.devices = AMap.empty
                member x.display = VrState.empty.display
                member x.renderTargetSize = size
                member x.frameTime = AVal.constant (MicroTime.FromMilliseconds 10.0)
                member x.fps = AVal.constant 100.0
                member x.runtime = this.Runtime :> IRuntime
                member x.signature = Unchecked.defaultof<_>
            }
        {
            VrSystemInfo.signature = Unchecked.defaultof<_>
            VrSystemInfo.hmd = hmd
            VrSystemInfo.bounds = None
            VrSystemInfo.render =
                {
                    framebufferSize = size
                    viewTrafos = AVal.constant [| Trafo3d.Identity |]
                    projTrafos = AVal.constant [| Trafo3d.Identity |]
                }
            VrSystemInfo.state = mstate
        }

    let statistics = EventSource<VrSystemStats>()

    member x.Statistics = statistics :> IEvent<_>
    
    interface IVrApplication with
        member x.Start m = { new IDisposable with member x.Dispose() = () }
        member x.SystemInfo = info
        member x.Runtime = x.Runtime :> IRuntime
        member x.Size = AVal.constant V2i.II
        member x.Kind = VRDisplayKind.None
        member x.Statistics = x.Statistics


[<RequireQualifiedAccess>]
type VRDisplay =
    internal
        | DisplayNone
        | DisplayFake
        | DisplayOpenVR of adjust : (V2i -> V2i)

    member x.Kind =
        match x with
        | DisplayNone -> VRDisplayKind.None
        | DisplayFake -> VRDisplayKind.Fake
        | DisplayOpenVR _ -> VRDisplayKind.OpenVR

    static member None = DisplayNone
    static member Fake = DisplayFake
    static member OpenVR (adjust : V2i -> V2i) = DisplayOpenVR adjust
    static member OpenVR (factor : float) =
        let adjust (v : V2i) =
            let d = factor * V2d v
            let s = V2i(max 1.0 d.X, max 1.0 d.Y)
            s
        DisplayOpenVR adjust
        
            


module VRApplication =
    let create (display : VRDisplay) (samples : int) (debug : bool) =
        match display with
        | VRDisplay.DisplayNone ->
            new VulkanNoVrApplication(debug) :> IVrApplication
        | VRDisplay.DisplayFake ->
            new VulkanFakeVrApplication(samples, debug) :> IVrApplication
        | VRDisplay.DisplayOpenVR adjust ->
            try new VulkanVRApplication(samples, debug, adjust) :> IVrApplication
            with _ -> new VulkanNoVrApplication(debug) :> IVrApplication

    let create' (display : VRDisplay) (backend : Aardvark.Application.Backend) (samples : int) (debug : bool) =
        match display, backend with
        | VRDisplay.DisplayNone, Backend.Vulkan ->
            new VulkanNoVrApplication(debug) :> IVrApplication
        | VRDisplay.DisplayFake, Backend.Vulkan ->
            new VulkanFakeVrApplication(samples, debug) :> IVrApplication
        | VRDisplay.DisplayOpenVR adjust, Backend.Vulkan ->
            try new VulkanVRApplication(samples, debug, adjust) :> IVrApplication
            with _ -> new VulkanNoVrApplication(debug) :> IVrApplication
        | VRDisplay.DisplayOpenVR adjust, Backend.GL ->
            new GLVRApplication(samples, debug, adjust) :> IVrApplication
        | _ -> failwith "display, backend combination not implemented"


