namespace Aardvark.Vr

open Valve.VR

open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
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




[<DomainType>]
type Pose =
    {
        deviceToWorld   : Trafo3d
        velocity        : V3d
        angularVelocity : V3d
        isValid         : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Trafo =
    
    let ofHmdMatrix34 (x : HmdMatrix34_t) =
        let t = 
            M44f(
                 x.m0, -x.m2,   x.m1,  x.m3, 
                -x.m8,  x.m10, -x.m9, -x.m11, 
                 x.m4, -x.m6,   x.m5,  x.m7, 
                 0.0f,  0.0f,   0.0f,  1.0f
            ) 
//        let t = 
//            M44f(
//                x.m0, x.m1, x.m2,  x.m3,
//                x.m4, x.m5, x.m6,  x.m7,
//                x.m8, x.m9, x.m10, x.m11,
//                0.0f, 0.0f, 0.0f, 1.0f
//            ) 

        let t = M44d.op_Explicit(t)
        Trafo3d(t,t.Inverse)

    let angularVelocity (v : HmdVector3_t) =
        // WRONG??
        V3d(-v.v0, -v.v1, -v.v2)

    let velocity (v : HmdVector3_t) =
        V3d(v.v0, v.v2, -v.v1)

    let inline inverse (t : Trafo3d) = t.Inverse


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pose =

    let none =
        { 
            deviceToWorld = Trafo3d.Identity
            velocity = V3d.Zero
            angularVelocity = V3d.Zero
            isValid = false
        }

    let ofOpenVR (pose : TrackedDevicePose_t) =
        let isValid =  pose.bDeviceIsConnected && pose.bPoseIsValid
        if isValid then
            let t = Trafo.ofHmdMatrix34 pose.mDeviceToAbsoluteTracking

            { 
                deviceToWorld = Trafo.ofHmdMatrix34 pose.mDeviceToAbsoluteTracking
                velocity = Trafo.velocity pose.vVelocity
                angularVelocity = Trafo.angularVelocity pose.vAngularVelocity
                isValid = true
            }
        else
            { deviceToWorld = Trafo3d.Identity; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = false}

type VRButtonKind =
  | MenuButton  
  | Trackpad    
  | SystemButton
  | Trigger     
  | GripButton  


type VrMessage =
    | ControllerConnected of controller : int
    | ControllerDisconnected of controller : int

    | Touch         of controller : int * axis : int
    | Untouch       of controller : int * axis : int
    
    | Press         of controller : int * axis : int
    | Unpress       of controller : int * axis : int

    | PressButton   of controller : int * button : int
    | UnpressButton of controller : int * button : int

    | ValueChange   of controller : int * axis : int * value : V2d
    | UpdatePose    of controller : int * pose : Pose

    | PointTo       of ray : Ray3d

type VrDeviceKind =
    | Invalid = -1
    | Hmd = 0
    | Controller = 1
    | BaseStation = 2
    | Tracker = 3
    | Unknown = 4

type VrAxisKind =
    | None = 0
    | Trigger = 1
    | Trackpad = 2
    | Joystick = 3


[<DomainType>]
type Hmd =
    {
        name        : string
        pose        : Pose
    }
    
[<DomainType>]
type Axis =
    {
        id          : int
        kind        : VrAxisKind
        value       : V2d
        touched     : bool
        pressed     : bool
    }
    
[<DomainType>]
type Button =
    {
        id          : int
        kind        : EVRButtonId
        pressed     : bool
    }

    
[<DomainType>]
type VrDevice =
    {
        id              : int
        kind            : VrDeviceKind
        pose            : Pose
        axis            : hmap<int, Axis>
        buttons         : hmap<int, Button>
        model           : Lazy<Option<ISg>>
        startVibrate    : MicroTime -> unit
        stopVibrate     : unit -> unit
    }
    
[<DomainType>]
type VrState =
    {
        runtime         : IRuntime
        framebuffer     : IFramebufferSignature
        display         : Hmd
        devices         : hmap<int, VrDevice>
        renderTargetSize : V2i
    }

module VrState =
    let empty =
        {
            runtime = Unchecked.defaultof<_>
            framebuffer = Unchecked.defaultof<_>
            display = { name = "HMD"; pose = Pose.none }
            devices = HMap.empty
            renderTargetSize = V2i(1024, 768)
        }

[<AutoOpen>]
module Mutable =

    type MPose(initial : Pose) =
        let _current = Mod.init initial
        let _deviceToWorld = Mod.init initial.deviceToWorld
        let _velocity = Mod.init initial.velocity
        let _angularVelocity = Mod.init initial.angularVelocity
        let _isValid = Mod.init initial.isValid


        member x.Update(pose : Pose) =
            if not (System.Object.ReferenceEquals(pose, _current.Value)) then
                _current.Value <- pose
                _deviceToWorld.Value <- pose.deviceToWorld
                _velocity.Value <- pose.velocity
                _angularVelocity.Value <- pose.angularVelocity
                _isValid.Value <- pose.isValid

        member x.Current = _current :> IMod<_>
        member x.deviceToWorld = _deviceToWorld :> IMod<_>
        member x.velocity = _velocity :> IMod<_>
        member x.angularVelocity = _angularVelocity :> IMod<_>
        member x.isValid = _isValid :> IMod<_>

        static member Create(pose : Pose) = MPose(pose)
        static member Update(m : MPose, p : Pose) = m.Update p
  
    type MHmd(initial : Hmd) =
        let _current = Mod.init initial
        let _pose = MPose.Create initial.pose

        member x.Update(hmd : Hmd) =
            if not (System.Object.ReferenceEquals(_current.Value, hmd)) then
                _current.Value <- hmd
                _pose.Update(hmd.pose)

        member x.Current = _current :> IMod<_>
        member x.name = _current.Value.name
        member x.pose = _pose

        static member Update(mhmd : MHmd, hmd : Hmd) = mhmd.Update hmd
        static member Create(hmd : Hmd) = MHmd(hmd)

    type MAxis(initial : Axis) =
        let _current = Mod.init initial
        let _value = Mod.init initial.value
        let _touched = Mod.init initial.touched
        let _pressed = Mod.init initial.pressed

        member x.id = _current.Value.id
        member x.kind = _current.Value.kind
        member x.value = _value :> IMod<_>
        member x.touched = _touched :> IMod<_>
        member x.pressed = _pressed :> IMod<_>

        static member inline Create(initial : Axis) = MAxis(initial)
        static member inline Update(m : MAxis, a : Axis) = m.Update a

        member x.Update(axis : Axis) =
            if not (System.Object.ReferenceEquals(_current.Value, axis)) then
                _current.Value <- axis
                if _value.Value <> axis.value then _value.Value <- axis.value
                if _touched.Value <> axis.touched then _touched.Value <- axis.touched
                if _pressed.Value <> axis.pressed then _pressed.Value <- axis.pressed
    
    type MButton(initial : Button) =
        let _current = Mod.init initial
        let _pressed = Mod.init initial.pressed
    
        member x.id = _current.Value.id
        member x.kind = _current.Value.kind
        member x.pressed = _pressed :> IMod<_>
    
        static member inline Create(initial : Button) = MButton(initial)
        static member inline Update(m : MButton, a : Button) = m.Update a
    
        member x.Update(button : Button) =
            if not (System.Object.ReferenceEquals(_current.Value, button)) then
                _current.Value <- button
                _pressed.Value <- button.pressed

    type MVrDevice(initial : VrDevice) =
        let current = Mod.init initial

        let mkind = Mod.init initial.kind
        let maxis = MMap<int, Axis, MAxis, MAxis>(initial.axis, MAxis.Create, MAxis.Update, id)
        let mpose = MPose.Create initial.pose
        let mbuttons = MMap<int, Button, MButton, MButton>(initial.buttons, MButton.Create, MButton.Update, id)
        let mmodel = Mod.init initial.model

        let realModel = mmodel |> Mod.map (fun l -> l.Value)

        member x.id = current.Value.id
        member x.kind = mkind :> IMod<_>
        member x.Current = current :> IMod<_>
        member x.pose = mpose
        member x.axis = maxis :> amap<_,_>
        member x.buttons = mbuttons :> amap<_,_>
        member x.Model = realModel

        static member inline Create(initial : VrDevice) = MVrDevice(initial)
        static member inline Update(m : MVrDevice, c : VrDevice) = m.Update c

        member x.Update(ctrl : VrDevice) =
            if not (System.Object.ReferenceEquals(current.Value, ctrl)) then
                current.Value <- ctrl

                mkind.Value <- ctrl.kind
                mpose.Update(ctrl.pose)
                maxis.Update(ctrl.axis)
                mbuttons.Update(ctrl.buttons)
                mmodel.Value <- ctrl.model

    type IMVrState =
        abstract member Current : VrState
        abstract member display : Hmd
        abstract member devices : amap<int, MVrDevice>
        abstract member renderTargetSize : V2i
        abstract member signature : IFramebufferSignature
        abstract member runtime : IRuntime
        abstract member frameTime : IMod<MicroTime>
        abstract member fps : IMod<float>

    type MVrState(initial : VrState) =
        let current = Mod.init initial

        let _devices = MMap<int, VrDevice, MVrDevice, MVrDevice>(initial.devices, MVrDevice.Create, MVrDevice.Update, id)
        let _frameTime = Mod.init MicroTime.Zero
        let _fps = Mod.init 0.0

        member x.Current = current.Value
        member x.display = current.Value.display
        member x.devices = _devices :> amap<_,_>
        member x.renderTargetSize = current.Value.renderTargetSize

        member x.signature = current.Value.framebuffer
        member x.runtime = current.Value.runtime
        member x.frameTime = _frameTime :> IMod<_>
        member x.fps = _fps :> IMod<_>

        member x.Update(state : VrState) =
            if not (System.Object.ReferenceEquals(current.Value, state)) then
                current.Value <- state
                _devices.Update(state.devices)
        
            ()

        member x.UpdateFrameTime(time : MicroTime) =
            _frameTime.Value <- time
            _fps.Value <- (if time.TotalSeconds > 0.0 then 1.0 / time.TotalSeconds else 0.0)
           
        static member inline Create(initial : VrState) = MVrState(initial)
        static member inline Update(m : MVrState, c : VrState) = m.Update c

        interface IMVrState with
            member x.Current = x.Current
            member x.display = x.display
            member x.devices = x.devices
            member x.renderTargetSize = x.renderTargetSize
            member x.runtime = x.runtime
            member x.signature = x.signature
            member x.frameTime = x.frameTime
            member x.fps = x.fps


type VrApp<'model, 'mmodel, 'msg> =
    {
        input       : VrMessage -> list<'msg>
        view        : 'mmodel -> IMVrState -> IRuntime -> IUniformProvider -> StencilMode -> RuntimeCommand
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
        model       : IMod<'model>
        scene       : RuntimeCommand
        stop        : unit -> unit
    }

    interface IMutableVrApp with
        member x.Scene = x.scene
        member x.Input msg = x.input msg
        member x.Stop() = x.stop()

type PoseInfo =
    {
        Pose                : IMod<Trafo3d>
        Velocity            : IMod<V3d>
        AngularVelocity     : IMod<V3d>
        IsValid             : IMod<bool>
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
        state       : IMVrState
    }
    member x.wrapSg (sg : ISg) =
        let hmdLocation = x.hmd.Pose |> Mod.map (fun t -> t.Forward.C3.XYZ)

        let uniforms =
            UniformProvider.ofList [
                "ViewTrafo", x.render.viewTrafos :> IMod
                "ProjTrafo", x.render.projTrafos :> IMod
                "CameraLocation", hmdLocation :> IMod
                "LightLocation", hmdLocation :> IMod
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

        Sg.UniformApplicator(uniforms, Mod.constant sg)
        |> Sg.stencilMode (Mod.constant stencilTest)


module MutableVrApp =

    let empty =
        { new IMutableVrApp with
            member x.Scene = RuntimeCommand.Empty
            member x.Input _ = ()
            member x.Stop() = ()
        }

    let start (app : VrApp<'model, 'mmodel, 'msg>) (info : VrSystemInfo) =
        let mutable model = app.initial
        let mmodel = app.unpersist.create app.initial
        let updateLock = obj()

        
        let initialThreads = app.threads model
        let mutable currentThreads = ThreadPool.empty

        let rec emit (msg : 'msg) =
            lock updateLock (fun _ -> 
                let m =  app.update model info.state.Current msg
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
            
            currentThreads <- ThreadPool<'msg>(HMap.choose2 merge currentThreads.store newThreads.store)

        let input (msg : VrMessage) =
            app.input msg |> Seq.iter emit
            
        
        let hmdLocation = info.hmd.Pose |> Mod.map (fun t -> t.Forward.C3.XYZ)

        let uniforms =
            UniformProvider.ofList [
                "ViewTrafo", info.render.viewTrafos :> IMod
                "ProjTrafo", info.render.projTrafos :> IMod
                "CameraLocation", hmdLocation :> IMod
                "LightLocation", hmdLocation :> IMod
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
            model = mmodel :> IMod<_>
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
    abstract member Size : IMod<V2i>
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
                HMap.ofList [
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
                HMap.ofList [
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
    let system = base.System

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
                axis = d.axis |> HMap.alter ai (fun old ->
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
                buttons = d.buttons |> HMap.alter bi (fun old ->
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
                    |> HMap.ofArray
                renderTargetSize = size
            }
        )

    let mutable loaded = false
    let mutable state = { display = { name = "HMD"; pose = Pose.none }; devices = HMap.empty; renderTargetSize = V2i.Zero; runtime = base.Runtime; framebuffer = base.FramebufferSignature }
    let mstate = MVrState.Create state
    let sem = new SemaphoreSlim(1)

    let vrSystem =
        {
            signature   = base.FramebufferSignature
            hmd         = PoseInfo.ofMotionState base.Hmd.MotionState
            render      = base.Info
            bounds      = base.Chaperone
            state       = mstate
        }

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
                                    axis = d.axis |> HMap.map (fun ai a -> 
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

    let win : ModRef<Option<ISimpleRenderWindow>> = Mod.init None
    let view = win |> Mod.bind (function Some w -> w.View | None -> Mod.constant [|Trafo3d.Identity; Trafo3d.Identity|])
    let proj = win |> Mod.bind (function Some w -> w.Proj | None -> Mod.constant [|Trafo3d.Identity; Trafo3d.Identity|])
    let size = win |> Mod.bind (function Some w -> w.Sizes | None -> Mod.constant V2i.II)

    let tasks = new System.Collections.Concurrent.BlockingCollection<RuntimeCommand>(1)

    let command = Mod.init RuntimeCommand.Empty

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
            command |> Mod.map (fun c ->
                CommandRenderObject(RenderPass.main, Ag.emptyScope, c) :> IRenderObject
            )
        let sg = 
            Sg.RenderObjectNode(ASet.ofModSingle obj) :> ISg
            
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
            { new IMVrState with
                member x.Current = { VrState.empty with display = { VrState.empty.display with pose = { deviceToWorld = view.GetValue().[0].Inverse; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = true } } }
                member x.devices = AMap.empty
                member x.display = { VrState.empty.display with pose = { deviceToWorld = view.GetValue().[0].Inverse; velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = true } }
                member x.renderTargetSize = size.GetValue()
                member x.frameTime = Mod.constant (MicroTime.FromMilliseconds 10.0)
                member x.fps = Mod.constant 100.0
                member x.runtime = vulkanApp.Runtime :> IRuntime
                member x.signature = Unchecked.defaultof<_>
            }
        let size = V2i(1024, 768)
        {
            VrSystemInfo.signature = Unchecked.defaultof<_>
            VrSystemInfo.hmd =
                {
                    Pose = view |> Mod.map (fun arr -> arr.[0].Inverse)
                    Velocity = Mod.constant V3d.Zero
                    AngularVelocity = Mod.constant V3d.Zero
                    IsValid = Mod.constant true
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
            Pose = Mod.constant Trafo3d.Identity
            Velocity = Mod.constant V3d.Zero
            AngularVelocity = Mod.constant V3d.Zero
            IsValid = Mod.constant false
        }
        

    let info =
        let size = V2i(1024, 768)
        let mstate =
            { new IMVrState with
                member x.Current = VrState.empty
                member x.devices = AMap.empty
                member x.display = VrState.empty.display
                member x.renderTargetSize = size
                member x.frameTime = Mod.constant (MicroTime.FromMilliseconds 10.0)
                member x.fps = Mod.constant 100.0
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
                    viewTrafos = Mod.constant [| Trafo3d.Identity |]
                    projTrafos = Mod.constant [| Trafo3d.Identity |]
                }
            VrSystemInfo.state = mstate
        }

    let statistics = EventSource<VrSystemStats>()

    member x.Statistics = statistics :> IEvent<_>
    
    interface IVrApplication with
        member x.Start m = { new IDisposable with member x.Dispose() = () }
        member x.SystemInfo = info
        member x.Runtime = x.Runtime :> IRuntime
        member x.Size = Mod.constant V2i.II
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
