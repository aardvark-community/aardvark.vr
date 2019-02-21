namespace Aardvark.Vr

open Valve.VR

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

//type MotionState =
//    {
//        pose            : IMod<Trafo3d>
//        velocity        : IMod<V3d>
//        angularVelocity : IMod<V3d>
//    }



type VrDeviceKind =
    | Hmd = 0
    | Controller = 1
    | BaseStation = 2
    | Unknown = 3

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
        display         : Hmd
        devices         : hmap<int, VrDevice>
        renderTargetSize : V2i
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
    
    member x.Update(button : Button) =
        if not (System.Object.ReferenceEquals(_current.Value, button)) then
            _current.Value <- button
            _pressed.Value <- button.pressed

type MVrDevice(initial : VrDevice) =
    let maxis = initial.axis |> HMap.map (constF MAxis.Create)
    let mpose = MPose.Create initial.pose
    let mbuttons = initial.buttons |> HMap.map (constF MButton.Create)
    let current = Mod.init initial

    member x.kind = current.Value.kind
    member x.Current = current :> IMod<_>
    member x.id = current.Value.id
    member x.pose = mpose
    member x.axis = maxis
    member x.buttons = mbuttons
    member x.Model = initial.model.Value

    static member inline Create(initial : VrDevice) = MVrDevice(initial)
    static member inline Update(m : MVrDevice, c : VrDevice) = m.Update c

    member x.Update(ctrl : VrDevice) =
        if not (System.Object.ReferenceEquals(current.Value, ctrl)) then
            current.Value <- ctrl

            mpose.Update(ctrl.pose)

            for (k,v) in HMap.toSeq ctrl.axis do
                match HMap.tryFind k maxis with 
                | Some ma -> ma.Update(v)
                | None -> ()

            for (k,v) in HMap.toSeq ctrl.buttons do
                match HMap.tryFind k mbuttons with 
                | Some ma -> ma.Update(v)
                | None -> ()

type MVrState(initial : VrState, runtime : IRuntime, signature : IFramebufferSignature) =
    let current = Mod.init initial

    let _devices = MMap<int, VrDevice, MVrDevice, MVrDevice>(initial.devices, MVrDevice.Create, MVrDevice.Update, id)
    let _frameTime = Mod.init MicroTime.Zero
    let _fps = Mod.init 0.0

    member x.display = current.Value.display
    member x.devices = _devices :> amap<_,_>
    member x.renderTargetSize = current.Value.renderTargetSize

    member x.signature = signature
    member x.runtime = runtime
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
//            
//    static member inline Create(initial : VrState) = MVrState(initial)
    static member inline Update(m : MVrState, c : VrState) = m.Update c





type Unpersist<'model, 'mmodel> =
    {
        create : 'model -> 'mmodel
        update : 'mmodel -> 'model -> unit
    }

module Unpersist =
    let inline instance<'model, 'mmodel when 'mmodel : (static member Create : 'model -> 'mmodel) and 'mmodel : (member Update : 'model -> unit)> =
        {
            create = fun m -> (^mmodel : (static member Create : 'model -> 'mmodel) (m))
            update = fun mm m -> (^mmodel : (member Update : 'model -> unit) (mm, m))
        }

type VrApp<'model, 'mmodel, 'msg> =
    {
        input       : VrMessage -> list<'msg>
        view        : 'mmodel -> MVrState -> IRuntime -> IUniformProvider -> StencilMode -> RuntimeCommand
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
        getState    : unit -> VrState
        bounds      : Option<Polygon3d>
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
        let mutable state = info.getState()
        let mmodel = app.unpersist.create app.initial
        let mstate = MVrState(state, info.signature.Runtime, info.signature)
        let updateLock = obj()

        
        let initialThreads = app.threads model
        let mutable currentThreads = ThreadPool.empty

        let rec emit (msg : 'msg) =
            lock updateLock (fun _ -> 
                state <- info.getState()
                let m =  app.update model state msg
                model <- m
                transact (fun () -> 
                    mstate.Update state
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

        let scene = app.view mmodel mstate info.signature.Runtime uniforms stencilTest

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

type IVrApplication =
    inherit IDisposable
    abstract member Runtime : IRuntime
    abstract member Size : IMod<V2i>
    abstract member Start : IMutableVrApp -> IDisposable
    abstract member SystemInfo : VrSystemInfo

[<AbstractClass; Sealed; Extension>]
type IVrApplicationExtensions private() =
    [<Extension>]
    static member Start(this : IVrApplication, app : VrApp<'model, 'mmodel, 'msg>) =
        let mapp = MutableVrApp.start app this.SystemInfo
        this.Start mapp

        
type VulkanVRApplication(samples : int, debug : bool) =
    inherit VulkanVRApplicationLayered(samples, debug)
    
    let mutable currentApp = MutableVrApp.empty
        

    static let sw = System.Diagnostics.Stopwatch.StartNew()
   
    static let toDeviceKind (clazz : ETrackedDeviceClass) =
        match clazz with
            | ETrackedDeviceClass.HMD -> VrDeviceKind.Hmd
            | ETrackedDeviceClass.Controller -> VrDeviceKind.Controller
            | ETrackedDeviceClass.TrackingReference -> VrDeviceKind.BaseStation
            | _ -> VrDeviceKind.Unknown

    static let toAxisKind (t : EVRControllerAxisType) =
        match t with
            | EVRControllerAxisType.k_eControllerAxis_Trigger -> VrAxisKind.Trigger
            | EVRControllerAxisType.k_eControllerAxis_TrackPad -> VrAxisKind.Trackpad
            | EVRControllerAxisType.k_eControllerAxis_Joystick -> VrAxisKind.Joystick
            | _ -> VrAxisKind.None

    static let getDevice (system : CVRSystem) (controllers : Aardvark.Application.OpenVR.VrDevice[]) (pulses : Dict<int, MicroTime>) (i : int) =
        let deviceType = system.GetTrackedDeviceClass (uint32 i)
        let kind = toDeviceKind deviceType

        let model =
            lazy (
                match controllers |> Array.tryFind (fun d -> d.Index = i) with   
                    | Some c -> c.Model
                    | _ -> None
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



    let pulses = Dict<int, MicroTime>()
    let mutable numFrames = 0
    let mutable totalTime = MicroTime.Zero
    let system = base.System
    
    let devices =
        let controllers = base.Controllers
        [|
            for i in 0 .. int OpenVR.k_unMaxTrackedDeviceCount-1 do
                yield getDevice system controllers pulses i
        |]

    let updateAxis (ci : int) (ai : int) (f : Axis -> Axis) =
        devices.[ci] <- 
            { devices.[ci] with 
                axis = devices.[ci].axis |> HMap.alter ai (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )
                        
            }

    let updateButton (ci : int) (bi : int) (f : Button -> Button) =
        devices.[ci] <- 
            { devices.[ci] with 
                buttons = devices.[ci].buttons |> HMap.alter bi (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )
                        
            }

    let getAxisIndex (b : VREvent_Controller_t) =
        let d = int b.button - int EVRButtonId.k_EButton_Axis0
        if d >= 0 && d < int OpenVR.k_unControllerStateAxisCount then
            Some d 
        else
            None

    let getState() =
        let hmd = devices |> Array.find (fun d -> d.kind = VrDeviceKind.Hmd)
        let mutable w = 0u
        let mutable h= 0u
        system.GetRecommendedRenderTargetSize(&w,&h)
        { 
            display = { name = "HMD"; pose = hmd.pose }
            devices = 
                devices |> Array.choose (fun d -> 
                    if d.kind = VrDeviceKind.Controller then
                        Some (d.id, d)      
                    else
                        None 
                )
                |> HMap.ofArray
            renderTargetSize = V2i(int w,int h)
        }

    let mutable loaded = false
    let mutable state = { display = { name = "HMD"; pose = Pose.none }; devices = HMap.empty; renderTargetSize = V2i.Zero }
    let mstate = MVrState(state, base.Runtime, base.FramebufferSignature)
    let sem = new SemaphoreSlim(1)

    let vrSystem =
        {
            signature   = base.FramebufferSignature
            hmd         = PoseInfo.ofMotionState base.Hmd.MotionState
            render      = base.Info
            getState    = getState
            bounds      = base.Chaperone
        }

    member x.SystemInfo = vrSystem

    member x.SetApp(app : IMutableVrApp) =
        lock x (fun () ->
            currentApp <- app
            x.RenderTask <- app.Scene
        )
        
    member x.State = getState()

    override x.Render() =
        let start = sw.MicroTime

        base.Render()

        let now = sw.MicroTime

        let dt = now - start
        numFrames <- numFrames + 1
        totalTime <- totalTime + dt
        
        if numFrames >= 30 then
            transact (fun () ->
                mstate.UpdateFrameTime(totalTime / numFrames)
            )
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
        mstate.Update state
        loaded <- true
        res

    override x.UpdatePoses(poses : TrackedDevicePose_t[]) =
        if loaded then
            if poses.Length <> devices.Length then
                Log.warn "[OpenVR] bad pose count: { is: %A; should: %A }" poses.Length devices.Length

            let cnt = min poses.Length devices.Length
            transact (fun () ->
                let changes = System.Collections.Generic.List<VrMessage>()
                for i in 0 .. cnt - 1 do
                    let mutable state = Unchecked.defaultof<VRControllerState_t>

                    if system.GetControllerState(uint32 i, &state, uint32 sizeof<VRControllerState_t>) then
                        let value (ai : int) =
                            match ai with
                                | 0 -> V2d(state.rAxis0.x, state.rAxis0.y)
                                | 1 -> V2d(state.rAxis1.x, state.rAxis1.y)
                                | 2 -> V2d(state.rAxis2.x, state.rAxis2.y)
                                | 3 -> V2d(state.rAxis3.x, state.rAxis3.y)
                                | 4 -> V2d(state.rAxis4.x, state.rAxis4.y)
                                | _ -> V2d.Zero

                        devices.[i] <- { 
                            devices.[i] with
                                axis = devices.[i].axis |> HMap.map (fun ai a -> 
                                    let n = value ai
                                    if a.value <> n then
                                        changes.Add(ValueChange(i, ai, n))
                                        { a with value = n }
                                    else
                                        a
                                ) 
                        }
                    let pose = Pose.ofOpenVR poses.[i]
                    devices.[i] <- { devices.[i] with pose = pose}
                    changes.Add(UpdatePose(i, pose))

                
                if changes.Count > 0 then changes |> Seq.iter currentApp.Input
            )

    override x.ProcessEvent(evt : VREvent_t) =
        if loaded then
            
            let eventType = evt.eventType |> int |> unbox<EVREventType>
            match eventType with
                | EVREventType.VREvent_TrackedDeviceActivated ->
                    let id = int evt.trackedDeviceIndex

                    // update the device-kind
                    let kind = system.GetTrackedDeviceClass(evt.trackedDeviceIndex) |> toDeviceKind
                    if devices.[id].kind <> kind then
                        devices.[id] <- getDevice system x.Controllers pulses id

                    if kind = VrDeviceKind.Controller then
                        currentApp.Input (ControllerConnected id)

                | EVREventType.VREvent_TrackedDeviceDeactivated ->
                    let id = int evt.trackedDeviceIndex
                    let oldKind = devices.[id].kind
                    if oldKind <> VrDeviceKind.Unknown then
                        devices.[id] <- { devices.[id] with kind = VrDeviceKind.Unknown }
                       
                    if oldKind =  VrDeviceKind.Controller then
                        currentApp.Input (ControllerDisconnected id)


                | EVREventType.VREvent_ButtonTouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = true })
                            //Log.line "[Aardvark.VR.App] Touching Axis %A" ai
                            currentApp.Input (Touch(ci, ai))
                        | Some _ when ci = -1 ->
                          Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | _ -> ()

                | EVREventType.VREvent_ButtonUntouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = false })
                            currentApp.Input (Untouch(ci, ai))
                        | Some _ when ci = -1 ->
                            Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | _ -> ()
                    
                | EVREventType.VREvent_ButtonPress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = true })
                            //Log.info "[Aardvark.VR.App] Pressing Axis %A" ai
                            currentApp.Input (Press(ci, ai))
                        | Some _ ->
                          Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | None ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = true })
                            Log.error "[Aardvark.VR.App] Pressing Button %A" bi
                            currentApp.Input (PressButton(ci, bi))
                            ()
                    
                | EVREventType.VREvent_ButtonUnpress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = false })
                            currentApp.Input (Unpress(ci, ai))
                        | Some _ ->
                          Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | None ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = false })
                            currentApp.Input (UnpressButton(ci, bi))

                | _ ->
                    ()



        ()
        
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

open Aardvark.Application.Utilities
type VulkanFakeVrApplication(samples : int, debug : bool) =
    //let app = new VulkanApplication(debug)

    let boot = new SemaphoreSlim(0)
    let mutable win = Unchecked.defaultof<_>
    let tasks = new System.Collections.Concurrent.BlockingCollection<RuntimeCommand>(1)

    let command = Mod.init RuntimeCommand.Empty


    let run() =
        let w = 
            let s = samples
            let d = debug
            window {
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
        win <- w
        boot.Release() |> ignore
        
        w.Run()

        w.Dispose()

    let uiThread = 
        let t = new Thread(ThreadStart(run), IsBackground = true, ApartmentState = ApartmentState.STA)
        t.Start()
        boot.Wait()
        boot.Dispose()
        t
    



    let view = win.View
    let proj = win.Proj

    let hmd : PoseInfo =
        {
            Pose = view |> Mod.map (fun arr -> arr.[0].Inverse)
            Velocity = Mod.constant V3d.Zero
            AngularVelocity = Mod.constant V3d.Zero
            IsValid = Mod.constant true
        }
        

    let info =
        let size = V2i(1024, 768)
        {
            VrSystemInfo.signature = Unchecked.defaultof<_>
            VrSystemInfo.hmd = hmd
            VrSystemInfo.render =
                {
                    framebufferSize = size
                    viewTrafos = view
                    projTrafos = proj
                }
            VrSystemInfo.getState = fun () ->
                {
                    VrState.devices = HMap.empty
                    VrState.display = { name = "Window"; pose = { Pose.deviceToWorld = view.GetValue().[0].Inverse; Pose.angularVelocity = V3d.Zero; Pose.velocity = V3d.Zero; Pose.isValid = true }}
                    VrState.renderTargetSize = size
                }
            VrSystemInfo.bounds = None
        }

    member x.SystemInfo = info

    member x.Start(mapp : IMutableVrApp) =
        transact (fun () -> command.Value <- mapp.Scene)
        { new IDisposable with
            member x.Dispose() =
                transact (fun () -> command.Value <- RuntimeCommand.Empty)
        }


    member x.Dispose() =
        tasks.CompleteAdding()
        uiThread.Join()

    member x.Runtime = win.Runtime
    member x.Size = win.Sizes 

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IVrApplication with
        member x.SystemInfo = x.SystemInfo
        member x.Start m = x.Start m
        member x.Runtime = win.Runtime
        member x.Size = win.Sizes 


type VulkanNoVrApplication(debug : bool) =
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
            VrSystemInfo.getState = fun () ->
                {
                    VrState.devices = HMap.empty
                    VrState.display = { name = "Window"; pose = { Pose.deviceToWorld = Trafo3d.Identity; Pose.angularVelocity = V3d.Zero; Pose.velocity = V3d.Zero; Pose.isValid = false }}
                    VrState.renderTargetSize = size
                }
        }
    
    interface IVrApplication with
        member x.Start m = { new IDisposable with member x.Dispose() = () }
        member x.SystemInfo = info
        member x.Runtime = x.Runtime :> IRuntime
        member x.Size = Mod.constant V2i.II


[<RequireQualifiedAccess>]
type VRDisplay =
    | None
    | Fake
    | OpenVR of factor : float


module VRApplication =
    let create (display : VRDisplay) (samples : int) (debug : bool) =
        match display with
        | VRDisplay.None ->
            new VulkanNoVrApplication(debug) :> IVrApplication
        | VRDisplay.Fake ->
            new VulkanFakeVrApplication(samples, debug) :> IVrApplication
        | VRDisplay.OpenVR factor ->
            try new VulkanVRApplication(samples, debug) :> IVrApplication
            with _ -> new VulkanFakeVrApplication(samples, debug, ScaleFactor = factor) :> IVrApplication



type VulkanVRApplicationElm<'model, 'mmodel, 'msg>(app : VrApp<'model, 'mmodel, 'msg>, samples : int, debug : bool) =
    inherit VulkanVRApplicationLayered(samples, debug)

    let initial = app.initial
    let mutable model = initial
    let mmodel = app.unpersist.create model

    let initialThreads = app.threads initial
    let mutable currentThreads = initialThreads

    let updateLock = obj()

    let mutable loaded = false
    let mutable state = { display = Unchecked.defaultof<Hmd>; devices = HMap.empty; renderTargetSize = V2i.Zero }
    let mutable mstate = Unchecked.defaultof<MVrState>
    let mutable system = Unchecked.defaultof<CVRSystem>

    let mutable states : MotionState[] = [||]
    let mutable devices : VrDevice[] = [||]

    let sw = System.Diagnostics.Stopwatch.StartNew()
   
    let toDeviceKind (clazz : ETrackedDeviceClass) =
        match clazz with
            | ETrackedDeviceClass.HMD -> VrDeviceKind.Hmd
            | ETrackedDeviceClass.Controller -> VrDeviceKind.Controller
            | ETrackedDeviceClass.TrackingReference -> VrDeviceKind.BaseStation
            | _ -> VrDeviceKind.Unknown


    let toAxisKind (t : EVRControllerAxisType) =
        match t with
            | EVRControllerAxisType.k_eControllerAxis_Trigger -> VrAxisKind.Trigger
            | EVRControllerAxisType.k_eControllerAxis_TrackPad -> VrAxisKind.Trackpad
            | EVRControllerAxisType.k_eControllerAxis_Joystick -> VrAxisKind.Joystick
            | _ -> VrAxisKind.None


    let getState() =
        let hmd = devices |> Array.find (fun d -> d.kind = VrDeviceKind.Hmd)
        let mutable w = 0u
        let mutable h= 0u
        system.GetRecommendedRenderTargetSize(&w,&h)
        { 
            display = { name = "HMD"; pose = hmd.pose }
            devices = 
                devices |> Array.choose (fun d -> 
                    if d.kind = VrDeviceKind.Controller then
                        Some (d.id, d)      
                    else
                        None 
                )
                |> HMap.ofArray
            renderTargetSize = V2i(int w,int h)
        }
        
    let rec emit (msg : 'msg) =
        lock updateLock (fun _ -> 
            state <- getState()
            let m =  app.update model state msg
            model <- m
            transact (fun () -> 
                mstate.Update state
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

    let update (msg : seq<VrMessage>) =
        lock updateLock (fun _ -> 
            state <- getState()
            let msg = msg |> Seq.toList |> List.collect app.input
            let m = msg |> List.fold (fun s m -> app.update s state m) model
            model <- m
            let newThreads = app.threads model
            adjustThreads newThreads
            transact (fun () -> 
                mstate.Update state
                app.unpersist.update mmodel m
            )
        )


    let updateAxis (ci : int) (ai : int) (f : Axis -> Axis) =
        devices.[ci] <- 
            { devices.[ci] with 
                axis = devices.[ci].axis |> HMap.alter ai (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )
                        
            }


    let updateButton (ci : int) (bi : int) (f : Button -> Button) =
        devices.[ci] <- 
            { devices.[ci] with 
                buttons = devices.[ci].buttons |> HMap.alter bi (fun old ->
                    match old with
                        | Some old ->
                            Some (f old)
                        | None ->
                            None
                )
                        
            }

    let getAxisIndex (b : VREvent_Controller_t) =
        let d = int b.button - int EVRButtonId.k_EButton_Axis0
        if d >= 0 && d < int OpenVR.k_unControllerStateAxisCount then
            Some d 
        else
            None

    let pulses = Dict<int, MicroTime>()

    let mutable numFrames = 0
    let mutable totalTime = MicroTime.Zero

    let getDevice (x : VulkanVRApplicationElm<'model, 'mmodel, 'msg>) (i : int) =
        let deviceType = system.GetTrackedDeviceClass (uint32 i)
        let kind = toDeviceKind deviceType

        let model =
            lazy (
                match x.Controllers |> Array.tryFind (fun d -> d.Index = i) with   
                    | Some c -> c.Model
                    | _ -> None
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

    member private x.SetRenderTask() =
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

        let hmdLocation = x.Hmd.MotionState.Pose |> Mod.map (fun t -> t.Forward.C3.XYZ)

        let uniforms =
            UniformProvider.ofList [
                "ViewTrafo", x.Info.viewTrafos :> IMod
                "ProjTrafo", x.Info.projTrafos :> IMod
                "CameraLocation", hmdLocation :> IMod
                "LightLocation", hmdLocation :> IMod
            ]

//        let wrap (sg : ISg) =
//            let sg =   
//                sg
//                |> Sg.stencilMode (Mod.constant stencilTest)
//                |> Sg.uniform "ViewTrafo" x.Info.viewTrafos
//                |> Sg.uniform "ProjTrafo" x.Info.projTrafos
//                |> Sg.uniform "CameraLocation" hmdLocation
//                |> Sg.uniform "LightLocation" hmdLocation
//
//            sg?Runtime <- x.Runtime :> IRuntime
//            sg

        let cmd = app.view mmodel mstate (x.Runtime :> IRuntime) uniforms stencilTest
        x.RenderTask <- cmd


    member x.MutableModel = mmodel
    member x.State = getState()
    member x.UpdateLock = updateLock
    member x.Emit (msg : seq<'msg>) =
        lock updateLock (fun () ->
            state <- getState()
            let m = msg |> Seq.fold (fun s m -> app.update s state m) model
            model <- m
            transact (fun () -> 
                mstate.Update state
                app.unpersist.update mmodel m
            )
        )



    override x.Render() =
        let start = sw.MicroTime

        base.Render()

        let now = sw.MicroTime

        let dt = now - start
        numFrames <- numFrames + 1
        totalTime <- totalTime + dt



        if numFrames >= 30 then
            transact (fun () ->
                mstate.UpdateFrameTime(totalTime / numFrames)
            )
            numFrames <- 0
            totalTime <- MicroTime.Zero


        for (KeyValue(c, time)) in Seq.toList pulses do
            if time > now then
                system.TriggerHapticPulse(uint32 c, 0u, char 65535us)
            else
                pulses.Remove c |> ignore



    override x.OnLoad(info) =
        let res = base.OnLoad(info)

        system <- x.System

        devices <-
            [|
                for i in 0 .. int OpenVR.k_unMaxTrackedDeviceCount-1 do
                    yield getDevice x i
            |]

        state <- getState()
        mstate <- MVrState(state, x.Runtime, x.FramebufferSignature)

        x.SetRenderTask()

        loaded <- true
        res

    override x.UpdatePoses(poses : TrackedDevicePose_t[]) =
        if loaded then
            if poses.Length <> devices.Length then
                Log.warn "[OpenVR] bad pose count: { is: %A; should: %A }" poses.Length devices.Length

            let cnt = min poses.Length devices.Length
            transact (fun () ->
                let changes = System.Collections.Generic.List<VrMessage>()
                for i in 0 .. cnt - 1 do
                    let mutable state = Unchecked.defaultof<VRControllerState_t>

                    if system.GetControllerState(uint32 i, &state, uint32 sizeof<VRControllerState_t>) then
                        let value (ai : int) =
                            match ai with
                                | 0 -> V2d(state.rAxis0.x, state.rAxis0.y)
                                | 1 -> V2d(state.rAxis1.x, state.rAxis1.y)
                                | 2 -> V2d(state.rAxis2.x, state.rAxis2.y)
                                | 3 -> V2d(state.rAxis3.x, state.rAxis3.y)
                                | 4 -> V2d(state.rAxis4.x, state.rAxis4.y)
                                | _ -> V2d.Zero

                        devices.[i] <- { 
                            devices.[i] with
                                axis = devices.[i].axis |> HMap.map (fun ai a -> 
                                    let n = value ai
                                    if a.value <> n then
                                        changes.Add(ValueChange(i, ai, n))
                                        { a with value = n }
                                    else
                                        a
                                ) 
                        }
                    let pose = Pose.ofOpenVR poses.[i]
                    devices.[i] <- { devices.[i] with pose = pose}
                    changes.Add(UpdatePose(i, pose))


                if changes.Count > 0 then update changes
            )

    override x.ProcessEvent(evt : VREvent_t) =
        if loaded then
            
            let eventType = evt.eventType |> int |> unbox<EVREventType>
            match eventType with
                | EVREventType.VREvent_TrackedDeviceActivated ->
                    let id = int evt.trackedDeviceIndex

                    // update the device-kind
                    let kind = system.GetTrackedDeviceClass(evt.trackedDeviceIndex) |> toDeviceKind
                    if devices.[id].kind <> kind then
                        devices.[id] <- getDevice x id

                    if kind = VrDeviceKind.Controller then
                        update [ControllerConnected id]

                | EVREventType.VREvent_TrackedDeviceDeactivated ->
                    let id = int evt.trackedDeviceIndex
                    let oldKind = devices.[id].kind
                    if oldKind <> VrDeviceKind.Unknown then
                        devices.[id] <- { devices.[id] with kind = VrDeviceKind.Unknown }
                       
                    if oldKind =  VrDeviceKind.Controller then
                        update [ControllerDisconnected id]


                | EVREventType.VREvent_ButtonTouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = true })
                            //Log.line "[Aardvark.VR.App] Touching Axis %A" ai
                            update [Touch(ci, ai)]
                        | Some _ when ci = -1 ->
                          Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | _ -> ()

                | EVREventType.VREvent_ButtonUntouch ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with touched = false })
                            update [Untouch(ci, ai)]
                        | Some _ when ci = -1 ->
                            Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | _ -> ()
                    
                | EVREventType.VREvent_ButtonPress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = true })
                            //Log.info "[Aardvark.VR.App] Pressing Axis %A" ai
                            update [Press(ci, ai)]
                        | Some _ ->
                          Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | None ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = true })
                            Log.error "[Aardvark.VR.App] Pressing Button %A" bi
                            update [PressButton(ci, bi)]
                            ()
                    
                | EVREventType.VREvent_ButtonUnpress ->
                    let ci = int evt.trackedDeviceIndex
                    match getAxisIndex evt.data.controller with
                        | Some ai when ci >= 0 -> 
                            updateAxis ci ai (fun old -> { old with pressed = false })
                            update [Unpress(ci, ai)]
                        | Some _ ->
                          Log.error "[Aardvark.VR.App] out of bounds controller index: %A" ci
                        | None ->
                            let bi = int evt.data.controller.button
                            updateButton ci bi (fun old -> { old with pressed = false })
                            update [UnpressButton(ci, bi)]

                | _ ->
                    ()



        ()

open Aardvark.Application
open Aardvark.Application.Slim
type FakeVRApplicationElm<'model, 'mmodel, 'msg>(app : VrApp<'model, 'mmodel, 'msg>, samples : int, debug : bool) =
    
    let vapp = new VulkanApplication(debug)
    let win = vapp.CreateGameWindow(samples)

    let mutable model = app.initial
    let mmodel = app.unpersist.create model


    member x.Run() =
        let initialCam = CameraView.lookAt (V3d(0.0, 0.0, 1.8)) (V3d(0.0, -1.0, 1.8)) V3d.OOI
        let view = initialCam |> DefaultCameraController.control win.Mouse win.Keyboard win.Time |> Mod.map CameraView.viewTrafo
        let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 90.0 0.1 1000.0 (float s.X / float s.Y)) |> Mod.map Frustum.projTrafo

        let location = view |> Mod.map (fun (t : Trafo3d) -> t.GetViewPosition())
        let getVrState() =
            {
                display = 
                    {
                        name = "Screen"
                        pose = { deviceToWorld = view.GetValue(); velocity = V3d.Zero; angularVelocity = V3d.Zero; isValid = true }
                    }

                devices = HMap.empty
                renderTargetSize = win.Sizes |> Mod.force
            }

        let mstate = MVrState(getVrState(), vapp.Runtime, win.FramebufferSignature)


        let uniforms =
            UniformProvider.ofList [
                "ViewTrafo", view :> IMod
                "ProjTrafo", proj :> IMod
                "CameraLocation", location :> IMod
                "LightLocation", location :> IMod
            ]

        let cmd = app.view mmodel mstate (vapp.Runtime :> IRuntime) uniforms StencilMode.Disabled

        

        win.Mouse.Move.Values.Add (fun _ ->
            let pp = win.Mouse.Position |> Mod.force
            let n = pp.NormalizedPosition

            let ndc = V3d(2.0 * n.X - 1.0, 1.0 - 2.0 * n.Y, 0.0)
            let proj = proj.GetValue()
            let view = view.GetValue()


            let world = ndc |> proj.Backward.TransformPosProj |> view.Backward.TransformPos
            let cam = view.GetViewPosition()

            let dir = Vec.normalize (world - cam)
            let origin = cam

            let ray = Ray3d(origin, dir)


            let n = app.input (VrMessage.PointTo ray) |> List.fold (fun m e -> app.update m (getVrState()) e) model
            model <- n
            transact (fun () ->
                app.unpersist.update mmodel n
            )
        )


        win.RenderTask <- new Aardvark.Rendering.Vulkan.Temp.CommandTask(vapp.Runtime.Device, unbox win.FramebufferSignature, cmd)
        win.Run()
        








[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VrApp =

    let run (samples : int) (debug : bool) (app : VrApp<'model, 'mmodel, 'msg>) =
        let app = new VulkanVRApplicationElm<'model, 'mmodel, 'msg>(app, samples, debug)

        app.Run()

        
    let runFake (samples : int) (debug : bool) (app : VrApp<'model, 'mmodel, 'msg>) =
        let app = new FakeVRApplicationElm<'model, 'mmodel, 'msg>(app, samples, debug)

        app.Run()
