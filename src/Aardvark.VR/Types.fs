namespace Aardvark.Vr

open Valve.VR

open Adaptify
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.SceneGraph

[<ModelType>]
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


[<ModelType>]
type Hmd =
    {
        name        : string
        pose        : Pose
    }
    
[<ModelType>]
type Axis =
    {
        id          : int
        kind        : VrAxisKind
        value       : V2d
        touched     : bool
        pressed     : bool
    }
    
[<ModelType>]
type Button =
    {
        id          : int
        kind        : EVRButtonId
        pressed     : bool
    }

    
[<ModelType>]
type VrDevice =
    {
        id              : int
        kind            : VrDeviceKind
        pose            : Pose
        axis            : HashMap<int, Axis>
        buttons         : HashMap<int, Button>
        model           : Lazy<Option<ISg>>
        startVibrate    : MicroTime -> unit
        stopVibrate     : unit -> unit
    }


[<ModelType>]
type VrState =
    {
        [<NonAdaptive>]
        runtime         : IRuntime
        [<NonAdaptive>]
        framebuffer     : IFramebufferSignature
        display         : Hmd
        devices         : HashMap<int, VrDevice>
        [<NonAdaptive>]
        renderTargetSize : V2i
    } 



module VrState =
    let empty =
        {
            runtime = Unchecked.defaultof<_>
            framebuffer = Unchecked.defaultof<_>
            display = { name = "HMD"; pose = Pose.none }
            devices = HashMap.empty
            renderTargetSize = V2i(1024, 768)
        }
