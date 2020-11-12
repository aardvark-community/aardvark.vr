open Aardvark.Base
open Aardvark.UI
open Aardvark.Vr
open Aardium
open Demo
open Suave
open Aardvark.Cef

[<EntryPoint>]
let main argv =

    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Chromium.init()

    Aardvark.Init()
    Aardium.init()

    let app = VRApplication.create' (VRDisplay.OpenVR 1.0) Aardvark.Application.Backend.GL 8 false
    let mapp = ComposedApp.start app (Demo.app app.Runtime)
   
    
    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart app.Runtime mapp
    ] |> ignore
    
    Aardium.run {
        url "http://localhost:4321"
    }

    0
