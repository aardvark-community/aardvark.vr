open Aardvark.Base
open Aardvark.UI
open Aardvark.Vr
open Aardium
open Demo
open Suave

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    let app = VRApplication.create (VRDisplay.OpenVR 1.0) 8 false
    let mapp = ComposedApp.start app Demo.app
    
    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart app.Runtime mapp
    ] |> ignore
    
    Aardium.run {
        url "http://localhost:4321"
    }

    0
