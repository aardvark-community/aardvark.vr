// Learn more about F# at http://fsharp.org

open System
open Aardvark.Vr
open Aardvark.Base
open Aardium
open Aardvark.UI
open Demo
open Suave

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()
    Aardium.init()

    let app = new VulkanVRApplication(1, false)
    let mapp = ComposedApp.start app Demo.app
    
    WebPart.startServer 4321 [
        MutableApp.toWebPart app.Runtime mapp
    ]
    
    Aardium.run {
        url "http://localhost:4321"
    }

    0
