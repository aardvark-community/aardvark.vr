open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Giraffe
open Aardvark.Vr
open Aardium
open Demo

[<EntryPoint>]
let main argv =
    Aardvark.Init()
    Aardium.Init()

    use app = VRApplication.create VRDisplay.Fake 8 false
    use mapp = ComposedApp.start app Demo.app
    
    Server.startLocalhost 4321 mapp.CancellationToken [
        MutableApp.toWebPart app.Runtime mapp
    ] |> ignore

    Aardium.run {
        url "http://localhost:4321"
#if DEBUG
        debug true
        log (fun msg -> Report.Line(2, $"[Aardium] {msg}"))
#else
        debug false
#endif
    }

    0
