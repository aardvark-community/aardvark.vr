namespace Demo

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Demo

[<AutoOpen>]
module Mutable =

    
    
    type MModel(__initial : Demo.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Model> = Aardvark.Base.Incremental.EqModRef<Demo.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Model>
        let _text = ResetMod.Create(__initial.text)
        
        member x.text = _text :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_text,v.text)
                
        
        static member Create(__initial : Demo.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : Demo.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let text =
                { new Lens<Demo.Model, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
