namespace Demo

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Vr

[<DomainType>]
type Model =
    {
        text    : string
        vr      : bool
    }

