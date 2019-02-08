namespace Demo

open Aardvark.Base
open Aardvark.Base.Incremental

[<DomainType>]
type Model =
    {
        text    : string
        vr      : bool
    }

