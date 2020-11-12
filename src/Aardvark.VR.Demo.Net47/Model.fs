namespace Demo

open Aardvark.Base
open Aardvark.Vr

open FSharp.Data.Adaptive
open Adaptify

[<ModelType>]
type Model =
    {
        text    : string
        vr      : bool
    }

