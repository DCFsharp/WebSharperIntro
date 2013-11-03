namespace DCFSharp.Oct2013

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<AutoOpen>]
[<JavaScript>]
module Vectors2D =

    let private sq x = x * x

    type Vec2D =
        {
            X : float
            Y : float
        }

        static member Zero =
            { X = 0.0; Y = 0.0 }

        static member Norm(v) =
            sqrt (sq v.X + sq v.Y)

        static member Average(vs) =
            let mutable x = 0.
            let mutable y = 0.
            let mutable n = 0
            for v in vs do
                x <- x + v.X
                y <- y + v.Y
                n <- n + 1
            { X = x / float n; Y = y / float n }

        static member Sum(vs) =
            let mutable x = 0.
            let mutable y = 0.
            for v in vs do
                x <- x + v.X
                y <- y + v.Y
            { X = x; Y = y }

    let ( +^ ) a b =
        { X = a.X + b.X; Y = a.Y + b.Y }

    let ( -^ ) a b =
        { X = a.X - b.X; Y = a.Y - b.Y }

    let ( *^ ) k v =
        { X = k * v.X; Y = k * v.Y }

    type Vec2D with

        static member Distance(a, b) =
            Vec2D.Norm (a -^ b)
