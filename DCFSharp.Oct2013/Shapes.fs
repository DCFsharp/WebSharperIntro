namespace DCFSharp.Oct2013

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<JavaScript>]
module Shapes =
    open System

    type T =
        private {
            Draw : CanvasRenderingContext2D -> unit
        }

    let Draw t (ctx: Ctx2D) =
        ctx.Save()
        t.Draw ctx
        ctx.Restore()

    let Define draw =
        { Draw = draw }

    let Fill style sh =
        Define <| fun ctx ->
            sh.Draw ctx
            ctx.FillStyle <- style
            ctx.Fill()

    let Triangle =
        let pi = Math.PI
        let da = 2.0 * pi / 3.0
        let a1 = pi / 2.0
        let a2 = a1 + da
        let a3 = a2 + da
        let p a = (cos a, sin a)
        let (x1, y1) = p a1
        let (x2, y2) = p a2
        let (x3, y3) = p a3
        Define <| fun ctx ->
            ctx.BeginPath()
            ctx.MoveTo(x1, y1)
            ctx.LineTo(x2, y2)
            ctx.LineTo(x3, y3)
            ctx.LineTo(x1, y1)
            ctx.ClosePath()

    let ScaleX k sh =
        Define <| fun ctx ->
            ctx.Scale(k, 1.0)
            sh.Draw ctx

    let ScaleY k sh =
        Define <| fun ctx ->
            ctx.Scale(1.0, k)
            sh.Draw ctx

    let Scale v sh =
        Define <| fun ctx ->
            ctx.Scale(v.X, v.Y)
            sh.Draw ctx

    let TranslateX d sh =
        Define <| fun ctx ->
            ctx.Translate(d, 0.0)
            sh.Draw ctx

    let TranslateY d sh =
        Define <| fun ctx ->
            ctx.Translate(0.0, d)
            sh.Draw ctx

    let Translate v sh =
        Define <| fun ctx ->
            ctx.Translate(v.X, v.Y)
            sh.Draw ctx

    let Rotate angle sh =
        Define <| fun ctx ->
            ctx.Rotate(angle)
            sh.Draw ctx

    let Align vec sh =
        let angle = Math.Atan2(vec.Y, vec.X) - Math.PI / 2.0
        Rotate angle sh

    let Union shapes =
        Define <| fun ctx ->
            for s in shapes do
                Draw s ctx
