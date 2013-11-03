namespace DCFSharp.Oct2013

open System
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JQuery
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<AutoOpen>]
[<JavaScript>]
module Extensions =

    type Ctx2D = CanvasRenderingContext2D

    type Double with

        [<Direct "Math.random()">]
        static member Random() = X<double>

    type AnimationConfig =
        {
            OnMouseMove : Vec2D -> unit
            OnTick : TimeSpan -> unit
        }

    type Canvas =
        {
            CanvasElement : Dom.Element
            CanvasHeight : int
            CanvasWidth : int
            Ctx2D : Ctx2D
        }

        static member Create(width: int, height: int, init) =
            HTML5.Tags.Canvas [
                Width (string width)
                Height (string height)
            ]
            |>! OnAfterRender (fun canvas ->
                let el = As<CanvasElement> canvas.Dom
                let ctx = el.GetContext("2d")
                let canvas =
                    {
                        CanvasElement = canvas.Dom
                        CanvasHeight = height
                        CanvasWidth = width
                        Ctx2D = ctx
                    }
                let w = float width / 2.0
                let h = float height / 2.0
                ctx.Translate(w, h)
                ctx.Scale(w, -h)
                init canvas)

        member canvas.Animate(cfg) =
            let t = ref DateTime.Now
            let fetch () =
                let n = DateTime.Now
                let d = n.Subtract(!t)
                t := n
                d
            let j = JQuery.Of(canvas.CanvasElement)
            j.Mousemove(Func<_,_,_>(fun el ev ->
                let o = j.Parent().Offset()
                let dX = float (ev.PageX - o.Left) / float canvas.CanvasWidth
                let dY = float (ev.PageY - o.Top) / float canvas.CanvasHeight
                let x = 2.0 * (dX - 0.5)
                let y = - 2.0 * (dY - 0.5)
                cfg.OnMouseMove { X = x; Y = y }))
            |> ignore
            let minD =
                TimeSpan.FromSeconds(1.0/24.)
            let rec loop () : Async<unit> =
                async {
                    let d = fetch ()
                    do canvas.Ctx2D.ClearRect(-1.0, -1.0, 2.0, 2.0)
                    do cfg.OnTick(d)
                    if d < minD then
                        let sleep = minD.Subtract(d).TotalMilliseconds
                        do! Async.Sleep(int sleep)
                    return! loop ()
                }
            loop ()
            |> Async.Start

    type CanvasRenderingContext2D with

        member ctx.Circle(cX, cY, rad, ?color) =
            ctx.Save()
            ctx.BeginPath()
            ctx.Arc(cX, cY, rad, 0., 2. * Math.PI, false)
            ctx.FillStyle <- defaultArg color "blue"
            ctx.Fill()
            ctx.Restore()

