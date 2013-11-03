namespace DCFSharp.Oct2013

open System
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<JavaScript>]
module Client =

    let Start input k =
        async {
            let! data = Remoting.Process(input)
            return k data
        }
        |> Async.Start

    let Animate ctx =
        Boids.Animate ctx

    let Main () =
        let input = Input [Text ""]
        let label = Div [Text ""]
        Div [
            Canvas.Create(480, 480, Animate)
            Hr []
            input
            label
            Button [Text "Click"]
            |>! OnClick (fun _ _ ->
                Start input.Value (fun out ->
                    label.Text <- out))
        ]
