namespace DCFSharp.Oct2013

open System
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<JavaScript>]
module Boids =

    type Boid =
        {
            mutable Position : Vec2D
            mutable Velocity : Vec2D
        }

        member boid.Move(d) =
            boid.Position <- boid.Position +^ d

    type BoidState =
        {
            mutable Mouse : Vec2D
            Boids : seq<Boid>
        }

    let FindBoidsWithin st rad pos =
        st.Boids
        |> Seq.filter (fun boid ->
            Vec2D.Distance(pos, boid.Position) <= rad)

    let SteerToCenterRule boid =
        - 0.1 *^ boid.Position

    let CenterOfMassRule st boid =
        let center =
            seq {
                for boid in FindBoidsWithin st 0.2 boid.Position ->
                    boid.Position
            }
            |> Vec2D.Average
        0.1 *^ (center -^ boid.Position)

    let Clip a b x =
        if x <= a then a elif x >= b then b else x

    let Repulsion (d: float) =
        Clip 0.0 1.0 (0.1 * sqrt d / d)

    let MouseScareRule st boid =
        let dm = boid.Position -^ st.Mouse
        let n = Vec2D.Norm dm
        let r = Repulsion n
        0.5 * r / n *^ dm

    let KeepAwayRule st boid =
        FindBoidsWithin st 0.1 boid.Position
        |> Seq.map (fun b -> boid.Position -^ b.Position)
        |> Vec2D.Sum

    let MatchVelocityRule st boid =
        let center =
            seq {
                for boid in FindBoidsWithin st 0.1 boid.Position ->
                    boid.Velocity
            }
            |> Vec2D.Average
        0.05 *^ (center -^ boid.Velocity)

    let Dampen v =
        let n = Vec2D.Norm v
        if n >= 0.3 then
            (0.3 / n) *^ v
        else
            v

    let Step st (dt: TimeSpan) =
        for b in st.Boids do
            let v =
                b.Velocity
                +^ MouseScareRule st b
                +^ SteerToCenterRule b
                +^ CenterOfMassRule st b
                +^ KeepAwayRule st b
                +^ MatchVelocityRule st b
            b.Velocity <- Dampen v
            b.Move(dt.TotalSeconds *^ b.Velocity)

    let BoidShape boid =
        Shapes.Triangle
        |> Shapes.Fill "blue"
        |> Shapes.Scale { X = 0.025; Y = 0.05 }
        |> Shapes.Align boid.Velocity
        |> Shapes.Translate boid.Position

    let BoidSetShape st =
        st.Boids
        |> Seq.map BoidShape 
        |> Shapes.Union

    let InitialBoids (number: int) =
        let rd () = 2.0 * (Double.Random() - 0.5)
        let boids =
            Array.init number (fun _ ->
                {
                    Position = { X = rd (); Y = rd () }
                    Velocity = { X = 0.0; Y = 0.0 }
                })
        {
            Boids = Seq.ofArray boids
            Mouse = Vec2D.Zero
        }

    let Animate (canvas: Canvas) =
        let boids = InitialBoids 50
        let draw () =
            Shapes.Draw (BoidSetShape boids) canvas.Ctx2D
        let onTick ts =
            Step boids ts
            draw ()
        let onMM pos =
            boids.Mouse <- pos
        canvas.Animate {
            OnTick = onTick
            OnMouseMove = onMM
        }
