namespace DCFSharp.Oct2013

open IntelliFactory.WebSharper

module Remoting =

    [<Remote>]
    let Process input =
        async {
            return "You said: " + input
        }
