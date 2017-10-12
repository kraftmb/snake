(* file: paddle.ml
   author: Bob Muller

   CSCI 1103 Computer Science I Honors

   A simple game with a ball dropping from the top.
   A player uses the left and right arrow keys to move
  a paddle in an attempt to catch the ball.

  Usage:

  > make
  > ./go
*)
open World
open Image
open Color
open Cs1103

let clockRate = 0.02

let displayWidth = 800.
let displayHeight = displayWidth
let margin = 10.0
let background = Image.rectangle displayWidth displayHeight Color.dodgerBlue

type model = unit

(* draw : model -> Image.t
*)
let draw () =
  let msg = Image.text "Nothing Here!" ~size:40.0 Color.white
  in
  Image.place_image msg (260., 360.) background

(* go : unit -> unit
*)
let go () =
  World.big_bang ()
    ~name: "Paddle"
    ~width: (f2I displayWidth)
    ~height: (f2I displayHeight)
    ~to_draw: draw

let s = go ()
