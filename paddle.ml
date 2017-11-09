(* file: paddle.ml
   author: Matthew Kraft
   date: Oct 19, 2017
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

let _ = Random.self_init ()

let randomColor () =
  let red   = Random.int 256 in
  let green = Random.int 256 in
  let blue  = Random.int 256
  in
  Color.make_color red green blue


let clockRate = 0.02

let displayWidth = 750.
let displayHeight = 750.
let margin = 10.0
let background  = Image.rectangle displayWidth displayHeight Color.white




type head = { x : float; y : float}

type bodypart = { x : float; y : float}

type state = Start | Left | Right | Up | Down | Eat

type food = { x : float; y : float }

type score = { n : int }

type model = { state    : state
             ; head     : head
             ; bodypart : bodypart
             ; score    : score
             ; food     : food
             }

(* draw : model -> Image.t
   This function's complexity is rooted in the color changes of the balls and the
   background, in correspondence to the score (referred to as "Levels"), which is a
   form of extra work that I thought would be creative and original.
*)
let draw { state; head; bodypart; score; food } =
  let headSpot = Image.rectangle 50. 50. Color.orangeRed
  in
  place_image headSpot (head.x, head.y) background


(* update : model -> model
   This function allows for the movement of each of the balls and allows the balls
   to reset to the top of the screen once it hits the paddle, adding 1 to the score.
*)
let update { state; head; bodypart; score; food } =
  match state with
  | Up -> World { state; head = { x = head.x; y = head.y -. 1.}; bodypart; score; food }
  | Down -> World { state; head = { x = head.x; y = head.y +. 1.}; bodypart; score; food }
  | Left -> World { state; head = { x = head.x -. 1.; y = head.y }; bodypart; score; food }
  | Right -> World { state; head = { x = head.x +. 1.;  y = head.y }; bodypart; score; food }
  | _ -> World { state; head; bodypart; score; food }




(* handleKey : model -> key -> model
   This function allows the paddle to move in accorance with the pressing of the keys.
*)
let handleKey model key =
  match (model.state, key) with
  | (Start, "left") -> World { model with state = Left }
  | (Start, "right") -> World { model with state = Right }
  | (Start, "up") -> World { model with state = Up }
  | (Start, "down") -> World { model with state = Down }
  | (Up, "left") | (Down, "left") -> World { model with state = Left }
  | (Up, "right") | (Down, "right") -> World { model with state = Right }
  | (Left, "up") | (Right, "up") -> World { model with state = Up }
  | (Left, "down") | (Right, "down") -> World { model with state = Down }
  | _ -> failwith "Not possible"


(* finished : model -> bool
   This function lets the World know when to produce the gameOver image.
*)
let finished { state; head; bodypart; score; food } = failwith ""

(* gameOver : model -> Image.t
   This function produces a new Image once the game is finished, and, depending
   on the score, will say Game Over or Congrats.
*)
let gameOver { state; head; bodypart; score; food } = failwith ""


(* updateMouse : model -> x -> y -> click -> model
   This extra function allows the game to pause when the mouse is clicked, and will
   resume (go back to state = Ready) when a key is pressed.
*)
  let updateMouse { state; head; bodypart; score; food } coordinate1 coordinate2 click =
  match (coordinate1, coordinate2, click) with
  | (0., 0., "button_down") -> World { state; head; bodypart; score; food }
  | anythingElse ->  World { state; head; bodypart; score; food }


(* go : unit -> unit
*)
let go () =
  let _ = Random.self_init () in
  World.big_bang { state = Start
                 ; head = { x = displayWidth /. 2.; y = displayHeight /. 2. }
                 ; bodypart = { x = 0.; y = 0.}
                 ; score = { n = 0 }
                 ; food = { x = 0.; y = 0.} }
    ~name: "Snake"
    ~width: (int_of_float displayWidth)
    ~height: (int_of_float displayHeight)
    ~to_draw: draw
    ~on_tick: update
    ~rate: clockRate
    ~on_key_press: handleKey
    ~on_mouse: updateMouse
    ~stop_when: finished
    ~to_draw_last: gameOver

let s = go ()
