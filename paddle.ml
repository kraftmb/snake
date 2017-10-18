(* file: paddle.ml
   author: Matthew Kraft

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

let displayWidth = 750.
let displayHeight = displayWidth
let margin = 10.0
let background = Image.rectangle displayWidth displayHeight Color.dodgerBlue
let initialX = Random.float 500.

type paddle = { x : float}


type state = Start | Ready


type ball = { x : float; y : float}

type ball2 = { x : float; y : float}

type score = { n : int }


type model = { state  : state
             ; ball   : ball
             ; paddle : paddle
             ; score  : score
             ; ball2  : ball2
             }


(* draw : model -> Image.t
*)
let draw { state; ball; paddle; score; ball2} =
  match { state; ball; paddle; score; ball2 } with
  | { state = Start; ball; paddle} ->
    (let msg = Image.text "Press left or right arrow to start." ~size:40.0 Color.white in
    let paddler = Image.rectangle 250. 35. Color.black in
    let images = [msg; paddler] in
    let posns = [((displayWidth /. 15.), (displayHeight /. 3.)); (paddle.x, (displayHeight -. (35. +. margin)))]
    in
    Image.place_images images posns background)
  | { state = Ready; ball; paddle} ->
    let scoreBoard = Image.text ("Score = " ^ string_of_int score.n)~size:30.0 Color.white in
    let paddler = Image.rectangle 250. 35. Color.black in
    let redBall = Image.circle 50. Color.red in
    let brownBall = Image.circle 50. Color.darkGoldenrod4 in
    let objects = [paddler; redBall; scoreBoard; brownBall] in
    let posn = [(paddle.x, (displayHeight -. (35. +. margin))); (ball.x, ball.y); (0., 0.); (ball2.x, ball2.y)] in
    Image.place_images objects posn background


(* update : model -> model
*)

let update { state; ball; paddle; score; ball2 } =
  match { state; ball; paddle; score; ball2 } with
  | { state = Start; ball; paddle; ball2 } -> World { state; ball; paddle; score; ball2 }
  | { state = Ready; ball; paddle; ball2 } ->
    match (ball.y +. 100. >= (displayHeight -. 45.)) && (ball.y < displayHeight) with
    | true ->
      ((match ball.x > (paddle.x -. 10.) && ball.x < (paddle.x +. 250.) with
       | true  -> World { state
                        ; ball = { x = Random.float (displayWidth -. 50.); y = 0.}
                        ; paddle
                        ; score = {n = score.n + 1}
                        ; ball2 = {x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 2.5) +. 0.1)}}
       | false -> match ball2.y +. 100. >= (displayHeight -. 45.) with
           | true ->
             (match ball2.x > (paddle.x -. 10.) && ball2.x < (paddle.x +. 250.) with
              | true  -> World { state
                               ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
                               ; paddle
                               ; score = {n = score.n + 1}
                               ; ball2 = { x = Random.float (displayWidth -. 50.); y = 0.}}
              | false -> World { state
                               ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
                               ; paddle
                               ; score
                               ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 2.5) +. 0.1)} })
           | false ->
             World { state
                   ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
                   ; paddle
                   ; score
                   ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 2.5) +. 0.1)} }))
    | false ->
      (match ball2.y +. 100. >= (displayHeight -. 45.) with
      | true ->
        (match ball2.x > (paddle.x -. 10.) && ball2.x < (paddle.x +. 250.) with
         | true  -> World { state
                          ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
                          ; paddle
                          ; score = {n = score.n + 1}
                          ; ball2 = { x = Random.float (displayWidth -. 50.); y = 0.}}
         | false -> World { state
                          ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
                          ; paddle
                          ; score
                          ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 2.5) +. 0.1)} })
      | false ->
        World { state
              ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
              ; paddle
              ; score
              ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 2.5) +. 0.1)} })



(* handleKey : model -> key -> model
*)

let handleKey model key =
  match (model.state, key) with
  | (Start, "left") | (Start, "right") -> World { model with state = Ready }
  | (Ready, "left")  ->
    (match model.paddle.x <= margin with
     | true  -> World { state = model.state
                      ; ball = model.ball
                      ; paddle = model.paddle
                      ; score = model.score
                      ; ball2 = model.ball2}
    | false -> World { model with paddle = { x = model.paddle.x -. 30. } })
  | (Ready, "right") ->
    (match model.paddle.x >= (displayWidth -. (margin +. 250.)) with
     | true -> World { state = model.state
                     ; ball = model.ball
                     ; paddle = model.paddle
                     ; score = model.score
                     ; ball2 = model.ball2}
    | false -> World { model with paddle = { x =  model.paddle.x +. 30. } })
  | (_,_) -> failwith "hello"


let finished { state; ball; paddle; ball2 } = ball.y > displayHeight || ball2.y > displayHeight

let gameOver { state; ball; paddle; ball2 } =
  let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
  place_image sign (125., 250.) background


(* go : unit -> unit
*)
let go () =
  World.big_bang { state = Start
                 ; ball = { x = initialX; y = 0.}
                 ; paddle = { x = displayWidth /. 2. -. 125.}
                 ; score = {n = 0}
                 ; ball2 = { x = Random.float (displayWidth /. 2.); y = 0.} }
    ~name: "Paddle"
    ~width: (f2I displayWidth)
    ~height: (f2I displayHeight)
    ~to_draw: draw
    ~on_tick: update
    ~rate: clockRate
    ~on_key_press: handleKey
    ~stop_when: finished
    ~to_draw_last: gameOver

let s = go ()
