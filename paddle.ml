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

let randomColor = randomColor()

let displayWidth = 750.
let displayHeight = displayWidth
let margin = 10.0
let background  = Image.rectangle displayWidth displayHeight Color.red
let background2 = Image.rectangle displayWidth displayHeight Color.orangeRed
let background3 = Image.rectangle displayWidth displayHeight Color.yellow
let background4 = Image.rectangle displayWidth displayHeight Color.green3
let background5 = Image.rectangle displayWidth displayHeight Color.blue
let background6 = Image.rectangle displayWidth displayHeight Color.darkBlue
let background7 = Image.rectangle displayWidth displayHeight Color.deepPink4
let background8 = Image.rectangle displayWidth displayHeight Color.black

type paddle = { x : float }


type state = Start | Ready


type ball = { x : float; y : float }

type ball2 = { x : float; y : float }

type score = { n : int }


type model = { state  : state
             ; ball   : ball
             ; paddle : paddle
             ; score  : score
             ; ball2  : ball2
             }

(* draw : model -> Image.t
*)
let draw { state; ball; paddle; score; ball2 } =
  match { state; ball; paddle; score; ball2 } with
  | { state = Start; ball; paddle; score; ball2 } ->
    (let msg = Image.text "Press left or right arrow to start/unpause." ~size:35.0 Color.white in
     let pauseMsg = Image.text "Click to pause" ~size:30.0 Color.white in
     let paddler = Image.rectangle 250. 35. Color.black in
     let images = [ msg
                  ; pauseMsg
                  ; paddler] in
     let posns = [((displayWidth /. 40.), (displayHeight /. 3.))
                 ; ((displayWidth /. 3.), (displayHeight /. 3.+. 75.))
                 ; (paddle.x, (displayHeight -. (35. +. margin)))]
    in
    Image.place_images images posns background)
  | { state = Ready; ball; paddle; score; ball2 } ->
    match score.n with
    | 0 | 1 | 2 | 3 | 4 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.black in
      let brownBall = Image.circle 50. Color.white in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background
    | 5 | 6 | 7 | 8 | 9 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.blue in
      let brownBall = Image.circle 50. Color.cyan in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background2
    | 10 | 11 | 12 | 13 | 14 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.gray10 in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.pink in
      let brownBall = Image.circle 50. Color.deepPink4 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold4 in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background3
    | 15 | 16 | 17 | 18 | 19 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.yellow2 in
      let brownBall = Image.circle 50. Color.white in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background4
    | 20 | 21 | 22 | 23 | 24 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.maroon in
      let brownBall = Image.circle 50. Color.pink4 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y); (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background5
    | 25 | 26 | 27 | 28 | 29 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.sienna in
      let brownBall = Image.circle 50. Color.goldenrod1 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background6
    | 30 | 31 | 32 | 33 | 34 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.greenYellow in
      let brownBall = Image.circle 50. Color.antiqueWhite4 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background7
    | 35 | 36 | 37 | 38 | 39 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let redBall = Image.circle 50. Color.gold2 in
      let brownBall = Image.circle 50. Color.gold3 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; redBall
                    ; scoreBoard
                    ; brownBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 190., 0.)]
      in
      Image.place_images objects posn background8
    | anythingElse ->
      let youWin = Image.text "You Win!" ~size:100.0 Color.gold
      in
      place_image youWin (150., 300.) background8

(* update : model -> model
*)

let update { state; ball; paddle; score; ball2 } =
  match { state; ball; paddle; score; ball2 } with
  | { state = Start; ball; paddle; ball2 } -> World { state
                                                    ; ball
                                                    ; paddle
                                                    ; score
                                                    ; ball2 }
  | { state = Ready; ball; paddle; ball2 } ->
    match (ball.y +. 100. >= (displayHeight -. 45.)) && (ball.y < displayHeight) with
    | true ->
      ((match ball.x > (paddle.x -. 10.) && ball.x < (paddle.x +. 250.) with
       | true  -> World { state
                        ; ball = { x = Random.float (displayWidth -. 50.); y = 0.}
                        ; paddle
                        ; score = {n = score.n + 1}
                        ; ball2 = {x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) +. 0.5)}}
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
                               ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) +. 0.5)} })
           | false ->
             World { state
                   ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
                   ; paddle
                   ; score
                   ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) +. 0.5)} }))
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
                          ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) +. 0.5)} })
      | false ->
        World { state
              ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.5)}
              ; paddle
              ; score
              ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) +. 0.5)} })



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
    | false -> World { model with paddle = { x = model.paddle.x -. 60. } })
  | (Ready, "right") ->
    (match model.paddle.x >= (displayWidth -. (margin +. 250.)) with
     | true -> World { state = model.state
                     ; ball = model.ball
                     ; paddle = model.paddle
                     ; score = model.score
                     ; ball2 = model.ball2}
    | false -> World { model with paddle = { x =  model.paddle.x +. 60. } })
  | (_,_) -> failwith "hello"


let finished { state; ball; paddle; score; ball2 } = ball.y > displayHeight || ball2.y > displayHeight

let gameOver { state; ball; paddle; score; ball2 } =
  match score.n >= 40 with
  | true -> let sign = Image.text "CONGRATS!" ~size:80.0 Color.yellow in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.white
    in
    place_images [sign; finalScore] [(125., 250.); (215., 350.)] background6
  | false ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.white
    in
    place_images [sign; finalScore] [(125., 250.); (215., 350.)] background






let updateMouse {state ; ball; paddle; score; ball2} coordinate1 coordinate2 click =
  match (coordinate1, coordinate2, click) with
  | (0., 0., "button_down") -> World {state; ball; paddle; score; ball2}
  | anythingElse ->  World { state = Start
                           ; ball
                           ; paddle
                           ; score
                           ; ball2}







(* go : unit -> unit
*)
let go () =
  let _ = Random.self_init () in
  World.big_bang { state = Start
                 ; ball = { x = Random.float (displayWidth -. 50.); y = 0.}
                 ; paddle = { x = displayWidth /. 2. -. 125.}
                 ; score = {n = 0}
                 ; ball2 = { x = Random.float (displayWidth -. 50.); y = 0.} }
    ~name: "Paddle"
    ~width: (f2I displayWidth)
    ~height: (f2I displayHeight)
    ~to_draw: draw
    ~on_tick: update
    ~rate: clockRate
    ~on_key_press: handleKey
    ~on_mouse: updateMouse
    ~stop_when: finished
    ~to_draw_last: gameOver

let s = go ()
