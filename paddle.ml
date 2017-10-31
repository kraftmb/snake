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


let clockRate = 0.02



let displayWidth = 1250.
let displayHeight = 775.
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
   This function's complexity is rooted in the color changes of the balls and the
   background, in correspondence to the score (referred to as "Levels"), which is a
   form of extra work that I thought would be creative and original.
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
     let posns = [((displayWidth /. 4.5), (displayHeight /. 3.))
                 ; ((displayWidth /. 2.5), (displayHeight /. 3.+. 75.))
                 ; (paddle.x, (displayHeight -. (35. +. margin)))]
     in
     Image.place_images images posns background)
  | { state = Ready; ball; paddle; score; ball2 } ->
    match score.n with
    | 0 | 1 | 2 | 3 | 4 | 35 | 36 | 37 | 38 | 39 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.black in
      let secondBall = Image.circle 50. Color.white in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x +. 1., ball.y +. 1.)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background
    | 5 | 6 | 7 | 8 | 9 | 40 | 41 | 42 | 43 | 44 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.blue in
      let secondBall = Image.circle 50. Color.cyan in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background2
    | 10 | 11 | 12 | 13 | 14 | 45 | 46 | 47 | 48 | 49 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.gray10 in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.pink in
      let secondBall = Image.circle 50. Color.deepPink4 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold4 in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background3
    | 15 | 16 | 17 | 18 | 19 | 50 | 51 | 52 | 53 | 54 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.yellow2 in
      let secondBall = Image.circle 50. Color.white in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background4
    | 20 | 21 | 22 | 23 | 24 | 55 | 56 | 57 | 58 | 59 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.maroon in
      let secondBall = Image.circle 50. Color.pink4 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y); (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background5
    | 25 | 26 | 27 | 28 | 29 | 60 | 61 | 62 | 63 | 64 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.sienna in
      let secondBall = Image.circle 50. Color.goldenrod1 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background6
    | 30 | 31 | 32 | 33 | 34 | 65 | 66 | 67 | 68 | 69 ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.greenYellow in
      let secondBall = Image.circle 50. Color.antiqueWhite4 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background7
    | anythingElse ->
      let scoreBoard = Image.text ("Score = " ^ string_of_int score.n) ~size:30.0 Color.white in
      let paddler = Image.rectangle 250. 35. Color.black in
      let firstBall = Image.circle 50. Color.gold2 in
      let secondBall = Image.circle 50. Color.gold3 in
      let levelUp = Image.text ("Level " ^ string_of_int (score.n / 5 + 1)) ~size:50.0 Color.gold in
      let objects = [ paddler
                    ; firstBall
                    ; scoreBoard
                    ; secondBall
                    ; levelUp] in
      let posn = [ (paddle.x, (displayHeight -. (35. +. margin)))
                 ; (ball.x, ball.y)
                 ; (0., 0.)
                 ; (ball2.x, ball2.y)
                 ; (displayWidth -. 200., 0.)]
      in
      Image.place_images objects posn background8

(* update : model -> model
   This function allows for the movement of each of the balls and allows the balls
   to reset to the top of the screen once it hits the paddle, adding 1 to the score.
*)
let update { state; ball; paddle; score; ball2 } =
  match score.n >= 40 with
  | true -> (match { state; ball; paddle; score; ball2 } with
      | { state = Start; ball; paddle; ball2 } -> World { state
                                                        ; ball
                                                        ; paddle
                                                        ; score
                                                        ; ball2 }
      | { state = Ready; ball; paddle; ball2 } ->
        match (ball.y +. 100. >= (displayHeight -. 45.)) && (ball.y < displayHeight) with
        | true ->
          ((match ball.x > (paddle.x -. 25.) && ball.x < (paddle.x +. 250.) with
              | true  -> World { state
                               ; ball = { x = Random.float (displayWidth -. 50.); y = 0.}
                               ; paddle
                               ; score = {n = score.n + 1}
                               ; ball2 = {x = ball2.x; y = ball2.y +. (((0.8 *. (1. /. 5. *. float score.n)) *. 1.25))}}
              | false -> match ball2.y +. 100. >= (displayHeight -. 45.) with
                | true ->
                  (match ball2.x > (paddle.x -. 55.) && ball2.x < (paddle.x +. 250.) with
                   | true  -> World { state
                                    ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.)}
                                    ; paddle
                                    ; score = {n = score.n + 1}
                                    ; ball2 = { x = Random.float (displayWidth -. 50.); y = 0.}}
                   | false -> World { state
                                    ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.)}
                                    ; paddle
                                    ; score
                                    ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) *. 0.8)} })
                | false ->
                  World { state
                        ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.)}
                        ; paddle
                        ; score
                        ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) *. 0.8)} }))
        | false ->
          (match ball2.y +. 100. >= (displayHeight -. 45.) with
           | true ->
             (match ball2.x > (paddle.x -. 55.) && ball2.x < (paddle.x +. 250.) with
              | true  -> World { state
                               ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.)}
                               ; paddle
                               ; score = {n = score.n + 1}
                               ; ball2 = { x = Random.float (displayWidth -. 50.); y = 0.}}
              | false -> World { state
                               ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.)}
                               ; paddle
                               ; score
                               ; ball2 = { x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) *. 0.8)} })
           | false ->
             World { state
                   ; ball = { x = ball.x; y = ball.y +. ((1. /. 5. *. float score.n) +. 1.)}
                   ; paddle
                   ; score
                   ; ball2 = { x = ball2.x; y = ball2.y +. ((0.8 *. (1. /. 5. *. float score.n) *. 1.25))} }))
  | false ->
    match { state; ball; paddle; score; ball2 } with
    | { state = Start; ball; paddle; ball2 } -> World { state
                                                      ; ball
                                                      ; paddle
                                                      ; score
                                                      ; ball2 }
    | { state = Ready; ball; paddle; ball2 } ->
      match (ball.y +. 100. >= (displayHeight -. 45.)) && (ball.y < displayHeight) with
      | true ->
        ((match ball.x > (paddle.x -. 55.) && ball.x < (paddle.x +. 250.) with
            | true  -> World { state
                             ; ball = { x = Random.float (displayWidth -. 50.); y = 0.}
                             ; paddle
                             ; score = {n = score.n + 1}
                             ; ball2 = {x = ball2.x; y = ball2.y +. (((1. /. 5. *. float score.n) *. 1.25) +. 0.5)}}
            | false -> match ball2.y +. 100. >= (displayHeight -. 45.) with
              | true ->
                (match ball2.x > (paddle.x -. 55.) && ball2.x < (paddle.x +. 250.) with
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
           (match ball2.x > (paddle.x -. 55.) && ball2.x < (paddle.x +. 250.) with
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
   This function allows the paddle to move in accorance with the pressing of the keys.
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
     | false -> World { model with paddle = { x = model.paddle.x -. (100. +. (float(model.score.n) *. 1.75)) } })
  | (Ready, "right") ->
    (match model.paddle.x >= (displayWidth -. (margin +. 250.)) with
     | true -> World { state = model.state
                     ; ball = model.ball
                     ; paddle = model.paddle
                     ; score = model.score
                     ; ball2 = model.ball2}
     | false -> World { model with paddle = { x =  model.paddle.x +. 100. +. (float(model.score.n) *. 1.75)} })
  | (_,_) -> failwith "Doesn't work"

(* finished : model -> bool
   This function lets the World know when to produce the gameOver image.
*)
let finished { state; ball; paddle; score; ball2 } = ball.y > displayHeight || ball2.y > displayHeight

(* gameOver : model -> Image.t
   This function produces a new Image once the game is finished, and, depending
   on the score, will say Game Over or Congrats.
*)
let gameOver { state; ball; paddle; score; ball2 } =
  match score.n with
  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14  ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.white in
    let mesg = Image.text "You SUCK" ~size:120.0 Color.black
    in
    place_images [sign; finalScore; mesg] [(displayWidth /. 3.25, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.5, 450.)] background
  | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.gray in
    let mesg = Image.text "You're ok" ~size:110.0 Color.black
    in
    place_images [sign; finalScore; mesg] [(displayWidth /. 3.25, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.5, 450.)] background3
  | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.gray in
    let mesg = Image.text "good score!" ~size:100.0 Color.black
    in
    place_images [sign; finalScore; mesg] [(displayWidth /. 3.25, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.5, 450.)] background4
  | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.gray in
    let mesg = Image.text "Incredible!" ~size:100.0 Color.black
    in
    place_images [sign; finalScore; mesg] [(displayWidth /. 3.5, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.4, 450.)] background5
  | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.black in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.gray in
    let mesg = Image.text "InSaNiTy" ~size:110.0 Color.black
    in
    place_images [sign; finalScore; mesg] [(displayWidth /. 3.25, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.25, 450.)] background6
  | 69 ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.blue in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.darkBlue in
    let mesg = Image.text "LOLOLOL" ~size:110.0 Color.blue in
    let secret = Image.text "(if you know what i mean)" ~size:40.0 Color.blue
    in
    place_images [sign; finalScore; mesg; secret] [(displayWidth /. 3.25, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.5, 450.)] background
  | anythingElse ->
    let sign = Image.text "GAME OVER" ~size:80.0 Color.goldenrod in
    let finalScore = Image.text ("Final Score = " ^ string_of_int score.n) ~size:40.0 Color.goldenrod in
    let mesg = Image.text "You're a god" ~size:95.0 Color.goldenrod
    in
    place_images [sign; finalScore; mesg] [(displayWidth /. 3.25, 250.); (displayWidth /. 2.5, 350.); (displayWidth /. 3.5, 450.)] background8

(* updateMouse : model -> x -> y -> click -> model
   This extra function allows the game to pause when the mouse is clicked, and will
   resume (go back to state = Ready) when a key is pressed.
*)
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
                 ; paddle = { x = displayWidth /. 2. -. 150.}
                 ; score = {n = 0}
                 ; ball2 = { x = Random.float (displayWidth -. 50.); y = 0.} }
    ~name: "Paddle"
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
