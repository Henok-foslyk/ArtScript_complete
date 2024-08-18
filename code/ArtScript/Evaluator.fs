module Evaluator

open AST

let evalColor (color: Color) : string =
    match color with
    | NoColor -> "none"
    | Red -> "rgb(255,0,0)"
    | Green -> "rgb(0,255,0)"
    | Blue -> "rgb(0,0,255)"
    | Purple -> "rgb(128,8,165)"
    | Black -> "rgb(0,0,0)"
    | Yellow -> "rgb(255,255,0)"
    | Gold -> "rgb(255,215,0)"
    | White -> "rgb(255,255,255)"
    | Orange -> "rgb(255,165,0)"
    | Pink -> "rgb(255,182,193)"
    | Brown -> "rgb(222,184,135)"
    | RGB(x,y,z) -> "rgb(" + (x|> string) + "," 
                                            + (y|> string) + "," 
                                            + (z|> string) + ")"

let rec polygoner (coordinates:(int*int) list) = 
                  match coordinates with
                  | [] -> ""
                  | (x, y)::xs -> (x |> string) + "," + (y|> string) + "  " + polygoner xs

// consider reaching canvas edge case



let rec evalCommand (command: Command)(state:State): string * State =
    match command, state with
    |Forward(len, color), { position = start; direction = dir; pen_up = false}
            ->  let end_point =
                    match dir with
                    | North -> {start with y = start.y - len}
                    | South -> {start with y = start.y + len}
                    | East -> {start with x = start.x + len}
                    | West -> {start with x = start.x - len}
                let line = "<line x1=\"" + ((start.x) |> string) + "\"" +
                                    " y1=\"" +        (start.y |> string) + "\"" +
                                    " x2=\"" +        (end_point.x |> string) + "\"" +
                                    " y2=\"" +        (end_point.y |> string) + "\"" +
                                    " style=\"stroke:" +
                                    (evalColor color) + ";stroke-width:2\" />\n"

                line, {state with position = end_point}
    |Rect(w, l, fill, color), { position = start; direction = _; pen_up = false}
           -> let rect = "<rect x=\"" +  ((start.x) |> string) + "\"" +
                                 " y=\"" +       (start.y |> string) + "\"" +
                                 " width=\"" +   (w |> string) + "\"" +
                                 " height =\"" + (l |> string) + "\"" +
                                 " fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " stroke-width =\"2\" />\n"  
              rect, state
    |Circle(r, fill, color), { position = start; direction = _; pen_up = false}
           -> let circ = "<circle cx=\"" +  ((start.x) |> string) + "\"" +
                                 " cy=\"" +       (start.y |> string) + "\"" +
                                 " r=\"" +   (r |> string) + "\"" +
                                 " fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " stroke-width =\"2\" />\n"  
              circ, state
    |Polygon(fill, color, coords), { position = start; direction = _; pen_up = false}
           -> let poly = "<polygon fill =\"" +   (evalColor fill) + "\"" +
                                 " stroke=\"" +  (evalColor color) + "\"" +
                                 " points=\" " +
                                 (polygoner coords) +
                                 "\"" +
                                 " stroke-width =\"2\" />\n"
              poly, state

    |Penup, { position = _; direction = _; pen_up = x}
            -> "", {state with pen_up = true}

    |Pendown, { position = _; direction = _; pen_up = x}
            -> "", {state with pen_up = false}
    |SetLocation(x_loc, y_loc), { position = _; direction = _; pen_up =_}
            -> "", {state with position = {x = x_loc; y = y_loc}}
    |Shift(len,dir), {position = start; direction = _; pen_up =_}
            ->  let new_position =
                    match dir with
                    | North -> {start with y = start.y - len}
                    | South -> {start with y = start.y + len}
                    | East -> {start with x = start.x + len}
                    | West -> {start with x = start.x - len}

                "", {state with position = new_position}

    |Forward(len, _), { position = start; direction = dir; pen_up = true}
            ->  let end_point =
                    match dir with
                    | North -> {start with y = start.y - len}
                    | South -> {start with y = start.y + len}
                    | East -> {start with x = start.x + len}
                    | West -> {start with x = start.x - len}

                "", {state with position = end_point}
    
    |TurnLeft, { position = _ ; direction = dir; pen_up = _}
            ->  let new_direction =
                    match dir with
                    | North -> West
                    | South -> East
                    | East -> North
                    | West -> South

                "", {state with direction = new_direction}
                    
    |TurnRight, { position = _ ; direction = dir; pen_up = _}
            ->  let new_direction =
                    match dir with
                    | North -> East
                    | South -> West
                    | East -> South
                    | West -> North

                "", {state with direction = new_direction}
     |_,_ -> "", state //in cases like trying to draw the rectange when the pen is up.
                    

let rec evalDraw (drawing: Drawing)(state:State) =
    match drawing with
    | [] -> "", state
    | d::ds -> let line, curr_state = (evalCommand d state) 
               let next_drawing, next_state = (evalDraw ds curr_state)
               (line + next_drawing), next_state

let eval (drawing: Drawing) : string =
    let start_state = { position = origin; direction = North; pen_up = false}
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    ((evalDraw drawing start_state) |> (fun (x, y) -> x)) //curr_change
    + "</svg>\n"
              