module Parser


open AST
open Combinator

let pad p = pbetween pws0 p pws0
let n = pmany1 pdigit |>> (fun digits -> stringify digits |> int)
let color =
    (pstr "none" |>> (fun _ -> NoColor)) <|>
    (pstr "red" |>> (fun _ -> Red)) <|>
    (pstr "green" |>> (fun _ -> Green)) <|>
    (pstr "blue" |>> (fun _ -> Blue)) <|>
    (pstr "purple" |>> (fun _ -> Purple)) <|>
    (pstr "black" |>> (fun _ -> Black)) <|>
    (pstr "yellow" |>> (fun _ -> Yellow)) <|>
    (pstr "gold" |>> (fun _ -> Gold)) <|>
    (pstr "white" |>> (fun _ -> White)) <|>
    (pstr "pink" |>> (fun _ -> Pink)) <|>
    (pstr "brown" |>> (fun _ -> Brown)) <|>
    (pstr "orange" |>> (fun _ -> Orange)) <|> //<!> "color"
    ((pbetween
            (pad (pstr "RGB("))
            (pseq
                (pad n)
                (pseq (pad n) (pad n) (fun (y, z) -> y,z))
                (fun (x,(y,z)) -> (x,y,z)))
            (pad (pstr ")"))

            ) |>> (fun (a,b,c) -> RGB(a,b,c)))
let direction =
    (pstr "up" |>> (fun _ -> North)) <|>
    (pstr "down" |>> (fun _ -> South)) <|>
    (pstr "left" |>> (fun _ -> West)) <|>
    (pstr "right" |>> (fun _ -> East)) 
let pcoord = pseq (pad n) (pad n) (fun (x_loc,y_loc) -> x_loc,y_loc)

let pcommand = 
        (pright (pad (pstr "go")) 
                        (pseq (pad n) (pad color) (fun (len,col) -> len,col))
                       |>> (fun (l, c) -> Forward(l, c))) <|>
        (pad (pstr "toright")  |>> (fun _ -> TurnRight)) <|> 
        (pad (pstr "toleft") |>> (fun _ -> TurnLeft)) <|>
        (pright (pad (pstr "shift")) 
                        (pseq (pad n) (pad direction) (fun (len, dir) -> Shift(len,dir))))<|>
        (pright (pstr "setlocation") 
                        pcoord
                        //(pseq (pad n) (pad n) (fun (x_loc,y_loc) -> x_loc,y_loc))
                        |>> (fun (x, y) -> SetLocation(x,y))) <|>
        (pright (pstr "rect")
                        (pseq (pad n)  
                            (pseq (pad n)
                                    (pseq (pad color) (pad color) (fun (fill, color) -> fill, color))
                                    (fun (l,(fill,color)) -> l, fill, color))
                                (fun (w, (l,fill,color)) -> w, l, fill, color))        
                        |>> (fun (w, l, fill, color) -> Rect(w, l, fill, color)))<|>
        (pright (pstr "circle")
                        (pseq (pad n)
                                (pseq (pad color) (pad color) (fun (fill, color) -> fill, color))
                                (fun (l,(fill,color)) -> l, fill, color))                 
                        |>> (fun (r, fill, color) -> Circle(r, fill, color)))<|>
        (pright (pstr "poly")
                        (pseq
                            (pseq (pad color) (pad color) (fun (fill, color) -> fill, color))
                            (pmany1 pcoord)
                            (fun ((fill, color), coords) -> fill, color, coords)
                            )
                        
                        |>> (fun (fill, color, coord_list) -> Polygon(fill, color, coord_list)) )<|>
        (pad (pstr "penup") |>> (fun _ -> Penup)) <|>
        (pad (pstr "pendown") |>> (fun _ -> Pendown)) <!> "pcommand"

let repeat =
    pseq
        (pright (pad (pstr "repeat")) (pad n))
        (pbetween
            (pad (pchar '('))
            (pmany1 pcommand)
            (pad (pchar ')'))
        )
        (fun (i, commands) ->
            [0..i-1] |> List.fold (fun acc _ -> commands @ acc) []
        ) <!> "repeat"

let expr =
    pmany1 (
        pcommand |>> (fun c -> [c]) <|>
        repeat
    ) |>> List.concat

let grammar = pleft expr peof <!> "expr"

let parse (input: string) : Drawing option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None
