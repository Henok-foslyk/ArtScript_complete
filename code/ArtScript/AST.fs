module AST


type Coordinate = { x: int; y: int }

type Color =
| NoColor
| Red
| Green
| Blue
| Purple
| Black
| Yellow
| Gold
| White
| Orange
| Pink
| Brown
| RGB of int*int*int

type Direction =
| North
| South
| East
| West


type Command =
| Forward of int*Color
| SetLocation of int*int
| TurnRight
| TurnLeft
| Shift of int*Direction
| Penup
| Pendown
| Rect of int*int*Color*Color
| Circle of int*Color*Color
| Polygon of  Color * Color * (int*int)list


type Drawing = Command list

type State = {position: Coordinate; direction: Direction; pen_up: bool}


let CANVAS_SZ = 1000
let origin = { x = CANVAS_SZ/2; y = CANVAS_SZ/2}

