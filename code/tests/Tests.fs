namespace codetests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Evaluator
open System
open Parser
open AST

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidParser () =
        let input = "go 50 RGB(255 100 100) toleft toright toleft"
        let expected = [Forward(50, RGB(255, 100, 100)); TurnLeft; TurnRight; TurnLeft]
        let result = parse input
        match result with
        | Some ast ->
            Assert.AreEqual(expected, ast)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.ValidLineParser () =
        let input = "go 50 RGB(255 100 100)"
        let expected = "<svg width=\"1000\" height=\"1000\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n"+
                       "<line x1=\"500\" y1=\"500\" x2=\"500\" y2=\"450\" style=\"stroke:rgb(255,100,100);stroke-width:2\" />\n" + "</svg>\n"        
        let result = parse input
        match result with
        | Some ast ->
            let svg = eval ast
            Assert.AreEqual(expected, svg)
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.ValidTurnParser () =
        let input = "toright"
        let expected = "<svg width=\"1000\" height=\"1000\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n"+
                       "</svg>\n"
        let result = parse input
        match result with
        | Some ast ->
            let svg = eval ast
            Assert.AreEqual(expected, svg)
        | None ->
            Assert.IsTrue false

    [<TestMethod>] 
    member this.ValidRedLineEvaluator () =
        let ast = [Forward(50, Red)]
        let svg = eval ast
        let expected = "<svg width=\"1000\" height=\"1000\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n"+
                       "<line x1=\"500\" y1=\"500\" x2=\"500\" y2=\"450\" style=\"stroke:rgb(255,0,0);stroke-width:2\" />\n" + "</svg>\n"        
        Assert.AreEqual(expected, svg)

    [<TestMethod>]
    member this.ValidColorsEvaluator () =
        let red = evalColor Red
        let green = evalColor Green
        let gold = evalColor Gold
        let random = evalColor (RGB(255, 199,123))
        let actual = red + " " + green + " " + gold + " " + random
        let expected = "rgb(255,0,0) rgb(0,255,0) rgb(255,215,0) rgb(255,199,123)"        
        Assert.AreEqual(expected,actual)

    [<TestMethod>]
    // uses polygon command
    member this.ValidCommandEvaluator () =
        let poly = Polygon(Green, Red, [(10, 10); (100,100); (200,300)])
        let state = { position = {x=0; y=0}; direction = North; pen_up = false}
        let eval_result = evalCommand poly state//Color color (int*int) list
        
        let actual = eval_result
        let expected = "<polygon fill =\"" +   "rgb(0,255,0)" + "\"" +
                                        " stroke=\"" +  "rgb(255,0,0)" + "\"" +
                                        " points=\" " +
                                        "10,10  100,100  200,300  " +
                                        "\"" +
                                        " stroke-width =\"2\" />\n", state      
        Assert.AreEqual(expected,actual)

    [<TestMethod>]
    member this.ValidDrawEvaluator () =
        let lines = [Forward(50, Green); TurnRight; Forward(100, Purple); Penup]
        let state = { position = {x=200; y=200}; direction = North; pen_up = false}
        let eval_result = evalDraw lines state

        let actual = eval_result
        let first_line =   "<line x1=\"" + "200" + "\"" +
                                    " y1=\"" + "200" + "\"" +
                                    " x2=\"" + "200" + "\"" +
                                    " y2=\"" + "150" + "\"" +
                                    " style=\"stroke:" +
                                    "rgb(0,255,0)" + ";stroke-width:2\" />\n"
        let second_line = "<line x1=\"" + "200" + "\"" +
                                    " y1=\"" + "150" + "\"" +
                                    " x2=\"" + "300" + "\"" +
                                    " y2=\"" + "150" + "\"" +
                                    " style=\"stroke:" +
                                    "rgb(128,8,165)" + ";stroke-width:2\" />\n"
        let expected =  first_line + second_line, { position = {x=300; y=150}; direction = East; pen_up = true}
        Assert.AreEqual(expected,actual)

