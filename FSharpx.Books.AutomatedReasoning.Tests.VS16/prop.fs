module FSharpx.Books.AutomatedReasoning.Tests.VS16

open NUnit.Framework

open FSharpx.Books.AutomatedReasoning.lib
open FSharpx.Books.AutomatedReasoning.intro
open FSharpx.Books.AutomatedReasoning.formulas
open FSharpx.Books.AutomatedReasoning.prop

let private evalPropFormula_1_Values : (string * (formula<prop> -> formula<prop>) * formula<prop> * string)[] = [| 
    (
        // idx 0
        // prop.p001
        @"p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)",
        (fun x -> And(x,x)),
        And
          (Iff
             (Imp (Atom (P "p"),Atom (P "q")),
              Or
                (And (Atom (P "r"),Atom (P "s")),
                 Iff (Atom (P "t"),And (Not (Not (Atom (P "u"))),Atom (P "v"))))),
           Iff
             (Imp (Atom (P "p"),Atom (P "q")),
              Or
                (And (Atom (P "r"),Atom (P "s")),
                 Iff (Atom (P "t"),And (Not (Not (Atom (P "u"))),Atom (P "v")))))),
        @"<<(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)) /\" + 
          " (p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))>>"
    );
    (
        // idx 1
        // prop.p002
        @"p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)",
        (fun x -> And (Or (x, x), x)),
        And
          (Or
             (Iff
                (Imp (Atom (P "p"),Atom (P "q")),
                 Or
                   (And (Atom (P "r"),Atom (P "s")),
                    Iff (Atom (P "t"),And (Not (Not (Atom (P "u"))),Atom (P "v"))))),
              Iff
                (Imp (Atom (P "p"),Atom (P "q")),
                 Or
                   (And (Atom (P "r"),Atom (P "s")),
                    Iff (Atom (P "t"),And (Not (Not (Atom (P "u"))),Atom (P "v")))))),
           Iff
             (Imp (Atom (P "p"),Atom (P "q")),
              Or
                (And (Atom (P "r"),Atom (P "s")),
                 Iff (Atom (P "t"),And (Not (Not (Atom (P "u"))),Atom (P "v")))))),
        @"<<((p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)) \/ " + 
            "(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))) /\ " + 
            "(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))>>"
    );
    |]

[<Test>]
[<TestCase(0, TestName = "prop.p001")>]
[<TestCase(1, TestName = "prop.p002")>]
let ``eval prop formula 1`` idx = 
    let (prop_formula_1, _, _, _) = evalPropFormula_1_Values.[idx]
    let (_, prop_formula_2, _, _) = evalPropFormula_1_Values.[idx]
    let (_, _, astResult, _) = evalPropFormula_1_Values.[idx]
    let (_, _, _, stringResult) = evalPropFormula_1_Values.[idx]
    
    //The parsed string is And'ed together
    let result = 
        parse_prop_formula prop_formula_1
        |> prop_formula_2

    //Is the parsed version And'ed together above equivalent to the code version
    Assert.AreEqual(result, astResult)

    //Is the string version of the result equivalent to the string version
    Assert.AreEqual(sprint_prop_formula result, stringResult)
