open Expecto
open Expecto.Flip
open FsCheck

open SimpleDiff

let createTestCase before after expected =
    let name = sprintf "%A -> %A" before after
    let test _ =
        List.diff before after
        |> Expect.equal "Unexpected diff" expected
    testCase name test

let (|NonEmptyList|) (NonEmptyArray items) =
    List.ofArray items

[<Tests>]
let tests =
    testList "Tests" [
        testList "no changes" [
            testCase "empty -> empty" <| fun _ ->
                List.diff [] []
                |> Expect.isEmpty "No diffs expected"

            testProperty "non-empty -> non-empty" <| fun (NonEmptyList items) ->
                List.diffBy hash items items
                |> Expect.equal "Expected a single `Equal` diff" [Equal items]
        ]

        testList "adding initial items and clearing" [
            testProperty "empty -> non-empty" <| fun (NonEmptyList items) ->
                List.diffBy hash [] items
                |> Expect.equal "Expected a single `Added` diff" [Added items]

            testProperty "non-empty -> empty" <| fun (NonEmptyList items) ->
                List.diffBy hash items []
                |> Expect.equal "Expected a single `Removed` diff" [Removed items]
        ]

        testList "diffing" [
            testProperty "can rebuild \"after\" list from diffs" <| fun (before : int list) after ->
                let diffs = List.diff before after
                let afterFromDiffs =
                    ([], diffs)
                    ||> List.fold (fun acc diff ->
                        match diff with
                        | Equal xs
                        | Added xs -> acc @ xs
                        | Removed _ -> acc)

                Expect.equal "Should be the same as \"after\"" after afterFromDiffs
        ]

        // These are the same tests as in the original implementation
        testList "upstream" [
            testList "insert" [
                createTestCase [1;3;4] [1..4] [
                    Equal [1]
                    Added [2]
                    Equal [3;4]
                ]

                createTestCase [1;2;3;8;9;12;13] [1..15] [
                    Equal [1..3]
                    Added [4..7]
                    Equal [8..9]
                    Added [10..11]
                    Equal [12..13]
                    Added [14..15]
                ]

                createTestCase [1..5] [1;2;2;3;4;5] [
                    Equal [1]
                    Added [2]
                    Equal [2..5]
                ]

                createTestCase [1..5] [1;2;2;3;4;4;5] [
                    Equal [1]
                    Added [2]
                    Equal [2..4]
                    Added [4]
                    Equal [5]
                ]

                createTestCase [1..5] [1;2;1;2;3;3;2;1;4;5] [
                    Added [1..2]
                    Equal [1..3]
                    Added [3;2;1]
                    Equal [4..5]
                ]
            ]

            testList "delete" [
                createTestCase [1..5] [1;2;5] [
                    Equal [1;2]
                    Removed [3;4]
                    Equal [5]
                ]

                createTestCase [1..8] [3;6;7] [
                    Removed [1..2]
                    Equal [3]
                    Removed [4..5]
                    Equal [6..7]
                    Removed [8]
                ]

                createTestCase ([1..5] @ [1..5]) [1..5] [
                    Equal [1..5]
                    Removed [1..5]
                ]
            ]

            testList "words" [
                let words (str : string) = str.Split ' ' |> List.ofArray

                yield createTestCase (words "The quick brown fox") (words "The slow green turtle") [
                    Equal ["The"]
                    Removed (words "quick brown fox")
                    Added (words "slow green turtle")
                ]

                yield createTestCase (words "jumps over the lazy dog") (words "walks around the orange cat") [
                    Removed (words "jumps over")
                    Added (words "walks around")
                    Equal ["the"]
                    Removed (words "lazy dog")
                    Added (words "orange cat")
                ]
            ]

            testList "chars" [
                let charsOf (str : string) = str.ToCharArray () |> List.ofArray

                yield createTestCase (charsOf "The quick brown fox.") (charsOf "The kuick brown fix.") [
                    Equal (charsOf "The ")
                    Removed (charsOf "q")
                    Added (charsOf "k")
                    Equal (charsOf "uick brown f")
                    Removed (charsOf "o")
                    Added (charsOf "i")
                    Equal (charsOf "x.") ]
            ]
        ]
    ]

[<EntryPoint>]
let main argv =
    runTestsWithArgs defaultConfig argv tests
