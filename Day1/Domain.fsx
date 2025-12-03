
#r "nuget: Unquote"


open System
open Swensen.Unquote


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// Domain

type FileName = string


module File =

    type ReadFile = FileName -> string list


    let readFile : ReadFile =
        fun fileName ->
            System.IO.File.ReadAllLines(fileName)
            |> Array.toList


type Clicks = int


type Movement = | R of Clicks | L of Clicks


module Movement =


    type ToString = Movement -> string


    let toString : ToString =
        function
        | R clicks -> sprintf "R%d" clicks
        | L clicks -> sprintf "L%d" clicks

    
    type Parse = string list -> Movement list


    let parse : Parse =
        fun inputs ->
            inputs
            |> List.map (fun s ->
                let direction = s.[0]
                let clicks = Int32.Parse(s.[1..])
                match direction with
                | 'R' -> R clicks
                | 'L' -> L clicks
                | _ -> failwithf "Invalid direction: %c" direction)


    type ToDirectedClicks = Movement -> Clicks


    let toDirectedClicks : ToDirectedClicks =
        function
        | R clicks -> clicks
        | L clicks -> -clicks



type Position = int


type Count = int


type Ring = { Start: Position; Length : Count; CurrentPosition : Position }


module Ring =
    

    type Create = Position -> Count -> Position -> Ring


    let create : Create =
        fun start length currentPosition ->
            { Start = start; Length = length; CurrentPosition = currentPosition }


    type ToString = Ring -> string


    let toString : ToString =
        fun ring ->
            sprintf "Ring(Start: %d, Length: %d, CurrentPosition: %d)" ring.Start ring.Length ring.CurrentPosition


    type LastRingPosition = Ring -> Position


    let lastRingPosition : LastRingPosition =
        fun ring ->
            ring.Start + ring.Length - 1


    type CompletedCycles = Count


    type RemaingMovement = Movement


    /// count is the length of the ring
    type CalcCompletedCyclesAndRemaingMovement = Count -> Movement -> (CompletedCycles * RemaingMovement)


    // uses Movement.toDirectedClicks to convert Movement to Clicks
    let calcCompletedCyclesAndRemaingMovement : CalcCompletedCyclesAndRemaingMovement =
        fun ringLength movement ->
            let directedClicks =
                Movement.toDirectedClicks movement

            let completedCycles =
                directedClicks / ringLength

            let remainingClicks =
                directedClicks % ringLength

            let remainingMovement =
                if remainingClicks >= 0 then
                    R remainingClicks
                else
                    L -remainingClicks

            completedCycles, remainingMovement


    type Move = Ring -> Movement ->  (CompletedCycles * RemaingMovement * Ring)


    // uses Movement.toDirectedClicks
    // uses calcCompletedCirclesAndRemaingMovement
    let move : Move =
        fun ring movement ->
            let completedCircles, remainingMovement =
                calcCompletedCyclesAndRemaingMovement ring.Length movement

            let directedClicks =
                Movement.toDirectedClicks remainingMovement

            let newPosition =
                (ring.CurrentPosition + directedClicks + ring.Length) % ring.Length

            let newRing =
                { ring with CurrentPosition = newPosition }

            completedCircles, remainingMovement, newRing


    type IsRingAtPosition = Position -> Ring -> bool


    let isRingAtPosition : IsRingAtPosition =
        fun position ring ->
            ring.CurrentPosition = position


    type CountRingAtPosition = Position -> Movement list -> Ring -> Count * Ring


    // only use move and isRingAtPosition
    let countRingAtPosition : CountRingAtPosition =
        fun position movements ring ->
            let rec loop currentRing remainingMovements totalCount =
                match remainingMovements with
                | [] -> totalCount, currentRing
                | movement :: rest ->
                    let _, _, newRing =
                        move currentRing movement
                    let countIncrement =
                        if isRingAtPosition position newRing then 1 else 0
                    loop newRing rest (totalCount + countIncrement)
            loop ring movements 0


    type CalculateRemainingMovePositions = Movement -> Ring -> Position list


    // uses Movement.toDirectedClicks
    let calculateRemainingMovePositions : CalculateRemainingMovePositions =
        fun movement ring ->
            let directedClicks =
                Movement.toDirectedClicks movement  
            let step =
                if directedClicks >= 0 then 1 else -1
            let positions =
                [1 .. Math.Abs directedClicks]
                |> List.map (fun i ->
                    (ring.CurrentPosition + i * step + ring.Length) % ring.Length
                )
            positions


    type CountPositionTransversals = Position -> Movement -> Ring -> Count * Ring


    // only use move and calculateRemainingMovePositions
    let countPositionTransversals : CountPositionTransversals =
        fun position movement ring ->
            let completedCycles, remainingMovement, newRing =
                move ring movement

            let baseCount = abs completedCycles // each completed cycle crosses the position once

            let remainingPositions =
                calculateRemainingMovePositions remainingMovement ring

            let additionalCount =
                remainingPositions
                |> List.filter (fun pos -> pos = position)
                |> List.length

            baseCount + additionalCount, newRing


    type CountPositionEncounters = Position -> Movement list -> Ring -> Count * Ring


    let countPositionEncounters : CountPositionEncounters =
        fun position movements ring ->
            let rec loop currentRing remainingMovements totalCount =
                match remainingMovements with
                | [] -> totalCount, currentRing
                | movement :: rest ->
                    let count, newRing =
                        countPositionTransversals position movement currentRing
                    loop newRing rest (totalCount + count)
            loop ring movements 0


module Tests =


    let ``test calculation of completed cycles and remaining movement with ring lenght 1`` () =
        let ring = Ring.create 0 1 0
        let testCases =
            [
                R 0, (0, R 0)
                R 1, (1, R 0)
                R 2, (2, R 0)
                R 3, (3, R 0)
                L 0, (0, R 0)
                L 1, (-1, R 0)
                L 2, (-2, R 0)
                (*
                L 3, (-3, L 0)
                *)
            ]
        testCases
        |> List.iter (fun (movement, expected) ->
            let result =
                Ring.calcCompletedCyclesAndRemaingMovement ring.Length movement

            result =! expected
        )


    let ``test calculation of completed cycles and remaining movement with ring lenght 5`` () =
        let ring = Ring.create 0 5 0
        let testCases =
            [
                R 0, (0, R 0)
                R 1, (0, R 1)
                R 4, (0, R 4)
                R 5, (1, R 0)
                R 6, (1, R 1)
                R 10, (2, R 0)
                L 0, (0, R 0)
                L 1, (0, L 1)
                L 4, (0, L 4)
                L 5, (-1, R 0)
                L 6, (-1, L 1)
                L 12, (-2, L 2)
            ]
        testCases
        |> List.iter (fun (movement, expected) ->
            let result =
                Ring.calcCompletedCyclesAndRemaingMovement ring.Length movement

            result =! expected
        )


    let ``test movement of ring with length 5`` () =
        let ring = Ring.create 0 5 2
        let testCases =
            [
                R 0, (0, R 0, { ring with CurrentPosition = 2 })
                R 1, (0, R 1, { ring with CurrentPosition = 3 })
                R 3, (0, R 3, { ring with CurrentPosition = 0 })
                R 5, (1, R 0, { ring with CurrentPosition = 2 })
                L 1, (0, L 1, { ring with CurrentPosition = 1 })
                L 3, (0, L 3, { ring with CurrentPosition = 4 })
                L 5, (-1, R 0, { ring with CurrentPosition = 2 })
            ]
        testCases
        |> List.iter (fun (movement, expected) ->
            let result =
                Ring.move ring movement

            result =! expected
        )


    let ``test calculate remaining move positions`` () =
        let ring = Ring.create 0 5 2
        let testCases =
            [
                R 3, [3; 4; 0]
                L 2, [1; 0]
                R 0, [ ]
                L 0, [ ]
            ]
        testCases
        |> List.iter (fun (movement, expected) ->
            let result =
                Ring.calculateRemainingMovePositions movement ring

            result =! expected
        )

    let ``test count position transversals`` () =
        let ring = Ring.create 0 5 2
        let testCases =
            [
                (3, R 3), (1, { ring with CurrentPosition = 0 })
                (0, R 3), (1, { ring with CurrentPosition = 0 })
                (1, R 3), (0, { ring with CurrentPosition = 0 })
                (1, L 4), (1, { ring with CurrentPosition = 3 })
                (2, L 6), (1, { ring with CurrentPosition = 1 })
                (4, R 10), (2, { ring with CurrentPosition = 2 })
            ]
        testCases
        |> List.iter (fun ((position, movement), expected) ->
            let result =
                Ring.countPositionTransversals position movement ring

            result =! expected
        )


    let ``test count ring with length 5 start 2 at position 0`` () =
        let ring = Ring.create 0 5 2
        let movements =
            [
                R 3
                L 4
                R 6
            ]
        let testCases =
            [
                0, (1, { ring with CurrentPosition = 2 })
                1, (1, { ring with CurrentPosition = 2 })
                2, (1, { ring with CurrentPosition = 2 })
                3, (0, { ring with CurrentPosition = 2 })
                4, (0, { ring with CurrentPosition = 2 })
            ]
        testCases
        |> List.iteri (fun i (position, expected) ->
            let result =
                Ring.countRingAtPosition position movements ring
            printfn "Test case %d: Position %d, Result %A" i position result
            result =! expected
        )  


    let ``test count position encounters for a ring with length 5 and start 0`` () =
        let ring = Ring.create 0 5 2
        let movements =
            [
                R 3
                L 4
                R 6
            ]
        let testCases =
            [
                0, (2, { ring with CurrentPosition = 2 })
                1, (2, { ring with CurrentPosition = 2 })
                2, (3, { ring with CurrentPosition = 2 })
                3, (3, { ring with CurrentPosition = 2 })
                4, (3, { ring with CurrentPosition = 2 })
            ]
        testCases
        |> List.iteri (fun i (position, expected) ->
            let result =
                Ring.countPositionEncounters position movements ring
            printfn "Test case %d: Position %d, Result %A" i position result
            result =! expected
        )

    let runAllTests () =
        ``test calculation of completed cycles and remaining movement with ring lenght 1`` ()
        ``test calculation of completed cycles and remaining movement with ring lenght 5`` ()
        ``test movement of ring with length 5`` ()
        ``test calculate remaining move positions`` ()
        ``test count position encounters for a ring with length 5 and start 0`` ()
        ``test count ring with length 5 start 2 at position 0`` ()
        ``test count position transversals`` ()


let file : FileName = "Day1.txt"


let dial : Ring = Ring.create 0 100 50

// Run Tests

let movements =
    [
        "L68"
        "L30"
        "R48"
        "L5"
        "R60"
        "L55"
        "L1"
        "L99"
        "R14"
        "L82"
    ]


movements
|> Movement.parse
|> fun movs -> dial |> Ring.countRingAtPosition 0 movs


file
|> File.readFile
|> Movement.parse
|> fun movs -> dial |> Ring.countRingAtPosition 0 movs

file
|> File.readFile
|> Movement.parse
|> fun movs -> dial |> Ring.countPositionEncounters 0 movs