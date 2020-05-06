module Oware
open System

type StartingPosition =
    | South
    | North

type Board = {
    houses: int*int*int*int*int*int*int*int*int*int*int*int  //Houses 1-12  Houses 1-6 belong to South player and Houses 7-12 belong to North player
    score : int*int // (south, north)
    pos: StartingPosition //checks whos turn it is true = South's turn whereas false = North's turn
    currentPos: int
    }

let getSeeds n board = 
    let (a,b,c,d,e,f,g,h,i,j,k,l) = board.houses
    match n with
    | 1 -> a | 2 -> b | 3 -> c | 4 -> d | 5 -> e | 6 -> f
    | 7 -> g | 8 -> h | 9 -> i | 10 -> j | 11 -> k | 12 -> l | _ -> failwith "Error"

let gameState board =  //Checks the state of the game and returns the state at which the game is in
    let south,north = board.score 
    match board.pos with
    | South ->
        match south=24 && north=24 with
        | true -> "Game ended in a draw"
        | false -> 
            match south>24 with
            | true -> "South won"
            | false -> "South's turn"
    | North ->
        match south=24 && north=24 with
        | true -> "Game ended in a draw"
        | false ->
            match north>24 with
            | true -> "North won"
            | false -> "North's turn"

let resetter board n = 
    let (a,b,c,d,e,f,g,h,i,j,k,l) = board.houses
    match n with
    | 1 -> {board with houses=(0,b,c,d,e,f,g,h,i,j,k,l)}
    | 2 -> {board with houses=(a,0,c,d,e,f,g,h,i,j,k,l)}
    | 3 -> {board with houses=(a,b,0,d,e,f,g,h,i,j,k,l)}
    | 4 -> {board with houses=(a,b,c,0,e,f,g,h,i,j,k,l)}
    | 5 -> {board with houses=(a,b,c,d,0,f,g,h,i,j,k,l)}
    | 6 -> {board with houses=(a,b,c,d,e,0,g,h,i,j,k,l)}
    | 7 -> {board with houses=(a,b,c,d,e,f,0,h,i,j,k,l)}
    | 8 -> {board with houses=(a,b,c,d,e,f,g,0,i,j,k,l)}
    | 9 -> {board with houses=(a,b,c,d,e,f,g,h,0,j,k,l)}
    | 10 -> {board with houses=(a,b,c,d,e,f,g,h,i,0,k,l)}
    | 11 -> {board with houses=(a,b,c,d,e,f,g,h,i,j,0,l)}
    | 12 -> {board with houses=(a,b,c,d,e,f,g,h,i,j,k,0)}
    | _ -> failwith "Error"

let southScore board =
    let rec adder board seeds = 
        let (a,b,c,d,e,f,g,h,i,j,k,l) = board.houses
        let south,north = board.score
        match (seeds=3 || seeds=2) with
        | true -> 
            match board.currentPos with
            | 7-> {board with houses=(a,b,c,d,e,f,0,h,i,j,k,l);score=(south+g,north)}
            | 8-> adder {board with houses=(a,b,c,d,e,f,g,0,i,j,k,l);score=(south+h,north);currentPos=7} g
            | 9-> adder {board with houses=(a,b,c,d,e,f,g,h,0,j,k,l);score=(south+i,north);currentPos=8} h
            | 10-> adder {board with houses=(a,b,c,d,e,f,g,h,i,0,k,l);score=(south+j,north);currentPos=9} i
            | 11-> adder {board with houses=(a,b,c,d,e,f,g,h,i,j,0,l);score=(south+k,north);currentPos=10} j
            | 12-> adder {board with houses=(a,b,c,d,e,f,g,h,i,j,k,0);score=(south+l,north);currentPos=11} k
            | _ -> failwith "Error"
        | false -> board 
    let seeds = getSeeds board.currentPos board
    adder board seeds
    
let northScore board =
    let rec adder board seeds = 
        let (a,b,c,d,e,f,g,h,i,j,k,l) = board.houses
        let south,north= board.score
        match (seeds=3 || seeds=2) with
        | true -> 
            match board.currentPos with
            | 1-> {board with houses=(0,b,c,d,e,f,g,h,i,j,k,l);score=(south,north+a)}
            | 2 -> adder {board with houses=(a,0,c,d,e,f,g,h,i,j,k,l);score=(south,north+b); currentPos=1} a
            | 3 -> adder {board with houses=(a,b,0,d,e,f,g,h,i,j,k,l);score=(south,north+c); currentPos=2} b
            | 4 -> adder {board with houses=(a,b,c,0,e,f,g,h,i,j,k,l);score=(south,north+d); currentPos=3} c
            | 5 -> adder {board with houses=(a,b,c,d,0,f,g,h,i,j,k,l);score=(south,north+e); currentPos=4} d
            | 6 -> adder {board with houses=(a,b,c,d,e,0,g,h,i,j,k,l);score=(south,north+f); currentPos=5} e
            | _ -> failwith "Error"
        | false -> board 
    let seeds = getSeeds board.currentPos board
    adder board seeds

let scoreChecker board =
    let (south,north) = board.score
    match board.pos with
    | South ->
        match board.currentPos with
        | 7 | 8 | 9 | 10 | 11 | 12 -> southScore board
        | _ -> board
    | North ->
        match board.currentPos with
        | 1 | 2 | 3 | 4 | 5 | 6 -> northScore board
        | _ -> board

//used with useHouse
let useHouse n board = 
    let rec myHouse board seeds n=
        let (a,b,c,d,e,f,g,h,i,j,k,l) = board.houses
        match seeds>0 with
        | true->
           match (board.currentPos = n-1) with
           | true -> myHouse {board with currentPos=n} (seeds) n
           | false ->
                match board.currentPos with
                | 1 -> myHouse {board with houses=(a,b+1,c,d,e,f,g,h,i,j,k,l); currentPos=2} (seeds-1) n
                | 2 -> myHouse {board with houses=(a,b,c+1,d,e,f,g,h,i,j,k,l); currentPos=3} (seeds-1) n
                | 3 -> myHouse {board with houses=(a,b,c,d+1,e,f,g,h,i,j,k,l); currentPos=4} (seeds-1) n
                | 4 -> myHouse {board with houses=(a,b,c,d,e+1,f,g,h,i,j,k,l); currentPos=5} (seeds-1) n
                | 5 -> myHouse {board with houses=(a,b,c,d,e,f+1,g,h,i,j,k,l); currentPos=6} (seeds-1) n
                | 6 -> myHouse {board with houses=(a,b,c,d,e,f,g+1,h,i,j,k,l); currentPos=7} (seeds-1) n
                | 7 -> myHouse {board with houses=(a,b,c,d,e,f,g,h+1,i,j,k,l); currentPos=8} (seeds-1) n
                | 8 -> myHouse {board with houses=(a,b,c,d,e,f,g,h,i+1,j,k,l); currentPos=9} (seeds-1) n
                | 9 -> myHouse {board with houses=(a,b,c,d,e,f,g,h,i,j+1,k,l); currentPos=10} (seeds-1) n 
                | 10 -> myHouse {board with houses=(a,b,c,d,e,f,g,h,i,j,k+1,l); currentPos=11} (seeds-1) n
                | 11 -> myHouse {board with houses=(a,b,c,d,e,f,g,h,i,j,k,l+1); currentPos=12} (seeds-1) n 
                | 12 -> myHouse {board with houses=(a+1,b,c,d,e,f,g,h,i,j,k,l); currentPos=1} (seeds-1) n
                | _ -> failwith "Error"
        |false -> resetter board n
    let seeds = getSeeds n board
    myHouse {board with currentPos=n} seeds n
let start position =
    match position with
    | South -> {houses=(4,4,4,4,4,4,4,4,4,4,4,4); score=(0,0); pos=South; currentPos=0}
    | North -> {houses=(4,4,4,4,4,4,4,4,4,4,4,4); score=(0,0); pos=North; currentPos=0}

let score board = board.score
let playGame board =
    let rec play newboard count =
        let board = scoreChecker newboard
        printf "\nIt is %A \nThe score is: %A \nAnd the board is as stands: %A" (gameState board) (score board) (board)
        let x = Console.ReadLine() |> int
        match gameState board with
        | "Game ended in a draw" | "North won" | "South won" -> board
        | _ -> 
            match count%2=0,board.pos with
            | true,South ->
                match x with
                | 1|2|3|4|5|6 -> play (useHouse x {board with pos=South}) (count+1)
                | _ -> play board count 
            | false,North ->
                match x with
                | 7|8|9|10|11|12 -> play (useHouse x {board with pos=North}) (count+1)
                | _ -> play board count
            | _ -> play board count
    match board.pos with
    | South -> play board 0
    | North -> play board 1

[<EntryPoint>]
let main _ =
    let game = start South
    let play = playGame game
    printf "%A and %s" play (gameState play)
    0 // return an integer exit code
