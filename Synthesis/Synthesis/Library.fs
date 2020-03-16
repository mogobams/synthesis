module Synthesis

let abelar number =
    number > 12 && number < 3097 && number % 12 = 0

let area abase height =
    match abase < 0.0 || height < 0.0 with
    | true -> failwith "cannot have negative length"
    | _ -> (abase * 0.5) * height

let zollo number =
    match number > 0 with 
    | true -> number * 2
    | _ -> number * -1

let min num1 num2 =
    match num1 < num2 with
    | true -> num1
    | _ -> num2

let max num1 num2 =
    match num1 > num2 with
    | true -> num1
    | _ -> num2

let ofTime hours mins seconds =
    (hours * 3600) + (mins * 60) + seconds

let toTime value =
    let hours = value / 3600
    let mins = (value % 3600) / 60 
    let seconds = (value % 3600) % 60

    match value < 0 with
    | true -> 0, 0, 0
    | _ -> hours, mins, seconds
        
let digits number =
    let rec loop value acc =
        match (value / 10) = 0 with
        | true -> acc + 1
        | _ -> loop (value / 10) (acc + 1)

    loop number 0

let minmax atuple =
    let a, b, c, d = atuple
    
    min (min a b) (min c d), max (max a b) (max c d)
    
let isLeap year =
    match year < 1582 with
    | true -> failwith "you are too far into the past now, come back!"
    | _ -> match year % 4 = 0 with
            | true -> match year % 400 = 0 with
                        | true -> true
                        | _ -> match year % 100 = 0 with
                                | true -> false
                                | _ -> true
            | _ -> false
        
let month value =
    match value with
    | 1 -> "January", 31
    | 2 -> "February", 28
    | 3 -> "March", 31
    | 4 -> "April", 30
    | 5 -> "May", 31
    | 6 -> "June", 30
    | 7 -> "July", 31
    | 8 -> "August", 31
    | 9 -> "September", 30
    | 10 -> "October", 31
    | 11 -> "November", 30
    | 12 -> "December", 31
    | _ -> failwith "input a value between 1 and 12 (inclusive)"

let toBinary number =
    let rec conversion value =
        match value < 0 with
        | true -> failwith "enter a positive integer, man"
        | _ -> match value with
                | 0 -> "0"
                | 1 -> "1"
                | _ -> match (value % 2) = 0 with
                        | true -> conversion (value / 2) + "0" 
                        | _ -> conversion (value / 2) + "1" 
             
    conversion number

let bizFuzz n =
    let rec divisible value acc3 acc5 acc35 = 
        match value <= 0 with
        | true -> acc3, acc5, acc35
        | _ -> match ((value % 3 = 0), (value % 5 = 0), (value % 3 = 0 && value % 5 = 0)) with
                | true, true, true -> divisible (value - 1) (acc3 + 1) (acc5 + 1) (acc35 + 1)
                | true, false, false -> divisible (value - 1) (acc3 + 1)  acc5 acc35
                | false, true, false -> divisible (value - 1) acc3 (acc5 + 1) acc35
                | false, false, true -> divisible (value - 1) acc3 acc5 (acc35 + 1)
                | false, false, false -> divisible (value - 1) acc3 acc5 acc35

    divisible n 0 0 0

let monthDay d y =
    match isLeap (y) with
    | true -> match d < 1 || d > 366 || y < 1582 with
                | true -> failwith "you are out of range. check your day and year and try again"
                | _ -> let rec loop value acc = 
                        let a, b = month acc
                        match acc with
                        | 2 ->  match value <= (b + 1) with
                                | true -> a
                                | _ -> loop (value - (b + 1)) (acc + 1)
                        | _ -> match value <= b with
                                | true -> a
                                | _ -> loop (value - b) (acc + 1) 
                       loop d 1

    | _ -> match d < 1 || d > 365 || y < 1582 with
            | true -> failwith "you are out of range. check your day and year and try again"
            | _ -> let rec loop value acc = 
                    let a, b = month acc
                    match value <= b with
                    | true -> a
                    | _ -> loop (value - b) (acc + 1)
                   loop d 1
                               
let coord x =
    let sqrt n = 
        let rec calculate guess i =
            match i with
            | 10 -> guess
            | _ ->
                let g = (guess + n/guess)/2.0
                calculate g (i + 1)
        match n <= 0.0 with
        | true -> failwith "too complex, bro!"
        | _ -> calculate (n / 2.0) 0
    
    failwith "i tried!"
    