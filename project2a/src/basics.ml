(******************************)
(* Part 1: Simple Functions   *)
(******************************)

let dup a b c = (a=b||b=c||a=c)

let head_divisor lst = match lst with 
        | []->false
        | a::[] ->false
        | a::b::c -> if (b mod a=0) then true else false

let second_element lst = match lst with 
        | [] -> -1
        | a::[] -> -1
        | a::b::c -> b

let max_first_three lst = match lst with 
        | [] -> -1
        | a :: [] -> a
        | a :: b :: [] -> (max a b)
        | a :: b :: c :: d -> max (max a b) (max b c)

(*********************************)
(* Part 2: Recursive Functions   *)
(*********************************)

let rec cubes n = match n with 
    | 0 -> 0
    | a -> a*a*a + cubes (n-1) 

let rec sum_odd lst = match lst with 
    | [] -> 0
    | a::b -> if (a mod 2 = 1) then a + sum_odd b else sum_odd b

let rec is_even_sum lst = match lst with 
    | [] -> true
    | a :: b -> if ((a mod 2 = 0) && is_even_sum b) || (a mod 2 = 1) && not (is_even_sum b) then true else false  

let rec count_occ lst target = match lst with
    | [] -> false 
    | a :: b -> if a = target then true else count_occ b target

let rec dup_list lst = match lst with
    | [] -> false
    | a::b -> if (count_occ b a) then true else dup_list b

(****************)
(* Part 3: Sets *)
(****************)

let rec elem x a = failwith "unimplemented"

let rec insert x a = failwith "unimplemented"

let rec subset a b = failwith "unimplemented"

let rec eq a b = failwith "unimplemented"

let rec remove x a = failwith "unimplemented"

let rec union a b = failwith "unimplemented"

let rec diff a b = failwith "unimplemented"

let rec cat x a = failwith "unimplemented"
