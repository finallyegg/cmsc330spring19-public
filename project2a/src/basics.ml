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
        | [] -> 0
        | a :: b -> if a = target then 1 + count_occ b target else count_occ b target
let rec count_occ1 lst target = match lst with
    | [] -> false 
    | a :: b -> if a = target then true else count_occ1 b target

let rec dup_list lst = match lst with
    | [] -> false
    | a::b -> if (count_occ1 b a) then true else dup_list b

(****************)
(* Part 3: Sets *)
(****************)

let rec elem x a = count_occ1 a x

let rec insert x a = match a with
        | [] -> [x]
        | _ -> if (count_occ1 a x) then a else x::a 

let rec subset a b = match a with
        | []-> true
        | x::y -> if (count_occ1 b x) then subset y b else false 

let rec eq a b = ((subset a b )&&( subset b a))

let rec remove x a = match a with
        | [] -> []
        | j::k -> if j = x then k else j::(remove x k)

let rec union a b = match a with
        | [] -> b
        | x::y -> union y (insert x b) 
let rec diff_help a b = match a with
        | [] -> []
        | x::y -> if (count_occ1 b x) then diff_help y b else x:: diff_help y b

let rec diff a b = union(diff_help b a) (diff_help a b)

let rec cat x a = match a with 
        | [] -> []
        | j::k -> (x,j)::cat x k
