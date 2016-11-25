(* Ex 2.3 *)
let test1 = not (true && false)

let test2 = float_of_int (int_of_float 5.0)

let test3 = (sin (3.14 /. 2.0) ** 2.0) +. (cos (3.14 /. 2.0) ** 2.0)

let test4 = sqrt (float_of_int ((3 * 3) + (4 * 4)))


(* Ex 2.4 *)
let my_and x y =
  if (x == true) then
	if (y == true) then
	  true
	else
      false
  else
	false

let my_or x y =
  if (x == true) then
	if (y == true) then
	  true
	else
      false
  else
	false

(* Ex 2.6 *)
let rounding y =
	if (y -. (floor y)) < 0.5 then
	  floor y
	else
      (floor y) +. 1.0

let dollar_to_yen x =
  int_of_float (rounding (x *. 111.12))

let yen_to_dollar x =
  rounding (x /. 111.12)

let dollar_to_string x =
  (string_of_int (int_of_float x))
  ^ " dollars are "
  ^ (string_of_int (dollar_to_yen x))
  ^ " yen."

let capitalize x =
  let num_x = int_of_char x in
  let num_a = int_of_char 'a' in
  let num_z = int_of_char 'z' in
  let num_cap = int_of_char 'A' - int_of_char 'a' in
  if((num_x >= num_a)
	 && (num_x <= num_z))
  then
	char_of_int (num_x + num_cap)
  else
	x

(* Ex 3.7 *)
let rec pow_1 x n =
  if(n < 1)
  then 1
  else x * (pow x (n - 1))

let rec pow_2 x n =
  if(n < 1)
  then 1
  else
	if((n mod 2) == 0)
	then
	  let hoge = pow_2 x (n / 2)
	  in (hoge * hoge)
	else
	  let hoge = pow_2 x (n / 2)
	  in (hoge * hoge) * x

(* Ex 3.11 *)
let rec gcd x y =
  let sub = x - y
  in if(sub = 0) then x
	 else if(sub > 0)
	 then
	   gcd y sub
	 else
	   gcd x (- sub)


let comb x y =
  let rec fact x =
	if (x < 1)
	then 1
	else x * fact(x - 1) in
  (fact x) / ((fact (x - y)) * (fact y))
