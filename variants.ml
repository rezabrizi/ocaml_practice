(* tuple of 2 floats *)
type point = float * float

(* variants: data types that take several different forms  *)
type shape = 
(* of is used to specify the type of the data that the variant carries *)
| Circle of {
  center: point;
  radius: float; 
}
| Rectangle of {
  lower_left: point;
  upper_right: point; 
}


let area = function 
| Circle {center; radius} -> 3.14 *. radius *. radius 
| Rectangle {lower_left; upper_right} -> begin
  let width = fst upper_right -. fst lower_left in 
  let height = snd upper_right -. snd lower_left in 
  width *. height

end 

let circle = Circle {center = (0.0, 0.0); radius = 3.0}

let () = Printf.printf "Area of circle: %f\n" (area circle); 