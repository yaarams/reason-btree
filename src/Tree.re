[%%debugger.chrome]
type tree = Empty | Node(int, tree, tree);

let rec insert = (t, newValue) => switch(t) {
    | Empty => Node(newValue, Empty, Empty)
    | Node(value, left, right) when (newValue < value) => Node(value, insert(left, newValue), right)
    | Node(value, left, right) when (newValue > value) => Node(value, left, insert(right, newValue))
    | _ => t
    };

let make = (arr) => List.fold_left(insert, Empty, arr);
let rec sum = 
    (t) => switch(t) {
        | Empty => 0
        | Node(value, left, right) => value + sum(left) + sum(right)
    };

let x = make([1,2,3])
Js.log(x)