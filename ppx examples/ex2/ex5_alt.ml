let[@foo] x=2 in x+1;;

let x=ref 1;;

begin[@foo][@foo2] x := !x+1; x := !x+1; end;;

let e = Some 2;;

match%foo e with
| None -> 0
| Some n -> n;;

let%foo x=2 in x+1;;
