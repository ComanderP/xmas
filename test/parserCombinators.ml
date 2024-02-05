open Xmas
open QCheck

(* let rec remove index list =
  if index == 0 then List.tl list
  else List.hd list :: remove (index - 1) (List.tl list)

(* Example property and unit test *)
let prop_remove =
  QCheck.Test.make ~count:1000 ~name:"Removes are equivalent"
    (tup2 (list int) small_nat)
    (fun (l, i) ->
      assume (List.length l > i && l <> []);
      Utils.remove i l = remove i l)

let test_replace () =
  let l = [ 1; 2; 3; 4; 5 ] in
  let result = Utils.replace 2 l 0 in
  Alcotest.(check $ list int) "Test result" [ 1; 2; 0; 4; 5 ] result *)


(* let () =
  let open Alcotest in
  let qcheck_props = [ prop_remove ] in
  run "Basic Parser Tests"
    [
      ("Replace Tests", [ test_case "Replace Test" `Quick test_replace ]);
      ("QuickCheck Tests", List.map QCheck_alcotest.to_alcotest qcheck_props);
    ] *)