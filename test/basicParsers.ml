(* open QCheck2
open Xmas

(*
   let () =
     let open Alcotest in
     run "Basic Parser Tests"
       [
         ( "char",
           [
             test_case "a" `Quick (fun () -> check (option char) "a" (Some 'a') a);
             test_case "b" `Quick (fun () ->
                 check (option char) "b" None (runParser "b" (char 'a')));
           ] );
       ] *) *)
