module StringSet = Set.Make(String)

let cartesian_product set1 set2 =
  let product_set = ref StringSet.empty in
  StringSet.iter (fun elem1 ->
    StringSet.iter (fun elem2 ->
      product_set := StringSet.add (elem1 ^ elem2) !product_set
    ) set2
  ) set1;
  !product_set

let getRules grammar set = 
  let rule_set = ref StringSet.empty in
    List.iter (fun (rule,prods) -> if StringSet.cardinal (StringSet.inter set prods) > 0 then rule_set := StringSet.add rule !rule_set) grammar
    ; !rule_set

let cyk_algorithm grammar sentence =
  let n = Array.length sentence in
  let table = Array.make_matrix n n (StringSet.empty) in

  (*Inicialização*)
  Array.iteri (fun i el -> (
    List.iter (fun (rule, prods) -> if StringSet.exists ((=) sentence.(i)) prods then table.(0).(i) <- StringSet.add rule (table.(0).(i))) grammar
  )) table.(0); 

  (* Step 2: |substrings| > 1 *)
  for i=1 to n-1 do 
    for j=0 to n-i-1 do 
      for k=0 to i-1 do 
        table.(i).(j) <- StringSet.union (table.(i).(j)) (getRules grammar (cartesian_product (table.(k).(j)) (table.(i-k-1).(j+k+1))));
      done
    done
  done;table
  
let is_sentence_in_language table start_symbol sentence =
  StringSet.mem start_symbol table.(Array.length sentence - 1).(0);;

let print_cyk_table cyk_table =
    Array.iteri (fun i row ->
      Array.iteri (fun j entry ->
        Printf.printf "[%d, %d]: " i j;
        StringSet.iter (fun elem -> Printf.printf "%s " elem) entry;
        Printf.printf "\n"
      ) row
    ) cyk_table;
    Printf.printf "\n"

let () =
  let cfg = [
    ("S", StringSet.of_list ["AB";"BC"]);
    ("A", StringSet.of_list ["aA";"a"]);
    ("B", StringSet.of_list ["AB";"a"])
  ] in

  let input_sentence = "a a a" |> String.split_on_char ' ' |> Array.of_list in
  let cyk_table = cyk_algorithm cfg input_sentence in
  print_cyk_table cyk_table;

  let start_symbol = "S" in
  if is_sentence_in_language cyk_table start_symbol input_sentence then
    Printf.printf "The sentence \"%s\" is in the language of the grammar.\n" (String.concat " " (Array.to_list input_sentence))
  else
    Printf.printf "The sentence \"%s\" is not in the language of the grammar.\n" (String.concat " " (Array.to_list input_sentence))
