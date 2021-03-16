open Format
exception ListVide of string

(* Fonctions utilitaires *)
let printlist l = List.iter (printf " %d ") l;;
let printlistlist l = List.iter(fun ll -> printf " [";printlist ll;printf "] ") l;;


(* Question 1.1 *)
let printn_char n c =
    if n > 0 then 
    for i = 1 to n do
        print_char c
        
    done;
;;



(* Question 1.2 *)
let etoiles n =
    printn_char n '*'
;;

let espaces n =
    printn_char n ' '
;;



(* Question 1.3 *)
let maximum l = 
    match l with
    [] -> raise (ListVide "La liste est vide!")
    |h1::t1   ->  let rec foo (head, tail) =
                        match tail with 
                        [] -> head
                        |h2::t2 -> let headRec = if h2 > head then h2 else head in 
                                   let tailRec = t2
                        in foo (headRec, tailRec)
                    in foo (h1, t1)
;;



(* Question 1.3 *)
let print_newline () = 
    printf "\n"
;;



(* Question 1.4 *)
let dessiner l =
    if l <> [] then
        let m = maximum l in
        let dessiner_rec n =
            let e = (m/2) - (n/2) in
            espaces e;
            etoiles n;
            print_newline ();
        in
        List.iter dessiner_rec l
;;



(* Question 12.5 *)
let double_sapin l =
    List.map (function x -> x*2) l
;;



(* Question 12.6 *)
let sous_fonction_chaines_croissantes l acc =
    match acc with
    [] -> [l]::acc
    |(h::t)::u -> if h>=l 
                  then (l::h::t)::u 
                  else [l]::acc
;;

let chaines_croissantes l = List.fold_right sous_fonction_chaines_croissantes l []
;;

(* Question 12.7 *)
let est_un_sapin l =
    List.fold_left (fun acc e -> List.length e != 1 && acc) true l
;;

(* Main *)

printf "Questions 12.1 a 12.2";;
print_newline ();;
etoiles 10;;
espaces 10;;
etoiles 10;;
print_newline ();;
print_newline ();;


let list1 = [2;4;8;10;2;2];; let res1 = maximum list1;;
printf "Questions 12.3";;
print_newline ();;
printf "list = ";;  let () = List.iter (printf "%d ") list1;;
printf "maximum = " ;; print_int res1;;
print_newline ();;
print_newline ();;


printf "Questions 12.4";;
print_newline ();;
printf "";;
dessiner list1;;
print_newline ();;
print_newline ();;

printf "Questions 12.5";;
print_newline ();;
let list1_double = double_sapin list1;;
dessiner list1_double;;
print_newline ();;
print_newline ();;

printf "Questions 12.6";;
print_newline ();;
let list2 = [2;4;6;4;6;8;10;2;4;10];; 
let list3 = chaines_croissantes list2;;
printf "Liste de base = ";; printlist list2;; print_newline ();;
printf "Chaines croissantes = ";; printlistlist list3;; print_newline ();;

printf "Questions 12.7";;
