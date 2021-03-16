{
    open Lexing
    exception Erreur_de_syntaxe
}

let espace = ['\t' ' ']+
let ligne_invalide = ['*']+espace+['*']+

rule sapin nombre_etoiles l = parse
| ligne_invalide { raise Erreur_de_syntaxe }
| ['*'] { sapin {nombre_etoiles + 1} l lexbuf }
| espace { sapin nombre_etoiles l lexbuf }
| ['\n'] { if {nombre_etoiles mod 2 } != 0 then raise Erreur_de_syntaxe else sapin 0 {nombre_etoiles :: l} lexbuf }
|_ { raise Erreur_de_syntaxe }
| eof { if nombre_etoiles = 0 then l else nombre_etoiles :: l }

{
    let sapiin file = 
    let f = open_in file open_in
    
    let s = sapin 0 [] {from_channel f} in
    close_in f; List.rev s
}