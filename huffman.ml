(* Création du programme de codage de Huffman *)

(* Structure d'arbre de tasmin *)
type 'a arbre = Noeud of 'a * 'a arbre * 'a arbre | Vide;;

let rec arbre_tasmin_insere a arb = match arb with
| Vide -> Noeud (a, Vide, Vide)
| Noeud(e, arb1, arb2) when e > a -> Noeud(a, Noeud(e, arb2, arb1), Vide)
| Noeud(e, arb1, arb2) -> Noeud(e, arb2, (arbre_tasmin_insere a arb1));;

let arbre_tasmin_extraitmin arb = if arb = Vide then (failwith "arbre vide")
    else let Noeud(e, arb1, arb2) = arb in match arb1, arb2 with
    | (Vide, _) -> e, arb2
    | (_, Vide) -> e, arb1
    | (_, _) -> (let rec aux a1 a2 = match a2 with
        | Vide -> a1
        | Noeud(p, arb11, arb12) -> aux (aux (arbre_tasmin_insere p a1) arb11) arb12
        in e, aux arb1 arb2);;

let rec arbre_tasmin_taille arb = match arb with
| Vide -> 0
| Noeud(_, arb1, arb2) -> 1 + (arbre_tasmin_taille arb1) + (arbre_tasmin_taille arb1);;

(* Structure de file de priorité *)
type 'a filedepriorite = {mutable arb: (int * 'a) arbre; mutable taille: int};;

let filedepriorite_cree_vide () = {arb = Vide; taille = 0};;

let filedepriorite_insere v p file = file.arb <- arbre_tasmin_insere (p, v) file.arb;
    file.taille <- file.taille + 1;;

let filedepriorite_extraitmin file = let e, arb1 = arbre_tasmin_extraitmin file.arb in
    file.arb <- arb1; file.taille <- file.taille - 1; e;;

let filedepriorite_taille file = file.taille;;

(* fonction supposée disponible *)
exception Fin_de_fichier;;
let cree_epelle chemin = let texte = chemin ^ "Lorem ipsum dolor sit amet, consectetur adipiscing elit" and i = ref 0 in
    let epelle () = incr i; if !i >= String.length texte then raise Fin_de_fichier else texte.[!i] in epelle;;

let cree_epelle_opt chemin = let epelle = cree_epelle chemin in 
    let epelle_opt () = try Some (epelle ()) with Fin_de_fichier -> None in epelle_opt;;

let compte_occurences fichier = let arr = Array.make 256 0 and epelle_opt = cree_epelle_opt fichier in
    let rec aux () = match (epelle_opt ()) with
    | Some c -> arr.(int_of_char c) <- arr.(int_of_char c) + 1; aux ()
    | None -> arr
    in aux ();;
    
(* Structure d'arbre de Huffman *)
type 'c xarbre = XNoeud of ('c xarbre * 'c xarbre) | Feuille of 'c;;

let construit_file_huffman occ = let f = filedepriorite_cree_vide () in
    for i = 0 to (Array.length occ) - 1 do
        let poids = occ.(i) in 
            if poids > 0 then filedepriorite_insere (Feuille (char_of_int i)) poids f
    done; f;;

let create_arbre_huffman file = 
    while filedepriorite_taille file > 1 do
        let p1, min1 = filedepriorite_extraitmin file and
            p2, min2 = filedepriorite_extraitmin file in
        filedepriorite_insere (XNoeud (min1, min2)) (p1 + p2) file
    done; let _, arbre = filedepriorite_extraitmin file in arbre;;

let create_code_table arbre = let arr = Array.make 256 [] in
    let rec aux cod arb = match arb with
    | Feuille c -> arr.(int_of_char c) <- cod
    | XNoeud(arb1, arb2) -> aux (0::cod) arb1; aux (1::cod) arb2
    in aux [] arbre; arr;;

let code_texte texte table = let f acc l = acc ^ (string_of_int l) in 
    let g c = List.fold_left f "" (table.(int_of_char c)) in
    String.fold_left (fun a c -> a ^ (g c)) "" texte;;

(* TESTS *)
let occ_test = compte_occurences "fichier";;

let file_huff_test = construit_file_huffman occ_test;;

let arbre_test = create_arbre_huffman file_huff_test;;

let table_test = create_code_table arbre_test;;