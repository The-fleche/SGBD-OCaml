(* Le type [dbtype] défini les différen ts types possiblement présents
   en base dans notre cadre.

  Nous n'aurons ici besoin que de deux types primitifs.

  - les entiers représentés ici par TInt
  - les textes représentés ici par TText
 *)
type dbtype =
  | TInt  (* type des entrées entières *)
  | TText (* type des entrées textes   *)
;;

(* Le type [coltype] est le type représentant un champ dans une table
   de notre système.

   Il est composé d'un couple comprenant le type des valeurs présentes
   dans ce champ d'une part et d'un booléen exprimant la possibilité
   (dans le cas où le booléen est à [true]) ou non pour ce champs
   d'adopter la valeur [NULL] *)
type coltype = dbtype * bool ;;

(* Le type [dbvalue] est le type des *VALEURS* présentes en base.

  Nous aurons besoin ici de trois types de valeurs.

  - les valeurs entières munies de leurs valeurs
  - les valeurs textuelles munies de leurs valeurs
  - la valeur null qui pourras être indifféremment considérée de type
   [TInt] et [TText].  *)
type dbvalue =
  | VInt of int     (* valeurs entières   *)
  | VText of string (* valeurs textuelles *)
  | VNull           (* la valeur NULL *)
;;

(* Le schéma d'une table est une liste de couple dont le premier
   élément est le nom du champs et le second est le type du champs de
   type [coltype] *)
type schema = (string*coltype) list ;;


(* Une ligne d'une table est une liste de valeurs.
 *)
type row = dbvalue list ;;

(* Une [table] est la donnée d'un schéma et d'une liste de lignes *)
type table = { cols : schema; rows : row list } ;;

(* Le type [fd] représente le type des dépendances fonctionnelles
   d'une table.

   Il est composé d'un couple (lhs,rhs) dont chacun des deux membres
   est une liste de nom de champs.

   La dépendance (lhs,rhs) représente bien évidement la dépendance lhs -> rhs. 
 *)
type fd = (string list) * (string list) ;;



(*
   Type        : dbvalue -> coltype -> bool

   @requires   : [v] est une valeur quelconque
               [col_type = (dbtype, nullable)] est un type de colonne valide

   @ensures    : Retourne [true] ssi la valeur [v] est compatible avec [coltype] :
               - [VNull]   est accepté ssi le booléen nullable vaut [true]
               - [VInt _]  est accepté ssi le dbtype vaut [TInt]
               - [VText _] est accepté ssi le dbtype vaut [TText]

   @raises     : Aucune.
*)
let value_matches_coltype (v : dbvalue) ((db_type, nullable) : coltype) : bool =
  match v with
  | VNull   -> nullable          (*Null est accepté ssi nullable est true*)
  | VInt  _ -> db_type = TInt    (*les valeurs entières sont acceptées ssi le type de la colonne est entier *)
  | VText _ -> db_type = TText   (*les valeurs textuelles sont acceptées ssi le type de la colonne est textuel*)

(*
   Type        : row -> schema -> bool

   @requires   : [row] et [schema] sont des listes quelconques

   @ensures    : Retourne [true] ssi les deux conditions suivantes sont
               réunies :
               - [row] et [schema] ont la même longueur (arité correcte),
               - pour tout indice i, la i-ème valeur de [row] est
                     compatible avec le i-ème coltype de [schema] au sens de
                     [value_matches_coltype]
               
               Retourne [false] si les longueurs diffèrent ou si une valeur
               est incompatible avec son type déclaré

   @raises     : Aucune.
*)
let rec row_matches_schema (row : row) (schema : schema) : bool = 
  match row, schema with 
     | [], [] -> true             (* on a autant de colonne que de valeur par ligne*)
     | [], _ | _, [] -> false     (*Le nombre de colonne n'est pas le même*)
     | v::reste_row, (_, col_type)::reste_schema ->
       value_matches_coltype v col_type && row_matches_schema reste_row reste_schema


(*  
   Type        : schema -> bool

   @requires   : [schema] est une liste quelconque

   @ensures    : Retourne [true] ssi au moins deux colonnes de [schema]
               portent le même nom
               Retourne [false] si tous les noms sont distincts, ou si
               [schema] est vide

   @raises     : Aucune
*)
let has_duplicate_names (schema : schema) : bool = 
   (*On trie la liste pour obtenir une complexité finale moindre qu'un double parcours de liste qui serait en O(n²) alors qu'en triant puis en parcourant dans la liste triée on est en O(nlog(n))*)
   match List.sort compare schema with 
     | [] -> false
     | [_] -> false
     | liste_triee -> 
       let rec check_double l =  
         match l with 
          | x::y::reste when x = y -> true (*si deux éléments consécutifs de la liste triée sont égaux alors il y a doublon*)
          | _::reste ->  check_double reste (*sinon on avance dans la liste*)
          | [_] | [] -> false (*Fin de la liste*)
       in check_double liste_triee


(* 
   Type        : table -> bool

   @requires   : [tbl] de type table

   @ensures    : Retourne [true] ssi la table est valide

   @raises     : Aucune
*)
let check_table (tbl : table) : bool = 
   not (has_duplicate_names tbl.cols)
   && List.for_all (fun row -> row_matches_schema row tbl.cols) tbl.rows
;;


(* 
   Type        : table -> row -> table

   @requires   : [tbl] de type table
                 [row] de type row

   @ensures    : insère si possible la ligne [r] dans la table [tbl] sinon renvoie l'ancienne table sans ajout.

   @raises     : Aucune
*)
let insert (tbl : table) (row : row) : table = 
   if row_matches_schema row tbl.cols  (*on vérifie si les valeurs de la ligne sont compatibles avec la table*)
   then { tbl with rows = tbl.rows @ [row] }
   else tbl

;;
(* 
   Type        : table -> table -> table

   @requires   : [tbl1] de type table
                 [tbl2] de type table

   @ensures    : effectue le produit cartésien des tables [tbl1] et [tbl2]

   @raises     : Aucune
*)
let prod (tbl1 : table) (tbl2 : table) : table =
  let combined_schema = tbl1.cols @ tbl2.cols in
  (* Pour chaque ligne r1 de tbl1, on la concatène avec chaque ligne r2 de tbl2 *)
  let combined_rows =
    List.concat_map
      (fun r1 -> List.map (fun r2 -> r1 @ r2) tbl2.rows)
      tbl1.rows
  in
  { cols = combined_schema; rows = combined_rows }
;;

(* 
   Type        : string -> list -> int

   @requires   : [nom] de type string
                 [l] de type string list

   @ensures    :  renvoie l'indice de [nom] dans la liste [l] si [nom] existe dans [l]
                  renvoie -1 si [nom] n'est pas dans [l]

   @raises     : Aucune
*)
let index_of (nom : string) (l: string list) = 
   let rec idx_of nom l n =
      match l with 
      | [] -> -1 (*Si on a aucun match alors on renvoie une valeur négative : -1*)
      | title::reste -> 
         if title = nom then n (*Si on a trouvé un match, on renvoie l'index*)
         else idx_of nom reste (n+1) (*Sinon on regarde pour la valeur suivante*)
   in 
   idx_of nom l 0


(* 
   Type        : a' liste -> int -> 'a 

   @requires   : [liste] de type liste
                 [n] de type int, un entier positif

   @ensures    : renvoie la valeur d'indice n dans [liste] si n est inférieur à la taille de la liste [liste]

   @raises     : soulève l'erreur "Index out of range" si n est plus grand que la taille de la liste [liste]
*)
let rec get_value_liste (liste : list) (n : int) =
   match liste with 
     | [] -> failwith "Index out of range"
     | head::tail -> 
         if n = 0 then head
         else get_value_liste tail (n-1)

(* 
   Type        : int list -> row -> int list

   @requires   : [indices] de type int list
                 [row] de type row

   @ensures    : Retourne la sous-liste de [row] formée des valeurs aux
                  positions listées dans [indices], dans l'ordre de [indices].
                  Si [indices] est vide, retourne [[]]

   @raises     : soulève l'erreur "Index out of range" si il y a un element dans indices qui est plus grand que la taille de la liste [liste]
*)
let project_row (indices : int list) (row : row) : row =
  List.map (fun i -> get_value_liste row i) indices

(* 
   Type        : table -> string list -> table

   @requires   : [tbl1] de type table
                 [fields] de type string list

   @ensures    : effectue la projection suivant la liste de champs [fields] de la table [tbl]

   @raises     : soulève l'erreur "Champ inconnu" s'il y a un champ de [fields] non présent dans la table [tbl]
*)
let projection (tbl : table) (fields : string list) = 
   (*On détermine les indices des colonnes de la liste des champs de projection*)
   let indices = List.map (fun f -> match index_of f tbl.cols with 
     | -1 -> failwith ("Champ inconnu : %s", f)  (*On a pas trouvé de match donc on soulève une erreur*)
     | i -> i        (*Si on a trouvé un match alors on renvoie l'indice*)
   ) fields in

   (*On récupère les colonnes de la table associées à ces indices*)
   let new_cols = List.map (fun i -> get_value_liste tbl.cols i) indices in 
   (*On récupère les colonnes des rows associées à ces indices*)
   let new_rows = List.map (project_row indices) tbl.rows in
   (*On renvoie la liste des colonnes projetés avec leur lignes lignes de valeurs associées*)
   {cols = new_cols; rows = new_rows}
   ;;


(* [restrict tbl test] effectue la restriction des données présentes
   dans la table [tbl] en accord avec la fonction [test]. On ne garde
   dans le résultat que les lignes pour lesquelles [test] retourne
   [true].  *)
let restrict tbl f = failwith "TODO" ;;
  
(* [compute_deps tbl] retourne TOUTES les dépendances fonctionnelles
   trouvées en étudiant les données présentes dans [tbl] *)
let compute_deps tbl = failwith "TODO" ;;

(* [compute_elementary_deps tbl] retourne TOUTES les dépendances
   fonctionnelles élémentaires trouvées en étudiant les données
   présentes dans [tbl] *)
let compute_elementary_deps tbl = failwith "TODO" ;;


(* [normalization_level tbl] retourne le niveau de normalisation de
   [tbl] sous forme d'un entier.  *)
let normalization_level tbl = failwith "TODO" ;;
