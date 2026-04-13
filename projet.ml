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
let value_matches_coltype (v : dbvalue) ((dbtype, nullable) : coltype) : bool =
  match v with
  | VNull   -> nullable
  | VInt  _ -> dbtype = TInt
  | VText _ -> dbtype = TText

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

(* [prod tbl1 tbl2] effectue le produit cartésien des tables [tbl1] et [tbl2]
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

(* [projection tbl fields] effectue la projection suivant la liste de
   champs [fields] de la table [tbl] *)
let projection tbl fields = failwith "TODO" ;;

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
