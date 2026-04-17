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
    Type        : schema -> string list

    @requires   : [schema] est un schema d'une table

    @ensures    : Retourne la liste des noms de colonnes de [schema] dans
                  le même ordre que [schema]. Retourne [[]] si [schema] est
                  vide

    @raises     : Aucun
*)
let col_names (schema : schema) : string list =
  List.map (fun (a, _) -> a) schema


(*
   Type        : dbvalue -> coltype -> bool

   @requires   : [v] est une valeur quelconque
               [col_type = (dbtype, nullable)] est un type de colonne valide

   @ensures    : Retourne [true] ssi la valeur [v] est compatible avec [coltype] :
               - [VNull]   est accepté ssi le booléen nullable vaut [true]
               - [VInt _]  est accepté ssi le dbtype vaut [TInt]
               - [VText _] est accepté ssi le dbtype vaut [TText]

   @raises     : Aucun
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

   @raises     : Aucun
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

   @raises     : Aucun
*)
let has_duplicate_names (schema : schema) : bool = 
   (*On trie la liste pour obtenir une complexité finale moindre qu'un double parcours de liste qui serait en O(n²) alors qu'en triant puis en parcourant dans la liste triée on est en O(nlog(n))*)
  match List.sort compare (col_names schema) with 
  | [] -> false
  | [_] -> false
  | liste_triee -> 
      let rec check_double l =  
        match l with 
        | x::y::reste when x = y -> true (*si deux éléments consécutifs de la liste triée sont égaux alors il y a doublon*)
        | _::reste ->  check_double reste (*sinon on avance dans la liste*)
        | [] -> false (*Fin de la liste*)
      in check_double liste_triee


(* 
   Type        : table -> bool

   @requires   : [tbl] de type table

   @ensures    : Retourne [true] ssi la table est valide

   @raises     : Aucun
*)
let check_table (tbl : table) : bool = 
  not (has_duplicate_names tbl.cols)
  && List.for_all (fun row -> row_matches_schema row tbl.cols) tbl.rows
;;


(* 
   Type        : table -> row -> table

   @requires   : [tbl] est une table valide au sens de check_table
                 [row] de type row

   @ensures    : insère si possible la ligne [r] dans la table [tbl] sinon renvoie l'ancienne table sans ajout.

   @raises     : Aucun
*)
let insert (tbl : table) (row : row) : table = 
  if row_matches_schema row tbl.cols  (*on vérifie si les valeurs de la ligne sont compatibles avec la table*)
  then { tbl with rows = tbl.rows @ [row] }
  else tbl
;;


(* 
   Type        : table -> table -> table

   @requires   : [tbl1] est une table valide au sens de check_table
                 [tbl2] est une table valide au sens de check_table

   @ensures    : effectue le produit cartésien des tables [tbl1] et [tbl2]

   @raises     : Aucun
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

   @raises     : Aucun
*)
let index_of (nom : string) (l: string list) : int = 
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
let rec get_value_liste (liste : 'a list) (n : int) : 'a =
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

   @requires   : [tbl1] est une table valide au sens de check_table
                 [fields] de type string list

   @ensures    : effectue la projection suivant la liste de champs [fields] de la table [tbl]

   @raises     : soulève l'erreur "Champ inconnu" s'il y a un champ de [fields] non présent dans la table [tbl]
*)
let projection (tbl : table) (fields : string list) : table = 
   (*On détermine les indices des colonnes de la liste des champs de projection*)
  let col_names_list = col_names tbl.cols in
  let indices = List.map (fun f -> match index_of f col_names_list with 
      | -1 -> failwith "Champ inconnu"  (*On a pas trouvé de match donc on soulève une erreur*)
      | i -> i        (*Si on a trouvé un match alors on renvoie l'indice*)
    ) fields in

   (*On récupère les colonnes de la table associées à ces indices*)
  let new_cols = List.map (fun i -> get_value_liste tbl.cols i) indices in 
   (*On récupère les colonnes des rows associées à ces indices*)
  let new_rows = List.map (project_row indices) tbl.rows in
   (*On renvoie la liste des colonnes projetés avec leur lignes lignes de valeurs associées*)
  {cols = new_cols; rows = new_rows}
;;


(* 
   Type        : table -> (row -> bool) -> table

   @requires   : [tbl] est une table valide au sens de check_table
                 [f] de type row -> bool

   @ensures    : effectue la restriction des données présentes
   dans la table [tbl] en accord avec la fonction [f]. On ne garde
   dans le résultat que les lignes pour lesquelles [f] retourne
   [true]. 

   @raises     : Les exceptions soulevées par f le sont aussi pour cette fonction
*)
let restrict (tbl : table) (f : row -> bool) : table= 
  {tbl with rows = List.filter f tbl.rows}
;;


(* 
   Type        : 'a list -> 'a list list

   @requires   : [liste] de type 'a list

   @ensures    : renvoie l'ensemble des sous-ensembles de liste

   @raises     : Aucun
*)
let rec subsets (liste : 'a list) : 'a list list = 
  match liste with 
  | [] -> [[]]
  | x::xs -> 
      let reste = subsets xs in
      reste @ List.map (fun s -> x::s) reste 


(* 
    Type        : 'a list -> 'a list list

    @requires   : [lst] est une liste quelconque

    @ensures    : Retourne la liste de tous les sous-ensembles non vides de
                  [lst]. Le résultat contient 2^n - 1 éléments

    @raises     : [Stack_overflow] (propagé depuis [subsets]) si [lst] est
                  de longueur excessive
*)
let nonempty_subsets (lst : 'a list) : 'a list list =
  List.filter (fun s -> s <> []) (subsets lst)


(*  
    Type        : schema -> row -> string list -> dbvalue list

    @requires   : [schema] est le schéma associé à [row] 
                  [row] et [schema] sont de même longueur 
                  tous les noms de [fields] sont présents dans [schema]

    @ensures    : Retourne la liste des valeurs de [row] correspondant aux
                  colonnes nommées dans [fields], dans l'ordre de [fields].
                  Si un nom est absent du schéma, la valeur associée devient [VNull].

    @raises     : Aucun
*)
let values_for_cols (schema : schema) (row : dbvalue list) (fields : string list) : dbvalue list =
  let cols_name_list = col_names schema in
  List.map (fun name ->
    (* 1. On cherche l'indice d'un champs field*)
      let i = index_of name cols_name_list in
    (* 2. On teste si l'indice est valide (-1 signifie "pas trouvé") *)
      if i = -1 then       (*Ce cas ne devrait pas se produire si les préconditions sont respectées*)
        VNull (* On renvoie une valeur vide si le champ n'existe pas *)
      else 
      (* 3. On récupère la valeur dans la ligne row *)
        get_value_liste row i
    ) fields


(*  
    Type        : table -> string list -> string list -> bool

    @requires   : [tbl] est une table valide au sens de [check_table] ;
                  tous les noms de [lhs] et [rhs] sont présents dans
                  [tbl.cols].

Aucun -> rhs] est satisfaite par les données de [tbl] :
                  pour toute paire de lignes (r1, r2), si r1 et r2
                  coïncident sur [lhs] alors elles coïncident sur [rhs].
                  Retourne [true] si [tbl.rows] est vide ou singleton
                  (condition universelle vacuellement vraie).

    @raises     : Aucun
*)
let fd_holds (tbl : table) (lhs : string list) (rhs : string list) : bool =
  List.for_all (fun r1 ->
      List.for_all (fun r2 ->
          if values_for_cols tbl.cols r1 lhs = values_for_cols tbl.cols r2 lhs
          then values_for_cols tbl.cols r1 rhs = values_for_cols tbl.cols r2 rhs
          else true
        ) tbl.rows
    ) tbl.rows


(*
    Type        : table -> fd list 

    @requires   : [tbl] est une table valide au sens de [check_table] ;
                  
    @ensures    : retourne TOUTES les dépendances fonctionnelles
                  trouvées en étudiant les données présentes dans [tbl] 

    @raises     : [Stack_overflow] (propagé depuis [noempty_subsets]) si tbl.cols est
                  de longueur excessive
*)
let compute_deps (tbl : table) : fd list =
  (*On récupère les noms de colonnes*)
  let names = col_names tbl.cols in
  (*On génère tous les ensembles candidats possible de lhs*)
  let all_lhs = nonempty_subsets names in
  
  List.concat_map (fun lhs ->
    (* On prépare les candidats à droite *)
      let potential_rhs = List.filter (fun name -> not (List.mem name lhs)) names in
    
    (*On filtre pour ne garder que les noms où la DF est vraie *)
      let valid_rhs_names = List.filter (fun rhs_name -> 
          fd_holds tbl lhs [rhs_name]
        ) potential_rhs in
    
    (*On mappe pour transformer chaque nom valide en format (lhs, [rhs]) que la fonction renvoie*)
      List.map (fun rhs_name -> (lhs, [rhs_name])) valid_rhs_names

    ) all_lhs


(*
    Type        : table -> fd -> bool 

    @requires   : [tbl] est une table valide au sens de [check_table]
                  [(lhs, rhs)] de type fd
                  
    @ensures    : Renvoie [true] ssi aucun des sous ensembles strict et non vide de lhs n'est aussi une DF pour rhs 
                  Renvoie [false] sinon
 
    @raises     : Aucun
*)
let has_no_subset_df (tbl : table) ((lhs, rhs): fd) : bool =
   (*On prend les sous-ensembles non vides qui sont des potentiels FD*)
  let subset_potential_df_not_empty = nonempty_subsets lhs in
   (*On prend uniquement les sous-ensembles stricts*)
  let proper_subsets = List.filter (fun s -> s <> lhs) subset_potential_df_not_empty in
   (*On filtre pour garder uniquement les sous-ensembles qui sont des df*)
  let subset_df = List.filter (fun s -> fd_holds tbl s rhs) proper_subsets
  in 
   (*si l'ensemble des sous-ensembles est vide alors on retourne true, sinon false*)
  subset_df = [] 


(*
    Type        : table -> fd list

    @requires   : [tbl] est une table valide au sens de [check_table]
                  [(lhs, rhs)] de type fd

    @ensures    : Retourne toutes les dépendances fonctionnelles élémentaires trouvées en étudiant les données présentes dans [tbl]

    @raises     : Aucun
*)
let compute_elementary_deps (tbl : table) : fd list = 
   (* 1. on récupère l'ensemble des DF *)
  let df_liste = compute_deps tbl in 
   (* 2. On filtre la liste des DF en ne gardant uniquement celles dont aucun de ses sous-ensembles n'est une DF *)
  List.filter (fun df -> has_no_subset_df tbl df) df_liste
;;


(*
    Type        : table -> string list -> bool

    @requires   : [tbl] est une table valide au sens de [check_table]
       [lhs] une liste de nom de colonnes de tbl

    @ensures    : Retourne [true] si [lhs] est une clée candidate

    @raises     : Aucun
*)
let is_key (tbl : table) (lhs : string list) : bool = 
  fd_holds tbl lhs (col_names tbl.cols)


(*
    Type        : 'a -> 'a list -> bool

    @requires   : Aucun

    @ensures    : Retourne [true] si [elmt] est un élement de [l]

    @raises     : Aucun
*)
let rec appartient (elmt :'a) (l : 'a list) : bool = 
  match l with 
  | [] -> false
  | head::tail -> head=elmt || appartient elmt tail



(*
    Type        : string list -> string list -> bool

    @requires   : Aucun

    @ensures    : Renvoie [true] si l1 est inclu dans l2
    
    @raises     : Aucun
*)
let is_inclued (l1: string list) (l2 : string list) : bool = 
  List.for_all (fun elmt -> appartient elmt l2) l1


(*
    Type        : table -> string list list

    @requires   : [tbl] une table valide au sens de check_table

    @ensures    : Renvoie la liste des clées candidates de la table [tbl]
    
    @raises     : [Stack_overflow] (propagé depuis [compute_deps]) si tbl.cols est
                  de longueur excessive
*)
let candidate_keys (tbl : table) : string list list = 
  let col_names_list = col_names tbl.cols in 
   (* On récupère toute les df = (lhs, rhs)*)
  let all_deps = compute_deps tbl in
   (* On récupère l'ensemble des lhs*)
  let all_lhs = List.map (fun (lhs, _) -> lhs) all_deps in 
   (* On garde uniquement celles qui détermine toutes les colonnes *)
  let maybe_candidate_keys = List.filter (fun lhs -> is_key tbl lhs) all_lhs in
   (* On garde uniquement les candidates minimales qui sont en faite les clées candidates *)
  let candidates = List.filter ( fun m_key -> 
      (* On regarde si ce n'est pas une augmentation d'une autre candidate de clée candidates *)
      List.for_all (fun other_key -> other_key = m_key || not (is_inclued other_key m_key)) maybe_candidate_keys
    ) maybe_candidate_keys in 
  if candidates = [] then 
    [col_names_list]
  else 
    candidates


(*
    Type        : string list -> string list list -> bool 

    @requires   : [a] est un nom de colonne
                  [keys] est la liste des clés candidates de la table

    @ensures    : Retourne [true] ssi [a] appartient à au moins une clée candidate
                  Retourne [false] sinon 
    
    @raises     : Aucun
*)
let is_prime (a : string) (keys : string list list) : bool =
  not (List.for_all (fun k -> not (appartient a k)) keys)


(*
    Type        : string list -> string list list -> bool 

    @requires   : [lhs] est un ensemble de noms de colonnes 
                  [keys] est la liste des clés candidates de la table

    @ensures    : Retourne [true] ssi [lhs] contient au moins une clée candidate (au sens de l'inclusion)
                  Retourne [false] si aucune clée candidate n'est incluse dans [lhs]
    
    @raises     : Aucun
*)
let is_superkey (lhs : string list) (keys : string list list) : bool =
  not (List.for_all (fun k -> not (is_inclued k lhs)) keys)



(*
    Type        : table -> string list list -> fd list -> bool 

    @requires   : [tbl] est une table valide au sens de check_table
                  [keys] est la liste des clés candidates de la table

    @ensures    : Retourne [true] si tbl est en 2FN
                  Retourne [false] sinon
    
    @raises     : Aucun
*)
let is_2nf (tbl : table) (keys : string list list) (e_deps : fd list) : bool =
  (* On vérifie si TOUTES les DF respectent la règle *)
  List.for_all (fun (lhs, rhs) ->
      let a = List.hd rhs in (* On sait que le rhs est unitaire *) 
      (* Si 'a' est un morceau de clé, la DF ne peut pas violer la 2NF *)
      if is_prime a keys then true 
      else
      (* Si 'a' n'est pas une clé, alors lhs ne doit être inclus dans AUCUNE clé *)
        List.for_all (fun k -> 
            not (is_inclued lhs k && lhs <> k)
          ) keys
    ) e_deps


(*
    Type        : string list list -> fd list -> bool 

    @requires   : [keys] est la liste des clés candidates d'une table
                  [e_deps] est la liste des DF élémentaire de la table

    @ensures    : Retourne [true] si tbl est en 3FN
                  Retourne [false] sinon
    
    @raises     : Aucun
*)
let check_3nf (keys : string list list) (e_deps : fd list) : bool =
  List.for_all (fun (lhs, rhs) ->
      (* On prend l'attribut à droite *)
      let a = List.hd rhs in 
      (* Condition 1 : X est super-clé *)
      if is_superkey lhs keys then true       
      (* Condition 2 : A est un attribut clé *)
      else if is_prime a keys then true      
      (* Si une des condition n'est pas vérifiée *)
      else false
    ) e_deps



(*
    Type        : table -> int

    @requires   : [keys] est la liste des clés candidates d'une table
                  [e_deps] est la liste des DF élémentaire de la table

    @ensures    : Retourne le niveau de normalisation de [tbl] sous forme d'un entier. 
    
    @raises     : [Stack_overflow] (propagé depuis [compute_elementary_deps]) si le nombre de colonnes de [tbl] est excessif
*)
let normalization_level (tbl : table) : int =
   (* on a juste à tester les conditions pour les formes normales 2FN et 3FN en incrémentant de la valeur de retour à chaque condition rempli et s'arrêter dès qu'une condition d'une forme normale n'est pas respectée. *)
  let clees_candidates = candidate_keys tbl in 
  let e_deps = compute_elementary_deps tbl in
  if not (is_2nf tbl clees_candidates e_deps)  then 1 (* On s'arrête à la 1NF *)
  else
    (* 2. Test de la 3NF *)
  if check_3nf clees_candidates e_deps then 3
  else 2