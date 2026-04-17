open Projet (* Permet d'importer tout le contenu compilé de projet.ml *)

let () = Printf.printf "\n============= DÉBUT DES TESTS =============\n\n"

(* ========================================================================= *)
(* 1. DÉFINITION DES BASES DE DONNÉES (Basé sur le TD et le cours)           *)
(* ========================================================================= *)

(* --- TABLE 1 : EMP-DEPT (Basée sur le TD de l'IMT Atlantique) --- *)
let schema_emp_dept = [
  ("EMPNO",  (TInt, false));
  ("ENAME",  (TText, false));
  ("JOB",    (TText, false));
  ("DEPTNO", (TInt, false));
  ("DNAME",  (TText, false))
]

(* Note : on inclut un cas "piège" du TD où DOE(7904) appartient à deux départements différents (20 et 30) *)
let row_emp1 = [VInt 7698; VText "BLAKE"; VText "MANAGER";  VInt 40; VText "SALES"]
let row_emp2 = [VInt 7499; VText "ALLEN"; VText "SALESMAN"; VInt 30; VText "SALES"]
let row_emp3 = [VInt 7904; VText "DOE";   VText "CLERK";    VInt 20; VText "RESEARCH"]
let row_emp4 = [VInt 7904; VText "DOE";   VText "SALESMAN"; VInt 30; VText "SALES"]

let tbl_emp_dept = { cols = schema_emp_dept; rows = [row_emp1; row_emp2; row_emp3; row_emp4] }

(* --- TABLE 2 : VOITURE (Basée sur le PDF 05_formes_normales, page 12) --- *)
let schema_voiture = [
  ("n_vehicule", (TText, false));
  ("type",       (TText, false));
  ("marque",     (TText, false));
  ("puissance",  (TInt, false));
  ("couleur",    (TText, false))
]

let row_v1 = [VText "872VXD75"; VText "clio";   VText "renault"; VInt 6; VText "bleue"]
let row_v2 = [VText "975SWT80"; VText "clio";   VText "renault"; VInt 6; VText "rouge"]
let row_v3 = [VText "111AAA11"; VText "208";    VText "peugeot"; VInt 5; VText "rouge"]
let row_v4 = [VText "222BBB22"; VText "megane"; VText "renault"; VInt 7; VText "noire"]

let tbl_voiture = { cols = schema_voiture; rows = [row_v1; row_v2; row_v3; row_v4] }

(* --- TABLE 3 : PRODUITS PHARMA (Basée sur le PDF, page 20) --- *)
let schema_pharma = [
  ("produit", (TText, false));
  ("type",    (TText, false));
  ("client",  (TText, false));
  ("remise",  (TInt, false))
]

let row_p1 = [VText "aspirine";  VText "A"; VText "C1"; VInt 3]
let row_p2 = [VText "compresse"; VText "A"; VText "C2"; VInt 5]
let row_p3 = [VText "vitamine";  VText "B"; VText "C1"; VInt 4]
let row_p4 = [VText "sirop";     VText "B"; VText "C2"; VInt 6]

let tbl_pharma = { cols = schema_pharma; rows = [row_p1; row_p2; row_p3; row_p4] }


(* ========================================================================= *)
(* 2. TESTS DES FONCTIONS DE BASE                                            *)
(* ========================================================================= *)

let () = Printf.printf "[TEST] check_table... "
let () = 
  assert (check_table tbl_emp_dept = true);
  assert (check_table tbl_voiture = true);
  
  (* Création d'une table invalide (doublon dans le schéma) *)
  let schema_invalide = [("A", (TInt, false)); ("A", (TText, false))] in
  let tbl_invalide = { cols = schema_invalide; rows = [] } in
  assert (check_table tbl_invalide = false);
  Printf.printf "OK!\n"

let () = Printf.printf "[TEST] insert... "
let () = 
  let row_emp5 = [VInt 7369; VText "SMITH"; VText "CLERK"; VInt 20; VText "RESEARCH"] in
  let tbl_emp_dept_ins = insert tbl_emp_dept row_emp5 in
  assert (List.length tbl_emp_dept_ins.rows = 5);
  
  (* Insertion d'une ligne invalide (type erroné pour DEPTNO qui attend un entier) *)
  let row_invalide = [VInt 7369; VText "SMITH"; VText "CLERK"; VText "VINGT"; VText "RESEARCH"] in
  let tbl_emp_dept_ins2 = insert tbl_emp_dept row_invalide in
  assert (List.length tbl_emp_dept_ins2.rows = 4); (* L'insertion a échoué silencieusement, comme attendu *)
  
  (* Insertion d'une ligne de mauvaise taille *)
  let tbl_emp_dept_ins3 = insert tbl_emp_dept [VInt 9999; VText "TROP_COURT"] in
  assert (List.length tbl_emp_dept_ins3.rows = 4);
  Printf.printf "OK!\n"

let () = Printf.printf "[TEST] prod (Produit Cartésien)... "
let () = 
  let tbl_A = { cols = [("A", (TInt, false))]; rows = [[VInt 1]; [VInt 2]] } in
  let tbl_B = { cols = [("B", (TText, false))]; rows = [[VText "X"]; [VText "Y"]] } in
  let tbl_AxB = prod tbl_A tbl_B in
  assert (List.length tbl_AxB.rows = 4);
  assert (List.length tbl_AxB.cols = 2);
  Printf.printf "OK!\n"

let () = Printf.printf "[TEST] projection... "
let () = 
  let tbl_proj = projection tbl_emp_dept ["EMPNO"; "ENAME"] in
  assert (List.length tbl_proj.cols = 2);
  assert (List.nth tbl_proj.cols 0 = ("EMPNO", (TInt, false)));
  assert (List.nth (List.nth tbl_proj.rows 0) 1 = VText "BLAKE");
  Printf.printf "OK!\n"

let () = Printf.printf "[TEST] restrict... "
let () = 
  (* Filtrage : on ne garde que le département SALES *)
  let is_sales r = (get_value_liste r 4 = VText "SALES") in (* DNAME est l'index 4 *)
  let tbl_rest = restrict tbl_emp_dept is_sales in
  assert (List.length tbl_rest.rows = 3); (* BLAKE, ALLEN, et DOE en Salesman *)
  Printf.printf "OK!\n"


(* ========================================================================= *)
(* 3. TESTS DES FONCTIONS DE NORMALISATION (DF, etc.)                        *)
(* ========================================================================= *)

let () = Printf.printf "[TEST] fd_holds... "
let () = 
  (* Dans tbl_emp_dept, EMPNO détermine bien ENAME de manière unique *)
  assert (fd_holds tbl_emp_dept ["EMPNO"] ["ENAME"] = true);
  
  (* Par contre, EMPNO ne détermine pas DEPTNO car DOE (7904) est rattaché au dep 20 ET 30 *)
  assert (fd_holds tbl_emp_dept ["EMPNO"] ["DEPTNO"] = false);

  (* Dans le TD, DNAME vers DEPTNO est faux car SALES correspond à 30 ET 40 *)
  assert (fd_holds tbl_emp_dept ["DNAME"] ["DEPTNO"] = false);

  (* Dans tbl_pharma, 'type' et 'client' déterminent la remise (voir cours p. 20) *)
  assert (fd_holds tbl_pharma ["type"; "client"] ["remise"] = true);
  Printf.printf "OK!\n"

let () = Printf.printf "[TEST] compute_elementary_deps... "
let () = 
  let deps = compute_elementary_deps tbl_voiture in
  (* On vérifie si la dépendance élémentaire {type} -> {marque} expliquée page 12 du cours a bien été trouvée *)
  let type_marque = List.mem (["type"], ["marque"]) deps in
  let type_puissance = List.mem (["type"], ["puissance"]) deps in
  assert type_marque;
  assert type_puissance;
  Printf.printf "OK!\n"


(* ========================================================================= *)
(* 4. TESTS DE LA FONCTION NORMALIZATION_LEVEL (En attente d'implémentation) *)
(* ========================================================================= *)


let () = Printf.printf "[TEST] normalization_level... "
let () = 
  (* La table EMP-DEPT classique n'est pas en 2NF car certains attributs ne dépendent 
     pas de toute la clé.*)
  assert (normalization_level tbl_emp_dept = 1);

  
  Printf.printf "OK!\n"


let () = Printf.printf "\n============= TOUS LES TESTS PASSENT ! ============\n\n"