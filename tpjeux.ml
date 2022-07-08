module type Game = sig
type state 
type action 


val getInitialState : unit -> state
val getActions : state -> action list
val getDepth : unit -> int
val getResult : state -> action -> bool -> state
val isTerminal : state -> bool
val getUtility :  state -> bool -> int
val print_action : action -> unit
val print_state : state -> unit
val jouer_partie : bool -> bool

val readAction : unit -> action
end ;;

module type Search = sig
type state
type action
val makeDecision :  state -> action
val getMetrics : unit -> int
val ordiTest : unit -> bool
end ;;

module type FSearch   =
functor (G : Game) ->
Search with type state = G.state and type action = G.action
;;

module MinimaxSearch0 (G : Game) = struct
    type state = G.state
    type action = G.action

    let expandedNodes = ref 0;;

    let rec maxValue (s : state) (p : bool) =
    begin
       assert ( p) ;
       expandedNodes := !expandedNodes + 1;
       if (G.isTerminal s) then G.getUtility s p
       else
       List.fold_right
       (fun a value -> max value (minValue (G.getResult s a p) (not p)))
       (G.getActions s) min_int
    end

    and

    minValue (s : state) (p : bool) =
    begin
       assert (not(p)) ;
       expandedNodes := !expandedNodes + 1;
       if (G.isTerminal) s then G.getUtility s p
       else
       List.fold_right
       (fun a value -> min value (maxValue (G.getResult s a p) (not p)))
       (G.getActions s) max_int
    end

    let makeDecision s = 
      let p = true (*pour Max*) in 
      let (v, a) =
      List.fold_right
       (fun a (value, act) ->
         (*let () = Printf.printf "[Debut fold]\n" in
         let () = G.print_action a in
         let () = Printf.printf "[Fin fold]\n" in*)
           let v = minValue (G.getResult s a p) (not p) in
	        if v > value then (v, a) else (value, act)
       )
       (G.getActions s) (min_int, List.hd (G.getActions s))
       in a

     let getMetrics () = !expandedNodes ;;

     let ordiTest () = 
       let s = G.getInitialState () in
       let rec partie_aux (j:bool) (st:state) =
            if G.isTerminal st then (not j) else
            if j then (
               let () = G.print_state st in
               let () = G.print_action(makeDecision st) in
               let () = Printf.printf "exploration de %d noeuds\n" (getMetrics ()) in
               partie_aux (not j) (G.getResult st (makeDecision st) j)
            )
         else
            let () = G.print_state st in
            let () = Printf.printf "Au tour du joueur. \n" in
            let rec check_move () = 
               let i = G.readAction () in
               if List.mem i (G.getActions st) then i else ( let () = Printf.printf "Ce coup n'est pas valide ! Réessayer. \n" in check_move () )
            in
            let move = check_move () in
            let () = G.print_action(move) in
            (*let () = G.print_state st in*)
            partie_aux (not j) (G.getResult st move j)
       in partie_aux false s
   ;;
       
end;;

module MinimaxSearch1 (G : Game) = struct
    type state = G.state
    type action = G.action

    let expandedNodes = ref 0;;

    let rec maxValue (s : state) (p : bool) (d : int) =
    begin
       assert ( p) ;
       expandedNodes := !expandedNodes + 1;
       (*let () = G.print_state s in*)
       if ((G.isTerminal s) || d = 0) then G.getUtility s p
       else
       List.fold_right
       (fun a value -> max value (minValue (G.getResult s a p) (not p) (d-1)))
       (G.getActions s) min_int
    end
       
    and

    minValue (s : state) (p : bool) (d : int) =
    begin
       assert (not(p)) ;
       expandedNodes := !expandedNodes + 1;
       (*let () = G.print_state s in*)
       if ((G.isTerminal s) || d = 0) then G.getUtility s p
       else
       List.fold_right
       (fun a value -> min value (maxValue (G.getResult s a p) (not p) (d-1)))
       (G.getActions s) max_int
    end

    let makeDecision s = 
      let p = true (*pour Max*) in 
      let (v, a) =
      List.fold_right
       (fun a (value, act) ->
         (*let () = Printf.printf "[Debut fold]\n" in
         let () = G.print_action a in
         let () = Printf.printf "[Fin fold]\n" in*)
           let v = minValue (G.getResult s a p) (not p) (G.getDepth ()) in
           if v > value then (v, a) else (value, act)
       )
       (G.getActions s) (min_int, List.hd (G.getActions s))
       in a

     let getMetrics () = !expandedNodes ;;

     let ordiTest () =
       let s = G.getInitialState () in
       let rec partie_aux (j:bool) (st:state) =
            if G.isTerminal st then (not j) else

            if j then (
               let () = G.print_state st in
               let () = G.print_action(makeDecision st) in
               let () = Printf.printf "exploration de %d noeuds\n" (getMetrics ()) in
               partie_aux (not j) (G.getResult st (makeDecision st) j)
            )
         else
            let () = G.print_state st in
            let () = Printf.printf "Au tour du joueur. \n" in
            let rec check_move () = 
               let i = G.readAction () in
               if List.mem i (G.getActions st) then i else ( let () = Printf.printf "Ce coup n'est pas valide ! Réessayer. \n" in check_move () )
            in
            let move = check_move () in
            let () = G.print_action(move) in
            partie_aux (not j) (G.getResult st move j)
       in partie_aux false s
   ;;
       
end;;

module MinimaxSearch2 (G : Game) = struct
    type state = G.state
    type action = G.action

    let expandedNodes = ref 0;;

    let alpha = ref min_int ;;

    let beta = ref max_int ;;

    let rec fold_right_until f l acc (p: int -> bool) =
    match l with (*this function functions the same way as fold_right but breaks the loop as soon as p is verified*)
    | x :: xs when p x -> acc
    | x :: xs -> f x (fold_right_until f xs acc p)
    | [] -> acc
      ;;


    let rec maxValue (s : state) (p : bool) (d : int)=
    begin
       assert ( p) ;
       expandedNodes := !expandedNodes + 1;
       if ((G.isTerminal s) || d = 0) then G.getUtility s p
       else
       let rec fold_until_beta g acc =
      match g with
      | x::xs -> (
         let res = max acc (minValue (G.getResult s x p) (not p) (d-1)) in
         if res >= !beta then res else ((alpha := max !alpha res); fold_until_beta xs res)
      )
      | [] -> acc
   in
   fold_until_beta (G.getActions s) min_int
    end
       
    and

    minValue (s : state) (p : bool) (d : int) =
    begin
       assert (not(p)) ;
       expandedNodes := !expandedNodes + 1;
       if ((G.isTerminal s) || d = 0) then G.getUtility s p
       else
       let rec fold_until_alpha g acc =
      match g with
      | x::xs -> (
         let res = min acc (maxValue (G.getResult s x p) (not p) (d-1)) in
         if res <= !alpha then res else ((beta := min !beta res); fold_until_alpha xs res)
      )
      | [] -> acc
   in
   fold_until_alpha (G.getActions s) max_int
    end

    let makeDecision s = 
      let p = true (*pour Max*) in 
      let (v, a) =
      List.fold_right
       (fun a (value, act) ->
         (*let () = Printf.printf "[Debut fold]\n" in
         let () = G.print_action a in
         let () = Printf.printf "[Fin fold]\n" in*)
           let v = minValue (G.getResult s a p) (not p) (G.getDepth ()) in
           if v > value then (v, a) else (value, act)
       )
       (G.getActions s) (min_int, List.hd (G.getActions s))
       in a

     let getMetrics () = !expandedNodes ;;

     let ordiTest () = 
       let s = G.getInitialState () in
       let rec partie_aux (j:bool) (st:state) =
            if G.isTerminal st then (not j) else

            if j then (
               let () = G.print_action(makeDecision st) in
               let () = Printf.printf "exploration de %d noeuds\n" (getMetrics ()) in
               partie_aux (not j) (G.getResult st (makeDecision st) j)
            )
         else
            let () = G.print_state st in
            let () = Printf.printf "Au tour du joueur. \n" in
            let rec check_move () = 
               let i = G.readAction () in
               if List.mem i (G.getActions st) then i else ( let () = Printf.printf "Ce coup n'est pas valide ! Réessayer. \n" in check_move () )
            in
            let move = check_move () in
            let () = G.print_action(move) in
            partie_aux (not j) (G.getResult st move j)
       in partie_aux false s
   ;;
       
end;;


module MinimaxSearch  : FSearch = MinimaxSearch0 ;;

module MinimaxSearchDepth : FSearch = MinimaxSearch1 ;;

module MinimaxAlphaBeta : FSearch = MinimaxSearch2 ;;


module Nim  : Game =
struct

   type state = int;;
   type action = int;;



   let getInitialState () = let () = Printf.printf "Veuillez rentrer le nombre d'allumette de départ : " in read_int ();;

   let getActions e = 
      if e = 1 then [] else
      if e = 2 then [1] else
      if e = 3 then [1;2] else
      if e >= 4 then [1;2;3] else []
   ;;

   let getResult e a p = e-a;;

   let isTerminal e = (e = 1);;

   let getUtility e p = 
      if (e mod 4) = 1 then (if p then (-1) else 1) else 0;;

   let getDepth () = 8;;

   let print_action a = Printf.printf "On enlève %d allumettes. \n" a ;;

   let print_state e = Printf.printf "Il y a %d allumettes. \n" e ;;

   let readAction () = read_int ();;

   let jouer_partie (p:bool) = 
      let s = getInitialState () in
      let rec partie_aux (j:bool) (st:state) =
         if isTerminal st then (if st = 1 then not j else j) else
         let () = print_state st in
         let () = Printf.printf "Au tour du joueur %b. \n" j in
         let rec check_move () = 
            let i = read_int () in
            if List.mem i (getActions st) then i else ( let () = Printf.printf "Ce coup n'est pas valide ! Réessayer. \n" in check_move () )
         in
         let move = check_move () in
         partie_aux (not j) (getResult st move j)
      in partie_aux p s
   ;;

   (* Printf.printf "Le gagnant est le joueur %b ! \n" (partie true) ;; *)
   
end ;;

module Connect4  : Game =
struct

   type state = int list;; (*a list representing the board. 0 is bottom left slot, 1 is directly above 0, etc. With that logic, 6 is the bottom slot of the second column. element is 0 if the case is empty, 1 if it contains a True Player piece, -1 if it contains a False Player piece.*)
   type action = int;;



   let getInitialState () = 
      let rec create_list (size:int) (l:int list) = 
      if size = 0 then l else create_list (size-1) (0::l) in
      create_list 42 []
      ;;

   let getActions e = 
      let top_row = (List.nth e 5)::(List.nth e 11)::(List.nth e 17)::(List.nth e 23)::(List.nth e 29)::(List.nth e 35)::(List.nth e 41)::[] in 
      let (a,b) = List.fold_left 
      (fun (rang,acc) c -> 
      if c = 0 then (rang+1,((rang)::acc)) else ((rang+1),acc)) (0,[]) top_row in
      b
   ;;

   let first_avail e a = 
      if (List.mem a (getActions e)) then (
         if (List.nth e ((6*a) + 4)) <> 0 then (6*a) + 5 else
         if (List.nth e ((6*a) + 3)) <> 0 then (6*a) + 4 else
         if (List.nth e ((6*a) + 2)) <> 0 then (6*a) + 3 else
         if (List.nth e ((6*a) + 1)) <> 0 then (6*a) + 2 else
         if (List.nth e ((6*a) + 0)) <> 0 then (6*a) + 1 else 6*a
      )
      else failwith "Not supposed to happen"
;;

   let getResult e a p = 
   let top = first_avail e a in
      if p 
      then List.mapi (fun i x -> if i = top then 1 else x) e
      else List.mapi (fun i x -> if i = top then -1 else x) e
   ;;


   let checkHorizontal4 e =
      let top_row = (List.nth e 5)::(List.nth e 11)::(List.nth e 17)::(List.nth e 23)::(List.nth e 29)::(List.nth e 35)::(List.nth e 41)::[] in
      let (_,isTopRow) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) top_row in
      let (_,isTopRow2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) top_row in
      let fifth_row = (List.nth e 4)::(List.nth e 10)::(List.nth e 16)::(List.nth e 22)::(List.nth e 28)::(List.nth e 34)::(List.nth e 40)::[] in
      let (_,isFifthRow) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fifth_row in
      let (_,isFifthRow2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fifth_row in
      let fourth_row = (List.nth e 3)::(List.nth e 9)::(List.nth e 15)::(List.nth e 21)::(List.nth e 27)::(List.nth e 33)::(List.nth e 39)::[] in
      let (_,isFourthRow) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fourth_row in
      let (_,isFourthRow2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fourth_row in
      let thrid_row = (List.nth e 2)::(List.nth e 8)::(List.nth e 14)::(List.nth e 20)::(List.nth e 26)::(List.nth e 32)::(List.nth e 38)::[] in
      let (_,isThirdRow) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) thrid_row in
      let (_,isThirdRow2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) thrid_row in
      let second_row = (List.nth e 1)::(List.nth e 7)::(List.nth e 13)::(List.nth e 19)::(List.nth e 25)::(List.nth e 31)::(List.nth e 37)::[] in
      let (_,isSecondRow) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) second_row in
      let (_,isSecondRow2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) second_row in
      let first_row = (List.nth e 0)::(List.nth e 6)::(List.nth e 12)::(List.nth e 18)::(List.nth e 24)::(List.nth e 30)::(List.nth e 36)::[] in
      let (_,isFirstRow) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) first_row in
      let (_,isFirstRow2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) first_row in
      (isFirstRow || isSecondRow || isThirdRow || isFourthRow || isFifthRow || isTopRow , isFirstRow2 || isSecondRow2 || isThirdRow2 || isFourthRow2 || isFifthRow2 || isTopRow2)
   ;;

   let checkVertical4 e =
      let seventh_col = (List.nth e 36)::(List.nth e 37)::(List.nth e 38)::(List.nth e 39)::(List.nth e 40)::(List.nth e 41)::[] in
      let (_,is7thCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) seventh_col in
      let (_,is7thCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) seventh_col in
      let sixth_col = (List.nth e 30)::(List.nth e 31)::(List.nth e 32)::(List.nth e 33)::(List.nth e 34)::(List.nth e 35)::[] in
      let (_,is6thCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) sixth_col in
      let (_,is6thCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) sixth_col in
      let fifth_col = (List.nth e 24)::(List.nth e 25)::(List.nth e 26)::(List.nth e 27)::(List.nth e 28)::(List.nth e 29)::[] in
      let (_,is5thCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fifth_col in
      let (_,is5thCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fifth_col in
      let fourth_col = (List.nth e 18)::(List.nth e 19)::(List.nth e 20)::(List.nth e 21)::(List.nth e 22)::(List.nth e 23)::[] in
      let (_,is4thCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fourth_col in
      let (_,is4thCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) fourth_col in
      let third_col = (List.nth e 12)::(List.nth e 13)::(List.nth e 14)::(List.nth e 15)::(List.nth e 16)::(List.nth e 17)::[] in
      let (_,is3rdCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) third_col in
      let (_,is3rdCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) third_col in
      let second_col = (List.nth e 6)::(List.nth e 7)::(List.nth e 8)::(List.nth e 9)::(List.nth e 10)::(List.nth e 11)::[] in
      let (_,is2ndCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) second_col in
      let (_,is2ndCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) second_col in
      let first_col = (List.nth e 0)::(List.nth e 1)::(List.nth e 2)::(List.nth e 3)::(List.nth e 4)::(List.nth e 5)::[] in
      let (_,isFirstCol) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) first_col in
      let (_,isFirstCol2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) first_col in
      (isFirstCol || is2ndCol || is3rdCol || is4thCol || is5thCol || is6thCol || is7thCol, isFirstCol2 || is2ndCol2 || is3rdCol2 || is4thCol2 || is5thCol2 || is6thCol2 || is7thCol2)
   ;;

   let checkDiagUp4 e =
      let startWith2Diag = (List.nth e 2)::(List.nth e 9)::(List.nth e 16)::(List.nth e 23)::[] in
      let (_,is2Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith2Diag in
      let (_,is2Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith2Diag in
      let startWith1Diag = (List.nth e 1)::(List.nth e 8)::(List.nth e 15)::(List.nth e 22)::(List.nth e 29)::[] in
      let (_,is1Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith1Diag in
      let (_,is1Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith1Diag in
      let startWith0Diag = (List.nth e 0)::(List.nth e 7)::(List.nth e 14)::(List.nth e 21)::(List.nth e 28)::(List.nth e 35)::[] in
      let (_,is0Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith0Diag in
      let (_,is0Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith0Diag in
      let startWith6Diag = (List.nth e 6)::(List.nth e 13)::(List.nth e 20)::(List.nth e 27)::(List.nth e 34)::(List.nth e 41)::[] in
      let (_,is6Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith6Diag in
      let (_,is6Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith6Diag in
      let startWith12Diag = (List.nth e 12)::(List.nth e 19)::(List.nth e 26)::(List.nth e 33)::(List.nth e 40)::[] in
      let (_,is12Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith12Diag in
      let (_,is12Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith12Diag in
      let startWith18Diag = (List.nth e 18)::(List.nth e 25)::(List.nth e 32)::(List.nth e 39)::[] in
      let (_,is18Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith18Diag in
      let (_,is18Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith18Diag in
      (is2Diag || is1Diag || is0Diag || is6Diag || is12Diag || is18Diag, is2Diag2 || is1Diag2 || is0Diag2 || is6Diag2 || is12Diag2 || is18Diag2)
   ;;

   let checkDiagDown4 e =
      let startWith23Diag = (List.nth e 23)::(List.nth e 28)::(List.nth e 33)::(List.nth e 38)::[] in
      let (_,is23Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith23Diag in
      let (_,is23Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith23Diag in
      let startWith17Diag = (List.nth e 17)::(List.nth e 22)::(List.nth e 27)::(List.nth e 32)::(List.nth e 37)::[] in
      let (_,is17Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith17Diag in
      let (_,is17Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith17Diag in
      let startWith11Diag = (List.nth e 11)::(List.nth e 16)::(List.nth e 21)::(List.nth e 26)::(List.nth e 31)::(List.nth e 36)::[] in
      let (_,is11Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith11Diag in
      let (_,is11Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith11Diag in
      let startWith5Diag = (List.nth e 5)::(List.nth e 10)::(List.nth e 15)::(List.nth e 20)::(List.nth e 25)::(List.nth e 30)::[] in
      let (_,is5Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith5Diag in
      let (_,is5Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith5Diag in
      let startWith4Diag = (List.nth e 4)::(List.nth e 9)::(List.nth e 14)::(List.nth e 19)::(List.nth e 24)::[] in
      let (_,is4Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith4Diag in
      let (_,is4Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith4Diag in
      let startWith3Diag = (List.nth e 18)::(List.nth e 25)::(List.nth e 32)::(List.nth e 39)::[] in
      let (_,is3Diag) = List.fold_left (fun (count,boolElt) c -> if c = 1 then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith3Diag in
      let (_,is3Diag2) = List.fold_left (fun (count,boolElt) c -> if c = (-1) then ((count+1),((count+1) >= 4)) else (0,boolElt)) (0,false) startWith3Diag in
      (is23Diag || is17Diag || is11Diag || is5Diag || is4Diag || is3Diag, is23Diag2 || is17Diag2 || is11Diag2 || is5Diag2 || is4Diag2 || is3Diag2)
   ;;

   let isTerminal e = (*fst (countHorizontal2 e) + fst (countVertical2 e) + fst (countDiagUp2 e) + fst (countDiagDown2) *)
      let horizontal = checkHorizontal4 e in
      (*let _ = Printf.printf "Le booléen de isTerminal : horizontal est %b \n" (fst horizontal) in
      let _ = Printf.printf "Le booléen de isTerminal : horizontal est %b \n" (snd horizontal) in*)
      let verical = checkVertical4 e in
      (*let _ = Printf.printf "Le booléen de isTerminal : verical est %b \n" (fst verical) in
      let _ = Printf.printf "Le booléen de isTerminal : verical est %b \n" (snd verical) in*)
      let diagUp = checkDiagUp4 e in
      (*let _ = Printf.printf "Le booléen de isTerminal : diagUp est %b \n" (fst diagUp) in
      let _ = Printf.printf "Le booléen de isTerminal : diagUp est %b \n" (snd diagUp) in*)
      let diagDown = checkDiagDown4 e in
      (*let _ = Printf.printf "Le booléen de isTerminal : diagDown est %b \n" (fst diagDown) in
      let _ = Printf.printf "Le booléen de isTerminal : diagDown est %b \n" (snd diagDown) in*)
      
      fst horizontal || fst verical || fst diagUp || fst diagDown || snd horizontal || snd verical || snd diagUp || snd diagDown
   ;;

   let countHorizontal2 e =
      let top_row = (List.nth e 5)::(List.nth e 11)::(List.nth e 17)::(List.nth e 23)::(List.nth e 29)::(List.nth e 35)::(List.nth e 41)::[] in
      let (_,amountTopRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) top_row in
      let (_,amountTopRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) top_row in
      let fifth_row = (List.nth e 4)::(List.nth e 10)::(List.nth e 16)::(List.nth e 22)::(List.nth e 28)::(List.nth e 34)::(List.nth e 40)::[] in
      let (_,amountFifthRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_row in
      let (_,amountFifthRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_row in
      let fourth_row = (List.nth e 3)::(List.nth e 9)::(List.nth e 15)::(List.nth e 21)::(List.nth e 27)::(List.nth e 33)::(List.nth e 39)::[] in
      let (_,amountFourthRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_row in
      let (_,amountFourthRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_row in
      let thrid_row = (List.nth e 2)::(List.nth e 8)::(List.nth e 14)::(List.nth e 20)::(List.nth e 26)::(List.nth e 32)::(List.nth e 38)::[] in
      let (_,amountThirdRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) thrid_row in
      let (_,amountThirdRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) thrid_row in
      let second_row = (List.nth e 1)::(List.nth e 7)::(List.nth e 13)::(List.nth e 19)::(List.nth e 25)::(List.nth e 31)::(List.nth e 37)::[] in
      let (_,amountSecondRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_row in
      let (_,amountSecondRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_row in
      let first_row = (List.nth e 0)::(List.nth e 6)::(List.nth e 12)::(List.nth e 18)::(List.nth e 24)::(List.nth e 30)::(List.nth e 36)::[] in
      let (_,amountFirstRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_row in
      let (_,amountFirstRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_row in
      (amountTopRow+amountFifthRow+amountFourthRow+amountThirdRow+amountSecondRow+amountFirstRow,amountTopRow2+amountFifthRow2+amountFourthRow2+amountThirdRow2+amountSecondRow2+amountFirstRow2)
   ;;

   let countVertical2 e =
      let seventh_col = (List.nth e 36)::(List.nth e 37)::(List.nth e 38)::(List.nth e 39)::(List.nth e 40)::(List.nth e 41)::[] in
      let (_,amount7thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) seventh_col in
      let (_,amount7thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) seventh_col in
      let sixth_col = (List.nth e 30)::(List.nth e 31)::(List.nth e 32)::(List.nth e 33)::(List.nth e 34)::(List.nth e 35)::[] in
      let (_,amount6thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) sixth_col in
      let (_,amount6thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) sixth_col in
      let fifth_col = (List.nth e 24)::(List.nth e 25)::(List.nth e 26)::(List.nth e 27)::(List.nth e 28)::(List.nth e 29)::[] in
      let (_,amount5thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_col in
      let (_,amount5thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_col in
      let fourth_col = (List.nth e 18)::(List.nth e 19)::(List.nth e 20)::(List.nth e 21)::(List.nth e 22)::(List.nth e 23)::[] in
      let (_,amount4thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_col in
      let (_,amount4thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_col in
      let third_col = (List.nth e 12)::(List.nth e 13)::(List.nth e 14)::(List.nth e 15)::(List.nth e 16)::(List.nth e 17)::[] in
      let (_,amount3rdCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) third_col in
      let (_,amount3rdCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) third_col in
      let second_col = (List.nth e 6)::(List.nth e 7)::(List.nth e 8)::(List.nth e 9)::(List.nth e 10)::(List.nth e 11)::[] in
      let (_,amount2ndCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_col in
      let (_,amount2ndCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_col in
      let first_col = (List.nth e 0)::(List.nth e 1)::(List.nth e 2)::(List.nth e 3)::(List.nth e 4)::(List.nth e 5)::[] in
      let (_,amountFirstCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_col in
      let (_,amountFirstCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_col in
      (amount7thCol+amount6thCol+amount5thCol+amount4thCol+amount3rdCol+amount2ndCol+amountFirstCol,amount7thCol2+amount6thCol2+amount5thCol2+amount4thCol2+amount3rdCol2+amount2ndCol2+amountFirstCol2)
   ;;

   let countDiagUp2 e =
      let startWith4Diag = (List.nth e 4)::(List.nth e 11)::[] in
      let (_,amount4Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let (_,amount4Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith4Diag in
      let startWith3Diag = (List.nth e 3)::(List.nth e 10)::(List.nth e 17)::[] in
      let (_,amount3Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let (_,amount3Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith3Diag in
      let startWith2Diag = (List.nth e 2)::(List.nth e 9)::(List.nth e 16)::(List.nth e 23)::[] in
      let (_,amount2Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      let (_,amount2Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith2Diag in
      let startWith1Diag = (List.nth e 1)::(List.nth e 8)::(List.nth e 15)::(List.nth e 22)::(List.nth e 29)::[] in
      let (_,amount1Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith1Diag in
      let (_,amount1Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith1Diag in
      let startWith0Diag = (List.nth e 0)::(List.nth e 7)::(List.nth e 14)::(List.nth e 21)::(List.nth e 28)::(List.nth e 35)::[] in
      let (_,amount0Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith0Diag in
      let (_,amount0Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith0Diag in
      let startWith6Diag = (List.nth e 6)::(List.nth e 13)::(List.nth e 20)::(List.nth e 27)::(List.nth e 34)::(List.nth e 41)::[] in
      let (_,amount6Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith6Diag in
      let (_,amount6Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith6Diag in
      let startWith12Diag = (List.nth e 12)::(List.nth e 19)::(List.nth e 26)::(List.nth e 33)::(List.nth e 40)::[] in
      let (_,amount12Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith12Diag in
      let (_,amount12Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith12Diag in
      let startWith18Diag = (List.nth e 18)::(List.nth e 25)::(List.nth e 32)::(List.nth e 39)::[] in
      let (_,amount18Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith18Diag in
      let (_,amount18Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith18Diag in
      let startWith24Diag = (List.nth e 24)::(List.nth e 31)::(List.nth e 38)::[] in
      let (_,amount24Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith24Diag in
      let (_,amount24Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith24Diag in
      let startWith30Diag = (List.nth e 30)::(List.nth e 37)::[] in
      let (_,amount30Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith30Diag in
      let (_,amount30Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith30Diag in
      (amount4Diag+amount3Diag+amount2Diag+amount1Diag+amount0Diag+amount6Diag+amount12Diag+amount18Diag+amount24Diag+amount30Diag, amount4Diag2+amount3Diag2+amount2Diag2+amount1Diag2+amount0Diag2+amount6Diag2+amount12Diag2+amount18Diag2+amount24Diag2+amount30Diag2)
   ;;

   let countDiagDown2 e =
      let startWith35Diag = (List.nth e 35)::(List.nth e 40)::[] in
      let (_,amount35Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith35Diag in
      let (_,amount35Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith35Diag in
      let startWith29Diag = (List.nth e 29)::(List.nth e 34)::(List.nth e 39)::[] in
      let (_,amount29Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith29Diag in
      let (_,amount29Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith29Diag in
      let startWith23Diag = (List.nth e 23)::(List.nth e 28)::(List.nth e 33)::(List.nth e 38)::[] in
      let (_,amount23Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith23Diag in
      let (_,amount23Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith23Diag in
      let startWith17Diag = (List.nth e 17)::(List.nth e 22)::(List.nth e 27)::(List.nth e 32)::(List.nth e 37)::[] in
      let (_,amount17Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith17Diag in
      let (_,amount17Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith17Diag in
      let startWith11Diag = (List.nth e 11)::(List.nth e 16)::(List.nth e 21)::(List.nth e 26)::(List.nth e 31)::(List.nth e 36)::[] in
      let (_,amount11Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith11Diag in
      let (_,amount11Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith11Diag in
      let startWith5Diag = (List.nth e 5)::(List.nth e 10)::(List.nth e 15)::(List.nth e 20)::(List.nth e 25)::(List.nth e 30)::[] in
      let (_,amount5Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith5Diag in
      let (_,amount5Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith5Diag in
      let startWith4Diag = (List.nth e 4)::(List.nth e 9)::(List.nth e 14)::(List.nth e 19)::(List.nth e 24)::[] in
      let (_,amount4Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let (_,amount4Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let startWith3Diag = (List.nth e 3)::(List.nth e 8)::(List.nth e 13)::(List.nth e 18)::[] in
      let (_,amount3Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let (_,amount3Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let startWith2Diag = (List.nth e 2)::(List.nth e 7)::(List.nth e 12)::[] in
      let (_,amount2Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      let (_,amount2Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      let startWith1Diag = (List.nth e 1)::(List.nth e 6)::[] in
      let (_,amount1Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith1Diag in
      let (_,amount1Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 2 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith1Diag in
      (amount35Diag+amount29Diag+amount23Diag+amount17Diag+amount11Diag+amount5Diag+amount4Diag+amount3Diag+amount2Diag+amount1Diag, amount35Diag2+amount29Diag2+amount23Diag2+amount17Diag2+amount11Diag2+amount5Diag2+amount4Diag2+amount3Diag2+amount2Diag2+amount1Diag2)
   ;;

   let countHorizontal3 e =
      let top_row = (List.nth e 5)::(List.nth e 11)::(List.nth e 17)::(List.nth e 23)::(List.nth e 29)::(List.nth e 35)::(List.nth e 41)::[] in
      let (_,amountTopRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) top_row in
      let (_,amountTopRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) top_row in
      let fifth_row = (List.nth e 4)::(List.nth e 10)::(List.nth e 16)::(List.nth e 22)::(List.nth e 28)::(List.nth e 34)::(List.nth e 40)::[] in
      let (_,amountFifthRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_row in
      let (_,amountFifthRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_row in
      let fourth_row = (List.nth e 3)::(List.nth e 9)::(List.nth e 15)::(List.nth e 21)::(List.nth e 27)::(List.nth e 33)::(List.nth e 39)::[] in
      let (_,amountFourthRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_row in
      let (_,amountFourthRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_row in
      let thrid_row = (List.nth e 2)::(List.nth e 8)::(List.nth e 14)::(List.nth e 20)::(List.nth e 26)::(List.nth e 32)::(List.nth e 38)::[] in
      let (_,amountThirdRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) thrid_row in
      let (_,amountThirdRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) thrid_row in
      let second_row = (List.nth e 1)::(List.nth e 7)::(List.nth e 13)::(List.nth e 19)::(List.nth e 25)::(List.nth e 31)::(List.nth e 37)::[] in
      let (_,amountSecondRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_row in
      let (_,amountSecondRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_row in
      let first_row = (List.nth e 0)::(List.nth e 6)::(List.nth e 12)::(List.nth e 18)::(List.nth e 24)::(List.nth e 30)::(List.nth e 36)::[] in
      let (_,amountFirstRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_row in
      let (_,amountFirstRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_row in
      (amountTopRow+amountFifthRow+amountFourthRow+amountThirdRow+amountSecondRow+amountFirstRow,amountTopRow2+amountFifthRow2+amountFourthRow2+amountThirdRow2+amountSecondRow2+amountFirstRow2)
   ;;

   let countVertical3 e =
      let seventh_col = (List.nth e 36)::(List.nth e 37)::(List.nth e 38)::(List.nth e 39)::(List.nth e 40)::(List.nth e 41)::[] in
      let (_,amount7thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) seventh_col in
      let (_,amount7thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) seventh_col in
      let sixth_col = (List.nth e 30)::(List.nth e 31)::(List.nth e 32)::(List.nth e 33)::(List.nth e 34)::(List.nth e 35)::[] in
      let (_,amount6thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) sixth_col in
      let (_,amount6thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) sixth_col in
      let fifth_col = (List.nth e 24)::(List.nth e 25)::(List.nth e 26)::(List.nth e 27)::(List.nth e 28)::(List.nth e 29)::[] in
      let (_,amount5thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_col in
      let (_,amount5thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_col in
      let fourth_col = (List.nth e 18)::(List.nth e 19)::(List.nth e 20)::(List.nth e 21)::(List.nth e 22)::(List.nth e 23)::[] in
      let (_,amount4thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_col in
      let (_,amount4thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_col in
      let third_col = (List.nth e 12)::(List.nth e 13)::(List.nth e 14)::(List.nth e 15)::(List.nth e 16)::(List.nth e 17)::[] in
      let (_,amount3rdCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) third_col in
      let (_,amount3rdCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) third_col in
      let second_col = (List.nth e 6)::(List.nth e 7)::(List.nth e 8)::(List.nth e 9)::(List.nth e 10)::(List.nth e 11)::[] in
      let (_,amount2ndCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_col in
      let (_,amount2ndCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_col in
      let first_col = (List.nth e 0)::(List.nth e 1)::(List.nth e 2)::(List.nth e 3)::(List.nth e 4)::(List.nth e 5)::[] in
      let (_,amountFirstCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_col in
      let (_,amountFirstCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_col in
      (amount7thCol+amount6thCol+amount5thCol+amount4thCol+amount3rdCol+amount2ndCol+amountFirstCol,amount7thCol2+amount6thCol2+amount5thCol2+amount4thCol2+amount3rdCol2+amount2ndCol2+amountFirstCol2)
   ;;

   let countDiagUp3 e =
      let startWith3Diag = (List.nth e 3)::(List.nth e 10)::(List.nth e 17)::[] in
      let (_,amount3Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let (_,amount3Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith3Diag in
      let startWith2Diag = (List.nth e 2)::(List.nth e 9)::(List.nth e 16)::(List.nth e 23)::[] in
      let (_,amount2Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      let (_,amount2Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith2Diag in
      let startWith1Diag = (List.nth e 1)::(List.nth e 8)::(List.nth e 15)::(List.nth e 22)::(List.nth e 29)::[] in
      let (_,amount1Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith1Diag in
      let (_,amount1Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith1Diag in
      let startWith0Diag = (List.nth e 0)::(List.nth e 7)::(List.nth e 14)::(List.nth e 21)::(List.nth e 28)::(List.nth e 35)::[] in
      let (_,amount0Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith0Diag in
      let (_,amount0Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith0Diag in
      let startWith6Diag = (List.nth e 6)::(List.nth e 13)::(List.nth e 20)::(List.nth e 27)::(List.nth e 34)::(List.nth e 41)::[] in
      let (_,amount6Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith6Diag in
      let (_,amount6Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith6Diag in
      let startWith12Diag = (List.nth e 12)::(List.nth e 19)::(List.nth e 26)::(List.nth e 33)::(List.nth e 40)::[] in
      let (_,amount12Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith12Diag in
      let (_,amount12Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith12Diag in
      let startWith18Diag = (List.nth e 18)::(List.nth e 25)::(List.nth e 32)::(List.nth e 39)::[] in
      let (_,amount18Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith18Diag in
      let (_,amount18Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith18Diag in
      let startWith24Diag = (List.nth e 24)::(List.nth e 31)::(List.nth e 38)::[] in
      let (_,amount24Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith24Diag in
      let (_,amount24Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith24Diag in
      (amount3Diag+amount2Diag+amount1Diag+amount0Diag+amount6Diag+amount12Diag+amount18Diag+amount24Diag, amount3Diag2+amount2Diag2+amount1Diag2+amount0Diag2+amount6Diag2+amount12Diag2+amount18Diag2+amount24Diag2)
   ;;

   let countDiagDown3 e =
      let startWith29Diag = (List.nth e 29)::(List.nth e 34)::(List.nth e 39)::[] in
      let (_,amount29Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith29Diag in
      let (_,amount29Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith29Diag in
      let startWith23Diag = (List.nth e 23)::(List.nth e 28)::(List.nth e 33)::(List.nth e 38)::[] in
      let (_,amount23Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith23Diag in
      let (_,amount23Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith23Diag in
      let startWith17Diag = (List.nth e 17)::(List.nth e 22)::(List.nth e 27)::(List.nth e 32)::(List.nth e 37)::[] in
      let (_,amount17Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith17Diag in
      let (_,amount17Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith17Diag in
      let startWith11Diag = (List.nth e 11)::(List.nth e 16)::(List.nth e 21)::(List.nth e 26)::(List.nth e 31)::(List.nth e 36)::[] in
      let (_,amount11Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith11Diag in
      let (_,amount11Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith11Diag in
      let startWith5Diag = (List.nth e 5)::(List.nth e 10)::(List.nth e 15)::(List.nth e 20)::(List.nth e 25)::(List.nth e 30)::[] in
      let (_,amount5Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith5Diag in
      let (_,amount5Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith5Diag in
      let startWith4Diag = (List.nth e 4)::(List.nth e 9)::(List.nth e 14)::(List.nth e 19)::(List.nth e 24)::[] in
      let (_,amount4Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let (_,amount4Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let startWith3Diag = (List.nth e 3)::(List.nth e 8)::(List.nth e 13)::(List.nth e 18)::[] in
      let (_,amount3Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let (_,amount3Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let startWith2Diag = (List.nth e 2)::(List.nth e 7)::(List.nth e 12)::[] in
      let (_,amount2Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      let (_,amount2Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 3 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      (amount29Diag+amount23Diag+amount17Diag+amount11Diag+amount5Diag+amount4Diag+amount3Diag+amount2Diag, amount29Diag2+amount23Diag2+amount17Diag2+amount11Diag2+amount5Diag2+amount4Diag2+amount3Diag2+amount2Diag2)
   ;;

   let countHorizontal4 e =
      let top_row = (List.nth e 5)::(List.nth e 11)::(List.nth e 17)::(List.nth e 23)::(List.nth e 29)::(List.nth e 35)::(List.nth e 41)::[] in
      let (_,amountTopRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) top_row in
      let (_,amountTopRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) top_row in
      let fifth_row = (List.nth e 4)::(List.nth e 10)::(List.nth e 16)::(List.nth e 22)::(List.nth e 28)::(List.nth e 34)::(List.nth e 40)::[] in
      let (_,amountFifthRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_row in
      let (_,amountFifthRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_row in
      let fourth_row = (List.nth e 3)::(List.nth e 9)::(List.nth e 15)::(List.nth e 21)::(List.nth e 27)::(List.nth e 33)::(List.nth e 39)::[] in
      let (_,amountFourthRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_row in
      let (_,amountFourthRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_row in
      let thrid_row = (List.nth e 2)::(List.nth e 8)::(List.nth e 14)::(List.nth e 20)::(List.nth e 26)::(List.nth e 32)::(List.nth e 38)::[] in
      let (_,amountThirdRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) thrid_row in
      let (_,amountThirdRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) thrid_row in
      let second_row = (List.nth e 1)::(List.nth e 7)::(List.nth e 13)::(List.nth e 19)::(List.nth e 25)::(List.nth e 31)::(List.nth e 37)::[] in
      let (_,amountSecondRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_row in
      let (_,amountSecondRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_row in
      let first_row = (List.nth e 0)::(List.nth e 6)::(List.nth e 12)::(List.nth e 18)::(List.nth e 24)::(List.nth e 30)::(List.nth e 36)::[] in
      let (_,amountFirstRow) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_row in
      let (_,amountFirstRow2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_row in
      (amountTopRow+amountFifthRow+amountFourthRow+amountThirdRow+amountSecondRow+amountFirstRow,amountTopRow2+amountFifthRow2+amountFourthRow2+amountThirdRow2+amountSecondRow2+amountFirstRow2)
   ;;

   let countVertical4 e =
      let seventh_col = (List.nth e 36)::(List.nth e 37)::(List.nth e 38)::(List.nth e 39)::(List.nth e 40)::(List.nth e 41)::[] in
      let (_,amount7thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) seventh_col in
      let (_,amount7thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) seventh_col in
      let sixth_col = (List.nth e 30)::(List.nth e 31)::(List.nth e 32)::(List.nth e 33)::(List.nth e 34)::(List.nth e 35)::[] in
      let (_,amount6thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) sixth_col in
      let (_,amount6thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) sixth_col in
      let fifth_col = (List.nth e 24)::(List.nth e 25)::(List.nth e 26)::(List.nth e 27)::(List.nth e 28)::(List.nth e 29)::[] in
      let (_,amount5thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_col in
      let (_,amount5thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fifth_col in
      let fourth_col = (List.nth e 18)::(List.nth e 19)::(List.nth e 20)::(List.nth e 21)::(List.nth e 22)::(List.nth e 23)::[] in
      let (_,amount4thCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_col in
      let (_,amount4thCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) fourth_col in
      let third_col = (List.nth e 12)::(List.nth e 13)::(List.nth e 14)::(List.nth e 15)::(List.nth e 16)::(List.nth e 17)::[] in
      let (_,amount3rdCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) third_col in
      let (_,amount3rdCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) third_col in
      let second_col = (List.nth e 6)::(List.nth e 7)::(List.nth e 8)::(List.nth e 9)::(List.nth e 10)::(List.nth e 11)::[] in
      let (_,amount2ndCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_col in
      let (_,amount2ndCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) second_col in
      let first_col = (List.nth e 0)::(List.nth e 1)::(List.nth e 2)::(List.nth e 3)::(List.nth e 4)::(List.nth e 5)::[] in
      let (_,amountFirstCol) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_col in
      let (_,amountFirstCol2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) first_col in
      (amount7thCol+amount6thCol+amount5thCol+amount4thCol+amount3rdCol+amount2ndCol+amountFirstCol,amount7thCol2+amount6thCol2+amount5thCol2+amount4thCol2+amount3rdCol2+amount2ndCol2+amountFirstCol2)
   ;;

   let countDiagUp4 e =
      let startWith2Diag = (List.nth e 2)::(List.nth e 9)::(List.nth e 16)::(List.nth e 23)::[] in
      let (_,amount2Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith2Diag in
      let (_,amount2Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith2Diag in
      let startWith1Diag = (List.nth e 1)::(List.nth e 8)::(List.nth e 15)::(List.nth e 22)::(List.nth e 29)::[] in
      let (_,amount1Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith1Diag in
      let (_,amount1Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith1Diag in
      let startWith0Diag = (List.nth e 0)::(List.nth e 7)::(List.nth e 14)::(List.nth e 21)::(List.nth e 28)::(List.nth e 35)::[] in
      let (_,amount0Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith0Diag in
      let (_,amount0Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith0Diag in
      let startWith6Diag = (List.nth e 6)::(List.nth e 13)::(List.nth e 20)::(List.nth e 27)::(List.nth e 34)::(List.nth e 41)::[] in
      let (_,amount6Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith6Diag in
      let (_,amount6Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith6Diag in
      let startWith12Diag = (List.nth e 12)::(List.nth e 19)::(List.nth e 26)::(List.nth e 33)::(List.nth e 40)::[] in
      let (_,amount12Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith12Diag in
      let (_,amount12Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith12Diag in
      let startWith18Diag = (List.nth e 18)::(List.nth e 25)::(List.nth e 32)::(List.nth e 39)::[] in
      let (_,amount18Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith18Diag in
      let (_,amount18Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount) ) else (0,amount)) (0,0) startWith18Diag in
      (amount2Diag+amount1Diag+amount0Diag+amount6Diag+amount12Diag+amount18Diag, amount2Diag2+amount1Diag2+amount0Diag2+amount6Diag2+amount12Diag2+amount18Diag2)
   ;;

   let countDiagDown4 e =
      let startWith23Diag = (List.nth e 23)::(List.nth e 28)::(List.nth e 33)::(List.nth e 38)::[] in
      let (_,amount23Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith23Diag in
      let (_,amount23Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith23Diag in
      let startWith17Diag = (List.nth e 17)::(List.nth e 22)::(List.nth e 27)::(List.nth e 32)::(List.nth e 37)::[] in
      let (_,amount17Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith17Diag in
      let (_,amount17Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith17Diag in
      let startWith11Diag = (List.nth e 11)::(List.nth e 16)::(List.nth e 21)::(List.nth e 26)::(List.nth e 31)::(List.nth e 36)::[] in
      let (_,amount11Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith11Diag in
      let (_,amount11Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith11Diag in
      let startWith5Diag = (List.nth e 5)::(List.nth e 10)::(List.nth e 15)::(List.nth e 20)::(List.nth e 25)::(List.nth e 30)::[] in
      let (_,amount5Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith5Diag in
      let (_,amount5Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith5Diag in
      let startWith4Diag = (List.nth e 4)::(List.nth e 9)::(List.nth e 14)::(List.nth e 19)::(List.nth e 24)::[] in
      let (_,amount4Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let (_,amount4Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith4Diag in
      let startWith3Diag = (List.nth e 3)::(List.nth e 8)::(List.nth e 13)::(List.nth e 18)::[] in
      let (_,amount3Diag) = List.fold_left (fun (count,amount) c -> if c = 1 then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      let (_,amount3Diag2) = List.fold_left (fun (count,amount) c -> if c = (-1) then (if (count+1) >= 4 then ((0),(amount+1)) else ((count+1),amount)) else (0,amount)) (0,0) startWith3Diag in
      (amount23Diag+amount17Diag+amount11Diag+amount5Diag+amount4Diag+amount3Diag, amount23Diag2+amount17Diag2+amount11Diag2+amount5Diag2+amount4Diag2+amount3Diag2)
   ;;

   let getUtility e p = (* if (e mod 5) = 1 then (if p then (-1) else 1) else 0 *)
      (*let onesPlayer1 = List.fold_left (fun amount c -> if c = 1 then amount+1 else amount) 0 e in*)
      let twosPlayer1 = fst (countHorizontal2 e) + fst (countVertical2 e) + fst (countDiagUp2 e) + fst (countDiagDown2 e)  in
      let threesPlayer1 = fst (countHorizontal3 e) + fst (countVertical3 e) + fst (countDiagUp3 e) + fst (countDiagDown3 e)  in
      let foursPalyer1 = fst (countHorizontal4 e) + fst (countVertical4 e) + fst (countDiagUp4 e) + fst (countDiagDown4 e) in

      (*let onesPlayer2 = List.fold_left (fun amount c -> if c = (-1) then amount+1 else amount) 0 e in*)
      let twosPlayer2 = snd (countHorizontal2 e) + snd (countVertical2 e) + snd (countDiagUp2 e) + snd (countDiagDown2 e)  in
      let threesPlayer2 = snd (countHorizontal3 e) + snd (countVertical3 e) + snd (countDiagUp3 e) + snd (countDiagDown3 e)  in
      let foursPalyer2 = snd (countHorizontal4 e) + snd (countVertical4 e) + snd (countDiagUp4 e) + snd (countDiagDown4 e) in

      ((20*foursPalyer1 + 5*threesPlayer1 + 2*twosPlayer1)-(20*foursPalyer2 + 5*threesPlayer2 + 2*twosPlayer2)) 
   ;;

   let getDepth () = 4;;

   let print_action a = Printf.printf "On pose un jeton dans la colonne %d. \n" a ;;

   let print_state e = (*Printf.printf "Il y a %d allumettes. \n" e ;;*)
      let _ = (Printf.printf "| %2d | %2d | %2d | %2d | %2d | %2d | %2d |\n") (List.nth e 5) (List.nth e 11) (List.nth e 17) (List.nth e 23) (List.nth e 29) (List.nth e 35) (List.nth e 41) in
      let _ = Printf.printf "+----------------------------------+\n" in
      let _ = (Printf.printf "| %2d | %2d | %2d | %2d | %2d | %2d | %2d |\n") (List.nth e 4) (List.nth e 10) (List.nth e 16) (List.nth e 22) (List.nth e 28) (List.nth e 34) (List.nth e 40) in
      let _ = Printf.printf "+----------------------------------+\n" in
      let _ = (Printf.printf "| %2d | %2d | %2d | %2d | %2d | %2d | %2d |\n") (List.nth e 3) (List.nth e 9) (List.nth e 15) (List.nth e 21) (List.nth e 27) (List.nth e 33) (List.nth e 39) in
      let _ = Printf.printf "+----------------------------------+\n" in
      let _ = (Printf.printf "| %2d | %2d | %2d | %2d | %2d | %2d | %2d |\n") (List.nth e 2) (List.nth e 8) (List.nth e 14) (List.nth e 20) (List.nth e 26) (List.nth e 32) (List.nth e 38) in
      let _ = Printf.printf "+----------------------------------+\n" in
      let _ = (Printf.printf "| %2d | %2d | %2d | %2d | %2d | %2d | %2d |\n") (List.nth e 1) (List.nth e 7) (List.nth e 13) (List.nth e 19) (List.nth e 25) (List.nth e 31) (List.nth e 37) in
      let _ = Printf.printf "+----------------------------------+\n" in
      let _ = (Printf.printf "| %2d | %2d | %2d | %2d | %2d | %2d | %2d |\n") (List.nth e 0) (List.nth e 6) (List.nth e 12) (List.nth e 18) (List.nth e 24) (List.nth e 30) (List.nth e 36) in
      Printf.printf "+----------------------------------+\n"

   let readAction () = (read_int ());;


   let jouer_partie (p:bool) = 
   
      let s = getInitialState () in
      let rec partie_aux (j:bool) (st:state) =
         let horizontal = checkHorizontal4 st in
         let verical = checkVertical4 st in
         let diagUp = checkDiagUp4 st in
         let diagDown = checkDiagDown4 st in
         if isTerminal st then (if (fst horizontal || fst verical || fst diagUp || fst diagDown) then not j else j) else
         let () = print_state st in
         let () = Printf.printf "Au tour du joueur %b. \n" j in
         let rec check_move () = 
            let i = read_int () in
            if List.mem i (getActions st) then i else ( let () = Printf.printf "Ce coup n'est pas valide ! Réessayer. \n" in check_move () )
         in
         let move = check_move () in
         partie_aux (not j) (getResult st move j)
      in partie_aux p s
       
   ;;

   (* Printf.printf "Le gagnant est le joueur %b ! \n" (partie true) ;; *)
   
end ;;





module NimTest = MinimaxSearch (Nim) ;;
module NimTestDepth = MinimaxSearchDepth (Nim) ;;
module NimTestAlphaBeta = MinimaxAlphaBeta (Nim) ;;


(*Printf.printf "Le gagnant est le joueur %b ! \n" (NimTest.ordiTest ())*) 

(*Printf.printf "Le gagnant est le joueur %b ! \n" (NimTestDepth.ordiTest ()) ;; *)

(* Printf.printf "Le gagnant est le joueur %b ! \n" (NimTestAlphaBeta.ordiTest ()) ;; *)



module Connect4Test = MinimaxSearch (Connect4) ;;
module Connect4TestDepth = MinimaxSearchDepth (Connect4) ;;
module Connect4TestAlphaBeta = MinimaxAlphaBeta (Connect4) ;;


let main = 
let () = Printf.printf "Bienvenue aux jeux ! Voulez vous jouer au Nim (1) ou au Puissance 4 (2) : " in 
match (read_int()) with
|1 -> (let () = Printf.printf "\n Voulez vous jouer contre: l'ordi sans depth avec temps d'attente très très long (1) \nl'ordi avec depth avec temps d'attente long (2) \nl'ordi avec AlphaBeta et temps d'attente modéré (3)\n" in
match (read_int()) with 
   |1 ->  Printf.printf "Le gagnant est le joueur %b ! \n" (NimTest.ordiTest ())
   |2 -> Printf.printf "Le gagnant est le joueur %b ! \n" (NimTestDepth.ordiTest ())
   |3 -> Printf.printf "Le gagnant est le joueur %b ! \n" (NimTestAlphaBeta.ordiTest ())
   |_ -> failwith "Unvalid Value"
)
|2 -> (let () = Printf.printf "\n Voulez vous jouer contre: l'ordi sans depth avec temps d'attente très très long (1) \nl'ordi avec depth avec temps d'attente long (2) \nl'ordi avec AlphaBeta et temps d'attente modéré (3)\n" in
match (read_int()) with 
   |1 -> Printf.printf "Le gagnant est le joueur %b ! \n" (Connect4Test.ordiTest ())
   |2 -> Printf.printf "Le gagnant est le joueur %b ! \n" (Connect4TestDepth.ordiTest ())
   |3 -> Printf.printf "Le gagnant est le joueur %b ! \n" (Connect4TestAlphaBeta.ordiTest ())
   |_ -> failwith "Unvalid Value"
)
|_ -> failwith "Unvalid Value"
;;

main ;;





   
