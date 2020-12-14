open Messages
open Letter
open Crypto

type author = {
  letter_bag : char list;
  score : int;
}

let make_letter_on_hash sk pk level head_hash letter : letter =
  Letter.make ~letter ~head:head_hash ~level ~pk ~sk

let make_letter_on_block sk pk level block letter : letter =
  let head_hash = hash block in
  make_letter_on_hash sk pk level head_hash letter

(* let random_char () = Random.int (122 - 65) + 65 |> Char.chr *)
let random_char () = Random.int 26 + 97 |> Char.chr

let get_random_char leetterbag =
  let n = Random.int (List.length leetterbag) in
  List.nth leetterbag n;;


let send_new_letter sk pk level store letter_bag =
  (* Get blockchain head *)
  
  Option.iter
    (fun head ->
      (* Create new random letter *)
      let letter =
        make_letter_on_block
          sk
          pk
          level
          (Word.to_bigstring head) 
          (get_random_char letter_bag)
      in
      (* Send letter *)
      let message = Messages.Inject_letter letter in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) store)

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in
  let author_score = ref 0 in
  (* Register to the server *)
  let reg_msg = Messages.Register pk in
  Client_utils.send_some reg_msg ;
  Unix.sleep 3;
  (* drop provided letter_bag *)
  (*
  ( match Client_utils.receive () with
  | Messages.Letters_bag _ -> () (*l -> letter_bag := l *)
  | _ -> assert false ) ; *)

  let rec wait_for_letterbag (): char list =
    match Client_utils.receive () with
      | Messages.Letters_bag letterbag -> letterbag
      | _ -> wait_for_letterbag ()
  in    
  let letter_bag = wait_for_letterbag () in


  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;

  (*
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false (*  Ici gerer correctement la réponse*)
  in
  *)

  let update_author_score head : int =
    let w = head.Word.word in 
      let rec compute_score w score =
        match w with
        | [] -> score 
        |x::xs -> if x.author = pk 
          then compute_score xs (score + (Consensus.letter_score x)) 
          else compute_score xs (score)
      in 
    compute_score w 0
  in

  let rec wait_for_wordpool (): Messages.wordpool =
    match Client_utils.receive () with
      | Messages.Full_wordpool wordpool -> wordpool
      | _ -> wait_for_wordpool ()
  in    
  let wordpool = wait_for_wordpool () in

  (* let auth : author = { letter_bag = letterbag; score=0 } in *)

  (* Generate initial blocktree *)
  let store = Store.init_words () in
  Store.add_words store wordpool.words ;
  (* Create and send first letter *)
  send_new_letter sk pk wordpool.current_period store letter_bag; (* attention période *)

  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen ;

  (* start main loop *)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then Log.log_success "FIN de l'auteur"
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
          Store.add_word store w ;
          Option.iter
            (fun head ->
              level := head.level ;
              if head = w then (
                Log.log_success "Head updated to incoming word %a@." Word.pp w ;
                author_score := (!author_score + (update_author_score head));
                Log.log_success "Score de l'auteur : %d\n" !author_score ;
                send_new_letter sk pk !level store letter_bag;
                )
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) store)
        | Messages.Inject_letter _ | _ -> () ) ;
      loop (max_iter - 1) )
  in
  loop max_iter;
  Log.log_success "Score final de l'auteur : %d\n" !author_score

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter: (20) ()

  in
  main
