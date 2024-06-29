(* PPL 2018*)

let combY = "Fun body -> Let wrapper = Fun this -> Fun arg -> body (this this) arg In wrapper wrapper";;

let techie = "Let combY = ("^combY^") In 
                Fun me -> 
                  combY (Fun this -> 
                            Fun state ->
                              Fun msg ->
                                Match msg With
                                 | `clear(disarmer) -> If (state = 0) Then ((Print \"techie done!\"); (disarmer <- `done (00)); (this state))
                                                                      Else ((Print \"techie sent!\");(disarmer <- `bomb (00)); (this (state-1))))";;

let disarmer = "Let combY = ("^combY^") In
                    Fun me ->
                        combY (Fun this -> Fun state -> Fun msg ->
                          Match msg With 
                          |`ready(_) -> (Print \"Received ready\"); (Fst(state) <- `clear(me)); (this (state, 1))
                          |`bomb(_) -> (Print \"Received bomb\"); If (Snd(state) = 1) Then (Fst(Fst(state)) <- `clear(me)); (this state)
                                        Else (Snd(Fst(state)) <- `clear(me)); (this state)
                          |`done(_) -> (Print \"Received done\"); If (Snd(state)) = 1 Then (Snd(Fst(state)) <- `clear(me)); (this (Fst(state), 2))
                                        Else (this state))";;

let question = "Let techie = ("^techie^") In
                Let disarmer = ("^disarmer^") In
                Let techie1 = Create(techie, 2) In
                Let techie2 = Create(techie, 2) In
                Let a_disarmer = Create(disarmer, (techie1, techie2)) In
                  a_disarmer <- `ready 0";;
                                                                    