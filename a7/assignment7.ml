(*
a. For your implementation, the producer should meet the following requirements:

`Create(producer_behavior, n) should return a producer actor with an initial stock of n 
products. Furthermore, it should respond to these messages:
`produce(_) - add a product to its internal stock.
`consume(a) - if the stock is not empty, send a <- `delivered(0) and remove one item from 
the internal stock; if empty, send a <- `wait(0) and `me <- `produce(0). This is a somewhat 
artificial model of production, the producer can make more stock by simply sending produce 
messages to itself. The consumer will also need to re-request to get an item delivered.
*)

    let producer_behavior = 
      "Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
        Fun me -> 
            y (Fun this -> 
                Fun state -> 
                  Fun msg -> 
                    Match msg With
                      | `produce(_) -> (Print \"produced 1 product.\");
                                     (this (state + 1))
                      | `consume(a) -> If state = 0 
                                        Then (
                                          (Print \"no product available for consumption, producing 1.\"); 
                                          (a <- `wait(00)); 
                                          (me <- `produce(00)); 
                                          (this state)
                                        )
                                        Else (
                                          (Print \"consumed 1 product.\");
                                          (a <- `delivered(00)); 
                                          (this (state-1))
                                        )
              )";;
(*
Here is a simple tester for the producer. The producer will start with one item, and a consumer 
will get an item the first time and the second time will get a wait and will need to re-request.
*)

    let producer_tester = "
    Let producer_behavior = ("^producer_behavior^") In
    Let producer = Create(producer_behavior, 1) In
    Let consumer_behavior = Fun me -> Fun _ -> 
      (Fun _ ->  
        producer <- `consume(me);
        (Fun msg -> 
            Match msg With 
            `delivered(_) -> 
                producer <- `consume(me);
                    (Fun msg -> 
                        Match msg With 
                        `wait(_) -> producer <- `consume(me);
                            (Fun msg -> 
                                Match msg With 
                                `delivered(_) -> Print \"It worked!\"))))
    In
    Let consumer = Create(consumer_behavior, 0) In
    consumer <- 0
    ";;

    let recur_producer_tester = "
    Let producer_behavior = ("^producer_behavior^") In
    Let producer = Create(producer_behavior, 0) In
    Let recur_consumer_behavior = 
    Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
      Fun me -> 
        y (Fun this -> 
            Fun init_call_nums -> 
              Fun msg ->  
                Match msg With
                  | `delivered(_) -> (
                    (producer <- `consume(me));
                    (this init_call_nums)
                    )
                  | `wait(_) -> If init_call_nums = 0 Then 
                  (
                  Print \"Complete all recursion\n\"; 
                  (this init_call_nums)
                  )
                  Else (
                    (Print \"Another recursion, current remaining: \"; 
                    (Print init_call_nums); 
                    Print \"\n\");
                    (producer <- `produce(00));
                    (producer <- `produce(00));
                    (producer <- `consume(me));
                    (me <- `delivered(00));
                    (this (init_call_nums - 1))
                  )
            )
    In
    Let consumer = Create(recur_consumer_behavior, 3) In
    consumer <- `delivered(00)
    ";;
(*
This is in fact not a very good test, the producer is both getting its own 
`produce message as well as the `consume message and since arrival order is 
nondeterministic by default the test will only work half the time. Our suggestion 
in debugging is to use the deterministic_delivery := true;; option so the former 
earlier message will beat the latter.

b. It is not too hard to fix this test even for nondeterministic execution: 
eventually the producer will get the `produce message since actors are fair, 
and so after some number of `wait messages the `consume will succeed. Modify 
the above code so it even works in nondeterministic mode. Note that solving 
question c. below could help you figure out how to solve this question.
*)

    let fixed_producer_tester = "
    Let producer_behavior = ("^producer_behavior^") In
    Let producer = Create(producer_behavior, 1) In
    Let consumer_behavior = Fun me -> Fun _ -> 
      (Fun _ ->  
        producer <- `consume(me);
        (Fun msg -> 
            Match msg With 
            `delivered(_) -> 
                producer <- `consume(me);
                    (Fun msg -> 
                        Match msg With 
                        `wait(_) -> producer <- `consume(me);
                        Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
                          y (Fun this -> Fun msg -> 
                            (
                                (Match msg With 
                                | `wait(_) -> producer <- `consume(me)
                                | `delivered(_) -> Print \"It worked!\");
                                this
                            )
                          )
                    )
        )
      )
    In
    Let consumer = Create(consumer_behavior, 0) In
    consumer <- 0
    ";;

(*
c. We can use the theory to help us understand why the consumer tester here 
needs to repeat its requests. For this question, give a sequence of global actor 
states G0 -> G1 -> G2 -> … of a computation of the the producer and your fixed 
tester which shows how an arbitrary number of `wait messages could be sent back 
to the consumer before its `consume message is finally processed. Put some “…” 
in your sequence for the part that could repeat.
*)

  (* a7c. ... YOUR ANSWER HERE ... *)


(*
Note that you can give your answer in the format show_states := true produces. 
Please don’t include any of the actor code in your answers, “…” will do in place 
of any code.

d. Now lets make a protocol for the consumer as well. The consumer should meet 
the following requirements:

`Create(consumer_behavior, (producer, n)) should return a consumer actor with a 
demand for n products from the producer actor. You can assume consumer actor will 
always start with demand greater than 0. Furthermore, it should respond to these
messages:
`purchase(_) - send `consume(me) to the producer.
`wait(_) - send another `consume(me) request to the producer.
`delivered(_) - check whether the demand is met; if so, finish the execution. 
If not, send me <- `purchase(_), and update the demand.
*)
    let consumer_behavior = 
    "Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
      Fun me -> 
        y (Fun this -> 
            Fun state -> 
              Fun msg ->  
                Match msg With
                  | `purchase(_) -> ((Fst(state) <- `consume(me)); (this state))
                  | `wait(_) -> ((Fst(state) <- `consume(me)); (this state))
                  | `delivered(_) -> If Snd(state) = 0 
                                     Then (
                                            (Print \"Actor\"; (Print me); Print \"all delivered!\n\");
                                            Fun _ -> Fun _ -> 0
                                          )
                                      Else (
                                              (me <- `purchase(_));
                                              (this (Fst(state), (Snd(state)-1)))
                                            )
          )";;
  
(*
Here is a sample runner which will simulate the transactions between a producer 
with initial stock of zero products, and two consumers with demand of one and two 
products respectively. Since the demands exceed the supply in this example, the two 
consumers will have to wait at least one and two times each before getting their 
products delivered. You can insert Print in your handling code for wait in the consumer 
actor to check for this behavior.
 *)

    let pc_tester = "
    Let producer_behavior = ("^producer_behavior^") In
    Let consumer_behavior = ("^consumer_behavior^") In
    Let producer = Create(producer_behavior, 0) In
    Let user = Create(consumer_behavior, (producer, 1)) In
    Let user2 = Create(consumer_behavior, (producer, 2)) In
    user <- `purchase(00);
    user2 <- `purchase(00) 
    ";;
  