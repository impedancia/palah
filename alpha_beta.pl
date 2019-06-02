    evaluate_and_choose([Move|Moves],Position,D,Alpha,Beta,Move1,BestMove) :-
		format('eval_choose1\r\n',[]),
		move(Move,Position,Position1),
		alpha_beta(D,Position1,Alpha,Beta,MoveX,Value),
		format('Value: ~d\r\n',[Value]),
        Value1 is -Value,   
        cutoff(Move,Value1,D,Alpha,Beta,Moves,Position,Move1,BestMove).
    evaluate_and_choose([],Position,D,Alpha,Beta,Move,(Move,A)) :-
        format('eval_choose2\r\n',[]).

    alpha_beta(0,Position,Alpha,Beta,Move,Value) :- 
		format('alpha_beta1\r\n',[]),
		value(Position,Value).
    alpha_beta(D,Position,Alpha,Beta,Move,Value) :- 
		format('alpha_beta2\r\n',[]),
		findall(M,move(Position,M),Moves),
        Alpha1 is -Beta, 
		Beta1 is -Alpha, 
        D1 is D-1,
%		 format(D1,[]),
		evaluate_and_choose(Moves,Position,D1,Alpha1,Beta1,nil,(Move,Value)).

    cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,(Move,Value)) :- 
		format('cutoff1\r\n',[]),
		Value >= Beta.
    cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-
		format('cutoff2\r\n',[]),
        Alpha < Value, Value < Beta, 
		evaluate_and_choose(Moves,Position,D,Value,Beta,Move,BestMove).
    cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-
		format('cutoff3\r\n',[]),
        Value =< Alpha, 
		evaluate_and_choose(Moves,Position,D,Alpha,Beta,Move1,BestMove).
