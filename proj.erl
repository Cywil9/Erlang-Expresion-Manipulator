%% @author Jakub Cywinski
%% @doc @todo AI Assignment.
%% date: 26/2/15


-module(proj).


%-export([tryIt/0, parser/1, par/1, par/3, evaluator/2, compiler/3, simulator/2]).
-compile(export_all).

tryItPar() ->
	par("((2+2)+1*2)").
%parser("((2+2)+1*2)").

%parser part two
%Reverse Polish notation
tryItPar2() ->
	par([{bracket,left},
		 {bracket,left},
		 {num,2},
		 {binop,plus},
		 {num,2},
		 {bracket,right},
		 {binop,plus},
		 {num,1},
		 {binop,multiply},
		 {num,2},
		 {bracket,right}],[],[]).

%evaluator
tryItEvaluator() ->
	evaluator([{num,2},{num,2},{plus},{num,1},{num,2},{multiply},{plus}],
			  []).

%compiler
tryItCompiler() ->
	compiler([{bracket,left},
		 {bracket,left},
		 {num,2},
		 {binop,plus},
		 {num,2},
		 {bracket,right},
		 {binop,plus},
		 {num,1},
		 {binop,multiply},
		 {num,2},
		 {bracket,right}],[],[]).

tryItSimulator() ->
	simulator([{push,2},
			{push,2},
			{plus},
			{push,1},
			{push,2},
			{multiply},
			{plus}],
			[]).

parser(WffString)->
    TokenList=par(WffString), %[{num,2}, {op, plus}, {num,1}]
	S = par(TokenList, [], []), % 21+
	E = evaluator(S,[]),		%result 3
	Com = compiler(TokenList, [], []),
	Sim = simulator(Com, []).

%add unary minus ~
par([]) ->
	[];
par([$(|Tail]) ->
	[{bracket,left}|par(Tail)];
par([$)|Tail]) ->
    [{bracket,right}|par(Tail)];
par([$+|Tail]) ->
	[{binop,plus}|par(Tail)];
par([$-|Tail]) ->
	[{binop,minus}|par(Tail)];
par([$*|Tail]) ->
	[{binop,multiply}|par(Tail)];
par([$/|Tail]) ->
	[{binop,divide}|par(Tail)];
par([$~|Tail]) ->
	[{unop,minus}|par(Tail)];
par([X|Tail]) ->
	X2 = X - 48,
	[{num,X2}|par(Tail)].

%parser part two
%Reverse Polish notation
par([],[],Q) ->
	Q;
par([],[H|T],Q) ->
	NQ = Q ++ [H],
	par([],T,NQ);
par([{num,Value}| Tail],S,Q) ->
	NQ = Q ++ [{num,Value}],
	par(Tail,S,NQ);
par([{binop, Operator}| Tail],S,Q) ->
	NS = [{Operator}] ++ S,
	par(Tail,NS,Q);
%par([{binop,plus}| Tail],S,Q) ->
%	NS = [{plus}] ++ S,
%	par(Tail,NS,Q);
%par([{binop,minus}| Tail],S,Q) ->
%	NS = [{minus}] ++ S,
%	par(Tail,NS,Q);
%par([{binop,multiply}| Tail],S,Q) ->
%	NS = [{multiply}] ++ S,
%	par(Tail,NS,Q);
%par([{binop,divide}| Tail],S,Q) ->
%	NS = [{divide}] ++ S,
%	par(Tail,NS,Q);
par([{unop,minus}| Tail],S,Q) ->
	NS = [{minus}] ++ S,
	par(Tail,NS,Q);
par([{bracket,left}| Tail],S,Q) ->
	NS = [{bracket,left}] ++ S,
	par(Tail,NS,Q);
par([{bracket,right}| Tail],[H|T],Q) when H /= {bracket,left}  ->
	NQ = Q ++ [H],
	par([{bracket,right}|Tail],T,NQ);
par([{bracket,right}| Tail],[H|T],Q) when H =:= {bracket,left}->
	par(Tail,T,Q).

%evaluator
evaluator([], Q) ->
	Q;
evaluator([{num,Value}|Tail], Q) ->
	NQ = [Value] ++ Q,
	evaluator(Tail, NQ);
evaluator([{plus}|Tail], [Value1, Value2|T]) ->
	Result = Value1 + Value2,
	N = [Result] ++ T,
	evaluator(Tail, N);
evaluator([{minus}|Tail], [Value1, Value2|T]) ->
	Result = Value2 - Value1,
	N = [Result] ++ T,
	evaluator(Tail, N);
evaluator([{multiply}|Tail], [Value1, Value2|T]) ->
	Result = Value1 * Value2,
	N = [Result] ++ T,
	evaluator(Tail, N);
evaluator([{divide}|Tail], [Value1, Value2|T]) ->
	Result = Value2 / Value1,
	N = [Result] ++ T,
	evaluator(Tail, N).


 %%compiler stack
compiler([],[],Q) -> 
	Q; 
compiler([],[H|T],Q) -> 
	NQ = Q ++ [H], 
	compiler([],T,NQ);
compiler([{num,Value}|T],S,Q) -> 
	NQ = Q++[{push,Value}],
	compiler(T,S,NQ); 
compiler([{binop, Operator}|T],S,Q) -> 
	NS = [{Operator}] ++ S, 
	compiler(T,NS,Q); 
compiler([{unop, Operator}| T],S,Q) ->
	NS = [{Operator}] ++ S, 
	compiler(T,NS,Q); 
compiler([{bracket,left}|T],S,Q) -> 
	NS = [{push,left}] ++ S, 
	compiler(T,NS,Q); 
compiler([{bracket,right}|T],[StackHead|StackTail],Q) when StackHead /= {push,left} -> 
	NQ = Q ++ [StackHead], 
	compiler([{bracket,right}|T],StackTail,NQ); 
compiler([{bracket,right}|T],[StackHead|StackTail],Q) when StackHead =:= {push,left} -> 
	compiler(T,StackTail,Q).


simulator([], Stack) ->
	stack(pop, Stack);
simulator([{push, Value} | T], Stack) ->
	NS = stack({push, Value}, Stack),
	simulator(T, NS);
simulator([{Operator} | T], Stack) ->
%result of calculation and stack tail
	{Result, NS1} = stack(Operator, Stack),
%new stack = result on top, with stack tail^
	NS2 = stack({push, Result}, NS1),
	simulator(T, NS2).
%simulator([{plus} | T], Stack) ->
%	{Result, NS1} = stack(add, Stack),
%	NS2 = stack({push, Result}, NS1),
%	simulator(T, NS2);
%simulator([{minus} | T], Stack) ->
%	{Result, NS1} = stack(sub, Stack),
%	NS2 = stack({push, Result}, NS1),
%	simulator(T, NS2);
%simulator([{multiply} | T], Stack) ->
%	{Result, NS1} = stack(multiply, Stack),
%	NS2 = stack({push, Result}, NS1),
%	simulator(T, NS2);
%simulator([{divide} | T], Stack) ->
%	{Result, NS1} = stack(divide, Stack),
%	NS2 = stack({push, Result}, NS1),
%	simulator(T, NS2).

%stack
stack({push, Value}, StackList) ->
	NStackList = [Value] ++ StackList;
%add
stack(plus, [Value1,Value2 | StackTail]) ->
	Result = Value1 + Value2,
	%this worked fine for shorter calculations (2+1), but not for longer calcs ((2+1)*3) 
	%NStackList = [{push,Value1 + Value2}] ++ StackTail;
	%returns the result and stack tail separately
	{Result, StackTail};
%subtract
stack(minus, [Value1,Value2 | StackTail]) ->
	Result = Value2 - Value1,
	{Result, StackTail};
%multiply
stack(multiply, [Value1,Value2 | StackTail]) ->
	Result = Value1 * Value2,
	{Result, StackTail};
%divide
stack(divide, [Value1,Value2 | StackTail]) ->
	Result = Value2 / Value1,
	{Result, StackTail};
%pop
stack(pop, [Head|Tail]) ->
	Head;
%exit
stack(pop, [])->
	[].