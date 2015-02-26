%% @author Kuba
%% @doc @todo Add description to proj.


-module(proj).


-export([parser/1, eval/2]).


parser(WffString)->
    TokenList=par(WffString), %[{num,2}, {op, plus}, {num,1}]
	S = par(TokenList, [], []), % 21+
	E = eval(S,[]),		%result 3
%	lists:last(E),
	Com = rpn(TokenList, [], []),
	Sim = simulator(Com, []).

%%add unary minus ~
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
par([{num,H}|Tail],S,Q) ->
	NQ = Q ++ [{num,H}],
	par(Tail,S,NQ);
par([{binop,H}|Tail],S,Q) ->
	NS = [{H}] ++ S,
	par(Tail,NS,Q);
par([{bracket,left}|Tail],S,Q) ->
	NS = [{bracket,left}] ++ S,
	par(Tail,NS,Q);
par([{bracket,right}|Tail],[H|T],Q) when H /= {bracket,left}  ->
	NQ = Q ++ [H],
	par([{bracket,right}|Tail],T,NQ);
par([{bracket,right}|Tail],[H|T],Q) when H =:= {bracket,left}->
	par(Tail,T,Q).

%evaluator
eval([], Q) ->
	Q;
eval([{num,H}|Tail], Q) ->
	NQ = [H] ++ Q,
	eval(Tail, NQ);
eval([{plus}|Tail], [H1,H2|T]) ->
	H3 = H2 + H1,
	H4 = [H3] ++ T,
	eval(Tail, H4);
eval([{minus}|Tail], [H1,H2|T]) ->
	H3 = H2 - H1,
	H4 = [H3] ++ T,
	eval(Tail, H4);
eval([{multiply}|Tail], [H1,H2|T]) ->
	H3 = H1 * H2,
	H4 = [H3] ++ T,
	eval(Tail, H4);
eval([{divide}|Tail], [H1,H2|T]) ->
	H3 = H1 / H2,
	H4 = [H3] ++ T,
	eval(Tail, H4).


 %%compiler stack
rpn([],[],Q) -> 
	Q; 
rpn([],[H|T],Q) -> 
	Q1 = Q ++ [H], 
	rpn([],T,Q1);
rpn([{num,H}|T],S,Q) -> 
	rpn(T,S,Q++[{push,H}]); 
rpn([{binop,H}|T],S,Q) -> 
	S1 = [{H}] ++ S, 
	rpn(T,S1,Q); 
rpn([{bracket,left}|T],S,Q) -> 
	S2 = [{push,left}] ++ S, 
	rpn(T,S2,Q); 
rpn([{bracket,right}|T],[SH|ST],Q) when SH /= {push,left} -> 
	Q1 = Q ++ [SH], 
	rpn([{bracket,right}|T],ST,Q1); 
rpn([{bracket,right}|T],[SH|ST],Q) when SH =:= {push,left} -> 
	rpn(T,ST,Q).


simulator([], Stack) ->
	stack(pop, Stack);
simulator([{push, Value} | T], Stack) ->
	NStack = stack({push, Value}, Stack),
	simulator(T, NStack);
%simulator([{binop, Action} | T], Stack) ->
%	Result = stack(Action, Stack),
%	NStack = stack({push, Result}, Stack),
%	simulator(T, NStack).
simulator([{plus} | T], Stack) ->
	{Result, NStack1} = stack(add, Stack),
	NStack2 = stack({push, Result}, NStack1),
	simulator(T, NStack2);
simulator([{minus} | T], Stack) ->
	{Result, NStack1} = stack(sub, Stack),
	NStack2 = stack({push, Result}, NStack1),
	simulator(T, NStack2);
simulator([{multiply} | T], Stack) ->
	{Result, NStack1} = stack(multiply, Stack),
	NStack2 = stack({push, Result}, NStack1),
	simulator(T, NStack2);
simulator([{divide} | T], Stack) ->
	{Result, NStack1} = stack(divide, Stack),
	NStack2 = stack({push, Result}, NStack1),
	simulator(T, NStack2).

%stack
stack({push, Value}, StackList) ->
	NStackList = [Value] ++ StackList;
%add
stack(add, [Value1,Value2 | StackTail]) ->
	Result = Value1 + Value2,
	%NStackList = [{push,Value1 + Value2}] ++ StackTail;
	{Result, StackTail};
%subtract
stack(sub, [Value1,Value2 | StackTail]) ->
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
stack(pop, [])->
	[].


	
	
	