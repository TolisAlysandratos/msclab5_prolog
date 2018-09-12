%%%%% Natural Language Program

sentence(S) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP, VP, S).

noun_phrase(NP) :-
	article(A),
	noun(N),
	append(A, N, NP).

verb_phrase(V) :-
	verb(V).
verb_phrase(VP) :-
	verb(V),
	noun_phrase(NP),
	append(V, NP, VP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Question a)

% takes in text and checks if it's a conjunction
conj(Text) :- sentence(Text).
conj(Text) :-
	append(Text1, [and | Text2], Text),
	sentence(Text1),
	(sentence(Text2); conj(Text2)).
	
%%%% Question b)

% find the head of a list
first(First,[First|_]).

% takes the length of a word and returns 'l' (long) 
% if longer than 3 and 's' (short) otherwise
isLong(Length, RChar) :-
	Length > 3, RChar = l ;
	Length =< 3, RChar = s.

% takes a word and returns 'a' if animate and 'd' otherwise
isAnimate(Word, RChar) :- 
	animate(Animates), 
	(member(Word, Animates), RChar = a ; 
	\+member(Word, Animates), RChar = d).

% helper to 'encode' that encodes  a single word
encodeWord(In, Out) :-
	atom_chars(In, InL),
	length(InL, Len),
	isLong(Len,LChar),
	isAnimate(In, AChar),
	first(First, InL),
	append([AChar, LChar], [First], ResL),
	atom_chars(Out, ResL).

% takes in text and replaces nouns with encoded version
encode([],[]).
encode([Word|T], Res) :-
	encode(T, NewT),
	(noun([Word]),encodeWord(Word, EWord) ; 
	\+noun([Word]), EWord = Word),
	append([EWord], NewT, Res).
	
%%%% Question c)

% takes a list and checks if all elements are the same
areSame([H|[]]).
areSame([H|T]) :-
	member(H,T),
	areSame(T).

% takes a conjunction and checks if actors are the same
same_actor(Text) :-
	storeActors(Text, [], Actors),
	areSame(Actors).

% takes a conjunction and isolates the first sentence
getSent([], [], []).
getSent(Conj, [], Conj) :- sentence(Conj).
getSent([and|_], Acc, Acc) :- sentence(Acc).
getSent([H|T], Acc, Res) :-
	append(Acc, [H], NewL),
	getSent(T, NewL, Res).

% takes a sentence and get the first noun
getNoun([], [], []).
getNoun([Noun, Verb|_], Res, Noun) :- noun([Noun]), verb([Verb]).
getNoun([H|T], Acc, Res) :-
	append(Acc, [H], NewL),
	getNoun(T, NewL, Res).

% takes a conjunction and a sentence 
% and removes first occurence of that sentence
removeSent(Conj, Sent, []) :- sentence(Conj).
removeSent([and|Ts], Sent, Ts).
removeSent([C|Tc], [S|Ts], Res) :-
	removeSent(Tc, Ts, Res).

% takes a conjunction of sentences and stores the actors
storeActors([],Res,Res).	
storeActors(Text, L, Res) :-
	getSent(Text, [], Sent),
	getNoun(Sent, [], Noun),
	append([Noun], L, NewL),
	removeSent(Text, Sent, NewT),	
	storeActors(NewT, NewL, Res).
