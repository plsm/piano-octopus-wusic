:- use_module(library(sgml_write)).

:- ['piano.svg.pl'].
:- ['piano.note.pl'].

%% octaveKeysXML(+Result)
%%
%% Unify Result with a XML representation of the piano keys that comprise a full octave.
%%
octaveKeysXML(Result) :-
	findall(
		XML,
		(  
			between(0, 7, Number),
			whiteKeyXML(Number, XML)
		),
		WhiteKeys
	),
	findall(
		XML,
		(
			between(0, 6, Number),
			blackKeyXML(Number, XML)
		),
		BlackKeys
	),
	Result = [
		element(g, [stroke = black, fill = white], WhiteKeys),
		element(g, [stroke = black, fill = black], BlackKeys)
	]
	.

namedOctaveKeysXML(Result) :-
	octaveKeysXML(Unnamed),
	findall(
		XML,
		(
			between(0, 7, Number),
			whiteKeyNameXML(Number, XML)
		),
		WhiteKeyNames
	),
	append([element(g, [stroke = black, 'text-anchor' = middle, 'font-family'='Arial'], WhiteKeyNames)], Unnamed, Result)
	.

pianoNote(ID, Name, Accidental, Octave) :-
	between(9, 96, ID),
	Octave is div(ID, 12),
	NM is mod(ID, 12),
	noteName(NM, Name, Accidental)
	.

scaleDeltaNotes(majorScale, [2, 2, 1, 2, 2, 2, 1]).

scaleNotes(Scale, ListNotes) :-
	scaleDeltaNotes(Scale, DeltaNotes),
	between(42, 53, ID),
	scanl(computeScaleNote, DeltaNotes, ID, ListNotes)
	.

chordDeltaNotes(majorTriad, [4, 3]).

chordNotes(ChordName, ListNotes) :-
	chordDeltaNotes(ChordName, DeltaNotes),
	between(36, 47, ID),
	scanl(computeScaleNote, DeltaNotes, ID, ListNotes)
	.

computeScaleNote(DeltaNote, PreviousNote, ResultNote) :-
	ResultNote is PreviousNote + DeltaNote
	.

noteID2Names(ListNoteIDs, ListNoteNames) :-
	ListNoteIDs = [],
	ListNoteNames = []
	;
	ListNoteIDs = [ID | RestNoteIDs],
	ListNoteNames = [Result | RestNoteNames],
	findall(
		n(N, A, O),
		pianoNote(ID, N, A, O),
		Candidate
	),
	(
		Candidate = [C],
		Result = C
	;
		Candidate = [_, _],
		Result = Candidate
	),
	noteID2Names(RestNoteIDs, RestNoteNames)
	.

%%% Local Variables:
%%% mode: prolog
%%% mode: flyspell-prog
%%% mode: auto-complete
%%% ispell-local-dictionary: "british"
%%% End:
