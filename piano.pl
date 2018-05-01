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

%%! scaleDeltaNotes(?Name, ?ListDeltaTones) is multi
%%
%% This predicate contains the representation of a scale as a list of the
%% tone difference between consecutive notes.
scaleDeltaNotes(major, [2, 2, 1, 2, 2, 2, 1]).
scaleDeltaNotes(dorian, [2, 1, 2, 2, 2, 1, 2]).
scaleDeltaNotes(mixolydian, [2, 2, 1, 2, 2, 1, 2]).
scaleDeltaNotes('major-pentatonic', [2, 2, 3, 2, 3]).
scaleDeltaNotes('minor-pentatonic', [3, 2, 2, 3, 2]).

%%! scaleNotes(?Scale:term, ?KeyNote, ?SetNotes:set) is multi
%%
%% Computes the notes that are part of a scale in a way that allows
%% comparison of notes of different scales.
%%
%% The returned list of notes is not the sequence that the scale is played.
%% It is a list of note IDs starting at 12.
%%
%% We assume that the distance between the lowest and highest pitch notes
%% is equal or less than an octave.
scaleNotes(Scale, KeyNote, SetNotes) :-
	scaleSequence(Scale, SequenceNotes),
	append([KeyNote], _, SequenceNotes),
	append(Notes, [_], SequenceNotes),
	maplist(computeScaleNote, Notes, ListNotes),
	list_to_ord_set(ListNotes, SetNotes)
	.

%%! scaleSequence(?Scale:term, ?ListNotes:set) is multi
%%
%% Computes the notes that are part of a scale in a way that allows
%% comparison of notes of different scales.
%%
%% The returned list of notes is not the sequence that the scale is played.
%% It is a list of note IDs starting at 12.
%%
%% We assume that the distance between the lowest and highest pitch notes
%% is equal or less than an octave.
scaleSequence(Scale, ListNotes) :-
	scaleDeltaNotes(Scale, DeltaNotes),
	between(42, 53, KeyNote),
	scanl(computeScaleSequence, DeltaNotes, KeyNote, ListNotes)
	.

chordDeltaNotes(majorTriad, [4, 3]).

chordNotes(ChordName, ListNotes) :-
	chordDeltaNotes(ChordName, DeltaNotes),
	between(36, 47, ID),
	scanl(computeScaleNote, DeltaNotes, ID, ListNotes)
	.

computeScaleSequence(DeltaNote, PreviousNote, ResultNote) :-
	ResultNote is PreviousNote + DeltaNote
	.

computeScaleNote(InNote, OutNote) :-
	OutNote is (InNote mod 12) + 12
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
