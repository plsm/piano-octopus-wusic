:- use_module(library(sgml_write)).

writeXML(FileName, XML) :-
	MainAttributes = [
		xmlns = 'http://www.w3.org/2000/svg',
		version = '1.1'
	],
	computeRectangularRegion(XML, no, no, MX, MY),
	getValue(MX, X),
	getValue(MY, Y),
	attributeTranslate(X, Y, Translate),
	GElement = element(g, [transform = Translate], XML),
	MainElement =  element(svg, MainAttributes, [GElement]),
	open(FileName, write, Stream, [create([read, write]), buffer(line)]),
	xml_write(Stream, MainElement, []),
	close(Stream)
	.

computeRectangularRegion(XML, BX, BY, RX, RY) :-
	XML = [],
	RX = BX,
	RY = BY
	;
	XML = [Element | Rest], !,
	(	%
		Element = element(g, Attributes, Contents),
		(	%
			Attributes = [translate(X, Y) | _], !,
			improve(BX, X, RfX),
			improve(BY, Y, RfY)
		;
			BX = RfX,
			BY = RfY
		),
		computeRectangularRegion(Contents, RfX, RfY, ReX, ReY)
	;
		Element = element(rect, [x = X, y = Y | _], _),
		improve(BX, X, ReX),
		improve(BY, Y, ReY)
	;
		Element = element(circle, [cx = X_, cy = Y_, r = R | _], _),
		X is X_ - R,
		Y is Y_ - R,
		improve(BX, X, ReX),
		improve(BY, Y, ReY)
	;
		Element = element(text, _, _),
		ReX = BX,
		ReY = BY
	),
	computeRectangularRegion(Rest, ReX, ReY, RX, RY)
	.

improve(no, V, yes(V)).
improve(yes(OldValue), CandidateValue, yes(NewValue)) :-
	OldValue < CandidateValue
	->
	NewValue = OldValue
	;
	NewValue = CandidateValue
	.

getValue(no, 0).
getValue(yes(Value), Result) :- Result is -Value.

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

majorScaleKeysXML(Result, KeyNote) :-
	scaleNotes(majorScale, ScaleNotes),
	ScaleNotes = [KeyNote | _],
	pianoNote(IDFrom, f, no, 2),
	pianoNote(IDTo, f, no, 4),
	keysBetweenXML(IDFrom, IDTo, [], [], BackgroundKeys),
	findall(
		XML,
		(	%
			member(ID, ScaleNotes),
			pianoKeyMarkedXML(ID, _Colour, _Number, XML)
		),
		MarkedKeys
	),
	append(BackgroundKeys, [element(g, [stroke = orange, fill = yellow, 'stroke-width' = 2], MarkedKeys)], Result)
	.

keysBetweenXML(IDFrom, IDTo, WhiteKeysXML, BlackKeysXML, Result) :-
	IDFrom > IDTo,
	Result = [
		element(g, [stroke = black, fill = white], WhiteKeysXML),
		element(g, [stroke = black, fill = black], BlackKeysXML)
	]
	;
	IDFrom =< IDTo,
	pianoKeyXML(IDFrom, Colour, _, KeyXML),
	(	%
		Colour = white,
		RecWKX = [KeyXML | WhiteKeysXML],
		RecBKX = BlackKeysXML
	;
		Colour = black,
		RecWKX = WhiteKeysXML,
		RecBKX = [KeyXML | BlackKeysXML]
	),
	NextFrom is IDFrom + 1,
	keysBetweenXML(NextFrom, IDTo, RecWKX, RecBKX, Result)
	.

whiteKeyXML(Number, Result) :-
	keyWidth(white, KeyWidth),
	X is Number * KeyWidth,
	Result = element(rect, [x = X, y = 0, width = KeyWidth, height = 150], [])
	.

blackKeyXML(Number, Result) :-
	keyWidth(white, WhiteKeyWidth),
	keyWidth(black, BlackKeyWidth),
	I is Number rem 7,
	deltaBlackKey(I, Delta),
	X is Delta + Number * WhiteKeyWidth,
	Result = element(rect, [x = X, y = 0, width = BlackKeyWidth, height = 90], [])
	.

pianoKeyXML(ID, Colour, Number, Result) :-
	pianoKey(ID, Colour, Number),
	keyWidth(white, WhiteKeyWidth),
	keyWidth(Colour, KeyWidth),
	keyHeight(Colour, KeyHeight),
	(	%
		Colour = white, !,
		Delta is 0
	;
		Colour = black, !,
		I is abs(Number rem 7),
		deltaBlackKey(I, Delta)
	),
	X is Delta + Number * WhiteKeyWidth,
	Result = element(rect, [x = X, y = 0, width = KeyWidth, height = KeyHeight], [])
	.

whiteKeyNameXML(Number, Result) :-
	I is Number rem 7,
	noteKey(ID, white(I)),
	noteName(ID, Name, no),
	keyWidth(white, KeyWidth),
	keyHeight(white, KeyHeight),
	X is (Number + 0.5) * KeyWidth,
	Y is KeyHeight + 15,
	Text = element(text, [], [Name]),
	attributeTranslate(X, Y, Translate),
	Result = element(g, [transform = Translate], [Text])
	.

pianoKeyMarkedXML(ID, Colour, Number, Result) :-
	pianoKey(ID, Colour, Number),
	keyWidth(white, WhiteKeyWidth),
	keyWidth(black, BlackKeyWidth),
	keyWidth(Colour, KeyWidth),
	keyHeight(Colour, KeyHeight),
	(	%
		Colour = white, !,
		Delta = 0
	;
		Colour = black, !,
		I is abs(Number rem 7),
		deltaBlackKey(I, Delta)
	),
	Radius is ceiling(BlackKeyWidth * 0.4),
	X is Delta + Number * WhiteKeyWidth + KeyWidth / 2,
	Y is KeyHeight - KeyWidth / 2,
	Result = element(circle, [cx = X, cy = Y, r = Radius], [])
	.

attributeTranslate(X, Y, Result) :-
	atom_chars('translate(', CL1),
	atom_chars(X, CL2),
	atom_chars(', ', CL3),
	atom_chars(Y, CL4),
	atom_chars(')', CL5),
	foldl(append, [CL5, CL4, CL3, CL2, CL1], [], CLR),
	atom_chars(Result, CLR)
	.

keyWidth(white, 22).
keyWidth(black, 14).

keyHeight(white, 150).
keyHeight(black, 90).

deltaBlackKey(0, 12).
deltaBlackKey(1, 18).
deltaBlackKey(3, 11).
deltaBlackKey(4, 15).
deltaBlackKey(5, 18).

noteName( 0, c, no).
noteName( 1, c, yes(sharp)).
noteName( 1, d, yes(flat)).
noteName( 2, d, no).
noteName( 3, d, yes(sharp)).
noteName( 3, e, yes(flat)).
noteName( 4, e, no).
noteName( 5, f, no).
noteName( 6, f, yes(sharp)).
noteName( 6, g, yes(flat)).
noteName( 7, g, no).
noteName( 8, g, yes(sharp)).
noteName( 8, a, yes(flat)).
noteName( 9, a, no).
noteName(10, a, yes(sharp)).
noteName(10, b, yes(flat)).
noteName(11, b, no).

noteKey( 0, white, 0).
noteKey( 1, black, 0).
noteKey( 2, white, 1).
noteKey( 3, black, 1).
noteKey( 4, white, 2).
noteKey( 5, white, 3).
noteKey( 6, black, 3).
noteKey( 7, white, 4).
noteKey( 8, black, 4).
noteKey( 9, white, 5).
noteKey(10, black, 5).
noteKey(11, white, 6).

pianoNote(ID, Name, Accidental, Octave) :-
	between(-3, 84, ID),
	Octave is div(ID, 12),
	NM is mod(ID, 12),
	noteName(NM, Name, Accidental)
	.

pianoKey(ID, Colour, Number) :-
	between(-3, 84, ID),
	Octave is div(ID, 12),
	NM is mod(ID, 12),
	noteKey(NM, Colour, C),
	Number is C + (Octave + 1) * 7
	.

scaleDeltaNotes(majorScale, [2, 2, 1, 2, 2, 2, 1]).

scaleNotes(Scale, ListNotes) :-
	scaleDeltaNotes(Scale, DeltaNotes),
	between(30, 41, ID),
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
