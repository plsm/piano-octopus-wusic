
writeSVG(FileName, SVG) :-
	MainAttributes = [
		xmlns = 'http://www.w3.org/2000/svg',
		version = '1.1'
	],
	computeRectangularRegion(SVG, no, no, MX, MY),
	getValue(MX, X),
	getValue(MY, Y),
	attributeTranslate(X, Y, Translate),
	GElement = element(g, [transform = Translate], SVG),
	MainElement =  element(svg, MainAttributes, [GElement]),
	open(FileName, write, Stream, [create([read, write]), buffer(line)]),
	xml_write(Stream, MainElement, []),
	close(Stream)
	.

computeRectangularRegion(SVG, BX, BY, RX, RY) :-
	SVG = [],
	RX = BX,
	RY = BY
	;
	SVG = [Element | Rest], !,
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

majorScaleKeysSVG(Result, KeyNote) :-
	scaleNotes(majorScale, ScaleNotes),
	ScaleNotes = [KeyNote | _],
	pianoNote(IDFrom, f, no, 2),
	pianoNote(IDTo, f, no, 4),
	pianoRepresentation(IDFrom, IDTo, ScaleNotes, Result)
	.

majorTriadKeysSVG(Result, RootNote) :-
	chordNotes(majorTriad, ChordNotes),
	ChordNotes = [RootNote | _],
	pianoNote(IDFrom, c, no, 3),
	pianoNote(IDTo, g, no, 4),
	pianoRepresentation(IDFrom, IDTo, ChordNotes, Result)
	.

pianoRepresentation(IDFrom, IDTo, IDMarkedKeys, Result) :-
	keysBetweenSVG(IDFrom, IDTo, [], [], BackgroundKeysSVG),
	findall(
		SVG,
		(	%
			member(ID, IDMarkedKeys),
			pianoKeyMarkedSVG(ID, SVG)
		),
		MarkedKeysSVG
	),
	append(BackgroundKeysSVG, [element(g, [stroke = orange, fill = yellow, 'stroke-width' = 2], MarkedKeysSVG)], Result)
	.

%! keysBetweenSVG(+IDFrom:int, +IDTo:int, +WhiteKeysSVG:list, +BlackKeysSVG:list, -Result:svg) is semidet.
%
% Computes the SVG representation of the piano keys between the
% identifications IDFrom and IDTo.
%
% @arg IDFrom identification of the piano key with the lowest pitch.
% @arg IDTo identification of the piano key with highest pitch.
% @arg WhiteKeysSVG an accumulator with the white keys that have been
% computed so far.
% @arg BlackKeysSVG an accumulator with the black keys that have been
% computed so far.
keysBetweenSVG(IDFrom, IDTo, WhiteKeysSVG, BlackKeysSVG, Result) :-
	IDFrom > IDTo,
	Result = [
		element(g, [stroke = black, fill = white], WhiteKeysSVG),
		element(g, [stroke = black, fill = black], BlackKeysSVG)
	]
	;
	IDFrom =< IDTo,
	pianoKeySVG(IDFrom, Colour, KeySVG),
	(	%
		Colour = white,
		RecWKX = [KeySVG | WhiteKeysSVG],
		RecBKX = BlackKeysSVG
	;
		Colour = black,
		RecWKX = WhiteKeysSVG,
		RecBKX = [KeySVG | BlackKeysSVG]
	),
	NextFrom is IDFrom + 1,
	keysBetweenSVG(NextFrom, IDTo, RecWKX, RecBKX, Result)
	.

%! pianoKeySVG(+ID:int, +Colour:atom, -Result:svg) is semidet
%! pianoKeySVG(-ID:int, -Colour:atom, -Result:svg) is multi
%
% Computes the SVG representation of a piano key.  Only the outline is
% computed.  Filling is done by predicate keysBetweenSVG/5.
%
% @arg ID the identification of the piano key, a number between -3 and 84.
% @arg Colour either the atom white or black
% @arg Result the SVG representation of the outline of the piano key.
pianoKeySVG(ID, Colour, Result) :-
	pianoKeyXPosition(ID, Colour, X),
	% keyWidth(white, WhiteKeyWidth),
	keyWidth(Colour, KeyWidth),
	keyHeight(Colour, KeyHeight),
	% (	%
	% 	Colour = white, !,
	% 	Delta is 0
	% ;
	% 	Colour = black, !,
	% 	I is abs(Number rem 7),
	% 	deltaBlackKey(I, Delta)
	% ),
	% X is Delta + Number * WhiteKeyWidth,
	Result = element(rect, [x = X, y = 0, width = KeyWidth, height = KeyHeight], [])
	.

%! pianoKeyMarkedSVG(+ID:int, -Result:svg) is semidet
%! pianoKeyMarkedSVG(-ID:int, -Result:svg) is multi
%
% Computes a SVG representation of a mark in the given key.
%
% @arg ID the identification of the piano key where a mark is to be put.
% @arg Result the SVG representation of the mark in the piano key.
pianoKeyMarkedSVG(ID, Result) :-
	pianoKeyXPosition(ID, Colour, KeyXPosition),
	keyWidth(black, BlackKeyWidth),
	keyWidth(Colour, KeyWidth),
	keyHeight(Colour, KeyHeight),
	Radius is ceiling(BlackKeyWidth * 0.4),
	X is KeyXPosition + KeyWidth / 2,
	Y is KeyHeight - KeyWidth / 2,
	Result = element(circle, [cx = X, cy = Y, r = Radius], [])
	.

pianoKeyNameSVG(ID, Result) :-
	pianoKeyXPosition(ID, Colour, KeyXPosition),
	keyWidth(Colour, KeyWidth),
	keyHeight(white, WhiteKeyHeight),
	noteName(ID, Name, no),
	X is KeyXPosition + KeyWidth / 2,
	Y is WhiteKeyHeight + 15,
	Text = element(text, [], [Name]),
	attributeTranslate(X, Y, Translate),
	Result = element(g, [transform = Translate], [Text])
	.

pianoKeyXPosition(ID, Colour, Result) :-
	between(-3, 84, ID),
	Octave is div(ID, 12),
	NM is mod(ID, 12),
	noteKey(NM, Colour, C),
	(	%
		Colour = white, !,
		Delta = 0
	;
		Colour = black, !,
		deltaBlackKey(C, Delta)
	),
	keyWidth(white, WhiteKeyWidth),
	Result is Delta + (C + Octave * 7) * WhiteKeyWidth
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

%%% Local Variables:
%%% mode: prolog
%%% mode: flyspell-prog
%%% mode: auto-complete
%%% ispell-local-dictionary: "british"
%%% End:
