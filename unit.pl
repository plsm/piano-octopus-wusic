:- [piano].

testMajorScaleKeys(FileName) :-
	majorScaleKeysSVG(SVG, KeyNote),
	format(atom(FileName), 'major-scale_~a.svg', [KeyNote]),
	writeSVG(FileName, SVG)
	.

testMajorTriadKeys(FileName) :-
	majorTriadKeysSVG(SVG, RootNote),
	format(atom(FileName), 'major-triad_~a.svg', [RootNote]),
	writeSVG(FileName, SVG)
	.

testScaleNotes :-
	scaleNotes(Scale, ListNoteIDs),
	format('~32a~n', Scale),
	noteID2Names(ListNoteIDs, ListNoteNames),
	formatNoteNames(ListNoteNames),
	waitForNewLine,
	fail
	.

testGrandPiano :-
	grandPianoSVG(SVG),
	writeSVG('grand-piano.svg', SVG)
	.

formatNoteNames([]).
formatNoteNames([Notes | Rest]) :-
	(	%
		Notes = n(Name, Accidental, Octave)
	;
		Notes = [n(Name, Accidental, Octave), _]
	),
	(	%
		Accidental = no,
		CharAccidental = ' '
	;
		Accidental = yes(sharp),
		CharAccidental = '♯'
	;
		Accidental = yes(flat),
		CharAccidental = '♭'
	),
	format('~a~d~a ', [Name, Octave, CharAccidental]),
	formatNoteNames(Rest)
	.

waitForNewLine :-
	get_char(Byte),
	write(Byte),
	(	%
		Byte = '\n'
	->
		true
	;
		waitForNewLine
	).

%%% Local Variables:
%%% mode: prolog
%%% mode: flyspell-prog
%%% mode: auto-complete
%%% ispell-local-dictionary: "british"
%%% End:
