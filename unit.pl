:- [piano].

testKeysPiano :-
	keysBetweenXML(-3, 84, [], [], XML),
	writeXML('piano.svg', XML)
	.

testMajorScaleKeys(FileName) :-
	majorScaleKeysXML(XML, KeyNote),
	format(atom(FileName), 'major-scale_~a.svg', [KeyNote]),
	writeXML(FileName, XML)
	.

testScaleNotes :-
	scaleNotes(Scale, ListNoteIDs),
	format('~32a~n', Scale),
	noteID2Names(ListNoteIDs, ListNoteNames),
	formatNoteNames(ListNoteNames),
	waitForNewLine,
	fail
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
