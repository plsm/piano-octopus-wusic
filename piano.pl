:- use_module(library(sgml_write)).

writeXML(FileName, XML) :-
	MainAttributes = [
		xmlns = 'http://www.w3.org/2000/svg',
		version = '1.1'
	],
	MainElement =  element(svg, MainAttributes, XML),
	open(FileName, write, Stream, [create([read, write]), buffer(line)]),
	xml_write(Stream, MainElement, []),
	close(Stream)
	.

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

noteKey( 0, white(0)).
noteKey( 1, black(0)).
noteKey( 2, white(1)).
noteKey( 3, black(1)).
noteKey( 4, white(2)).
noteKey( 5, white(3)).
noteKey( 6, black(3)).
noteKey( 7, white(4)).
noteKey( 8, black(4)).
noteKey( 9, white(5)).
noteKey(10, black(5)).
noteKey(11, white(6)).

