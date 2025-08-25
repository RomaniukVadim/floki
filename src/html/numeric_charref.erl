-module(numeric_charref).

-export([to_unicode_number/1]).

% REPLACEMENT CHARACTER
to_unicode_number(16#00) -> {ok, {table, 16#FFFD}};
% EURO SIGN (€)
to_unicode_number(16#80) -> {ok, {table, 16#20AC}};
% SINGLE LOW-9 QUOTATION MARK (‚)
to_unicode_number(16#82) -> {ok, {table, 16#201A}};
% LATIN SMALL LETTER F WITH HOOK (ƒ)
to_unicode_number(16#83) -> {ok, {table, 16#0192}};
% DOUBLE LOW-9 QUOTATION MARK („)
to_unicode_number(16#84) -> {ok, {table, 16#201E}};
% HORIZONTAL ELLIPSIS (…)
to_unicode_number(16#85) -> {ok, {table, 16#2026}};
% DAGGER (†)
to_unicode_number(16#86) -> {ok, {table, 16#2020}};
% DOUBLE DAGGER (‡)
to_unicode_number(16#87) -> {ok, {table, 16#2021}};
% MODIFIER LETTER CIRCUMFLEX ACCENT (ˆ)
to_unicode_number(16#88) -> {ok, {table, 16#02C6}};
% PER MILLE SIGN (‰)
to_unicode_number(16#89) -> {ok, {table, 16#2030}};
% LATIN CAPITAL LETTER S WITH CARON (Š)
to_unicode_number(16#8A) -> {ok, {table, 16#0160}};
% SINGLE LEFT-POINTING ANGLE QUOTATION MARK (‹)
to_unicode_number(16#8B) -> {ok, {table, 16#2039}};
% LATIN CAPITAL LIGATURE OE (Œ)
to_unicode_number(16#8C) -> {ok, {table, 16#0152}};
% LATIN CAPITAL LETTER Z WITH CARON (Ž)
to_unicode_number(16#8E) -> {ok, {table, 16#017D}};
% LEFT SINGLE QUOTATION MARK (‘)
to_unicode_number(16#91) -> {ok, {table, 16#2018}};
% RIGHT SINGLE QUOTATION MARK (’)
to_unicode_number(16#92) -> {ok, {table, 16#2019}};
% LEFT DOUBLE QUOTATION MARK (“)
to_unicode_number(16#93) -> {ok, {table, 16#201C}};
% RIGHT DOUBLE QUOTATION MARK (”)
to_unicode_number(16#94) -> {ok, {table, 16#201D}};
% BULLET (•)
to_unicode_number(16#95) -> {ok, {table, 16#2022}};
% EN DASH (–)
to_unicode_number(16#96) -> {ok, {table, 16#2013}};
% EM DASH (—)
to_unicode_number(16#97) -> {ok, {table, 16#2014}};
% SMALL TILDE (˜)
to_unicode_number(16#98) -> {ok, {table, 16#02DC}};
% TRADE MARK SIGN (™)
to_unicode_number(16#99) -> {ok, {table, 16#2122}};
% LATIN SMALL LETTER S WITH CARON (š)
to_unicode_number(16#9A) -> {ok, {table, 16#0161}};
% SINGLE RIGHT-POINTING ANGLE QUOTATION MARK (›)
to_unicode_number(16#9B) -> {ok, {table, 16#203A}};
% LATIN SMALL LIGATURE OE (œ)
to_unicode_number(16#9C) -> {ok, {table, 16#0153}};
% LATIN SMALL LETTER Z WITH CARON (ž)
to_unicode_number(16#9E) -> {ok, {table, 16#017E}};
% LATIN CAPITAL LETTER Y WITH DIAERESIS (Ÿ)
to_unicode_number(16#9F) -> {ok, {table, 16#0178}};

%% This clause handles surrogate pairs and values outside the Unicode range.
%% These are mapped to the replacement character (U+FFFD).
to_unicode_number(Number)
    when (Number >= 16#D800 andalso Number =< 16#DFFF) orelse
         (Number > 16#10FFFF) ->
    {ok, {range_one, 16#FFFD}};

%% This clause handles various disallowed code points, including control
%% characters and "non-characters" as defined by the Unicode standard.
to_unicode_number(Number)
    when (Number >= 16#0001 andalso Number =< 16#0008) orelse
         (Number >= 16#000E andalso Number =< 16#001F) orelse
         (Number >= 16#007F andalso Number =< 16#009F) orelse
         (Number >= 16#FDD0 andalso Number =< 16#FDEF) orelse
         (Number =:= 16#000B)   orelse (Number =:= 16#FFFE)   orelse (Number =:= 16#FFFF)   orelse
         (Number =:= 16#1FFFE)  orelse (Number =:= 16#1FFFF)  orelse
         (Number =:= 16#2FFFE)  orelse (Number =:= 16#2FFFF)  orelse
         (Number =:= 16#3FFFE)  orelse (Number =:= 16#3FFFF)  orelse
         (Number =:= 16#4FFFE)  orelse (Number =:= 16#4FFFF)  orelse
         (Number =:= 16#5FFFE)  orelse (Number =:= 16#5FFFF)  orelse
         (Number =:= 16#6FFFE)  orelse (Number =:= 16#6FFFF)  orelse
         (Number =:= 16#7FFFE)  orelse (Number =:= 16#7FFFF)  orelse
         (Number =:= 16#8FFFE)  orelse (Number =:= 16#8FFFF)  orelse
         (Number =:= 16#9FFFE)  orelse (Number =:= 16#9FFFF)  orelse
         (Number =:= 16#AFFFE)  orelse (Number =:= 16#AFFFF)  orelse
         (Number =:= 16#BFFFE)  orelse (Number =:= 16#BFFFF)  orelse
         (Number =:= 16#CFFFE)  orelse (Number =:= 16#CFFFF)  orelse
         (Number =:= 16#DFFFE)  orelse (Number =:= 16#DFFFF)  orelse
         (Number =:= 16#EFFFE)  orelse (Number =:= 16#EFFFF)  orelse
         (Number =:= 16#FFFFE)  orelse (Number =:= 16#FFFFF)  orelse
         (Number =:= 16#10FFFE) orelse (Number =:= 16#10FFFF) ->
    {ok, {list_of_errors, Number}};

to_unicode_number(Number) when is_integer(Number) andalso
                               Number >= 0 ->
    {ok, {unicode, Number}};

to_unicode_number(Number) -> {error, {negative_number, Number}}.
