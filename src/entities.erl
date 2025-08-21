-module(entities).

-export([decode/1,
         encode/1]).

-doc """
  Decode charrefs and numeric charrefs.

  This is useful if you want to decode any charref. The tokenizer will
  use a different algorithm, so this may not be necessary.
  """.

  decode(Charref) when is_binary(Charref) ->
      case Charref of
          <<"&#", Numeric/binary>> ->
              case extract_byte_from_num_charref(Numeric) of
                  {ok, Number} ->
                      case numeric_charref:to_unicode_number(Number) of
                          {ok, {_, Unicode_number}} -> {ok, <<Unicode_number/utf8>>};
                          {error, {negative_number, _}} -> {error, not_found}
                      end;
                  error -> {error, not_found}
              end;

          <<"&", _/binary>> = Binary ->
              case codepoints:get(Binary) of
                  [] -> {error, not_found};
                  Codepoints -> {ok, list_to_binary(Codepoints)}
              end;
          _Other -> {error, not_found}
      end.

extract_byte_from_num_charref(<<MaybeX, Rest/binary>>) when MaybeX =:= $x; MaybeX =:= $X ->
    case integer:parse(Rest, 16) of
        {Number, _} -> {ok, Number};
        {error, _}  -> error
    end;

extract_byte_from_num_charref(Binary) when is_binary(Binary) ->
    case integer:parse(Binary, 10) of
        {Number, _} -> {ok, Number};
        {error, _}  -> error
    end.

-doc """
  Encode HTML entities in a string.

  Currently only encodes the main HTML entities:

  * single quote - ' - is replaced by "&#39;".
  * double quote - " - is replaced by "&quot;".
  * ampersand - & - is replaced by "&amp;".
  * less-than sign - < - is replaced by "&lt;".
  * greater-than sign - > - is replaced by "&gt;".

  All other symbols are going to remain the same.

  Optimized IO data implementation from Plug.HTML
  """.

-spec encode(iodata()) -> iodata().
encode(String) when is_binary(String) ->
    % Start in the initial scanning mode (encode/4)
    encode(String, 0, String, []);
encode(Data) ->
    encode(list_to_binary(Data)).

%% Initial scanning mode (Arity 4)
%% Handle each special character with its own function clause
encode(<<"<", Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip + 1, Original, [Acc | "&lt;"]);
encode(<<">", Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip + 1, Original, [Acc | "&gt;"]);
encode(<<"&", Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip + 1, Original, [Acc | "&amp;"]);
encode(<<"\"", Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip + 1, Original, [Acc | "&quot;"]);
encode(<<"'", Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip + 1, Original, [Acc | "&#39;"]);

% If no special char matches, we found a "safe" character.
% Switch to substring scanning mode (encode/5) and start a counter at 1.
encode(<<_Char, Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip, Original, Acc, 1);

% Base case: end of binary
encode(<<>>, _Skip, _Original, Acc) ->
    Acc.


%% Substring scanning mode (Arity 5) - active while reading safe characters
%% If we find a special character, the safe substring has ended.
encode(<<"<", Rest/bits>>, Skip, Original, Acc, Len) ->
    Part = binary:part(Original, Skip, Len),
    % Go back to initial scanning mode (encode/4)
    encode(Rest, Skip + Len + 1, Original, [Acc, Part | "&lt;"]);
% ... clauses for >, &, ", ' would go here, same as above ...

% If we find another safe character, just increment the length counter.
encode(<<_Char, Rest/bits>>, Skip, Original, Acc, Len) ->
    encode(Rest, Skip, Original, Acc, Len + 1);

% Base cases: end of binary
% If skip is 0, no replacements were ever made, return the original.
encode(<<>>, 0, Original, _Acc, _Len) ->
    Original;
% Otherwise, append the final safe substring.
encode(<<>>, Skip, Original, Acc, Len) ->
    [Acc | binary:part(Original, Skip, Len)].
