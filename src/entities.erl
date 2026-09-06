-module(entities).

-export([decode/1,
         encode/1]).

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

extract_byte_from_num_charref(<<MaybeX, Rest/binary>>) when MaybeX =:= $x orelse MaybeX =:= $X ->
    case string:to_integer(Rest) of
        {error, _}  -> error;
        {Number, _} -> {ok, Number}
    end;
extract_byte_from_num_charref(Binary) when is_binary(Binary) ->
    case string:to_integer(Binary) of
        {error, _}  -> error;
        {Number, _} -> {ok, Number}
    end.

-spec encode(iodata()) -> iodata().
encode(String) when is_binary(String) ->
    encode(String, 0, String, []);
encode(Data) ->
    encode(list_to_binary(Data)).

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
encode(<<_Char, Rest/bits>>, Skip, Original, Acc) ->
    encode(Rest, Skip, Original, Acc, 1);
encode(<<>>, _Skip, _Original, Acc) ->
    Acc.

encode(<<"<", Rest/bits>>, Skip, Original, Acc, Len) ->
    Part = binary:part(Original, Skip, Len),
    encode(Rest, Skip + Len + 1, Original, [Acc, Part | "&lt;"]);
encode(<<">", Rest/bits>>, Skip, Original, Acc, Len) ->
    Part = binary:part(Original, Skip, Len),
    encode(Rest, Skip + Len + 1, Original, [Acc, Part | "&gt;"]);
encode(<<"&", Rest/bits>>, Skip, Original, Acc, Len) ->
    Part = binary:part(Original, Skip, Len),
    encode(Rest, Skip + Len + 1, Original, [Acc, Part | "&amp;"]);
encode(<<"\"", Rest/bits>>, Skip, Original, Acc, Len) ->
    Part = binary:part(Original, Skip, Len),
    encode(Rest, Skip + Len + 1, Original, [Acc, Part | "&quot;"]);
encode(<<"'", Rest/bits>>, Skip, Original, Acc, Len) ->
    Part = binary:part(Original, Skip, Len),
    encode(Rest, Skip + Len + 1, Original, [Acc, Part | "&#39;"]);
encode(<<_Char, Rest/bits>>, Skip, Original, Acc, Len) ->
    encode(Rest, Skip, Original, Acc, Len + 1);
encode(<<>>, 0, Original, _Acc, _Len) ->
    Original;
encode(<<>>, Skip, Original, Acc, Len) ->
    [Acc | binary:part(Original, Skip, Len)].
