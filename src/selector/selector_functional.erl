-module(selector_functional).

-export([parse/1]).

  % Represents a functional notation for a selector
-record(selector_functional, {stream,
                             a,
                             b}).

parse(Expr) when is_list(Expr) ->
    parse(to_string(Expr));

parse(Expr) when is_binary(Expr) ->
    ExprDowncase = list_to_binary(string:lowercase(binary_to_list(Expr))),
    Regex = <<"^\\s*(?<a>[-+]?[0-9]*[n])\\s*(?<b>[+-]\\s*[0-9]+)?\\s*$">>,

    case re:run(ExprDowncase, Regex, [{capture, all_names, binary}]) of
      undefined -> invalid;
        {match, Captured} ->
            #{<<"a">> := A, <<"b">> := B} = maps:from_list(Captured),
            {ok, build(A, B)}
    end.

build(A, <<"">>) -> build(A, <<"0">>);

build(A, B) ->
    AParsed = parse_num(A),
    BParsed = parse_num(B),
    Seq = lists:seq(0, 100_000),

    Stream = stream_map(fun(X) ->
                                AParsed * X + BParsed
                        end, Seq),

      #selector_functional{stream = Stream, a = AParsed, b = BParsed}.

parse_num(NStr) ->
    Replaced = binary:replace(NStr, <<" ">>, <<"">>),
    Trimmed = trim(Replaced, <<"n">>),
    case Trimmed of
      <<"-">> -> -1;
      <<"">> -> 1;
      N -> binary_to_integer(N)
    end.

to_string(Functional) ->
      <<(Functional#selector_functional.a/binary),"x+",(Functional#selector_functional.b)/binary>>.

% The "stream" is just a function that produces the next value and the next function.
-type stream() :: fun(() -> {any(), stream()} | done).

% Our lazy map implementation
-spec stream_map(function(), stream()) -> stream().
stream_map(Fun, Next) ->
    fun() ->
        case Next() of
            {Value, NextFun} ->
                {Fun(Value), stream_map(Fun, NextFun)};
            done ->
                done
        end
    end.

% A helper to consume the stream and turn it into a list
consume(Stream) -> consume(Stream(), []).
consume({Value, NextFun}, Acc) -> consume(NextFun(), [Value | Acc]);
consume(done, Acc) -> lists:reverse(Acc).

trim(Bin, ToTrim) ->
    trim_leading(trim_trailing(Bin, ToTrim), ToTrim).

trim_leading(<<C, Rest/binary>>, ToTrim) when C =:= ToTrim ->
    trim_leading(Rest, ToTrim);
trim_leading(Bin, ToTrim) ->
    Bin.

trim_trailing(Bin, ToTrim) ->
    trim_trailing_rev(binary:reverse(Bin), ToTrim).

trim_trailing_rev(<<C, Rest/binary>>, ToTrim) when C =:= ToTrim ->
    trim_trailing_rev(Rest, ToTrim);
trim_trailing_rev(Bin, _ToTrim) ->
    binary:reverse(Bin).
