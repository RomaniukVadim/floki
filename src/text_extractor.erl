-module(text_extractor).
-export([extract_input_value/1]).

-define(allowed_input_types, [
                              <<"color">>,
                              <<"date">>,
                              <<"datetime-local">>,
                              <<"email">>,
                              <<"month">>,
                              <<"number">>,
                              <<"search">>,
                              <<"tel">>,
                              <<"text">>,
                              <<"time">>,
                              <<"url">>,
                              <<"week">>
                             ]).

extract_input_value(Attrs) ->
  Pred = fun(Item) -> is_match({<<"type">>, '_'}, Item) end,
  {<<"type">>, T} = find(Attrs, {<<"type">>, <<"text">>}, Pred),

  case lists:member(T, ?allowed_input_types) of
    true -> extract_value(Attrs);
    false -> <<>>
  end.

extract_value(Attrs) ->
  Pred = fun({<<"value">>, V}) -> V;
            (_) -> false
         end,
    find_value(Attrs, <<>>, Pred).

%% Mimics Enum.find_value/3
find_value([], Default, _Fun) ->
    Default;
find_value([H|T], Default, Fun) ->
    case Fun(H) of
        % In Elixir, `nil` and `false` are falsy.
        % In Erlang, `false` is the only falsy value.
        % This case matches on `false` and continues the search.
        false ->
            find_value(T, Default, Fun);
        % Any other result is considered the found value.
        Result ->
            Result
    end.

%% Mimics Enum.find/3
find(List, Default, Pred) ->
    case lists:search(Pred, List) of
        {ok, Value} ->
            Value;
        false ->
            Default
    end.

%% Mimics Elixir's match?/2 macro.
%% Note: This is an unusual Erlang pattern. Usually, you'd just use a `case`
%% statement directly rather than wrapping it like this.
is_match(Pattern, Value) ->
    try
        true = (Pattern = Value),
        true
    catch
        _:_ -> false
    end.
