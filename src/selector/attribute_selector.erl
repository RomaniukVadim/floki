-module(attribute_selector).
-export([match/2,
         to_string/1]).

  % It is very similar to the `Selector` module, but is specialized in attributes
  % and attribute selectors.

-record(attribute_selector, {match_type = undefined :: match_type(),
                            attribute = undefined :: binary(),
                            value = undefined :: binary() | undefined,
                            flag = undefined :: binary() | undefined}).

  -type match_type() ::
  undefined
  | equal
  | includes
  | dash_match
  | prefix_match
  | suffix_match
  | substring_match.


to_string(Selector) ->
    <<"[", (Selector#attribute_selector.attribute)/binary,
      (type(Selector#attribute_selector.match_type))/binary,
      (Selector#attribute_selector.value)/binary,
      (flag(Selector#attribute_selector.flag))/binary,
      "]">>.

type(MatchType) ->
      case MatchType of
        equal -> <<"=">>;
        includes -> <<"~=">>;
        dash_match -> <<"|=">>;
        prefix_match -> <<"^=">>;
        suffix_match -> <<"$=">>;
        substring_match -> <<"*=">>;
        _ -> <<"">>
      end.

flag(undefined) -> <<"">>;
flag(Flag) -> <<" ", Flag/binary>>.

% Returns if attributes of a node matches with a given attribute selector.
match(Attributes, S = #attribute_selector{match_type = undefined, value = undefined})
      when is_list(Attributes) or is_map(Attributes) ->
    attribute_present(S#attribute_selector.attribute, Attributes);

% Case-insensitive matches

match(Attributes, S = #attribute_selector{match_type = equal, flag = <<"i">>}) ->
    ValueFromAttributes = get_value(S#attribute_selector.attribute, Attributes),
    SelectorValueDowncase = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    ValueFromAttributesDowncase = list_to_binary(string:lowercase(binary_to_list(ValueFromAttributes))),

    ValueFromAttributesDowncase == SelectorValueDowncase;

match(Attributes, S = #attribute_selector{match_type = includes, flag = <<"i">>}) ->
    SelectorValue = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),

    Value =  get_value(S#attribute_selector.attribute, Attributes),
    % Splits by whitespaces ("a  b c" -> ["a", "b", "c"])
    Splitted = binary:split(Value, [<<" ">>, <<"\t">>, <<"\n">>], [trim_all, global]),
    Pred = fun(V) -> list_to_binary(string:lowercase(binary_to_list(V))) == SelectorValue end,
    lists:any(Pred, Splitted);

match(Attributes, S = #attribute_selector{match_type = dash_match, flag = <<"i">>}) ->
    SelectorValue = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    Value = list_to_binary(string:lowercase(binary_to_list(get_value(S#attribute_selector.attribute, Attributes)))),

    Value == SelectorValue orelse case Value of
                                      <<"#{selector_value}-", _/binary>> -> true;
                                      _ -> false
                                  end;

match(Attributes, S = #attribute_selector{match_type = prefix_match, flag = <<"i">>}) ->
    AttributeValue = get_value(S#attribute_selector.attribute, Attributes),
    DowncaseAttributeValue = list_to_binary(string:lowercase(binary_to_list(AttributeValue))),
    DowncaseValue = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    Size = byte_size(DowncaseValue),
    case DowncaseAttributeValue of
        <<DowncaseValue:Size/binary, _/binary>> -> true;
        _ -> false
    end;

match(Attributes, S = #attribute_selector{match_type = suffix_match, flag = <<"i">>}) ->
    Value = get_value(S#attribute_selector.attribute, Attributes),
    ValueDowncase = list_to_binary(string:lowercase(binary_to_list(Value))),
    AttrValueDowncase = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    Size = byte_size(AttrValueDowncase),
    case ValueDowncase of
        <<AttrValueDowncase:Size/binary, _/binary>> -> true;
        _ -> false
    end;

match(Attributes, S = #attribute_selector{match_type = substring_match, flag = <<"i">>}) ->
    AttributeValue = get_value(S#attribute_selector.attribute, Attributes),
    AttributeValueDowcase = list_to_binary(string:lowercase(binary_to_list(AttributeValue))),
    ValueDowncase = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    Size = byte_size(ValueDowncase),
    case AttributeValueDowcase of
        <<ValueDowncase:Size/binary, _/binary>> -> true;
        _ -> false
    end;

  % Case-sensitive matches

match(Attributes, S = #attribute_selector{match_type = equal}) ->
    get_value(S#attribute_selector.attribute, Attributes) == S#attribute_selector.value;

match(Attributes, S = #attribute_selector{match_type = includes, value = Value}) ->
    AttributeValue = get_value(S#attribute_selector.attribute, Attributes),
    Splitted = binary:split(AttributeValue, [<<" ">>, <<"\t">>, <<"\n">>], [trim_all, global]),
    lists:member(Value, Splitted);

match(Attributes, S = #attribute_selector{match_type = dash_match}) ->
    Value = get_value(S#attribute_selector.attribute, Attributes),

    Value == S#attribute_selector.value orelse case Value of
                                                   <<"#{s.value}-", _/binary>> -> true;
                                                   _ -> false
                                               end;

match(Attributes, S = #attribute_selector{match_type = prefix_match}) ->

    AttrValue = get_value(S#attribute_selector.attribute, Attributes),
    Value = S#attribute_selector.value,
    Size = byte_size(Value),
    case AttrValue of
        <<Value:Size/binary, _/binary>> -> true;
        _ -> false
    end;

match(Attributes, S = #attribute_selector{match_type = suffix_match}) ->
    AttrValue = get_value(S#attribute_selector.attribute, Attributes),
    Value = S#attribute_selector.value,
    SkipSize = byte_size(AttrValue) - byte_size(Value),
    case AttrValue of
        <<_Skip:SkipSize/binary, Value>> -> true;
        _ -> false
    end;

match(Attributes, S = #attribute_selector{match_type = substring_match}) ->
    AttrSelector = get_value(S#attribute_selector.attribute, Attributes),
    case binary:match(AttrSelector, S#attribute_selector.value) of
        nomatch -> false;
        _ -> true
    end.

get_value(AttrName, Attributes) ->
    Size = byte_size(AttrName),
    Pred = fun({<<Key:Size/binary, _/binary>>, Value}) when Key == AttrName -> Value;
              (_) -> false
           end,

    find_value(Attributes, <<"">>, Pred).

attribute_present(Name, Attributes) ->
    Size = byte_size(Name),
    Pred = fun({<<Key:Size/binary, _/binary>>, _V}) when Key == Name -> true;
      (_) -> false
    end,
    lists:any(Pred, Attributes).


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

