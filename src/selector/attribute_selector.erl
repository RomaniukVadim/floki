-module(attribute_selector).
-export([match/2,
         to_string/1]).

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

match(Attributes, S = #attribute_selector{match_type = undefined, value = undefined})
      when is_list(Attributes) or is_map(Attributes) ->
    attribute_present(S#attribute_selector.attribute, Attributes);

match(Attributes, S = #attribute_selector{match_type = equal, flag = <<"i">>}) ->
    ValueFromAttributes = get_value(S#attribute_selector.attribute, Attributes),
    SelectorValueDowncase = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    ValueFromAttributesDowncase = list_to_binary(string:lowercase(binary_to_list(ValueFromAttributes))),
    ValueFromAttributesDowncase == SelectorValueDowncase;

match(Attributes, S = #attribute_selector{match_type = includes, flag = <<"i">>}) ->
    SelectorValue = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    Value = get_value(S#attribute_selector.attribute, Attributes),
    Splitted = binary:split(Value, [<<" ">>, <<"\t">>, <<"\n">>], [trim_all, global]),
    Pred = fun(V) -> list_to_binary(string:lowercase(binary_to_list(V))) == SelectorValue end,
    lists:any(Pred, Splitted);

match(Attributes, S = #attribute_selector{match_type = dash_match, flag = <<"i">>}) ->
    SelectorValue = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    Value = list_to_binary(string:lowercase(binary_to_list(get_value(S#attribute_selector.attribute, Attributes)))),
    Size = byte_size(SelectorValue),
    Value == SelectorValue orelse case Value of
        <<SelectorValue:Size/binary, "-", _/binary>> -> true;
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
    SkipSize = byte_size(ValueDowncase) - Size,
    case SkipSize >= 0 of
        true ->
            case ValueDowncase of
                <<_:SkipSize/binary, AttrValueDowncase:Size/binary>> -> true;
                _ -> false
            end;
        false -> false
    end;

match(Attributes, S = #attribute_selector{match_type = substring_match, flag = <<"i">>}) ->
    AttributeValue = get_value(S#attribute_selector.attribute, Attributes),
    AttributeValueDowcase = list_to_binary(string:lowercase(binary_to_list(AttributeValue))),
    ValueDowncase = list_to_binary(string:lowercase(binary_to_list(S#attribute_selector.value))),
    case binary:match(AttributeValueDowcase, ValueDowncase) of
        nomatch -> false;
        _ -> true
    end;

match(Attributes, S = #attribute_selector{match_type = equal}) ->
    get_value(S#attribute_selector.attribute, Attributes) == S#attribute_selector.value;

match(Attributes, S = #attribute_selector{match_type = includes, value = Value}) ->
    AttributeValue = get_value(S#attribute_selector.attribute, Attributes),
    Splitted = binary:split(AttributeValue, [<<" ">>, <<"\t">>, <<"\n">>], [trim_all, global]),
    lists:member(Value, Splitted);

match(Attributes, S = #attribute_selector{match_type = dash_match}) ->
    Value = get_value(S#attribute_selector.attribute, Attributes),
    SelectorValue = S#attribute_selector.value,
    Size = byte_size(SelectorValue),
    Value == SelectorValue orelse case Value of
        <<SelectorValue:Size/binary, "-", _/binary>> -> true;
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
    Size = byte_size(Value),
    SkipSize = byte_size(AttrValue) - Size,
    case SkipSize >= 0 of
        true ->
            case AttrValue of
                <<_:SkipSize/binary, Value:Size/binary>> -> true;
                _ -> false
            end;
        false -> false
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

find_value([], Default, _Fun) -> Default;
find_value([H|T], Default, Fun) ->
    case Fun(H) of
        false -> find_value(T, Default, Fun);
        Result -> Result
    end.
