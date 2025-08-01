-module(selector_parser).

-include("selector_pseudo_class.hrl").
-include("selector_attribute_selector.hrl").
-include("selector_combinator.hrl").

-export([parse/1]).

-define(IS_ATTR_MATCH_TYPE(X), ((X) =:= equal orelse 
                                (X) =:= dash_match orelse
                                (X) =:= includes orelse
                                (X) =:= prefix_match orelse
                                (X) =:= suffix_match orelse
                                (X) =:= substring_match
                               )).

parse(Selector) when is_binary(Selector) ->
    TokenList = selector_tokenizer:tokenize(Selector),
    parse(TokenList);

parse(Tokens) ->
    do_parse_all(Tokens, []).

do_parse_all([], Selectors) ->
  lists:reverse(Selectors);

do_parse_all(Tokens, Selectors) ->
    {Selector, Remaining_tokens} = do_parse(Tokens, #selector{}),
    do_parse_all(Remaining_tokens, [Selector | Selectors]).

do_parse([], Selector) -> {optimize_selector(Selector), []};
do_parse([{close_parentesis, _} | T], Selector) -> {optimize_selector(Selector), T};
do_parse([{comma, _} | T], Selector) -> {optimize_selector(Selector), T};

do_parse([{identifier, _, Namespace}, {namespace_pipe, _} | T], Selector) ->
  do_parse(T, Selector#selector{namespace = list_to_binary(Namespace)});

do_parse([{identifier, _, Type} | T], Selector) ->
  do_parse(T, Selector#selector{type = list_to_binary(Type)});

do_parse([{'*', _} | T], Selector) ->
  do_parse(T, Selector#selector{type = <<"*">>});

do_parse([{hash, _, Id} | T], Selector) ->
  do_parse(T, Selector#selector{id = list_to_binary(Id)});

do_parse([{class, _, Class} | T], Selector) ->
  do_parse(T, Selector#selector{classes = [list_to_binary(Class) | Selector#selector.classes]});

do_parse([{'[', _} | T], Selector) ->
  {NewT, Result} = consume_attribute(T),

  do_parse(NewT, Selector#selector{attributes = [Result | Selector#selector.attributes]});

do_parse([{pseudo_not, _} | T], Selector) ->
  {NewT, PseudoNotClass} =
  parse_pseudo_with_inner_selector(T, #pseudo_class{name = <<"not">>, value = []}),
  RejectFun = fun(Item) -> Item =:= undefined end,
  PseudoClasses = lists:filter(RejectFun, [PseudoNotClass | Selector#selector.pseudo_classes]),
  do_parse(NewT, Selector#selector{pseudo_classes = PseudoClasses});

do_parse([{pseudo_has, _} | T], Selector) ->
  {NewT, PseudoHasClass} =
  parse_pseudo_with_inner_selector(T, #pseudo_class{name = <<"has">>, value = []}),

  RejectFun = fun(Item) -> Item =:= undefined end,
  PseudoClasses = lists:filter(RejectFun, [PseudoHasClass | Selector#selector.pseudo_classes]),
  do_parse(NewT, Selector#selector{pseudo_classes = PseudoClasses});

do_parse([{pseudo, _, PseudoClass} | T], Selector) ->
  PseudoClasses = [#pseudo_class{name = list_to_binary(PseudoClass)} | Selector#selector.pseudo_classes],
  do_parse(T, Selector#selector{pseudo_classes = PseudoClasses});

do_parse([{pseudo_class_int, _, PseudoClassInt} | T], Selector) ->
  [PseudoClass | PseudoClasses] = Selector#selector.pseudo_classes,

  do_parse(T, Selector#selector{pseudo_classes = [PseudoClass#pseudo_class{value = PseudoClassInt} | PseudoClasses]});

do_parse([{pseudo_class_even, _} | T], Selector) ->
  [PseudoClass | PseudoClasses] = Selector#selector.pseudo_classes,
  do_parse(T, Selector#selector{pseudo_classes = [PseudoClass#pseudo_class{value = <<"even">>} | PseudoClasses]});

do_parse([{pseudo_class_odd, _} | T], Selector) ->
  [PseudoClass | PseudoClasses] = Selector#selector.pseudo_classes,
  do_parse(T, Selector#selector{pseudo_classes = [PseudoClass#pseudo_class{value = <<"odd">>} | PseudoClasses]});

do_parse([{pseudo_class_pattern, _, Pattern} | T], Selector) ->
  [PseudoClass | PseudoClasses] = Selector#selector.pseudo_classes,

  ResultValue =
  case selector_functional:parse(Pattern) of
    invalid -> list_to_binary(Pattern);
    {ok, Value} -> Value
  end,

  do_parse(T, Selector#selector{pseudo_classes = [PseudoClass#pseudo_class{value = ResultValue} | PseudoClasses]});

do_parse([{pseudo_class_quoted, _, Pattern} | T], Selector) ->
    [PseudoClass | PseudoClasses] = Selector#selector.pseudo_classes,

    do_parse(T, Selector#selector{pseudo_classes = [PseudoClass#pseudo_class{value = list_to_binary(Pattern)} | PseudoClasses]});

do_parse([{space, _} | T], Selector) ->
    {RemainingTokens, Combinator} = consume_combinator(T, descendant),
    {Selector#selector{combinator = Combinator}, RemainingTokens};

do_parse([{greater, _} | T], Selector) ->
    {RemainingTokens, Combinator} = consume_combinator(T, child),
    {Selector#selector{combinator = Combinator}, RemainingTokens};

do_parse([{plus, _} | T], Selector) ->
    {RemainingTokens, Combinator} = consume_combinator(T, adjacent_sibling),
    {Selector#selector{combinator = Combinator}, RemainingTokens};

do_parse([{tilde, _} | T], Selector) ->
    {RemainingTokens, Combinator} = consume_combinator(T, general_sibling),
    {Selector#selector{combinator = Combinator}, RemainingTokens}; 

do_parse([{unknown, _, Unknown} | T], Selector) ->
    logger:debug("Unknown token ~p. Ignoring.", [Unknown]),
    do_parse(T, Selector).

consume_attribute(Tokens) -> consume_attribute(consuming, Tokens, #attribute_selector{}).
consume_attribute(_, [], AttrSelector) -> {[], AttrSelector};
consume_attribute(done, Tokens, AttrSelector) -> {Tokens, AttrSelector};

consume_attribute(consuming, [{identifier, _, Identifier} | T], AttrSelector) ->
    NewSelector = set_attribute_name_or_value(AttrSelector, Identifier),
    consume_attribute(consuming, T, NewSelector);

consume_attribute(consuming, [{MatchType, _} | T], AttrSelector)
       when ?IS_ATTR_MATCH_TYPE(MatchType) ->
    NewSelector = AttrSelector#attribute_selector{match_type = MatchType},
    consume_attribute(consuming, T, NewSelector);

consume_attribute(consuming, [{quoted, _, Value} | T], AttrSelector) ->
    NewSelector = AttrSelector#attribute_selector{value = list_to_binary(Value)},
    consume_attribute(consuming, T, NewSelector);

consume_attribute(consuming, [{attribute_identifier, _, Value} | T], AttrSelector) ->
    Flag = binary:at(list_to_binary(Value), byte_size(Value)-2),
    New_selector = AttrSelector#attribute_selector{flag = Flag},
    consume_attribute(done, T, New_selector);

consume_attribute(consuming, [{']', _} | T], AttrSelector) ->
    consume_attribute(done, T, AttrSelector);

consume_attribute(consuming, [Unknown | T], AttrSelector) ->
    logger:debug("Unknown token ~p. Ignoring.", [Unknown]),
    consume_attribute(consuming, T, AttrSelector).

set_attribute_name_or_value(AttrSelector, Identifier) ->
    % When match type is not defined, this is an attribute name.
    % Otherwise, it is an attribute value.
    case AttrSelector#attribute_selector.match_type of
      undefined -> AttrSelector#attribute_selector{attribute = list_to_binary(Identifier)};
      _ -> AttrSelector#attribute_selector{value = list_to_binary(Identifier)}
    end.

consume_combinator(Tokens, CombinatorType) when is_atom(CombinatorType) ->
    consume_combinator(Tokens, #combinator{match_type = CombinatorType, selector = #selector{}});

consume_combinator([], Combinator) -> {[], Combinator};

consume_combinator(Tokens, Combinator) ->
    {Selector, RemainingTokens} = do_parse(Tokens, #selector{}),
    {RemainingTokens, Combinator#combinator{selector = Selector}}.

parse_pseudo_with_inner_selector([], PseudoClass) ->
    {[], PseudoClass};

parse_pseudo_with_inner_selector([{close_parentesis, _} | T], PseudoClass) ->
    {T, PseudoClass};

parse_pseudo_with_inner_selector([{space, _} | T], PseudoClass) ->
    parse_pseudo_with_inner_selector(T, PseudoClass);

% At this point we want to ignore comma, because it's going to start a new selector.
parse_pseudo_with_inner_selector([{comma, _} | T], PseudoClass) ->
    parse_pseudo_with_inner_selector(T, PseudoClass);

parse_pseudo_with_inner_selector(Tokens, PseudoClass) ->
    parse_pseudo_with_inner_selector(Tokens, #selector{}, PseudoClass).

parse_pseudo_with_inner_selector([], PseudoWithInnerSelector, PseudoClass) ->
    NewPseudoClass = update_pseudo_with_inner_selector(PseudoClass, PseudoWithInnerSelector),
    {[], NewPseudoClass};

parse_pseudo_with_inner_selector([{close_parentesis, _} | T],
                                 PseudoWithInnerSelector,
                                 PseudoClass) ->
    NewPseudoClass = update_pseudo_with_inner_selector(PseudoClass, PseudoWithInnerSelector),
    {T, NewPseudoClass};

parse_pseudo_with_inner_selector( [{comma, _} | T],
                                  PseudoWithInnerSelector,
                                  PseudoClass) ->
    NewPseudoClass = update_pseudo_with_inner_selector(PseudoClass, PseudoWithInnerSelector),
    parse_pseudo_with_inner_selector(T, NewPseudoClass);

parse_pseudo_with_inner_selector([{space, _} | T], PseudoWithInnerSelector, PseudoClass) ->
    parse_pseudo_with_inner_selector(T, PseudoWithInnerSelector, PseudoClass);

parse_pseudo_with_inner_selector([{'[', _} | Tokens], PseudoWithInnerSelector, PseudoClass) ->
    {Remaining_tokens, Result} = consume_attribute(Tokens),

    Selector = PseudoWithInnerSelector#selector{attributes = [Result | PseudoWithInnerSelector#selector.attributes]},

    NewPseudoClass = update_pseudo_with_inner_selector(PseudoClass, Selector),
    parse_pseudo_with_inner_selector(Remaining_tokens, NewPseudoClass);

parse_pseudo_with_inner_selector([NextToken | T], PseudoWithInnerSelector, PseudoClass) ->
    {NewPseudoWithInnerSelector, _} = do_parse([NextToken], PseudoWithInnerSelector),
    parse_pseudo_with_inner_selector(T, NewPseudoWithInnerSelector, PseudoClass).

update_pseudo_with_inner_selector(PseudoClass,
                                  PseudoWithInnerSelector = #selector{combinator = undefined}) ->
    Value = [PseudoWithInnerSelector | maps:get(value, PseudoClass, [])],
    PseudoClass#pseudo_class{value = Value};

update_pseudo_with_inner_selector(#pseudo_class{name = Name}, _PseudoWithInnerSelector) ->
    logger:debug("Only simple selectors are allowed in ~p pseudo-class. Ignoring.", [Name]),
    undefined.

% Reorders classes in selector to improve matching performance.
optimize_selector(Selector) ->
    Fun = fun(Item, Item2) -> bit_size(Item) >= bit_size(Item2) end,
    Classes = lists:sort(Fun, Selector#selector.classes),
    Selector#selector{classes = Classes}.
