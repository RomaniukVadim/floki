-module(selector).

-export([match/3]).

-include("selector/selector_combinator.hrl").
-include("selector/selector_pseudo_class.hrl").
-include("html_tree/html_node.hrl").
-include("html_tree/text.hrl").
-include("html_tree/comment.hrl").

-define(IS_WILDCARD(X), ((X) =:= undefined orelse (X) =:= <<"*">>)).

match(_Node,
      #selector{
         id = undefined,
         type = undefined,
         classes = [],
         attributes = [],
         namespace = undefined,
         pseudo_classes = [],
         combinator = undefined
        }, _Tree) -> false;

match(undefined, _Selector, _Tree) -> false;
match({comment, _Comment}, _Selector, _Tree) -> false;
match({pi, _Xml, _XmlAttrs}, _Selector, _Tree) -> false;
match(#text{}, _Selector, _Tree) -> false;
match(#comment{}, _Selector, _Tree) -> false;

match(HtmlNode, Selector, Tree) ->
    can_match_combinator(HtmlNode, Selector#selector.combinator) andalso
    id_match(HtmlNode, Selector#selector.id) andalso
    namespace_match(HtmlNode, Selector#selector.namespace) andalso
    type_match(HtmlNode, Selector#selector.type) andalso
    classes_matches(HtmlNode, Selector#selector.classes) andalso
    attributes_matches(HtmlNode, Selector#selector.attributes) andalso
    pseudo_classes_match(HtmlNode, Selector#selector.pseudo_classes, Tree).

can_match_combinator(_Node, undefined) -> true;

can_match_combinator(
  #html_node{children_nodes_ids = []},
  #selector{combinator = #combinator{match_type = MatchType}}
 )
  when MatchType =:= child orelse MatchType =:= descendant ->
    false;

can_match_combinator(_Node, _Combinator) -> true.

id_match(_Node, undefined) -> true;
id_match(Node, Id) -> attribute_value(Node, <<"id">>) == Id.

namespace_match(_Node, Namespace) when ?IS_WILDCARD(Namespace) -> true;

namespace_match(Node, Namespace) ->
    NamespaceSize = byte_size(Namespace),

    case type_maybe_with_namespace(Node) of
        <<Namespace:NamespaceSize/binary, ":", _/binary>> -> true;
        _ -> false
    end.

type_match(_Node, Type) when ?IS_WILDCARD(Type) -> true;

type_match(Node, Type) ->
    TypeMaybeWithNamespace = type_maybe_with_namespace(Node),
    case TypeMaybeWithNamespace of
        Type ->
            true;

        FullName when byte_size(FullName) > byte_size(Type) + 1 ->
            NamespaceSize = byte_size(FullName) - byte_size(Type) - 1,
            case FullName of
                <<_Namespace:NamespaceSize/binary, ":", Type/binary>> ->
                    true;
                _ ->
                    false
            end;

        _ ->
            false
    end.

classes_matches(_Node, []) -> true;

classes_matches(Node, Classes) ->
    ClassAttrValue = attribute_value(Node, <<"class">>),
    do_classes_matches(ClassAttrValue, Classes).

do_classes_matches(undefined, _Classes) -> false;

do_classes_matches(ClassAttrValue, [Class | _])
  when bit_size(ClassAttrValue) < bit_size(Class) ->
    false;

do_classes_matches(ClassAttrValue, [Class])
  when bit_size(ClassAttrValue) == bit_size(Class) ->
    Class == ClassAttrValue;

do_classes_matches(Class_attr_value, [Class]) ->
    Splitted = binary:split(Class_attr_value, [<<" ">>, <<"\t">>, <<"\n">>], [trim_all, global]),
    lists:member(Class, Splitted);

do_classes_matches(ClassAttrValue, Classes) ->
    Reduce = fun(Item, Acc) ->
                     Acc + 1 + bit_size(Item)
             end,
    MinSize = lists:foldl(Reduce, -1, Classes),
    CanMatch = bit_size(ClassAttrValue) >= MinSize,
    SplittedClassAttrValue = binary:split(ClassAttrValue, [<<" ">>, <<"\t">>, <<"\n">>], [trim_all, global]),
    CanMatch andalso Classes -- SplittedClassAttrValue == [].


attribute_value(Node, AttributeName) ->
    Attributes = attributes(Node),
    get_attribute_value(Attributes, AttributeName).

attributes({_Type, Attributes, _Children}) -> Attributes;
attributes(#html_node{type = pi}) -> [];
attributes(#html_node{attributes = Attributes}) -> Attributes.

get_attribute_value(Attributes, AttributeName) when is_list(Attributes) ->
    proplists:get_value(AttributeName, Attributes, undefined);

get_attribute_value(Attributes, AttributeName) when is_map(Attributes) ->
    maps:get(AttributeName, Attributes).

type_maybe_with_namespace({Type, _Attributes, _Children}) when is_binary(Type) -> Type;
type_maybe_with_namespace(#html_node{type = Type}) when is_binary(Type) -> Type;
type_maybe_with_namespace(_) -> undefined.


attributes_matches(_Node, []) -> true;

attributes_matches(Node, AttributesSelectors) ->
    Attributes = attributes(Node),
    Pred = fun(AttributeSelector) ->
                   attribute_selector:match(Attributes, AttributeSelector)
           end,
    length(Attributes) =/= 0 andalso lists:all(Pred, AttributesSelectors).

pseudo_classes_match(_HtmlNode, [], _Tree) -> true;

pseudo_classes_match(HtmlNode, PseudoClasses, Tree) ->
    Pred = fun(Item) ->
                   pseudo_class_match(HtmlNode, Item, Tree)
           end,
    lists:all(Pred, PseudoClasses).

pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"nth-child">>}, Tree) ->
    selector_pseudo_class:match_nth_child(Tree, HtmlNode, PseudoClass);

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"first-child">>}, Tree) ->
    selector_pseudo_class:match_nth_child(Tree, HtmlNode, #pseudo_class{name = <<"nth-child">>, value = 1});

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"last-child">>}, Tree) ->
    selector_pseudo_class:match_nth_last_child(Tree, HtmlNode, #pseudo_class{
                                                                  name = <<"nth-last-child">>,
                                                                  value = 1
                                                                 });

pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"nth-last-child">>}, Tree) ->
    selector_pseudo_class:match_nth_last_child(Tree, HtmlNode, PseudoClass);

pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"nth-of-type">>}, Tree) ->
    selector_pseudo_class:match_nth_of_type(Tree, HtmlNode, PseudoClass);

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"first-of-type">>}, Tree) ->
    selector_pseudo_class:match_nth_of_type(Tree, HtmlNode, #pseudo_class{
                                                               name = <<"nth-of-type">>,
                                                               value = 1
                                                              });

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"last-of-type">>}, Tree) ->
    selector_pseudo_class:match_nth_last_of_type(Tree, HtmlNode, #pseudo_class{
                                                                    name = <<"nth-last-of-type">>,
                                                                    value = 1
                                                                   });

pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"nth-last-of-type">>}, Tree) ->
    selector_pseudo_class:match_nth_last_of_type(Tree, HtmlNode, PseudoClass);

pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"not">>}, Tree) ->
    Pred = fun(Item) -> not selector:match(HtmlNode, Item, Tree) end,
    lists:all(Pred, PseudoClass#pseudo_class.value);

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"checked">>}, _Tree) ->
    selector_pseudo_class:match_checked(HtmlNode);

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"disabled">>}, _Tree) ->
    selector_pseudo_class:match_disabled(HtmlNode);

pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"fl-contains">>}, Tree) ->
    selector_pseudo_class:match_contains(Tree, HtmlNode, PseudoClass);

% Case insensitive contains
pseudo_class_match(HtmlNode, PseudoClass = #pseudo_class{name = <<"fl-icontains">>}, Tree) ->
    selector_pseudo_class:match_icontains(Tree, HtmlNode, PseudoClass);

pseudo_class_match(HtmlNode, #pseudo_class{name = <<"root">>}, Tree) ->
    selector_pseudo_class:match_root(HtmlNode, Tree);

pseudo_class_match(Html_node, Pseudo_class = #pseudo_class{name = <<"has">>}, Tree) ->
    selector_pseudo_class:match_has(Tree, Html_node, Pseudo_class);

pseudo_class_match(_HtmlNode, #pseudo_class{name = UnknownPseudoClass}, _Tree) ->
    logger:debug("Pseudo-class ~p is not implemented. Ignoring.", [UnknownPseudoClass]),
    false.
