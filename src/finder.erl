-module(finder).

-export([find_by_id/2,
         find/2,
         list_wrap/1]).

-include("html_tree.hrl").
-include("selector/selector_combinator.hrl").
-include("selector/selector_pseudo_class.hrl").

-spec find_by_id(#html_tree{} | #html_node{}, binary()) -> #html_node{} | undefined.
find_by_id(HtmlTreeAsTuple, Id) ->
    HtmlTreeAsTupleWrapped = list_wrap(HtmlTreeAsTuple),
    traverse_find_by_id(HtmlTreeAsTupleWrapped, Id).

traverse_find_by_id([{_Type, _Attributes, Children} = HtmlTuple | Rest], Id) ->
    case selector:id_match(HtmlTuple, Id) of
        true -> HtmlTuple;
        false -> traverse_find_by_id(Children, Id) orelse traverse_find_by_id(Rest, Id)
    end;

traverse_find_by_id([_ | Rest], Id) ->
    traverse_find_by_id(Rest, Id);

traverse_find_by_id([], _Id) ->
    undefined.

find([], _) -> [];
find(HtmlAsString, _) when is_binary(HtmlAsString) -> [];

find(HtmlTree, SelectorAsString) when is_binary(SelectorAsString) ->
    Selectors = selector_parser:parse(SelectorAsString),

    case Selectors =/= [] of
        true -> find(HtmlTree, Selectors);
        false -> []
    end;

find(Html_tree, #selector{} = Selector) ->
    find(Html_tree, [Selector]);

find(HtmlTreeAsTuple, Selectors) when is_list(Selectors) ->
    case (is_list(HtmlTreeAsTuple) orelse floki:is_html_node(HtmlTreeAsTuple)) andalso
         is_list(Selectors) of
        true ->
            case traverse_html_tuples(Selectors) of
                true ->
                    [Selector] = Selectors,
                    HtmlTreeAsTupleWrapped = list_wrap(HtmlTreeAsTuple),
                    Results = traverse_html_tuples(HtmlTreeAsTupleWrapped, Selector, []),
                    lists:reverse(Results);
                false ->
                    Tree = html_tree:build(HtmlTreeAsTuple),
                    Results = find(Tree, Selectors),
                    Fun = fun(Html_node) ->
                                  html_tree:to_tuple(Tree, Html_node)
                          end,
                    [Fun(HtmlNode) || HtmlNode <- Results]
            end;
        false when is_list(Selectors) ->
            case HtmlTreeAsTuple of
                #html_tree{} = Tree ->
                    Fun = fun(S) ->
                                  traverse_html_tree(Tree#html_tree.node_ids, S, Tree, [])
                          end,
                    Flatmap = lists:flatmap(Fun, Selectors),
                    Sorted = case length(Flatmap) =:= 0 of
                                 true -> [];
                                 false when length(Flatmap) >= 2 ->
                                     SortFun = fun(A, B) -> A#html_node.node_id =< B#html_node.node_id end,
                                     lists:sort(SortFun, Flatmap);
                                 false -> Flatmap
                             end,
                    lists:uniq(Sorted);
                _ -> undefined
            end;
        false -> undefined
    end.

list_wrap(undefined) -> [];
list_wrap(Term) when is_list(Term) -> Term;
list_wrap(Term) -> [Term].

% some selectors can be applied with the raw html tree tuples instead of
% using an intermediate HTMLTree:
% - single selector
% - single child or adjacent sibling combinator, and as the last combinator
% - no pseudo classes
traverse_html_tuples([#selector{} = Selector]) ->
    traverse_html_tuples(Selector);

traverse_html_tuples(#selector{combinator = undefined, pseudo_classes = Pseudo_classes}) ->
    traverse_html_tuples(Pseudo_classes);

traverse_html_tuples(#selector{combinator = Combinator, pseudo_classes = PseudoClasses}) ->
    traverse_html_tuples(PseudoClasses) andalso traverse_html_tuples(Combinator);

traverse_html_tuples(#combinator{match_type = MatchType, selector = Selector})
  when MatchType == descendant orelse MatchType == general_sibling ->
    traverse_html_tuples(Selector);

traverse_html_tuples(#combinator{
                        match_type = MatchType,
                        selector = #selector{combinator = undefined} = Selector
                       })
  when MatchType == child orelse MatchType == adjacent_sibling ->
    traverse_html_tuples(Selector);

traverse_html_tuples([#pseudo_class{name = Name} | Rest])
  when Name == <<"checked">> orelse <<"disabled">> ->
    traverse_html_tuples(Rest);

traverse_html_tuples([#pseudo_class{name = <<"not">>, value = Value} | Rest]) ->
    Pred = fun(Item) ->
                   traverse_html_tuples(Item)
           end,
    lists:all(Pred, Value) andalso traverse_html_tuples(Rest);

traverse_html_tuples([]) -> true;
traverse_html_tuples(_) -> false.

traverse_html_tree([], _Selector, _Tree, Acc) -> Acc;

traverse_html_tree(
  [NodeId | Rest],
  #selector{combinator = undefined} = Selector,
  Tree,
  Acc
 ) ->
    HtmlNode = get_node(NodeId, Tree),

    NewAcc =
    case selector:match(HtmlNode, Selector, Tree) of
        true -> [HtmlNode | Acc];
        false -> Acc
    end,

    traverse_html_tree(Rest, Selector, Tree, NewAcc);

traverse_html_tree(
  [NodeId | Rest],
  #selector{combinator = Combinator} = Selector,
  Tree,
  Acc
 ) ->
    HtmlNode = get_node(NodeId, Tree),

    NewAcc =
    case selector:match(HtmlNode, Selector, Tree) of
        true ->
            Nodes = get_selector_nodes(Combinator, HtmlNode, Tree),
            traverse_html_tree(Nodes, Combinator#combinator.selector, Tree, Acc);
        false -> Acc
    end,

    traverse_html_tree(Rest, Selector, Tree, NewAcc).

get_selector_nodes(#combinator{match_type = child}, HtmlNode, _Tree) ->
    lists:reverse(HtmlNode#html_node.children_nodes_ids);

get_selector_nodes(#combinator{match_type = adjacent_sibling}, HtmlNode, Tree) ->
    case get_siblings(HtmlNode, Tree) of
        [SiblingId | _] -> [SiblingId];
        _ -> []
    end;

get_selector_nodes(#combinator{match_type = general_sibling}, HtmlNode, Tree) ->
    get_siblings(HtmlNode, Tree);

get_selector_nodes(#combinator{match_type = descendant}, HtmlNode, Tree) ->
    get_descendant_ids(HtmlNode#html_node.node_id, Tree).

get_node(Id, Tree) ->
    maps:get(Id, Tree#html_tree.nodes).


% When a selector has a combinator with match type descendant or
% general_sibling we are able to use the combinator selector directly it's
% siblings or children for the traversal when there's a match.
% For selectors with child and adjacent_sibling combinators we have to make
% sure we don't propagate the selector to more elements than the combinator
% specifies. For matches of these combinators we use the Selector.Combinator
% term in the traversal to keep track of this information.
traverse_html_tuples([], _Selector, Acc) ->
    Acc;

traverse_html_tuples([{_Type, _Attributes, Children} = HtmlTuple | Siblings],
                     #selector{combinator = undefined} = Selector, Acc) ->
    NewAcc =
    case selector:match(HtmlTuple, Selector, undefined) of
        true -> [HtmlTuple | Acc];
        false -> Acc
    end,

    FinalAcc = traverse_html_tuples(Children, Selector, NewAcc),
    traverse_html_tuples(Siblings, Selector, FinalAcc);

traverse_html_tuples([{_Type, _Attributes, Children} = HtmlTuple | Siblings],
                     #selector{
                        combinator =  #combinator{
                                         match_type = descendant,
                                         selector = CombinatorSelector
                                        }
                       } = Selector, Acc) ->
    NewAcc =
    case selector:match(HtmlTuple, Selector, undefined) of
        true -> traverse_html_tuples(Children, CombinatorSelector, Acc);
        false -> traverse_html_tuples(Children, Selector, Acc)
    end,

    traverse_html_tuples(Siblings, Selector, NewAcc);

traverse_html_tuples([{_Type, _Attributes, Children} = HtmlTuple | Siblings],
                      #selector{
                         combinator = #combinator{ match_type = child} = Combinator} = Selector,
                      Acc) ->
    NewAcc =
    case selector:match(HtmlTuple, Selector, undefined) of
        true -> traverse_html_tuples(Children, Combinator, Acc);
        false -> Acc
    end,

    FinalAcc = traverse_html_tuples(Children, Selector, NewAcc),
    traverse_html_tuples(Siblings, Selector, FinalAcc);

traverse_html_tuples([{_Type, _Attributes, Children} = HtmlTuple | Siblings],
                     #selector{
                        combinator = #combinator{ match_type = adjacent_sibling} = Combinator
                       } = Selector, Acc) ->
    NewAcc =
    case selector:match(HtmlTuple, Selector, undefined) of
        true -> traverse_html_tuples(Siblings, Combinator, Acc);
        false -> Acc
    end,

    FinalAcc = traverse_html_tuples(Children, Selector, NewAcc),
    traverse_html_tuples(Siblings, Selector, FinalAcc);

traverse_html_tuples([{_Type, _Attributes, Children} = HtmlTuple | Siblings],
                     #selector{
                        combinator = #combinator{
                                        match_type = general_sibling,
                                        selector = CombinatorSelector
                                       }
                        = Selector}, Acc) ->
    NewAcc = traverse_html_tuples(Children, Selector, Acc),

    case selector:match(HtmlTuple, Selector, undefined) of
        true -> traverse_html_tuples(Siblings, CombinatorSelector, NewAcc);
        false -> traverse_html_tuples(Siblings, Selector, NewAcc)
    end;

traverse_html_tuples([{_type, _attributes, _children} = HtmlTuple | Siblings],
                     #combinator{match_type = child, selector = Selector} = Combinator, Acc) ->
    NewAcc =
    case selector:match(HtmlTuple, Selector, undefined) of
        true -> [HtmlTuple | Acc];
        false -> Acc
    end,

    traverse_html_tuples(Siblings, Combinator, NewAcc);

traverse_html_tuples([{_Type, _Attributes, _Children} = HtmlTuple | _Siblings],
                     #combinator{match_type = adjacent_sibling, selector = Selector}, Acc) ->
    % adjacent_sibling combinator targets only the first html_tag, so we don't
    % continue the traversal
    case selector:match(HtmlTuple, Selector, undefined) of
        true -> [HtmlTuple | Acc];
        false -> Acc
    end;

traverse_html_tuples( [_ | Siblings], Selector, Acc) ->
    traverse_html_tuples(Siblings, Selector, Acc).

get_sibling_ids_from([], _HtmlNode) -> [];

get_sibling_ids_from(Ids, HtmlNode) ->
    Reversed = lists:reverse(Ids),
    Pred = fun(Id) -> Id =/= HtmlNode#html_node.node_id end,
    tl(lists:dropwhile(Pred, Reversed)).

get_siblings(HtmlNode, Tree) ->
    Parent = get_node(HtmlNode#html_node.parent_node_id, Tree),

    Ids =
    case Parent of
        true -> get_sibling_ids_from(Parent#html_node.children_nodes_ids, HtmlNode);
        false -> get_sibling_ids_from(lists:reverse(Tree#html_tree.root_nodes_ids), HtmlNode)
    end,

    Pred = fun(Id) ->
                   case get_node(Id, Tree) of
                       #html_node{} -> true;
                       _ -> false
                   end
           end,
    lists:filter(Pred, Ids).

% finds all descendant node ids recursively through the tree preserving the order
get_descendant_ids(NodeId, Tree) ->
    case get_node(NodeId, Tree) of
        #{children_nodes_ids := NodeIds} ->
            ReversedIds = lists:reverse(NodeIds),
            F = fun(Item) ->
                        get_descendant_ids(Item, Tree)
                end,
            ReversedIds ++ lists:flatmap(F, ReversedIds);
        _ ->
            []
    end.
