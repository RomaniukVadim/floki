-module(selector_pseudo_class).
-include("selector_pseudo_class.hrl").
-include("selector_functional.hrl").
-include("../html_tree/text.hrl").
-include("../html_tree.hrl").

-export([match_nth_child/3,
         match_nth_of_type/3,
         match_nth_last_child/3,
         match_nth_last_of_type/3,
         match_checked/1,
         match_disabled/1,
         match_contains/3,
         match_icontains/3,
         match_root/2,
         match_has/3]).

-define(disableable_html_nodes(X), X =:= <<"button">> orelse
        X =:= <<"input">> orelse
        X =:= <<"select">> orelse
        X =:= <<"option">> orelse
        X =:= <<"textarea">>).

match_checked(#html_node{type = <<"input">>} = HtmlNode) ->
    attribute_is_present(HtmlNode#html_node.attributes, <<"checked">>);

match_checked(#html_node{type = <<"option">>} = HtmlNode) ->
    attribute_is_present(HtmlNode#html_node.attributes, <<"selected">>);

match_checked({<<"input">>, Attributes, _Children}) ->
    attribute_is_present(Attributes, <<"checked">>);

match_checked({<<"option">>, Attributes, _Children}) ->
    attribute_is_present(Attributes, <<"selected">>);

match_checked(_) -> false.

match_disabled(#html_node{type = Type} = HtmlNode) when ?disableable_html_nodes(Type) ->
    attribute_is_present(HtmlNode#html_node.attributes, <<"disabled">>);

match_disabled({Type, Attributes, _Children}) when ?disableable_html_nodes(Type) ->
    attribute_is_present(Attributes, <<"disabled">>);

match_disabled(_HtmlNode) -> false.

match_contains(Tree, HtmlNode, #pseudo_class{value = Value}) ->
    Predicate = fun(Id) ->
        case map:get(Id, Tree#html_tree.nodes) of
          #text{content = Content} ->
                binary:match(Content, Value) =/= nomatch;
          _ -> false
        end
      end,

    lists:any(Predicate, HtmlNode#html_node.children_nodes_ids).

% Case insensitive contains
match_icontains(Tree, HtmlNode, #pseudo_class{value = Value}) ->
    DowncaseValue = list_to_binary(string:lowercase(binary_to_list(Value))),

    Predicate = fun(Id) ->
        case maps:get(Id, Tree#html_tree.nodes) of
            #text{content = Content} ->
                DowncasedContent = list_to_binary(string:lowercase(binary_to_list(Content))),
                binary:match(DowncasedContent, DowncaseValue) =/= nomatch;
            _ ->
                false
        end
    end,
    lists:any(Predicate, HtmlNode#html_node.children_nodes_ids).

%% For attribute lists, like [{href, "/"}, {class, "main"}]
attribute_is_present(Attributes, AttributeName) when is_list(Attributes) ->
    lists:keyfind(AttributeName, 1, Attributes) =/= false;

%% For attribute maps, like #{href => "/", class => "main"}
attribute_is_present(Attributes, AttributeName) when is_map(Attributes) ->
    case maps:find(AttributeName, Attributes) of
        {ok, Value} ->
            Value =/= undefined;
        error ->
            false
    end.


match_root(HtmlNode, Tree) ->
    lists:member(HtmlNode#html_node.node_id, Tree#html_tree.root_nodes_ids).

match_has(Tree, HtmlNode, #pseudo_class{value = Value} = PseudoClass) ->
    Predicate = fun(InnerSelector) ->
                        InnerPredicate = fun(Id) ->
                                                 Child = maps:get(Id, Tree#html_tree.nodes),

                                                 is_record(Child, html_node) andalso
                                                 (selector:match(Child, InnerSelector, Tree) orelse
                                                  match_has(Tree, Child, PseudoClass))
                                         end,
                        lists:any(InnerPredicate, HtmlNode#html_node.children_nodes_ids)
                end,
    lists:any(Predicate, Value).

match_nth_child(Tree, HtmlNode, #pseudo_class{value = Value}) ->
    PseudoNodes = pseudo_nodes(Tree, HtmlNode),
    PseudoNodesReverse = lists:reverse(PseudoNodes),
    NodePosition = node_position(PseudoNodesReverse, HtmlNode),
    match_position(NodePosition, Value, <<"nth-child">>).

match_nth_of_type(Tree, HtmlNode, #pseudo_class{value = Value}) ->
    PseudoNodes = pseudo_nodes(Tree, HtmlNode),
    FilterNodesByType = filter_nodes_by_type(PseudoNodes, Tree#html_tree.nodes, HtmlNode#html_node.type),
    Reverse = lists:reverse(FilterNodesByType),
    NodePosition = node_position(Reverse, HtmlNode),
    match_position(NodePosition, Value, <<"nth-of-type">>).

match_nth_last_child(Tree, HtmlNode, #pseudo_class{value = Value}) ->
    PseudoNodes = pseudo_nodes(Tree, HtmlNode),
    NodePosition = node_position(PseudoNodes, HtmlNode),
    match_position(NodePosition, Value, <<"nth-last-child">>).

match_nth_last_of_type(Tree, HtmlNode, #pseudo_class{value = Value}) ->
    PseudoNodes = pseudo_nodes(Tree, HtmlNode),
    FilterNodesByType = filter_nodes_by_type(PseudoNodes, Tree#html_tree.nodes, HtmlNode#html_node.type),
    NodePosition = node_position(FilterNodesByType, HtmlNode),
    match_position(NodePosition, Value, <<"nth-last-of-type">>).

pseudo_nodes(Tree, #html_node{parent_node_id = undefined}) ->
    filter_only_html_nodes(Tree#html_tree.root_nodes_ids, Tree#html_tree.nodes);

pseudo_nodes(Tree, #html_node{parent_node_id = ParentNodeId}) ->
    ParentNode = maps:get(ParentNodeId, Tree#html_tree.nodes, undefined),
    filter_only_html_nodes(ParentNode#html_node.children_nodes_ids, Tree#html_tree.nodes).

filter_only_html_nodes(Ids, Nodes) ->
  Pred = fun(Id) ->
      case Nodes of
        #{Id := #html_node{}} -> true;
        _ -> false
      end
    end,
  lists:filter(Pred, Ids).

filter_nodes_by_type(Ids, Nodes, Type) ->
  Pred = fun(Id) ->
      case Nodes of
        #{Id := #html_node{type = Type}} -> true;
        _ -> false
      end
    end,
  lists:filter(Pred, Ids).

match_position(RelativePosition, Value, Name) ->
  case Value of
    Position when is_integer(Position) ->
      RelativePosition == Position;

    <<"even">> ->
      (RelativePosition rem 2) == 0;

    <<"odd">> ->
      (RelativePosition rem 2) == 1;

    #functional{stream = S} ->
      lists:member(RelativePosition, S);

    Expression ->
      logger:debug("Pseudo-class ~p with expressions like ~p are not supported yet. Ignoring.",
                   [Name, Expression]),
      false
  end.

node_position(Ids, #html_node{node_id = NodeId}) ->
  Pred = fun(Id) -> Id == NodeId end,
  Position = find_index(Ids, Pred),
  Position + 1.

%% @doc Finds the 0-based index of the first element for which Pred(Elem) is true.
%% Returns the integer index or the atom 'not_found'.
find_index(List, Pred) ->
  find_index(List, Pred, 0).

find_index([], _Pred, _Index) ->
  not_found;

find_index([H | T], Pred, Index) ->
  case Pred(H) of
    true ->
      Index;
    false ->
      find_index(T, Pred, Index + 1)
  end.
