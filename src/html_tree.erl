-module(html_tree).
-include("html_tree.hrl").
-include("html_tree/comment.hrl").
-include("html_tree/text.hrl").

-export([build/1,
        delete_node/2,
        to_tuple_list/1,
        to_tuple/2,
        enumerable_count/1,
        enumerable_slice/1,
        enumerable_member/2,
        enumerable_reduce/3,
        patch_nodes/2]).

build({comment, Comment}) ->
  #html_tree{
     root_nodes_ids = [1],
     node_ids = [1],
     nodes = #{ 1 => #comment{content = Comment, node_id = 1} }
    };

build({Tag, Attrs, Children}) ->
    RootId = id_seeder:seed([]),
    RootNode = #html_node{type = Tag, attributes = Attrs, node_id = RootId},

    {NodeIds, Nodes} =
      build_tree(
        [RootId],
        [],
        Children,
        RootNode,
        []
      ),

      #html_tree{
         root_nodes_ids = [RootId],
         node_ids = NodeIds,
         nodes =  maps:from_list(Nodes)
        };

build(HtmlTuples) when is_list(HtmlTuples) ->
    Reducer = fun
      ({pi, _, _}, State) ->
        State;

      ({Tag, Attrs, Children}, {RootNodesIds, NodeIds, Nodes}) ->
        RootId = id_seeder:seed(NodeIds),
        RootNode = #html_node{type = Tag, attributes = Attrs, node_id = RootId},

        FullRootNodesIds = [RootId | RootNodesIds],
        FullNodeIds = [RootId | NodeIds],

        {NewNodeIds, NewNodes} =
          build_tree(
            FullNodeIds,
            Nodes,
            Children,
            RootNode,
            []
          ),

        {FullRootNodesIds, NewNodeIds, NewNodes};

      (Text, {RootNodesIds, NodeIds, Nodes}) when is_binary(Text) ->
        RootId = id_seeder:seed(NodeIds),
        RootNode = #text{content = Text, node_id = RootId},

        FullRootNodesIds = [RootId | RootNodesIds],
        FullNodeIds = [RootId | NodeIds],

        {NewNodeIds, NewNodes} =
          build_tree(
            FullNodeIds,
            Nodes,
            [],
            RootNode,
            []
          ),

        {FullRootNodesIds, NewNodeIds, NewNodes};

      ({comment, Comment}, {RootNodesIds, NodeIds, Nodes}) ->
        RootId = id_seeder:seed(NodeIds),
        RootNode = #comment{content = Comment, node_id = RootId},

        FullRootNodesIds = [RootId | RootNodesIds],
        FullNodeIds = [RootId | NodeIds],

        {NewNodeIds, NewNodes} =
          build_tree(
            FullNodeIds,
            Nodes,
            [],
            RootNode,
            []
          ),

        {FullRootNodesIds, NewNodeIds, NewNodes};

      (_, State) -> State
    end,

    {RootNodesIds, NodeIds, Nodes} = lists:foldl(Reducer, {[], [], []}, HtmlTuples),

    #html_tree{
      root_nodes_ids = RootNodesIds,
      node_ids = NodeIds,
      nodes = maps:from_list(Nodes)
    };

build(_) -> #html_tree{}.

delete_node(Tree, HtmlNode) ->
    do_delete(Tree, [HtmlNode], []).

to_tuple_list(HtmlTree) ->
  ReversedTree = lists:reverse(HtmlTree#html_tree.root_nodes_ids),
  Pred = fun(NodeId) ->
             Root = maps:get(NodeId, HtmlTree#html_tree.nodes),
             html_tree:to_tuple(HtmlTree, Root)
         end,
  lists:map(Pred, ReversedTree).

to_tuple(_Tree, #text{content = Text}) -> Text;
to_tuple(_Tree, #comment{content = Comment}) -> {comment, Comment};

to_tuple(Tree, HtmlNode) ->
  ReversedNode = lists:reverse(HtmlNode#html_node.children_nodes_ids),
  Pred = fun(Id) ->
             to_tuple(Tree, maps:get(Id, Tree#html_tree.nodes))
         end,
  Children = lists:map(Pred, ReversedNode),

  {HtmlNode#html_node.type, HtmlNode#html_node.attributes, Children}.

do_delete(Tree, [], []) -> Tree;

do_delete(Tree, [HtmlNode | T], StackIds) ->
    NewTreeNodes = delete_node_from_nodes(Tree#html_tree.nodes, HtmlNode),
    IdsForStack = get_ids_for_delete_stack(HtmlNode),

    do_delete(
      Tree#html_tree{
        nodes = NewTreeNodes,
        node_ids = lists:delete(HtmlNode#html_node.node_id, Tree#html_tree.node_ids),
        root_nodes_ids = lists:delete(HtmlNode#html_node.node_id, Tree#html_tree.root_nodes_ids)
       },
      T,
      IdsForStack ++ StackIds
    );

do_delete(Tree, [], StackIds) ->
  TakeStackIds = maps:with(StackIds, Tree#html_tree.nodes),
  HtmlNodes = maps:values(TakeStackIds),

  do_delete(Tree, HtmlNodes, []).

delete_node_from_nodes(Nodes, HtmlNode) ->
    TreeNodes = maps:remove(HtmlNode#html_node.node_id, Nodes),
    ParentNode = maps:get(HtmlNode#html_node.parent_node_id, Nodes),

    case ParentNode =/= undefined of
      true ->
        ChildrenIds = lists:delete(HtmlNode#html_node.node_id, ParentNode#html_node.children_nodes_ids),
        NewParent = ParentNode#html_node{children_nodes_ids = ChildrenIds},
        TreeNodes#{NewParent#html_node.node_id => newParent};
      false ->
        TreeNodes
    end.

get_ids_for_delete_stack(#html_node{children_nodes_ids = Ids}) -> Ids;
get_ids_for_delete_stack(_) -> [].

build_tree(NodeIds, Nodes, [], ParentNode, []) ->
  {NodeIds, [{ParentNode#html_node.node_id, ParentNode} | Nodes]};

build_tree(NodeIds, Nodes, [{pi, _, _} | Children], ParentNode, Stack) ->
  build_tree(NodeIds, Nodes, Children, ParentNode, Stack);

build_tree(NodeIds, Nodes, [{Tag, Attrs, ChildChildren} | Children], ParentNode, Stack) ->
  NewId = id_seeder:seed(NodeIds),

  NewNode = #html_node{
               type = Tag,
               attributes = Attrs,
               node_id = NewId,
               parent_node_id = ParentNode#html_node.node_id
    },

  NewParentNode = ParentNode#html_node{
                    children_nodes_ids = [NewId | ParentNode#html_node.children_nodes_ids]
                   },

  build_tree(
    [NewId | NodeIds],
    Nodes,
    ChildChildren,
    NewNode,
    [{NewParentNode, Children} | Stack]
   );

build_tree(NodeIds, Nodes, [{comment, Comment} | Children], ParentNode, Stack) ->
  NewId = id_seeder:seed(NodeIds),
  NewNode = #comment{content = Comment, node_id = NewId, parent_node_id = ParentNode#html_node.node_id},

  NewParentNode = ParentNode#html_node{
                    children_nodes_ids = [NewId | ParentNode#html_node.children_nodes_ids]
                   },

  build_tree(
    [NewId | NodeIds],
    [{NewId, NewNode} | Nodes],
    Children,
    NewParentNode,
    Stack
   );

build_tree(NodeIds, Nodes, [Text | Children], ParentNode, Stack) when is_binary(Text) ->
    NewId = id_seeder:seed(NodeIds),
    NewNode = #text{content = Text, node_id = NewId, parent_node_id = ParentNode#html_node.node_id},

    NewParentNode = ParentNode#html_node{
      children_nodes_ids = [NewId | ParentNode#html_node.children_nodes_ids]
    },

    build_tree(
      [NewId | NodeIds],
      [{NewId, NewNode} | Nodes],
      Children,
      NewParentNode,
      Stack
    );

build_tree(NodeIds, Nodes, [_Other | Children], ParentNode, Stack) ->
    build_tree(NodeIds, Nodes, Children, ParentNode, Stack);

build_tree(NodeIds, Nodes, [], ParentNode,
         [{NextParentNode, NextParentChildren} | Stack]) ->
    build_tree(
      NodeIds,
      [{ParentNode#html_node.node_id, ParentNode} | Nodes],
      NextParentChildren,
      NextParentNode,
      Stack
    ).

patch_nodes(HtmlTree, OperationWithNodes) ->
  Reducer = fun(NodeWithOp, Tree) ->
      case NodeWithOp of
        {update, Node} ->
          put_in(Tree#html_tree.nodes, Node#html_node.node_id, Node);

        {delete, Node} ->
          delete_node(Tree, Node);

        {no_op, _Node} ->
          Tree
      end
    end,

    lists:foldl(Reducer, HtmlTree, OperationWithNodes).

%% @doc
%% Mimics the core logic of Elixir's put_in/3 for lists.
-spec put_in(list(), integer(), any()) -> list().
put_in(List, Index, NewElement) ->
    lists:replace(Index, NewElement, List).

% Enables using functions from `Enum` and `Stream` modules
enumerable_count(HtmlTree) ->
      {ok, length(HtmlTree#html_tree.node_ids)}.

enumerable_member(HtmlTree, HtmlNode = #html_node{node_id = NodeId}) ->
      ANode = maps:get(NodeId, HtmlTree#html_tree.nodes),
      {ok, ANode == HtmlNode};

enumerable_member(_, _) ->
      {ok, false}.

enumerable_slice(_) ->
      {error, ?MODULE}.

enumerable_reduce(HtmlTree, State, Fun) ->
      enumerable_do_reduce(HtmlTree#html_tree{node_ids = lists:reverse(HtmlTree#html_tree.node_ids)}, State, Fun).

enumerable_do_reduce(_, {halt, Acc}, _Fun) -> {halted, Acc};
enumerable_do_reduce(Tree, {suspend, Acc}, Fun) -> {suspended, Acc, fun(Item) -> enumerable_do_reduce(Tree, Item, Fun) end};
enumerable_do_reduce(#html_tree{node_ids = []}, {cont, Acc}, _Fun) -> {done, Acc};

enumerable_do_reduce(HtmlTree = #html_tree{node_ids = [H | T]}, {cont, Acc}, Fun) ->
      Tree = HtmlTree#html_tree{node_ids = T},
      HeadNode = maps:get(H, HtmlTree#html_tree.nodes),
      enumerable_do_reduce(Tree, Fun(HeadNode, Acc), Fun).

%   defimpl Inspect do
%     import Inspect.Algebra
%
%     def inspect(html_tree, opts) do
%       open = "#Floki.HTMLTree["
%       close = "]"
%       container_opts = [separator: "", break: :flex]
%
%       container_doc(
%         open,
%         nodes_with_tree(html_tree, html_tree.root_nodes_ids),
%         close,
%         opts,
%         &fun/2,
%         container_opts
%       )
%     end
%
%     defp fun({html_tree, %HTMLNode{} = html_node}, opts) do
%       {open, close, container_opts} = build_node(html_node, opts)
%
%       container_doc(
%         open,
%         nodes_with_tree(html_tree, html_node.children_nodes_ids),
%         close,
%         opts,
%         &fun/2,
%         container_opts
%       )
%     end
%
%     defp fun(%Comment{content: comment}, opts),
%       do: color(concat(["<!-- ", comment, " -->"]), :comment, opts)
%
%     defp fun(%Text{content: text}, opts), do: color(text, :string, opts)
%
%     defp nodes_with_tree(html_tree, nodes_ids) do
%       nodes_ids
%       |> Enum.reverse()
%       |> Enum.map(fn node_id ->
%         with %HTMLNode{} = html_node <- Map.get(html_tree.nodes, node_id) do
%           {html_tree, html_node}
%         end
%       end)
%     end
%
%     defp build_node(%HTMLNode{} = node, opts) do
%       tag_color = :map
%       attribute_color = :map
%
%       built_attributes =
%         for {name, value} <- node.attributes do
%           concat([
%             color(" #{name}=", attribute_color, opts),
%             color("\"#{value}\"", :string, opts)
%           ])
%         end
%         |> concat()
%
%       open =
%         concat([
%           color("<#{node.type}", tag_color, opts),
%           built_attributes,
%           color(">", tag_color, opts)
%         ])
%
%       close = color("</#{node.type}>", tag_color, opts)
%       container_opts = [separator: "", break: :strict]
%
%       {open, close, container_opts}
%     end
%   end
% end

