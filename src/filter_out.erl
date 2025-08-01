-module(filter_out).
-export([filter_out/2]).

%% @doc Filters an HTML tree or node based on a CSS selector or node type.
%% If Type is 'text' or 'comment', it recursively removes those nodes.
%% Otherwise, it uses a Floki CSS Selector to remove matching elements.
-spec filter_out(floki:html_tree() | floki:html_node(), atom() | binary()) ->
  floki:html_tree() | floki:html_node().

filter_out(HtmlTreeOrNode, Type) when Type =:= text; Type =:= comment ->
    mapper(HtmlTreeOrNode, Type);

filter_out(HtmlTree, Selector) when is_list(HtmlTree) ->
    floki:find_and_update(HtmlTree, Selector, fun
        ({_Tag, _Attrs}) -> delete;
        (Other) -> Other
    end);

filter_out(HtmlNode, Selector) ->
    Found = floki:find_and_update([HtmlNode], Selector, fun
        ({_Tag, _Attrs}) -> delete;
        (Other) -> Other
    end),
    case Found of
        [H | _] -> H;
        _ -> []
    end.


%% ===================================================================
%% Internal Functions for 'text' and 'comment' removal
%% ===================================================================

%% @private The main recursive mapper. Replaces the Elixir Stream pipeline
%% with a more idiomatic and efficient Erlang list comprehension.
mapper(Nodes, Selector) when is_list(Nodes) ->
    [mapper(Node, Selector) || Node <- Nodes, filter(Node, Selector)];

mapper({Tag, Attrs, Children}, Selector) ->
    {Tag, Attrs, mapper(Children, Selector)};

mapper(Node, _) ->
    Node.

%% @private Predicate function to determine if a node should be kept (true) or discarded (false).
filter({Nodetext, _, _}, Selector) when Nodetext == Selector -> false;
filter({Nodetext, _}, Selector) when Nodetext == Selector -> false;
filter(Text, text) when is_binary(Text) -> false;
filter(_, _) -> true.
