-module(floki).

-export([parse_document/1,
         parse_document/2,
         find/2,
         text/1,
         validate/2,
         is_html_node/1,
         find_and_update/3]).

-include("html_tree/html_node.hrl").
is_html_node(Value) ->
  is_binary(Value) orelse tuple_size(Value) == 3 orelse
  (tuple_size(Value) == 2 andalso lists:member(element(1, Value), [pi, comment])) orelse
  (tuple_size(Value) == 4 andalso element(1, Value) == doctype).


parse_document(Document) ->
  parse_document(Document, []).

parse_document(Document, Opts) ->
  case html_parser:parse_document(Document, Opts) of
    {ok, ParsedDocument} -> {ok, ParsedDocument};
    {error, Message} -> erlang:error({floki_parse_error, Message})
  end.

find(HtmlTreeAsTuple, Selector) ->
  case is_list(HtmlTreeAsTuple) orelse is_html_node(HtmlTreeAsTuple) of
    true -> finder:find(HtmlTreeAsTuple, Selector);
    false -> undefined
  end.

text(Html) ->
  text(Html, []).

text(Html, Opts) ->
  Defaults = [{deep, true},
              {js, false},
              {style, true},
              {sep, <<>>}, 
              {include_inputs, false}],

  {ok, ValidOpts} = validate(Opts, Defaults),

  HtmlJs = clean_html_tree(Html, js, proplists:get_value(js, ValidOpts)),
  Cleaned_html_tree = clean_html_tree(HtmlJs, style, proplists:get_value(style, ValidOpts)),

  SearchStrategy = case proplists:get_value(deep, ValidOpts) of
                      true -> deep_text;
                      false -> flat_text 
                    end,

  SearchStrategy:get(Cleaned_html_tree,
                proplists:get_value(sep, ValidOpts), 
                proplists:get_value(include_inputs, ValidOpts)).

clean_html_tree(HtmlTree, js, true) ->
  HtmlTree;
clean_html_tree(HtmlTree, js, _) ->
  filter_out(HtmlTree, <<"script">>);

clean_html_tree(HtmlTree, style, true) -> 
  HtmlTree;
clean_html_tree(HtmlTree, style, _) ->
  filter_out(HtmlTree, <<"style">>).

filter_out(Elements, Selector) ->
    filter_out:filter_out(Elements, Selector).

find_and_update(Html_tree, Selector, Fun) ->
    Tree = html_tree:build(Html_tree),
    Results = finder:find(Tree, Selector),

    Operations_with_nodes =
    lists:map(
      fun(Node = #html_node{}) ->
          %% This clause matches Floki HTML Node records.
          case Fun({Node#html_node.type, Node#html_node.attributes}) of
            {UpdatedTag, UpdatedAttrs} ->
              UpdatedNode = Node#html_node{type = UpdatedTag, attributes = UpdatedAttrs},
              {update, UpdatedNode};
            delete ->
              {delete, Node}
          end;
         (Other) ->
          %% This is the catch-all for anything that is not an #html_node{}
          %% record, like a text binary.
          {no_op, Other}
      end,
      Results
     ),

    TreePatched = html_tree:patch_nodes(Tree, Operations_with_nodes),
    html_tree:to_tuple_list(TreePatched).

%% @doc Validates a proplist against a list of allowed keys with defaults.
%% It returns an error if the proplist contains any keys not in the
%% Allowed list. Otherwise, it returns the proplist with defaults applied.
validate(Proplist, AllowedWithOptions) ->
    ProvidedKeys = proplists:get_keys(Proplist),
    AllowedKeys = proplists:get_keys(AllowedWithOptions),

    % First, find any keys in the input that are not allowed.
    UnknownKeys = ProvidedKeys -- AllowedKeys,

    case UnknownKeys of
        [] ->
            % No unknown keys were found. Proceed to apply defaults for missing keys.
            MissingDefaults = [
                {Key, Value}
                || {Key, Value} <- AllowedWithOptions,
                   not proplists:is_defined(Key, Proplist)
            ],
            {ok, Proplist ++ MissingDefaults};
        _ ->
            % Found unknown keys, so we return an error.
            {error, {unknown_keys, UnknownKeys}}
    end.
