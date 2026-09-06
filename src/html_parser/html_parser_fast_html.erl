-module(html_parser_fast_html).
-behaviour(html_parser).

-export([parse_document/2,
         parse_fragment/2,
         parse_document_with_attributes_as_maps/2,
         parse_fragment_with_attributes_as_maps/2]).

parse_document(Html, Args) ->
  execute_with_module_isolated(fun(Module) -> Module:decode(Html, Args) end).

parse_fragment(Html, Args) ->
  execute_with_module_isolated(fun(Module) -> Module:decode_fragment(Html, Args) end).

parse_document_with_attributes_as_maps(_Html, _Args) ->
  error(<<"parsing with attributes as maps is not supported yet for FastHTML">>).

parse_fragment_with_attributes_as_maps(_Html, _Args) ->
  error(<<"parsing with attributes as maps is not supported yet for FastHTML">>).

%% Spawns an isolated process and deep-copies the result to prevent binary memory leaks
execute_with_module_isolated(Fun) ->
  case code:ensure_loaded(fasthtml_worker) of
    {module, Module} ->
      {Pid, Ref} = spawn_monitor(fun() ->
          case Fun(Module) of
              {ok, Result} ->
                  exit({success, deep_copy(Result)});
              {error, Message} ->
                  exit({parse_error, Message})
          end
      end),
      receive
          {'DOWN', Ref, process, Pid, {success, SafeResult}} ->
              {ok, SafeResult};
          {'DOWN', Ref, process, Pid, {parse_error, Message}} ->
              {error, Message};
          {'DOWN', Ref, process, Pid, Reason} ->
              {error, Reason}
      end;
    {error, _Reason} ->
      error(<<"Expected module fasthtml_worker to be available.">>)
  end.

deep_copy(Bin) when is_binary(Bin) -> binary:copy(Bin);
deep_copy(Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    list_to_tuple([deep_copy(X) || X <- List]);
deep_copy([H|T]) -> [deep_copy(H) | deep_copy(T)];
deep_copy(Map) when is_map(Map) ->
    maps:from_list([{deep_copy(K), deep_copy(V)} || {K, V} <- maps:to_list(Map)]);
deep_copy(Other) -> Other.
