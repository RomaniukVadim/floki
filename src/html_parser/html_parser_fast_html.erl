-module(html_parser_fast_html).

-export([parse_document/2,
         parse_fragment/2,
         parse_document_with_attributes_as_maps/2,
         parse_fragment_with_attributes_as_maps/2]).

parse_document(Html, Args) ->
  execute_with_module(fun(Module) -> Module:decode(Html, Args) end).

parse_fragment(Html, Args) ->
  execute_with_module(fun(Module) ->
                          Module:decode_fragment(Html, Args)
                      end).

parse_document_with_attributes_as_maps(_Html, _Args) ->
  error(<<"parsing with attributes as maps is not supported yet for FastHTML">>).

parse_fragment_with_attributes_as_maps(_Html, _Args) ->
    error(<<"parsing with attributes as maps is not supported yet for FastHTML">>).

execute_with_module(Fun) ->
  case code:ensure_loaded(fasthtml_worker) of
    {module, Module} ->
      case Fun(Module) of
        {ok, Result} -> {ok, Result};
        {error, _Message} = Error -> Error
      end;

    {error, _Reason} -> error(<<"Expected module :fast_html to be available.">>)
  end.
