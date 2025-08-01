-module(html_parser_html5ever).

-export([parse_document/2,
         parse_fragment/2,
         parse_document_with_attributes_as_maps/2,
         parse_fragment_with_attributes_as_maps/2]).

parse_document(Html, _Args) ->
  case code:ensure_loaded(html5ever) of
    {module, Module} ->
      case apply(Module, parse, [list_to_binary(Html)]) of
        {ok, Result} -> {ok, Result};
        {error, _Message} = Error -> Error
      end;

    {error, _Reason} ->
      error(<<"Expected module Html5ever to be available.">>)
  end.

% NOTE: html5ever does not implement parse_fragment yet.
parse_fragment(Html, Args) -> parse_document(Html, Args).

parse_document_with_attributes_as_maps(Html, _Args) ->
  apply(ensure_module(), parse_with_attributes_as_maps, [Html]).

parse_fragment_with_attributes_as_maps(Html, _Args) ->
  apply(ensure_module(), parse_with_attributes_as_maps, [Html]).

ensure_module() ->
  case code:ensure_loaded(html5ever) of
    {module, Module} -> Module;
    {error, _Reason} -> error(<<"Expected module Html5ever to be available.">>)
  end.
