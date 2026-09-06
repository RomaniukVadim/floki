-module(html_parser_mochiweb).
-behaviour(html_parser).

-export([parse_document/2,
        parse_fragment/2,
        parse_fragment_with_attributes_as_maps/2,
        parse_document_with_attributes_as_maps/2]).

-define(root_node, <<"floki">>).

parse_document(Html, Args) ->
    NewHtml = <<"<#", ?root_node/binary, ">", Html/binary, "</#",?root_node/binary,">">>,
    %% Extract children from the wrapper tag
    [{_, _, Children}] = floki_mochi_html:parse(NewHtml, Args),
    {ok, finder:list_wrap(Children)}.

parse_fragment(Html, Args) -> parse_document(Html, Args).

parse_document_with_attributes_as_maps(Html, Args) ->
  NewProplist = [{attributes_as_maps, true} | proplists:delete(attributes_as_maps, Args)],
  parse_document(Html, NewProplist).

parse_fragment_with_attributes_as_maps(Html, Args) ->
  NewProplist = [{attributes_as_maps, true} | proplists:delete(attributes_as_maps, Args)],
  parse_document(Html, NewProplist).
