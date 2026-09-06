-module(html_parser_mochiweb).
-behaviour(html_parser).

-export([parse_document/2,
        parse_fragment/2,
        parse_fragment_with_attributes_as_maps/2,
        parse_document_with_attributes_as_maps/2]).

-moduledoc """
  The default HTML parser engine for Floki.

  This parser is historically not compliant to the HTML5 specs, but will
  parse faster and in most of the cases it will produce a valid HTML tree.

  For alternative, please check the `README.md` file.
  """.

-define(root_node, <<"floki">>).

parse_document(Html, Args) ->
    NewHtml = <<"<floki>", Html/binary, "</floki>">>,
    [{<<"floki">>, _, Children}] = floki_mochi_html:parse(NewHtml, Args),
    {ok, Children}.

  % NOTE: mochi_html cannot make a distinction of a fragment and document.
parse_fragment(Html, Args) -> parse_document(Html, Args).


parse_document_with_attributes_as_maps(Html, Args) ->
    NewProplist = [{attributes_as_maps, true} | proplists:delete(attributes_as_maps, Args)],
    parse_document(Html, NewProplist).

parse_fragment_with_attributes_as_maps(Html, Args) ->
  NewProplist = lists:keyreplace(attributes_as_maps, 1, Args, {attributes_as_maps, true}),
  parse_document(Html, NewProplist).
