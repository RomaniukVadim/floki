-module(html_parser).

-export([parse_document/2,
         parse_fragment/2]).

-define(DEFAULT_PARSER, html_parser_mochiweb).

parse_document(Html, Opts) ->
  DefaultOpts = [{attributes_as_maps, false},
                 {html_parser, parser()}, 
                 {parser_args, []}],

  {ok, NewOpts} = floki:validate(Opts, DefaultOpts),
    ParserArgs = proplists:get_value(parser_args, NewOpts),
    Parser = proplists:get_value(html_parser, NewOpts),

    case proplists:get_value(attributes_as_maps, NewOpts) of
      true -> Parser:parse_document_with_attributes_as_maps(Html, ParserArgs);
      false -> Parser:parse_document(Html, ParserArgs)
    end.

parse_fragment(Html, Opts) ->
  DefaultOpts = [{attributes_as_maps, false},
                 {html_parser, parser()},
                 {parser_args, []}],
  {ok, NewOpts} = floki:validate(Opts, DefaultOpts),

  ParserArgs = proplists:get_value(parser_args, NewOpts),
  Parser = proplists:get_value(html_parser, NewOpts),

  case proplists:get_value(attributes_as_maps, NewOpts) of
    true -> Parser:parse_fragment_with_attributes_as_maps(Html, ParserArgs);
    false -> Parser:parse_fragment(Html, ParserArgs)
  end.

parser() ->
  application:get_env(floki, html_parser, ?DEFAULT_PARSER).
