-module(selector_tokenizer).

-export([tokenize/1]).

% It decodes a given selector and returns the tokens that represents it.
% Check the rules in "src/floki_selector_lexer.xrl"
tokenize(Selector) ->
  CharList = binary_to_list(Selector),
  TrimmedSelector =  string:trim(CharList),
  {ok, TokenList, _} = floki_selector_lexer:string(TrimmedSelector),
  TokenList.
