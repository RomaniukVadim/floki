-module(deep_text).

-export([get/1,
         get/2,
         get/3]).

  % DeepText is a strategy to get text nodes from a HTML tree using a deep search
  % algorithm. It will get all string nodes and concat them.

% -type html_tree :: tuple | list.

get(HtmlTree) ->
  get(HtmlTree, <<>>, false).
get(HtmlTree, Sep) ->
  get(HtmlTree, Sep, false).
get(HtmlTree, Sep, IncludeInputs) ->
  Text = get_text(HtmlTree, [], Sep, IncludeInputs),
  iolist_to_binary(Text).

get_text(Text, [], _Sep, _) when is_binary(Text) -> Text;
get_text(Text, Acc, Sep, _) when is_binary(Text) -> [Acc, Sep, Text];

get_text(Nodes, Acc, Sep, Include_inputs) when is_list(Nodes) ->
  Pred = fun(Child, Istr) ->
             get_text(Child, Istr, Sep, Include_inputs)
         end,
  lists:foldl(Pred, Acc, Nodes);

get_text({comment, _}, Acc, _, _) -> Acc;
get_text({<<"br">>, _, _}, Acc, _, _) -> [Acc, <<"\n">>];

get_text({<<"input">>, Attrs, _}, Acc, _, true) ->
    [Acc, text_extractor:extract_input_value(Attrs)];

get_text({"textarea", Attrs, _}, Acc, _, true) ->
    [Acc, text_extractor:extract_input_value(Attrs)];

get_text({_, _, Nodes}, Acc, Sep, Include_inputs) ->
    get_text(Nodes, Acc, Sep, Include_inputs).
