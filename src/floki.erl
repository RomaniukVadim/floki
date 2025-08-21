-module(floki).

-export([parse_document/1,
         parse_document/2,
         parse_fragment/1,
         parse_fragment/2,
         attr/4,
         raw_html/1,
         raw_html/2,
         get_by_id/2,
         find/2,
         text/1,
         traverse_and_update/2,
         traverse_and_update/3,
         validate/2,
         is_html_node/1,
         find_and_update/3]).

-include("html_tree/html_node.hrl").
-include("selector.hrl").

-moduledoc """
  Floki is a simple HTML parser that enables search for nodes using CSS selectors.

  ## Example

  Assuming that you have the following HTML:

  ```html
  <!doctype html>
  <html>
  <body>
    <section id="content">
      <p class="headline">Floki</p>
      <a href="http://github.com/philss/floki">Github page</a>
      <span data-model="user">philss</span>
    </section>
  </body>
  </html>
  ```

  To parse this, you can use the function `floki:parse_document/1`:

  ```erlang
  # {ok, Html} = floki:parse_document(Doc)
  #   {ok,[{<<"html">>,[],
  #     [{<<"head">>,[],[]},
  #      {<<"body">>,[],
  #       [<<"\n    ">>,
  #        {<<"section">>,
  #         [{<<"id">>,<<"content">>}],
  #         [<<"\n      ">>,
  #          {<<"p">>,[{<<"class">>,<<"headline">>}],[<<"Floki">>]},
  #          <<"\n      ">>,
  #          {<<"a">>,
  #           [{<<"href">>,<<"http://github.co"...>>}],
  #           [<<"Github page">>]},
  #          <<"\n      ">>,
  #          {<<"span">>,[{<<"data-model">>,<<"user">>}],[<<"philss">>]},
  #          <<"\n    ">>]},
  #        <<"\n  \n  \n">>]}]}]}
  ```

  With this document you can perform queries such as:

    * `floki:find(html, <<"#content">>)`
    * `floki:find(html, <<".headline">>)`
    * `floki:find(html, <<"a">>)`
    * `floki:find(html, <<"[data-model=user]">>)`
    * `floki:find(html, <<"#content a">>)`
    * `floki:find(html, <<".headline, a">>)`

  Each HTML node is represented by a tuple like:

      {TagName, Attributes, ChildrenNodes}

  Example of node:

      {<<"p">>, [{<<"class">>, <<"headline">>}], [<<"Floki">>]}

  So even if the only child node is the element text, it is represented
  inside a list.
  """.

-type html_tree() :: [html_node()].

-type css_selector() :: binary() | #selector{} | [#selector{}].

is_html_node(Value) ->
    is_binary(Value) orelse tuple_size(Value) == 3 orelse
    (tuple_size(Value) == 2 andalso lists:member(element(1, Value), [pi, comment])) orelse
    (tuple_size(Value) == 4 andalso element(1, Value) == doctype).


-doc """
  Parses an HTML document from a string.

  This is the main function to get a tree from an HTML string.

  ## Options

    * `:attributes_as_maps` - Change the behaviour of the parser to return the attributes
      as maps, instead of a list of `{"key", "value"}`. Default to `false`.

    * `:html_parser` - The module of the backend that is responsible for parsing
      the HTML string. By default it is set to the built-in parser, and the module
      name is equal to `Floki.HTMLParser.Mochiweb`, or from the value of the
      application env of the same name.

      See https://github.com/philss/floki#alternative-html-parsers for more details.

    * `:parser_args` - A list of options to the parser. This can be used to pass options
      that are specific for a given parser. Defaults to an empty list.

  ## Examples

      iex> Floki.parse_document("<html><head></head><body>hello</body></html>")
      {:ok, [{"html", [], [{"head", [], []}, {"body", [], ["hello"]}]}]}

      iex> Floki.parse_document("<html><head></head><body>hello</body></html>", html_parser: Floki.HTMLParser.Mochiweb)
      {:ok, [{"html", [], [{"head", [], []}, {"body", [], ["hello"]}]}]}

      iex> Floki.parse_document(
      ...>   "<html><head></head><body class=main>hello</body></html>",
      ...>   attributes_as_maps: true,
      ...>   html_parser: Floki.HTMLParser.Mochiweb
      ...>)
      {:ok, [{"html", %{}, [{"head", %{}, []}, {"body", %{"class" => "main"}, ["hello"]}]}]}

  """.

-spec parse_document(binary()) -> {ok, html_tree()} | {error, binary()}.
parse_document(Document) ->
    parse_document(Document, []).

-spec parse_document(binary(), list()) -> {ok, html_tree()} | {error, binary()}.
parse_document(Document, Opts) ->
    case html_parser:parse_document(Document, Opts) of
        {ok, ParsedDocument} -> {ok, ParsedDocument};
        {error, Message} -> erlang:error({floki_parse_error, Message})
    end.

-doc """
  Parses an HTML fragment from a string.

  This is mostly for parsing sections of an HTML document.

  ## Options

    * `:attributes_as_maps` - Change the behaviour of the parser to return the attributes
      as maps, instead of a list of `{"key", "value"}`. Remember that maps are no longer
      ordered since OTP 26. Default to `false`.

    * `:html_parser` - The module of the backend that is responsible for parsing
      the HTML string. By default it is set to the built-in parser, and the module
      name is equal to `Floki.HTMLParser.Mochiweb`, or from the value of the
      application env of the same name.

      See https://github.com/philss/floki#alternative-html-parsers for more details.

    * `:parser_args` - A list of options to the parser. This can be used to pass options
      that are specific for a given parser. Defaults to an empty list.

  """.

-spec parse_fragment(binary()) -> {ok, html_tree()} | {error, binary()}.
parse_fragment(Fragment) ->
    parse_fragment(Fragment, []).

-spec parse_fragment(binary(), list()) -> {ok, html_tree()} | {error, binary()}.
parse_fragment(Fragment, Opts) ->
    case html_parser:parse_fragment(Fragment, Opts) of
      {ok, ParsedFragment} -> {ok, ParsedFragment};
      {error, Message} -> error(floki_parse_error, Message)
    end.

-doc """
  Converts HTML tree to raw HTML.

  Note that the resultant HTML may be different from the original one.
  Spaces after tags and doctypes are ignored.

  ## Options

    * `:encode` - A boolean option to control if special HTML characters
    should be encoded as HTML entities. Defaults to `true`.

    You can also control the encoding behaviour at the application level via
    `config :floki, :encode_raw_html, false`

    * `:pretty` - Controls if the output should be formatted, ignoring
    breaklines and spaces from the input and putting new ones in order
    to pretty format the html. Defaults to `false`.

  ## Examples

      iex> Floki.raw_html({"div", [{"class", "wrapper"}], ["my content"]})
      ~s(<div class="wrapper">my content</div>)

      iex> Floki.raw_html({"div", [{"class", "wrapper"}], ["10 > 5"]})
      ~s(<div class="wrapper">10 &gt; 5</div>)

      iex> Floki.raw_html({"div", [{"class", "wrapper"}], ["10 > 5"]}, encode: false)
      ~s(<div class="wrapper">10 > 5</div>)

      iex> Floki.raw_html({"div", [], ["\\n   ", {"span", [], "Fully indented"}, "    \\n"]}, pretty: true)
      \"\"\"
      <div>
        <span>
          Fully indented
        </span>
      </div>
      \"\"\"
  """.

-spec raw_html(html_tree() | html_node() | binary()) -> binary().
raw_html(HtmlTree) ->
    raw_html(HtmlTree, []).

-spec raw_html(html_tree() | html_node() | binary(), list()) -> binary().
raw_html(HtmlTree, Options ) ->
    raw_html:raw_html(HtmlTree, Options).

-doc """
  Find elements inside an HTML tree or string.

  ## Examples

      iex> {:ok, html} = Floki.parse_fragment("<p><span class=hint>hello</span></p>")
      iex> Floki.find(html, ".hint")
      [{"span", [{"class", "hint"}], ["hello"]}]

      iex> {:ok, html} = Floki.parse_fragment("<div id=important><div>Content</div></div>")
      iex> Floki.find(html, "#important")
      [{"div", [{"id", "important"}], [{"div", [], ["Content"]}]}]

      iex> {:ok, html} = Floki.parse_fragment("<p><a href='https://google.com'>Google</a></p>")
      iex> Floki.find(html, "a")
      [{"a", [{"href", "https://google.com"}], ["Google"]}]

      iex> Floki.find([{ "div", [], [{"a", [{"href", "https://google.com"}], ["Google"]}]}], "div a")
      [{"a", [{"href", "https://google.com"}], ["Google"]}]

  """.


find(HtmlTreeAsTuple, Selector) ->
    case is_list(HtmlTreeAsTuple) orelse is_html_node(HtmlTreeAsTuple) of
        true -> finder:find(HtmlTreeAsTuple, Selector);
        false -> undefined
    end.

-doc """
  Finds the first element in an HTML tree by id.

  Returns `nil` if no element is found.

  This is useful when there are IDs that contain special characters that
  are invalid when passed as is as a CSS selector.
  It is similar to the `getElementById` method in the browser.

  ## Examples

      iex> {:ok, html} = Floki.parse_fragment(~s[<p><span class="hint" id="id?foo_special:chars">hello</span></p>])
      iex> Floki.get_by_id(html, "id?foo_special:chars")
      {"span", [{"class", "hint"}, {"id", "id?foo_special:chars"}], ["hello"]}
      iex> Floki.get_by_id(html, "does-not-exist")
      nil

  """.

-spec get_by_id(html_tree() | html_node(), binary()) -> html_node() | undefined.
get_by_id(HtmlTreeAsTuple, Id) ->
      finder:find_by_id(HtmlTreeAsTuple, Id).

-doc """
  Changes the attribute values of the elements matched by `selector`
  with the function `mutation` and returns the whole element tree.

  ## Examples

      iex> Floki.attr([{"div", [{"id", "a"}], []}], "#a", "id", fn(id) -> String.replace(id, "a", "b") end)
      [{"div", [{"id", "b"}], []}]

      iex> Floki.attr([{"div", [{"class", "name"}], []}], "div", "id", fn _ -> "b" end)
      [{"div", [{"id", "b"}, {"class", "name"}], []}]

  """.

-spec attr(html_tree() | html_node(), css_selector(), binary(), fun((binary()) -> binary())) -> html_tree().
attr(HtmlElemTuple, Selector, AttributeName, Mutation) when is_tuple(HtmlElemTuple) ->
    attr([HtmlElemTuple], Selector, AttributeName, Mutation);

attr(HtmlTreeList, Selector, AttributeName, Mutation) when is_list(HtmlTreeList) ->
    %% 1. A function to check if an attribute is the one we're looking for.
    Predicate = fun({Name, _}) ->
                        Name =:= AttributeName
                end,

    %% 2. A function to apply the mutation to the target attribute.
    Mapper = fun({AttrName, Value}) -> {AttrName, Mutation(Value)};
                (OtherAttribute) -> OtherAttribute
             end,

    %% 3. The main update function that orchestrates the logic.
    Updater = fun({Tag, Attrs}) ->
                      ModifiedAttrs =
                      case lists:any(Predicate, Attrs) of
                          true ->
                              % Attribute exists: map and update it.
                              lists:map(Mapper, Attrs);
                          false ->
                              % Attribute doesn't exist: add it.
                              [{AttributeName, Mutation(undefined)} | Attrs]
                      end,
                      {Tag, ModifiedAttrs};
                 (Other) ->
                      Other
              end,

    find_and_update(HtmlTreeList, Selector, Updater).

-doc """
  Returns the text nodes from a HTML tree.

  By default, it will perform a deep search through the HTML tree.
  You can disable deep search with the option `deep` assigned to false.
  You can include content of script or style tags by setting the `:js` or
  `:style` flags, respectively, to true.
  You can specify a separator between nodes content.

  ## Options

    * `:deep` - A boolean option to control how deep the search for
      text is going to be. If `false`, only the level of the HTML node
      or the first level of the HTML document is going to be considered.
      Defaults to `true`.

    * `:js` - A boolean option to control if the contents of `<script>` tags
      should be considered as text. Defaults to `false`.

    * `:style` - A boolean to control if the contents of `<style>` tags
      should be considered as text. Defaults to `false`.

    * `:sep` - A separator string that is added between text nodes.
      Defaults to `""`.

    * `:include_inputs` - A boolean to control if `<input>` or `<textarea>`
      values should be included in the resultant string.
      Defaults to `false`.

    * `:html_parser` - The module of the backend that is responsible for parsing
      the HTML string. By default it is set to `Floki.HTMLParser.Mochiweb`.

  ## Examples

      iex> Floki.text({"div", [], [{"span", [], ["hello"]}, " world"]})
      "hello world"

      iex> Floki.text({"div", [], [{"span", [], ["hello"]}, " world"]}, deep: false)
      " world"

      iex> Floki.text({"div", [], [{"script", [], ["hello"]}, " world"]})
      " world"

      iex> Floki.text([{"input", [{"type", "date"}, {"value", "2017-06-01"}], []}], include_inputs: true)
      "2017-06-01"

      iex> Floki.text({"div", [], [{"script", [], ["hello"]}, " world"]}, js: true)
      "hello world"

      iex> Floki.text({"ul", [], [{"li", [], ["hello"]}, {"li", [], ["world"]}]}, sep: "-")
      "hello-world"

      iex> Floki.text([{"div", [], ["hello world"]}])
      "hello world"

      iex> Floki.text([{"p", [], ["1"]},{"p", [], ["2"]}])
      "12"

      iex> Floki.text({"div", [], [{"style", [], ["hello"]}, " world"]}, style: false)
      " world"

      iex> Floki.text({"div", [], [{"style", [], ["hello"]}, " world"]}, style: true)
      "hello world"

  """.

-spec text(html_tree() | html_node()) -> binary().
text(Html) ->
    text(Html, []).

-spec text(html_tree() | html_node(), list()) -> binary().
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


-doc """
  Searches for elements inside the HTML tree and update those that matches the selector.

  It will return the updated HTML tree.

  This function works in a way similar to `traverse_and_update`, but instead of updating
  the children nodes, it will only updates the `tag` and `attributes` of the matching nodes.

  If `fun` returns `:delete`, the HTML node will be removed from the tree.

  ## Examples

      iex> Floki.find_and_update([{"a", [{"href", "http://elixir-lang.com"}], ["Elixir"]}], "a", fn
      iex>   {"a", [{"href", href}]} ->
      iex>     {"a", [{"href", String.replace(href, "http://", "https://")}]}
      iex>   other ->
      iex>     other
      iex> end)
      [{"a", [{"href", "https://elixir-lang.com"}], ["Elixir"]}]
  """.

  -spec find_and_update(
          html_tree(),
          css_selector(),
          fun(({binary(), html_attributes()}) -> {binary(), html_attributes()} | delete)) -> html_tree().

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

-doc """
  Traverses and updates a HTML tree structure.

  This function returns a new tree structure that is the result of applying the
  given `fun` on all nodes except text nodes.
  The tree is traversed in a post-walk fashion, where the children are traversed
  before the parent.

  When the function `fun` encounters HTML tag, it receives a tuple with `{name,
  attributes, children}`, and should either return a similar tuple, a list of
  tuples to split current node or `nil` to delete it.

  The function `fun` can also encounter HTML doctype, comment or declaration and
  will receive, and should return, different tuple for these types. See the
  documentation for `t:html_comment/0`, `t:html_doctype/0` and
  `t:html_declaration/0` for details.

  **Note**: this won't update text nodes, but you can transform them when working
  with children nodes.

  ## Examples

      iex> html = [{"div", [], ["hello"]}]
      iex> Floki.traverse_and_update(html, fn
      ...>   {"div", attrs, children} -> {"p", attrs, children}
      ...>   other -> other
      ...> end)
      [{"p", [], ["hello"]}]

      iex> html = [{"div", [], [{:comment, "I am comment"}, {"span", [], ["hello"]}]}]
      iex> Floki.traverse_and_update(html, fn
      ...>   {"span", _attrs, _children} -> nil
      ...>   {:comment, text} -> {"span", [], text}
      ...>   other -> other
      ...> end)
      [{"div", [], [{"span", [], "I am comment"}]}]
  """.

  -spec traverse_and_update(
          html_node() | html_tree(),
          fun((html_node()) -> html_node() | [html_node()] | undefined)
        ) -> html_node() | html_tree().

traverse_and_update(HtmlTree, Fun)  ->
    traversal:traverse_and_update(HtmlTree, Fun).

-doc """
  Traverses and updates a HTML tree structure with an accumulator.

  This function returns a new tree structure and the final value of accumulator
  which are the result of applying the given `fun` on all nodes except text nodes.
  The tree is traversed in a post-walk fashion, where the children are traversed
  before the parent.

  When the function `fun` encounters HTML tag, it receives a tuple with
  `{name, attributes, children}` and an accumulator. It and should return a
  2-tuple like `{new_node, new_acc}`, where `new_node` is either a similar tuple
  or `nil` to delete the current node, and `new_acc` is an updated value for the
  accumulator.

  The function `fun` can also encounter HTML doctype, comment or declaration and
  will receive, and should return, different tuple for these types. See the
  documentation for `t:html_comment/0`, `t:html_doctype/0` and
  `t:html_declaration/0` for details.

  **Note**: this won't update text nodes, but you can transform them when working
  with children nodes.

  ## Examples

      iex> html = [{"div", [], [{:comment, "I am a comment"}, "hello"]}, {"div", [], ["world"]}]
      iex> Floki.traverse_and_update(html, 0, fn
      ...>   {"div", attrs, children}, acc ->
      ...>     {{"p", [{"data-count", to_string(acc)} | attrs], children}, acc + 1}
      ...>   other, acc -> {other, acc}
      ...> end)
      {[
         {"p", [{"data-count", "0"}], [{:comment, "I am a comment"}, "hello"]},
         {"p", [{"data-count", "1"}], ["world"]}
       ], 2}

      iex> html = {"div", [], [{"span", [], ["hello"]}]}
      iex> Floki.traverse_and_update(html, [deleted: 0], fn
      ...>   {"span", _attrs, _children}, acc ->
      ...>     {nil, Keyword.put(acc, :deleted, acc[:deleted] + 1)}
      ...>   tag, acc ->
      ...>     {tag, acc}
      ...> end)
      {{"div", [], []}, [deleted: 1]}
  """.

  -spec traverse_and_update(
          html_node() | html_tree(),
          TraverseAcc :: any(),
          fun((html_node(), TraverseAcc :: any()) ->
             {html_node() | [html_node()] | undefined, TraverseAcc :: any()})
        ) -> {html_node() | html_tree(), TraverseAcc :: any()}.

traverse_and_update(HtmlTree, Acc, Fun) ->
    traversal:traverse_and_update(HtmlTree, Acc, Fun).

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
