-module(floki_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/selector.hrl").

-define(current_parser, application:get_env(floki, html_parser, mochiweb)).
-define(plain_text_tags, [
    <<"script">>,
    <<"style">>,
    <<"title">>,
    <<"textarea">>
  ]).

-define(html, <<"
  <html>
  <head>
  <title>Test</title>
  </head>
  <body>
    <a href=\"http://foo.com/blah?hi=blah&foo=&#43;Park\" class=\"foo\">test</a>
    <div class=\"content\">
      <a href=\"http://google.com\" class=\"js-google js-cool\">Google</a>
      <a href=\"http://elixir-lang.org\" class=\"js-elixir js-cool\">Elixir lang</a>
      <a href=\"http://java.com\" class=\"js-java\">Java</a>
    </div>
  </body>
  </html>
  ">>).

-define(html_with_data_attributes, <<"
  <html>
  <head>
  <title>Test</title>
  </head>
  <body>
    <div class=\"content\">
      <a href=\"http://google.com\" class=\"js-google js-cool\" data-action=\"lolcats\">Google</a>
      <a href=\"http://elixir-lang.org\" class=\"js-elixir js-cool\">Elixir lang</a>
      <a href=\"http://java.com\" class=\"js-java\">Java</a>
    </div>
  </body>
  </html>
  ">>).

-define(html_with_img, <<"
  <html>
  <body>
  <a href=\"http://twitter.com\">
    <img src=\"http://twitter.com/logo.png\" class=\"js-twitter-logo\" />
  </a>
  <!-- this is a comment -->
  <div class=\"logo-container\">
    <img src=\"http://twitter.com/logo.png\" class=\"img-without-closing-tag\">
    <img src=\"logo.png\" id=\"logo\" />
  </div>
  </body>
  </html>
  ">>).

html_body(Body) ->
    <<"<html><head></head><body>", Body/binary, "</body></html>">>.

document(HtmlString) ->
    document(HtmlString, []).

document(HtmlString, Opts) ->
    case floki:parse_document(HtmlString, Opts) of
        {ok, [{doctype, <<"html">>, <<>>, <<>>}, {<<"html">>, _, _} = Html | _]} ->
            Html;

        {ok, [{<<"html">>, _, _} = Html | _]} ->
            Html
    end.

html_with_tag_that_should_not_have_children(Tag) ->
    html_body(
      <<"<",Tag/binary,">this is not a <tag>\nthis is also </not> a tag\n and this is also not <a></a> tag</",Tag/binary,">">>
    ).

assert_find(Document, Selector, Expected) ->
    ?assert(floki:find(Document, Selector) == Expected),

    Tree = html_tree:build(Document),

    Fun = fun(HtmlNode) -> html_tree:to_tuple(Tree, HtmlNode) end,
    List = finder:find(Tree, Selector),
    HtmlTreeResults = lists:map(Fun, List),

    ?assert(HtmlTreeResults == Expected).

% parse_document/2
parse_document_simple_html_test() ->
      Html = html_body(<<"<div><a href=\"https://dev.to\" class=\"link\">Dev.to</a><p>Content <b>here</b>!</p></div>">> ),

      {ok, Parsed} = floki:parse_document(Html),

      ?assertEqual([
               {<<"html">>, [],
                [
                  {<<"head">>, [], []},
                  {<<"body">>, [],
                   [
                     {<<"div">>, [],
                      [
                        {<<"a">>, [{<<"href">>, <<"https://dev.to">>}, {<<"class">>, <<"link">>}], [<<"Dev.to">>]},
                        {<<"p">>, [], [<<"Content ">>, {<<"b">>, [], [<<"here">>]}, <<"!">>]}
                      ]}
                   ]}
                ]}
             ], Parsed).

parse_document_html_with_xml_inside_test() ->
      Html =
        html_body(
          <<"<P><B><U><SPAN lang=\"EN-AU\" style='FONT-FAMILY: &quot;Times New Roman&quot;,serif; mso-ansi-language: EN-AU'>Overview<?xml:namespace prefix = \"o\" ns = \"urn:schemas-microsoft-com:office:office\" /?><o:p></o:p></SPAN></U></B></P>">>),

      {ok, Parsed} = floki:parse_document(Html),

      [
       {<<"html">>, [],
        [
         {<<"head">>, [], []},
         {<<"body">>, [],
          [
           {<<"p">>, [],
            [
             {<<"b">>, [],
              [
               {<<"u">>, [],
                [
                 {<<"span">>,
                  [
                   {<<"lang">>, <<"EN-AU">>},
                   {<<"style">>,
                    <<"FONT-FAMILY: \"Times New Roman\",serif; mso-ansi-language: EN-AU">>}
                  ], OverviewSection}
                ]}
              ]}
            ]}
          ]}
        ]}
      ] = Parsed,

      CurrentParser = application:get_env(floki, html_parser, mochiweb),

      case CurrentParser of
        mochiweb ->
          ?assert(OverviewSection =:=
                   [
                     <<"Overview">>,
                     {pi, <<"xml:namespace">>,
                      [
                        {<<"prefix">>, <<"o">>},
                        {<<"ns">>, <<"urn:schemas-microsoft-com:office:office">>}
                      ]},
                     {<<"o:p">>, [], []}
                   ]);

        html5ever ->
          ?assert(OverviewSection =:=
                   [
                     <<"Overview">>,
                     {comment,
                      <<"?xml:namespace prefix = \"o\" ns = \"urn:schemas-microsoft-com:office:office\" /?">>},
                     {<<"o:p">>, [], []}
                   ]);

        fast_html ->
          ?assert(OverviewSection =:=
                   [
                     <<"Overview">>,
                     {comment,
                      <<"?xml:namespace prefix = \"o\" ns = \"urn:schemas-microsoft-com:office:office\" /?">>},
                     {<<"o:p">>, [], []}
                   ])
      end.

parse_document_html_with_tags_that_are_plain_text_test() ->
      ValidateHtml = fun(Tag) ->
                             HtmlWithTag = html_with_tag_that_should_not_have_children(Tag),
                             {ok, Parsed} = floki:parse_document(HtmlWithTag),

                             ?assert(Parsed =:=
                                     [
                                      {<<"html">>, [],
                                       [
                                        {<<"head">>, [], []},
                                        {<<"body">>, [],
                                         [
                                          {Tag, [],
                                           [
                                            <<"this is not a <tag>\nthis is also </not> a tag\n and this is also not <a></a> tag">>
                                           ]}
                                         ]}
                                       ]}
                                     ])
                     end,
        [ValidateHtml(Tag) || Tag <- ?plain_text_tags].


parse_document_all_element_as_string_by_default_test() ->
      Html = html_body(<<"<div><p>Content</p><custom>Custom</custom></div>">>),

      {ok, Parsed} = floki:parse_document(Html),

      ?assert([
               {
                 <<"html">>,
                 [],
                 [
                   {<<"head">>, [], []},
                   {
                     <<"body">>,
                     [],
                     [
                       {<<"div">>, [], [{<<"p">>, [], [<<"Content">>]}, {<<"custom">>, [], [<<"Custom">>]}]}
                     ]
                   }
                 ]
               }
             ] =:= Parsed).

%% TODO: format html_atoms not working
parse_document_known_elements_as_atoms_when_html_atoms_format_argument_is_given_test() ->
      Html = html_body(<<"<div><p>Content</p><custom>Custom</custom></div>">>),

      {ok, Parsed} = floki:parse_document(Html, [{parser_args, [{format, [html_atoms]}]}]),

      ?assert([
               {
                 'html',
                 [],
                 [
                   {'head', [], []},
                   {
                     'body',
                     [],
                     [
                       {'div', [], [{'p', [], [<<"Content">>]}, {<<"custom">>, [], [<<"Custom">>]}]}
                     ]
                   }
                 ]
               }
             ] =:= Parsed).

% parse_fragment/2
parse_fragment_does_not_parse_a_table_row_with_missing_parent_table_tag_by_default_test() ->
      Html = <<"<tr><td>Column 1</td><td>Column 2</td></tr>">>,

      {ok, Parsed} = floki:parse_fragment(Html),

      ?assert([<<"Column 1Column 2">>] =:= Parsed).

parse_fragment_table_row_with_missing_parent_table_tag_when_context_is_given_test() ->
      Html = <<"<tr><td>1</td><td>2</td></tr>">>,

      {ok, Parsed} = floki:parse_fragment(Html, [{parser_args, [{context, <<"table">>}]}]),

      ?assert([
               {
                 <<"tbody">>,
                 [],
                 [{<<"tr">>, [], [{<<"td">>, [], [<<"1">>]}, {<<"td">>, [], [<<"2">>]}]}]
               }
             ] =:= Parsed).

  % Floki.raw_html/2

raw_html_test() ->
    Html = html_body(<<"<div id=\"content\"><p><a href=\"uol.com.br\" class=\"bar\"><span>UOL</span><img src=\"foo.png\"/></a></p><br/></div>">>),

    RawHtml = floki:raw_html(document(Html)),

    ?assertEqual(RawHtml, iolist_to_binary(Html)),

    HtmlWithDoctype = [
      {doctype, <<"html">>, <<>>, <<>>},
      {
        <<"html">>,
        [],
        [
          {<<"head">>, [], [{<<"title">>, [], [<<"hello">>]}]},
          {<<"body">>, [], [{<<"h1">>, [], [<<"world">>]}]}
        ]
      }
    ],

    ?assert(floki:raw_html(HtmlWithDoctype) =:=
             <<"<!DOCTYPE html><html><head><title>hello</title></head><body><h1>world</h1></body></html>">>),

    SpanWithEntities = html_body(<<"<span>&lt;video&gt; São Paulo</span>">>),

    Parsed = document(SpanWithEntities),

    ?assertEqual(floki:raw_html(Parsed), iolist_to_binary(SpanWithEntities)).

raw_html_with_plain_text_test() ->
    ?assertEqual(<<"plain text node">>, floki:raw_html(<<"plain text node">>)).

raw_html_after_find_test() ->
      HtmlString = <<"<div id=\"content\"><p><a href=\"site\" class=\"bar\"><span>lol</span><img src=\"foo.png\"/></a></p><br/></div>">>,
      HtmlBody = html_body(HtmlString),
      Document = document(HtmlBody),
      Find = floki:find(Document, <<"a">>),
      RawHtml = floki:raw_html(Find),

    ?assert(RawHtml =:= <<"<a href=\"site\" class=\"bar\"><span>lol</span><img src=\"foo.png\"/></a>">>).

raw_html_with_boolean_attribute_test() ->
    RawHtml = floki:raw_html({<<"div">>, [<<"hidden">>], []}),
    ?assert(RawHtml =:= <<"<div hidden></div>">>).

raw_html_with_attribute_as_iodata_test() ->
    RawHtml = floki:raw_html({<<"div">>, [{<<"class">>, [<<"class1">>, <<" ">>, [<<"class">>, <<"2">>]]}], []}),
    ?assert(RawHtml =:= <<"<div class=\"class1 class2\"></div>">>).

raw_html_with_self_closing_tag_without_content_test() ->
    RawHtml = floki:raw_html({<<"link">>, [{<<"href">>, <<"www.example.com">>}], []}),
    ?assert(RawHtml =:= <<"<link href=\"www.example.com\"/>">>).

raw_html_with_self_closing_tag_with_content_test() ->
    RawHtml = floki:raw_html({<<"link">>, [], [<<"www.example.com">>]}),
    ?assert(RawHtml =:= <<"<link>www.example.com</link>">>).

raw_html_with_custom_self_closing_tag_without_content_and_without_attributes_test() ->
    OriginalSelfClosingTags = application:get_env(floki, self_closing_tags),
    application:set_env(floki, self_closing_tags, [<<"shy">>]),
    RawHtml = floki:raw_html({<<"shy">>, [], []}),
    ?assert(RawHtml =:= <<"<shy/>">>),

    application:set_env(floki, self_closing_tags, OriginalSelfClosingTags).

raw_html_with_custom_self_closing_tag_without_content_test() ->
    OriginalSelfClosingTags = application:get_env(floki, self_closing_tags),
    application:set_env(floki, self_closing_tags, [<<"download">>]),
    RawHtml = floki:raw_html({<<"download">>, [{<<"href">>, <<"//www.example.com/file.zip">>}], []}),
    ?assert(RawHtml =:= <<"<download href=\"//www.example.com/file.zip\"/>">>),
    application:set_env(floki, self_closing_tags, OriginalSelfClosingTags).

raw_html_with_custom_self_closing_tag_with_content_and_with_attribute_test() ->
    OriginalSelfClosingTags = application:get_env(floki, self_closing_tags),
    application:set_env(floki, self_closing_tags, [<<"download">>]),
    RawHtml = floki:raw_html({<<"download">>, [{<<"href">>, <<"//www.example.com/file.zip">>}], [<<"Download file.zip">>]}),
    ?assert(RawHtml == <<"<download href=\"//www.example.com/file.zip\">Download file.zip</download>">>),
    application:set_env(floki, self_closing_tags, OriginalSelfClosingTags).

raw_html_with_custom_self_closing_tag_with_content_and_without_attribute_test() ->
    OriginalSelfClosingTags = application:get_env(floki, self_closing_tags),
    application:set_env(floki, self_closing_tags, [<<"strike">>]),
    RawHtml = floki:raw_html({<<"strike">>, [], [<<"stroke text">>]}),
    ?assert(RawHtml =:= <<"<strike>stroke text</strike>">>),
    application:set_env(floki, self_closing_tags, OriginalSelfClosingTags).

raw_html_with_default_self_closing_tag_that_isnt_set_while_custom_self_closing_tags_are_set_must_fail_test() ->
    OriginalSelfClosingTags = application:get_env(floki, self_closing_tags),
    application:set_env(floki, self_closing_tags, [<<"page">>]),
    RawHtml = floki:raw_html({<<"br">>, [], []}),
    ?assert(RawHtml =/= <<"<br/>">>),
    application:set_env(floki, self_closing_tags, OriginalSelfClosingTags).

raw_html_with_script_and_style_tags_test() ->
    Tree = {
      <<"body">>,
      [],
      [
        {<<"div">>, [], [<<"< \"test\" >">>]},
        {<<"script">>, [], [<<"alert(\"hello\");">>]},
        {<<"style">>, [], [<<".foo[data-attr=\"bar\"] { width: 100%; }">>]}
      ]
    },

    ?assertEqual(floki:raw_html(Tree),
             <<"<body><div>&lt; &quot;test&quot; &gt;</div><script>alert(\"hello\");</script><style>.foo[data-attr=\"bar\"] { width: 100%; }</style></body>">>).

raw_html_with_quote_and_double_quote_inside_the_attribute_test() ->
    HtmlWithAttributesContainingQuotes = <<"
     <!doctype html>
     <html>
     <head>
     <title>Test</title>
     </head>
     <body>
       <div class=\"content\">
         <span id=\"double_quoted\" data-action=\"lol 'cats' lol\"></span>
         <span id=\"single_quoted\" data-action='lol \"cats\" lol'></span>
       </div>
     </body>
     </html>
   ">>,

    Tree = document(HtmlWithAttributesContainingQuotes),
    Raw = floki:raw_html(Tree),
    RerenderedTree = document(Raw),

    ?assertEqual(floki:attribute(RerenderedTree, <<"#double_quoted">>, <<"data-action">>),  [<<"lol 'cats' lol">>]),

    ?assertEqual(floki:attribute(RerenderedTree, <<"#single_quoted">>, <<"data-action">>),
                 [<<"lol \"cats\" lol">>]),

    ?assertEqual(RerenderedTree,  Tree).

raw_html_with_both_quote_and_double_quote_inside_the_attribute_test() ->
    ExpectedHtml = <<"<html><head></head><body><span data-stuff=\"&quot;&#39;\"></span></body></html>">>,

    Tree = document(html_body(<<"<span data-stuff=\"&quot;&#39;\"></span>">>)),
    ?assertEqual(floki:raw_html(Tree), ExpectedHtml),

    Tree2 = document(html_body(<<"<span data-stuff='\"&#39;'></span>">>)),
    ?assertEqual(floki:raw_html(Tree2), ExpectedHtml),

    Tree3 = document(html_body(<<"<span data-stuff=\"&quot;'\"></span>">>)),
    ?assertEqual(floki:raw_html(Tree3), ExpectedHtml),

    ExpectedHtml2 = <<"
    <html>
      <head>
      </head>
      <body>
        <div>
          <style data-attrs-test=\"{&quot;event&quot;:&quot;buggy software&quot;,&quot;properties&quot;:{&quot;_builderButtonEvent&quot;:true}}\">
          </style>
          <a data-attrs-event=\"{&quot;event&quot;:&quot;buggy software&quot;,&quot;properties&quot;:{&quot;_builderButtonEvent&quot;:true}}\">
            Next
          </a>
        </div>
      </body>
    </html>
    ">>,

    Tree4 =
      document(
        html_body(<<"
        <div>
          <style data-attrs-test=\"{&quot;event&quot;:&quot;buggy software&quot;,&quot;properties&quot;:{&quot;_builderButtonEvent&quot;:true}}\">
          </style>
          <a data-attrs-event=\"{&quot;event&quot;:&quot;buggy software&quot;,&quot;properties&quot;:{&quot;_builderButtonEvent&quot;:true}}\">
            Next
          </a>
        </div>
        ">>)
      ),

    ?assertEqual(floki:raw_html(Tree4, [{pretty, true}]), ExpectedHtml2).

raw_html_with_rigth_arrow_test() ->
    ExpectedHtml = <<"<html><head></head><body><span data-stuff=\"&gt;\"></span></body></html>">>,
    Tree = document(html_body(<<"<span data-stuff=\">\"></span>">>)),
    ?assertEqual(floki:raw_html(Tree), ExpectedHtml).

raw_html_with_left_arrow_test() ->
    ExpectedHtml = <<"<html><head></head><body><span data-stuff=\"&lt;\"></span></body></html>">>,
    Tree = document(html_body(<<"<span data-stuff=\"<\"></span>">>)),
    ?assertEqual(floki:raw_html(Tree), ExpectedHtml).

raw_html_can_configure_encoding_test() ->
    Input = <<"<html><head></head><body>< \"test\" ></body></html>">>,
    EncodedOutput = <<"<html><head></head><body>&lt; &quot;test&quot; &gt;</body></html>">>,
    Tree = document(Input),

    ?assertEqual(floki:raw_html(Tree),  EncodedOutput),
    ?assertEqual(floki:raw_html(Tree, [{encode, true}]), EncodedOutput),
    ?assertEqual(floki:raw_html(Tree, [{encode, false}]), Input).

raw_html_encode_false_does_not_encode_attribute_values_test() ->
    Input = <<"<html><head></head><body class=\"1 > 0\">< \"test\" ></body></html>">>,
    Tree = document(Input),

    ?assertEqual(floki:raw_html(Tree, [{encode, false}]), Input).

raw_html_pretty_with_doctype_test() ->
    Html = <<"
      <!doctype html>
      <html>
      <head>
      <title>Test</title>
      </head>
      <body>
        <div class=\"content\">
          <span>
            <div>
              encoded content
              &
              '
              \"

      <span>
                <small>

      very deep content

                </small>
              </span>
    </div>

            <img src=\"file.jpg\" />
                    </span>
        </div>
      </body>
      </html>
    ">>,


    Document =  document(Html),
    PrettyHtml = floki:raw_html(Document, [{pretty, true}]),

    ?assertEqual(PrettyHtml,  <<"
           <html>
             <head>
               <title>
                 Test
               </title>
             </head>
             <body>
               <div class=\"content\">
                 <span>
                   <div>
                     encoded content
                     &amp;
                     &#39;
                     &quot;
                     <span>
                       <small>
                         very deep content
                       </small>
                     </span>
                   </div>
                   <img src=\"file.jpg\"/>
                 </span>
               </div>
             </body>
           </html>
           ">>).

raw_html_when_attributes_as_maps_options_was_used_to_parse_new_floki_v0_35_0_test() ->
    HtmlString =
    <<"<div id=\"content\"><p><a href=\"site\" class=\"bar\"><span>lol</span><img src=\"foo.png\"/></a></p><br/></div>">>,

    {ok, Parsed} = floki:parse_document(HtmlString, [{attributes_as_maps, true}]),

    % no guarantee of attribute order from a map
    Recombined =
    case floki:raw_html(Parsed) of
        <<"<div id=\"content\"><p><a class=\"bar\" href=\"site\"><span>lol</span><img src=\"foo.png\"/></a></p><br/></div>">> ->
            true;

        <<"<div id=\"content\"><p><a href=\"site\" class=\"bar\"><span>lol</span><img src=\"foo.png\"/></a></p><br/></div>">> ->
            true;

        _Other ->
            false
    end,

    ?assert(Recombined).

raw_html_treats_the_contents_of_title_tags_as_plain_text_test() ->
    HtmlString = <<"<html><head><title> <b> bold </b> text </title></head><body></body></html>">>,
    {ok, Parsed} = floki:parse_document(HtmlString),
    ?assertEqual(HtmlString, floki:raw_html(Parsed)).

  % Floki.find/2 - Classes

find_elements_with_a_given_class_test() ->
    assert_find(document(?html), <<".js-cool">>, [
      {
        <<"a">>,
        [
          {<<"href">>, <<"http://google.com">>},
          {<<"class">>, <<"js-google js-cool">>}
        ],
        [<<"Google">>]
      },
      {
        <<"a">>,
        [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
        [<<"Elixir lang">>]
      }
    ]).

find_elements_with_a_given_class_and_attributes_as_maps_test() ->
    assert_find(document(?html, [{attributes_as_maps, true}]), <<".js-cool">>, [
      {
        <<"a">>,
        #{
          <<"href">> => <<"http://google.com">>,
          <<"class">> => <<"js-google js-cool">>
        },
        [<<"Google">>]
      },
      {
        <<"a">>,
        #{<<"href">> => <<"http://elixir-lang.org">>, <<"class">> => <<"js-elixir js-cool">>},
        [<<"Elixir lang">>]
      }
    ]).

find_elements_with_two_classes_combined_test() ->
    ClassSelector = <<".js-cool.js-elixir">>,

    assert_find(document(?html), ClassSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
        [<<"Elixir lang">>]
      }
    ]).

find_elements_with_anormal_class_spacing_test() ->
    Html =
      document(
        html_body(<<"
        <div class=\"js-cool\t\t  js-elixir\"></div>
        ">>)
      ),

    ClassSelector = <<".js-cool.js-elixir">>,

    assert_find(Html, ClassSelector, [
      {
        <<"div">>,
        [{<<"class">>, <<"js-cool\t\t  js-elixir">>}],
        []
      }
    ]).

find_elements_with_a_given_class_in_html_without_html_tag_test() ->
    HtmlWithoutHtmlTag = <<"
    <h2 class=\"js-cool\">One</h2>
    <p>Two</p>
    <p>Three</p>
    ">>,

    {ok, Html} = floki:parse_fragment(HtmlWithoutHtmlTag),

    assert_find(Html, <<".js-cool">>, [{<<"h2">>, [{<<"class">>, <<"js-cool">>}], [<<"One">>]}]).

find_element_that_does_not_have_child_node_test() ->
    ClassSelector = <<".js-twitter-logo">>,

    assert_find(document(?html_with_img), ClassSelector, [
      {
        <<"img">>,
        [{<<"src">>, <<"http://twitter.com/logo.png">>}, {"class", "js-twitter-logo"}],
        []
      }
    ]).

find_element_that_does_not_close_the_tag_test() ->
    ClassSelector = <<".img-without-closing-tag">>,

    assert_find(document(?html_with_img), ClassSelector, [
      {
        <<"img">>,
        [
          {<<"src">>, <<"http://twitter.com/logo.png">>},
          {<<"class">>, <<"img-without-closing-tag">>}
        ],
        []
      }
    ]).

does_not_find_elements_test() ->
    ClassSelector = <<".nothing">>,
    assert_find(document(?html), ClassSelector, []).

find_elements_with_colon_in_class_names_test() ->
    Html =
      document(
        html_body(<<"
        <div class=\"w-56 flex justify-end astro-SCKKX6R4\"></div>
        <div class=\"m-auto max-w-7xl px-4 pt-12 pb-20 flex flex-col xl:flex-row space-y-16
        md:space-y-20 xl:space-y-0\"></div>
        <section class=\"flex flex-col xl:flex-row\"></section>
        ">>)
      ),

    assert_find(Html, <<".xl\\:flex-row.md\\:space-y-20">>, [
      {
        <<"div">>,
        [
          {
            <<"class">>,
            <<"m-auto max-w-7xl px-4 pt-12 pb-20 flex flex-col xl:flex-row space-y-16\nmd:space-y-20 xl:space-y-0">>
          }
        ],
        []
      }
    ]).

% Floki.find/2 - Tag name

select_elements_by_tag_name_test() ->
    Html = document(html_body(<<"<strong>Name</strong><a href=\"profile\">Julius</a>">>)),

    assert_find(Html, <<"a">>, [{<<"a">>, [{<<"href">>, <<"profile">>}], [<<"Julius">>]}]).

  % Floki.find/2 - ID

find_element_by_id_test() ->
    assert_find(document(?html_with_img), <<"#logo">>, [
      {
        <<"img">>,
        [{<<"src">>, <<"logo.png">>}, {<<"id">>, <<"logo">>}],
        []
      }
    ]).

find_element_by_id_when_tree_has_attributes_as_maps_test() ->
    assert_find(document(?html_with_img, [{attributes_as_maps, true}]), <<"#logo">>, [
      {
        <<"img">>,
        #{<<"src">> => <<"logo.png">>, <<"id">> => <<"logo">>},
        []
      }
    ]).

  %% Floki.find/2 - Attributes

find_elements_with_a_tag_and_a_given_attribute_value_with_shorthand_syntax_test() ->
    AttributeSelector = <<"a[data-action=lolcats]">>,

    assert_find(document(?html_with_data_attributes), AttributeSelector, [
      {
        <<"a">>,
        [
          {<<"href">>, <<"http://google.com">>},
          {<<"class">>, <<"js-google js-cool">>},
          {<<"data-action">>, <<"lolcats">>}
        ],
        [<<"Google">>]
      }
    ]).

find_elements_with_a_tag_and_a_given_attribute_value_with_tree_containing_attributes_as_maps_test() ->
    AttributeSelector = <<"a[data-action=lolcats]">>,

    assert_find(
      document(?html_with_data_attributes, [{attributes_as_maps, true}]),
      AttributeSelector,
      [
        {
          <<"a">>,
          #{
            <<"class">> => <<"js-google js-cool">>,
            <<"data-action">> => <<"lolcats">>,
            <<"href">> => <<"http://google.com">>
          },
          [<<"Google">>]
        }
      ]
    ).

find_elements_only_by_given_attribute_walue_with_shorthand_syntax_test()->
    AttributeSelector = <<"[data-action=lolcats]">>,

    assert_find(document(?html_with_data_attributes), AttributeSelector, [
      {
        <<"a">>,
        [
          {<<"href">>, <<"http://google.com">>},
          {<<"class">>, <<"js-google js-cool">>},
          {<<"data-action">>, <<"lolcats">>}
        ],
        [<<"Google">>]
      }
    ]).

find_elements_by_the_attributes_selector_test() ->
    AttributeSelector = <<"a[href|='http://elixir']">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
        [<<"Elixir lang">>]
      }
    ]).

find_elements_by_the_attributes_selector_2_test() ->
    AttributeSelector = <<"a[href^='http://g']">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://google.com">>}, {<<"class">>, <<"js-google js-cool">>}],
        [<<"Google">>]
      }
    ]).

find_elements_by_the_attributes_selector_against_a_tree_with_attributes_as_maps_test() ->
    AttributeSelector = <<"a[href^='http://g']">>,

    assert_find(document(?html, [{attributes_as_maps, true}]), AttributeSelector, [
      {
        <<"a">>,
        #{<<"href">> => <<"http://google.com">>, <<"class">> => <<"js-google js-cool">>},
        [<<"Google">>]
      }
    ]).

find_elements_by_the_attributes_selector_3_test() ->
    AttributeSelector = <<"a[href$='.org']">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
        [<<"Elixir lang">>]
      }
    ]).

find_elements_by_the_attributes_selector_4_test() ->
    AttributeSelector = <<"a[class*='google']">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://google.com">>}, {<<"class">>, <<"js-google js-cool">>}],
        [<<"Google">>]
      }
    ]).

find_elements_only_by_given_case_insensitive_attribute_value_test() ->
    AttributeSelector = <<"meta[name='robots' i]">>,
    Html = document(html_body(<<"<meta name=\"ROBOTS\" content=\"INDEX, FOLLOW, NOIMAGEINDEX\"/>">>)),

    assert_find(Html, AttributeSelector, [
      {
        <<"meta">>,
        [
          {<<"name">>, <<"ROBOTS">>},
          {<<"content">>, <<"INDEX, FOLLOW, NOIMAGEINDEX">>}
        ],
        []
      }
    ]).

find_elements_by_the_attributes_selector_with_case_insensitive_flag_test() ->
    AttributeSelector = <<"a[href|='HTTP://ELIXIR' i]">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
        [<<"Elixir lang">>]
      }
    ]).

find_elements_by_the_attributes_selector_with_case_insensitive_flag_2_test() ->
    AttributeSelector = <<"a[href^='HTTP://G' i]">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://google.com">>}, {<<"class">>, <<"js-google js-cool">>}],
        [<<"Google">>]
      }
    ]).

find_elements_by_the_attributes_selector_with_case_insensitive_flag_3_test() ->
    AttributeSelector = <<"a[href$='.ORG' i]">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
        [<<"Elixir lang">>]
      }
    ]).

find_elements_by_the_attributes_selector_with_case_insensitive_flag_4_test() ->
    AttributeSelector = <<"a[class*='GOOGLE' i]">>,

    assert_find(document(?html), AttributeSelector, [
      {
        <<"a">>,
        [{<<"href">>, <<"http://google.com">>}, {<<"class">>, <<"js-google js-cool">>}],
        [<<"Google">>]
      }
    ]).

  % Floki.find/2 - Selector with descendant combinator

get_elements_descending_the_parent_test() ->
    Doc =
      document(
        html_body(<<"
        <div id=\"first-div\">
          <div id=\"second-div\">
            <span id=\"first-span\"></span>
          </div>
          <span id=\"second-span\"></span>
        </div>
        ">>)
      ),

    Expected = [
      {<<"span">>, [{<<"id">>, <<"first-span">>}], []},
      {<<"span">>, [{<<"id">>, <<"second-span">>}], []}
    ],

    assert_find(Doc, <<"div span">>, Expected).

  % Floki.find/2 - Selector with child combinator

find_children_elements_test() ->
    Expected = [
      {
        <<"img">>,
        [{<<"src">>, <<"http://twitter.com/logo.png">>}, {<<"class">>, <<"img-without-closing-tag">>}],
        []
      },
      {
        <<"img">>,
        [
          {<<"src">>, <<"logo.png">>},
          {<<"id">>, <<"logo">>}
        ],
        []
      }
    ],

    assert_find(document(?html_with_img), <<"div.logo-container > img">>, Expected),
    assert_find(document(?html_with_img), <<"body > div.logo-container > img">>, Expected),
    assert_find(document(?html_with_img), <<"body > img">>, []).

find_only_immediate_children_elements_test() ->
    Expected = [
      {<<"img">>, [{<<"src">>, <<"http://facebook.com/logo.png">>}], []}
    ],

    Html =
      document(
        html_body(<<"
        <div>
          <p>
            <span>
              <img src=\"http://facebook.com/logo.png\" />
            </span>
          </p>
        </div>
        ">>)
      ),

    assert_find(Html, <<"div > p > img">>, []),
    assert_find(Html, <<"div > p > span > img">>, Expected).

find_a_sibling_after_immediate_child_chain_test() ->
    Expected = [
      {
        <<"img">>,
        [{<<"src">>, <<"http://twitter.com/logo.png">>}, {<<"class">>, <<"img-without-closing-tag">>}],
        []
      }
    ],

    Html =
      document(
        html_body(<<"
        <div>
          <p>
            <span>
              <img src=\"http://facebook.com/logo.png\" />
              <img src=\"http://twitter.com/logo.png\" class=\"img-without-closing-tag\" />
            </span>
          </p>
        </div>
        ">>)
      ),

    assert_find(Html, <<"div > p > span > img + img">>, Expected).

  % Floki.find/2 - Adjacent sibling combinator

find_adjacent_sibling_element_test() ->
    Html = document(html_body(<<"
              <a href=\"t\"><img src=\"/l.png\" class=\"js-l\"></a>
              <!-- comment -->
              <div class=\"l-c\"><img src=\"l.png\" class=\"img\"><img src=\"l.png\" id=\"lg\"></div>
            ">>)),

    Expected = [
      {<<"div">>, [{<<"class">>, <<"l-c">>}],
       [
         {<<"img">>, [{<<"src">>, <<"l.png">>}, {<<"class">>, <<"img">>}], []},
         {<<"img">>, [{<<"src">>, <<"l.png">>}, {<<"id">>, <<"lg">>}], []}
       ]}
    ],

    assert_find(Html, <<"a + div">>, Expected),
    assert_find(Html, <<"a + .l-c">>, Expected),

    assert_find(Html, <<"a + div #lg">>, [
      {<<"img">>, [{<<"src">>, <<"l.png">>}, {<<"id">>, <<"lg">>}], []}
    ]),

    assert_find(Html, <<"a + #lg">>, []).

  % Floki.find/2 - General sibling combinator

find_general_sibling_elements_test() ->
    Expected = [
      {<<"a">>, [{<<"href">>, <<"http://elixir-lang.org">>}, {<<"class">>, <<"js-elixir js-cool">>}],
       [<<"Elixir lang">>]},
      {<<"a">>, [{<<"href">>, <<"http://java.com">>}, {<<"class">>, <<"js-java">>}], [<<"Java">>]}
    ],

    assert_find(document(?html), <<"a.js-google ~ a">>, Expected),
    assert_find(document(?html), <<"body > div > a.js-google ~ a">>, Expected),
    assert_find(document(?html), <<"body > div ~ a">>, []),
    assert_find(document(?html), <<"a.js-java ~ a">>, []).

  % Floki.find/2 - Using groups with comma

get_multiple_elements_using_comma_test() ->
    Expected = [
      {<<"img">>, [{<<"src">>, <<"http://twitter.com/logo.png">>}, {<<"class">>, <<"js-twitter-logo">>}], []},
      {<<"img">>, [{<<"src">>, <<"logo.png">>}, {<<"id">>, <<"logo">>}], []}
    ],

    assert_find(document(?html_with_img), <<".js-twitter-logo, #logo">>, Expected),
    assert_find(document(?html_with_img), <<"#logo, .js-twitter-logo">>, Expected).

get_one_element_when_search_for_multiple_and_just_one_exist_test() ->
    Expected = [{<<"img">>, [{<<"src">>, <<"logo.png">>}, {<<"id">>, <<"logo">>}], []}],
    assert_find(document(?html_with_img), <<".js-x-logo, #logo">>, Expected).

  % Floki.find/2 - Pseudo-class

get_elements_by_nth_child_and_first_child_pseudo_classes_test() ->
    Html = <<"
    <html>
    <body>
      <a href=\"/a\">1</a>
      ignores this text
      <a href=\"/b\">2</a>
      <a href=\"/c\">3</a>
      <!-- also ignores this comment -->
      <a href=\"/d\">4</a>
      <a href=\"/e\">5</a>
      <a href=\"/f\">6</a>
      <a href=\"/g\">7</a>
    </html>
    ">>,

    assert_find(document(Html), <<"a:nth-child(2)">>, [
      {<<"a">>, [{<<"href">>, <<"/b">>}], [<<"2">>]}
    ]),

    assert_find(document(Html), <<"a:nth-child(even)">>, [
      {<<"a">>, [{<<"href">>, <<"/b">>}], [<<"2">>]},
      {<<"a">>, [{<<"href">>, <<"/d">>}], [<<"4">>]},
      {<<"a">>, [{<<"href">>, <<"/f">>}], [<<"6">>]}
    ]),

    assert_find(document(Html), <<"a:nth-child(odd)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/c">>}], [<<"3">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]},
      {<<"a">>, [{<<"href">>, <<"/g">>}], [<<"7">>]}
    ]),

    assert_find(document(Html), <<"a:first-child">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]}
    ]),

    % same as first-child
    assert_find(document(Html), <<"a:nth-child(0n+1)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]}
    ]),

    assert_find(document(Html), <<"a:nth-child(3n+4)">>, [
      {<<"a">>, [{<<"href">>, <<"/d">>}], [<<"4">>]},
      {<<"a">>, [{<<"href">>, <<"/g">>}], [<<"7">>]}
    ]).

get_root_elements_by_nth_child_and_first_child_pseudo_classes_test() ->
    {ok, Tree} = floki:parse_fragment(<<"<p>A</p><p>B</p>">>),

    assert_find(Tree, <<"p:nth-child(1)">>, [{<<"p">>, [], [<<"A">>]}]),
    assert_find(Tree, <<"p:nth-child(2)">>, [{<<"p">>, [], [<<"B">>]}]),
    assert_find(Tree, <<"p:first-child">>, [{<<"p">>, [], [<<"A">>]}]).

get_elements_by_nth_last_child_pseudo_class_test() ->
    Html = <<"
    <html>
    <body>
      <a href=\"/a\">1</a>
      ignores this text
      <a href=\"/b\">2</a>
      <a href=\"/c\">3</a>
      <!-- also ignores this comment -->
      <a href=\"/d\">4</a>
      <a href=\"/e\">5</a>
      <a href=\"/f\">6</a>
      <a href=\"/g\">7</a>
    </html>
    ">>,

    assert_find(document(Html), <<"a:nth-last-child(2)">>, [
      {<<"a">>, [{<<"href">>, <<"/f">>}], [<<"6">>]}
    ]),

    assert_find(document(Html), <<"a:nth-last-child(even)">>, [
      {<<"a">>, [{<<"href">>, <<"/b">>}], [<<"2">>]},
      {<<"a">>, [{<<"href">>, <<"/d">>}], [<<"4">>]},
      {<<"a">>, [{<<"href">>, <<"/f">>}], [<<"6">>]}
    ]),

    assert_find(document(Html), <<"a:nth-last-child(odd)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/c">>}], [<<"3">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]},
      {<<"a">>, [{<<"href">>, <<"/g">>}], [<<"7">>]}
    ]),

    assert_find(document(Html), <<"a:nth-last-child(0n+1)">>, [
      {<<"a">>, [{<<"href">>, <<"/g">>}], [<<"7">>]}
    ]),

    assert_find(document(Html), <<"a:nth-last-child(3n+4)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/d">>}], [<<"4">>]}
    ]).

get_elements_by_last_child_pseudo_class_test() ->
    Html = <<"
    <html>
    <body>
      <div>
        <p>1</p>
        <p>2</p>
      </div>
      ignores this text
      <!-- also ignores this comment -->
      <div>
        <p>3</p>
        <h2>4</h2>
      </div>
      ignores this text
      <!-- also ignores this comment -->
    </html>
    ">>,

    assert_find(document(Html), <<"p:last-child">>, [
      {<<"p">>, [], [<<"2">>]}
    ]),

    assert_find(document(Html), <<"div :last-child">>, [
      {<<"p">>, [], [<<"2">>]},
      {<<"h2">>, [], [<<"4">>]}
    ]).

get_root_elements_by_last_child_pseudo_class_test() ->
    {ok, Tree} = floki:parse_fragment(<<"<p>A</p><p>B</p>">>),
    assert_find(Tree, <<"p:last-child">>, [{<<"p">>, [], [<<"B">>]}]).

get_elements_by_nth_of_type_first_of_type_and_last_of_type_pseudo_classes_test() ->
    Html =
      document(<<"
      <html>
      <body>
        ignores this text
        <h1>Child 1</h1>
        <!-- also ignores this comment -->
        <div>Child 2</div>
        <div>Child 3</div>
        <div>Child 4</div>
        <a href=\"/a\">1</a>
        ignores this text
        <a href=\"/b\">2</a>
        <a href=\"/c\">3</a>
        <!-- also ignores this comment -->
        <a href=\"/d\">4</a>
        <a href=\"/e\">5</a>
      </body>
      </html>
      ">>),

    assert_find(Html, <<"a:nth-of-type(2)">>, [
      {<<"a">>, [{<<"href">>, <<"/b">>}], [<<"2">>]}
    ]),

    assert_find(Html, <<"a:nth-of-type(even)">>, [
      {<<"a">>, [{<<"href">>, <<"/b">>}], [<<"2">>]},
      {<<"a">>, [{<<"href">>, <<"/d">>}], [<<"4">>]}
    ]),

    assert_find(Html, <<"a:nth-of-type(odd)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/c">>}], [<<"3">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]}
    ]),

    % same as nth-of-type(odd)
    assert_find(Html, <<"a:nth-of-type(2n+1)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/c">>}], [<<"3">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]}
    ]),

    % same as first-of-type
    assert_find(Html, <<"a:nth-of-type(0n+1)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]}
    ]),

    assert_find(Html, <<"a:first-of-type">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]}
    ]),

    assert_find(Html, <<"body :first-of-type">>, [
      {<<"h1">>, [], [<<"Child 1">>]},
      {<<"div">>, [], [<<"Child 2">>]},
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]}
    ]),

    assert_find(Html, <<"body :last-of-type">>, [
      {<<"h1">>, [], [<<"Child 1">>]},
      {<<"div">>, [], [<<"Child 4">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]}
    ]).

get_root_elements_by_nth_of_type_first_of_type_and_last_of_type_pseudo_classes_test() ->
    Tree = floki:parse_fragment(<<"<p>A</p><div>B</div><p>C</p><div>D</div>">>),

    assert_find(Tree, <<":nth-of-type(1)">>, [
      {<<"p">>, [], [<<"A">>]},
      {<<"div">>, [], [<<"B">>]}
    ]),

    assert_find(Tree, <<":first-of-type">>, [
      {<<"p">>, [], [<<"A">>]},
      {<<"div">>, [], [<<"B">>]}
    ]),

    assert_find(Tree, <<":nth-of-type(2)">>, [
      {<<"p">>, [], [<<"C">>]},
      {<<"div">>, [], [<<"D">>]}
    ]),

    assert_find(Tree, <<":last-of-type">>, [
      {<<"p">>, [], [<<"C">>]},
      {<<"div">>, [], [<<"D">>]}
    ]).

get_elements_by_nth_last_of_type_pseudo_classes_test() ->
    Html =
      document(<<"
      <html>
      <body>
        ignores this text
        <h1>Child 1</h1>
        <!-- also ignores this comment -->
        <a href=\"/a\">1</a>
        <div>Child 2</div>
        <div>Child 3</div>
        <div>Child 4</div>
        ignores this text
        <a href=\"/b\">2</a>
        <a href=\"/c\">3</a>
        <!-- also ignores this comment -->
        <a href=\"/d\">4</a>
        <a href=\"/e\">5</a>
      </html>
      ">>),

    assert_find(Html, <<"a:nth-last-of-type(2)">>, [
      {<<"a">>, [{<<"href">>, <<"/d">>}], [<<"4">>]}
    ]),

    assert_find(Html, <<"div:nth-last-of-type(even)">>, [
      {<<"div">>, [], [<<"Child 3">>]}
    ]),

    assert_find(Html, <<"a:nth-last-of-type(odd)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/c">>}], [<<"3">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]}
    ]),

    assert_find(Html, <<"a:nth-last-of-type(2n+1)">>, [
      {<<"a">>, [{<<"href">>, <<"/a">>}], [<<"1">>]},
      {<<"a">>, [{<<"href">>, <<"/c">>}], [<<"3">>]},
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]}
    ]),

    assert_find(Html, <<"a:nth-last-of-type(0n+1)">>, [
      {<<"a">>, [{<<"href">>, <<"/e">>}], [<<"5">>]}
    ]).

get_root_elements_by_nth_last_of_type_pseudo_classes_test() ->
    Tree = floki:parse_fragment(<<"<p>A</p><div>B</div><p>C</p><div>D</div>">>),

    assert_find(Tree, <<":nth-last-of-type(1)">>, [
      {<<"p">>, [], [<<"C">>]},
      {<<"div">>, [], [<<"D">>]}
    ]),

    assert_find(Tree, <<":nth-last-of-type(2)">>, [
      {<<"p">>, [], [<<"A">>]},
      {<<"div">>, [], [<<"B">>]}
    ]).

not_pseudo_class_test() ->
    Html =
      document(<<"
      <html>
        <body>
          <div id=\"links\">
            <a class=\"link foo\">A foo</a>
            <a class=\"link bar\" style=\"crazyColor\">A bar</a>
            <a class=\"link baz\">A baz</a>
          </div>
        </body>
      </html>
      ">>),

    ExpectedResult = [
      {<<"a">>, [{<<"class">>, <<"link foo">>}], [<<"A foo">>]},
      {<<"a">>, [{<<"class">>, <<"link baz">>}], [<<"A baz">>]}
    ],

    assert_find(Html, <<"a.link:not(.bar)">>, ExpectedResult),
    assert_find(Html, <<"div#links > a.link:not(.bar)">>, ExpectedResult),
    assert_find(Html, <<"a.link:not(:nth-child(2))">>, ExpectedResult),
    assert_find(Html, <<"a.link:not([style*=crazy])">>, ExpectedResult).

not_pseudo_class_with_multiple_selectors_test() ->
    Html =
      document(<<"
      <html>
        <body>
          <div id=\"links\">
            <a class=\"link foo\">A foo</a>
            <a class=\"link bar\" style=\"crazyColor\">A bar</a>
            <a class=\"link baz\">A baz</a>
            <a class=\"link bin\">A bin</a>
          </div>
        </body>
      </html>
      ">>),

    FooMatch = {<<"a">>, [{<<"class">>, <<"link foo">>}], [<<"A foo">>]},
    BinMatch = {<<"a">>, [{<<"class">>, <<"link bin">>}], [<<"A bin">>]},

    assert_find(Html, <<"a.link:not(.bar, .baz)">>, [FooMatch, BinMatch]),
    assert_find(Html, <<"a.link:not(.bar,.baz)">>, [FooMatch, BinMatch]),
    assert_find(Html, <<"a.link:not(.bar):not(.baz)">>, [FooMatch, BinMatch]),
    assert_find(Html, <<"a.link:not(.bar, .bin):not(.baz)">>, [FooMatch]),
    assert_find(Html, <<"a.link:not([style*=crazy], .bin):not(.baz)">>, [FooMatch]).

contains_pseudo_class_test() ->
    Doc = document(html_body(<<"<p>One</p><p>Two</p><div>nothing<b>42</b></div>">>)),

    assert_find(Doc, <<"p:fl-contains('Two')">>, [
      {<<"p">>, [], [<<"Two">>]}
    ]).

icontains_pseudo_class_test() ->
    Doc = document(html_body(<<"<p>One</p><p>Two</p><div>nothing<b>42</b></div>">>)),

    assert_find(Doc, <<"p:fl-icontains('two')">>, [
      {<<"p">>, [], [<<"Two">>]}
    ]).

contains_psuedo_class_with_substring_test() ->
    Html =
      document(
        html_body(<<"<ul><li>A podcast</li><li>Another podcast</li><li>A video</li></ul>">>)
      ),

    Expected = [
      {<<"li">>, [], [<<"A podcast">>]},
      {<<"li">>, [], [<<"Another podcast">>]}
    ],

    assert_find(Html, <<":fl-contains(' podcast')">>, Expected).

checked_pseudo_class_test() ->
    Html =
      document(
        html_body(<<"
        <input type=\"checkbox\" name=\"1\" checked>
        <input type=\"checkbox\" name=\"2\" checked=\"checked\">
        <input type=\"checkbox\" name=\"3\">
        <input type=\"radio\" name=\"4\" checked>
        <input type=\"radio\" name=\"5\">
        <select>
          <option id=\"option-6\" selected>6</option>
          <option>7</option>
        </select>
        ">>)
      ),

    HtmlTree = html_tree:build(Html),

    Results = floki:find(Html, <<":checked">>),

    F = fun(HtmlNode) -> html_tree:to_tuple(HtmlTree, HtmlNode) end,
    HtmlTreeResults = lists:map(F, finder:find(HtmlTree, <<":checked">>)),

    ?assert([
             {<<"input">>, [{<<"type">>, <<"checkbox">>}, {<<"name">>, <<"1">>}, {<<"checked">>, _}], []},
             {<<"input">>, [{<<"type">>, <<"checkbox">>}, {<<"name">>, <<"2">>}, {<<"checked">>, _}], []},
             {<<"input">>, [{<<"type">>, <<"radio">>}, {<<"name">>, <<"4">>}, {<<"checked">>, _}], []},
             {<<"option">>, [{<<"id">>, <<"option-6">>}, {<<"selected">>, _}], [<<"6">>]}
            ] = Results),

    ?assertEqual(HtmlTreeResults,  Results).

disabled_pseudo_class_test() ->
    Html =
      document(
        html_body(<<"
        <button id=\"button-1\" disabled=\"disabled\">button 1</button>
        <button id=\"button-2\" disabled>button 2</button>
        <button id=\"button-3\">button 3</button>

        <input type=\"text\" name=\"text 1\" disabled=\"disabled\">
        <input type=\"text\" name=\"text 2\" disabled>
        <input type=\"text\" name=\"text 3\">

        <select name=\"select 1\" disabled=\"disabled\"><option value=\"option 1\">Option 1</option></select>
        <select name=\"select 2\" disabled><option value=\"option 2\">Option 2</option></select>
        <select name=\"select 3\"><option value=\"option 3\">Option 3</option></select>

        <select name=\"select 4\"><option value=\"option 4\" disabled=\"disabled\">Option 4</option></select>
        <select name=\"select 5\"><option value=\"option 5\" disabled>Option 5</option></select>
        <select name=\"select 6\"><option value=\"option 6\">Option 6</option></select>

        <textarea name=\"text area 1\" disabled=\"disabled\">Text Area 1</textarea>
        <textarea name=\"text area 2\" disabled>Text Area 2</textarea>
        <textarea name=\"text area 3\">Text Area 3</textarea>
        ">>)
      ),

    HtmlTree = html_tree:build(Html),

    Results = floki:find(Html, <<":disabled">>),

    F = fun(HtmlNode) -> html_tree:to_tuple(HtmlTree, HtmlNode) end,
    HtmlTreeResults = lists:map(F, finder:find(HtmlTree, <<":disabled">>)),

    ?assert([
             {<<"button">>, [{<<"id">>, <<"button-1">>}, {<<"disabled">>, _}], [<<"button 1">>]},
             {<<"button">>, [{<<"id">>, <<"button-2">>}, {<<"disabled">>, _}], [<<"button 2">>]},
             {<<"input">>, [{<<"type">>, <<"text">>}, {<<"name">>, <<"text 1">>}, {<<"disabled">>, _}], []},
             {<<"input">>, [{<<"type">>, <<"text">>}, {<<"name">>, <<"text 2">>}, {<<"disabled">>, _}], []},
             {<<"select">>, [{<<"name">>, <<"select 1">>}, {<<"disabled">>, _}],
              [{<<"option">>, [{<<"value">>, <<"option 1">>}], [<<"Option 1">>]}]},
             {<<"select">>, [{<<"name">>, <<"select 2">>}, {<<"disabled">>, _}],
              [{<<"option">>, [{<<"value">>, <<"option 2">>}], [<<"Option 2">>]}]},
             {<<"option">>, [{<<"value">>, <<"option 4">>}, {<<"disabled">>, _}], [<<"Option 4">>]},
             {<<"option">>, [{<<"value">>, <<"option 5">>}, {<<"disabled">>, _}], [<<"Option 5">>]},
             {<<"textarea">>, [{<<"name">>, <<"text area 1">>}, {<<"disabled">>, _}], [<<"Text Area 1">>]},
             {<<"textarea">>, [{<<"name">>, <<"text area 2">>}, {<<"disabled">>, _}], [<<"Text Area 2">>]}
           ] = Results),

    ?assertEqual(HtmlTreeResults, Results).

root_pseudo_class_test() ->
    Doc = document(html_body(<<"<div><div>a</div><div>b</div></div>">>)),

    assert_find(Doc, <<":root>body>div>div">>, [
      {<<"div">>, [], [<<"a">>]},
      {<<"div">>, [], [<<"b">>]}
    ]).

has_pseudo_class_simple_test() ->
    RawHtml =
      <<"
      <div>
        <h1>Header</h1>
        <p class=\"foo\">some data</p>
      </div>
      <div>
        <h2>Header 2</h2>
        <img src=\"https://example.com\"></img>
      </div>
      <div>
        <h3>Header 3</h3>
        <p class=\"bar\">some data</p>
      </div>
      <div>
        <img src=\"picture.jpg\"></img>
        <input type=\"checkbox\" checked></input>
      </div>
      ">>,
      Replaced = re:replace(RawHtml, "\\n|\\s{2,}", "", [global, {return, binary}]),
      HtmlBody = html_body(Replaced),
      Html = document(HtmlBody),

    assert_find(Html, <<"div:has(h1)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [{<<"class">>, <<"foo">>}], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(h2)">>, [
      {<<"div">>, [],
       [
         {<<"h2">>, [], [<<"Header 2">>]},
         {<<"img">>, [{<<"src">>, <<"https://example.com">>}], []}
       ]}
    ]),

    assert_find(Html, <<"div:has(p)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [{<<"class">>, <<"foo">>}], [<<"some data">>]}
       ]},
      {<<"div">>, [],
       [
         {<<"h3">>, [], [<<"Header 3">>]},
         {<<"p">>, [{<<"class">>, <<"bar">>}], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(p.foo)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [{<<"class">>, <<"foo">>}], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(img[src='https://example.com'])">>, [
      {<<"div">>, [],
       [
         {<<"h2">>, [], [<<"Header 2">>]},
         {<<"img">>, [{<<"src">>, <<"https://example.com">>}], []}
       ]}
    ]),

    CheckedValue =
      case ?current_parser of
        mochiweb -> <<"checked">>;
        _ -> <<"">>
      end,

    assert_find(Html, <<"div:has(:checked)">>, [
      {<<"div">>, [],
       [
         {<<"img">>, [{<<"src">>, <<"picture.jpg">>}], []},
         {<<"input">>, [{<<"type">>, <<"checkbox">>}, {<<"checked">>, CheckedValue}], []}
       ]}
    ]).

has_pseudo_class_with_multiple_selectors_test() ->
    RawHtml =
      <<"
      <div>
        <h1>Header</h1>
        <p>some data</p>
      </div>
      <div>
        <h2>Header 2</h2>
        <img src=\"https://example.com\"></img>
        <p>some data</p>
      </div>
      ">>,
      Replace = re:replace(RawHtml, "\\n|\\s{2,}", "", [global, {return, binary}]),
      HtmlBody = html_body(Replace),
      Html = document(HtmlBody),

    assert_find(Html, <<"div:has(h1):has(h2)">>, []),

    assert_find(Html, <<"div:has(h1, h2)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [], [<<"some data">>]}
       ]},
      {<<"div">>, [],
       [
         {<<"h2">>, [], [<<"Header 2">>]},
         {<<"img">>, [{<<"src">>, <<"https://example.com">>}], []},
         {<<"p">>, [], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(h2):has(img):has(p)">>, [
      {<<"div">>, [],
       [
         {<<"h2">>, [], [<<"Header 2">>]},
         {<<"img">>, [{<<"src">>, <<"https://example.com">>}], []},
         {<<"p">>, [], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(h2, img, p)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [], [<<"some data">>]}
       ]},
      {<<"div">>, [],
       [
         {<<"h2">>, [], [<<"Header 2">>]},
         {<<"img">>, [{<<"src">>, <<"https://example.com">>}], []},
         {<<"p">>, [], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(h1):has(p)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"div:has(p):has(h1)">>, [
      {<<"div">>, [],
       [
         {<<"h1">>, [], [<<"Header">>]},
         {<<"p">>, [], [<<"some data">>]}
       ]}
    ]).

has_pseudo_class_with_table_test() ->
    RawHtml =
      <<"
      <table>
        <tbody>
          <tr>
            <td><h1>Header</h1></td>
            <td>some data</td>
          </tr>
          <tr>
            <th class=\"empty\">No Label</th>
            <td>some data</td>
          </tr>
          <tr>
            <th><label>TEST</label></th>
            <td>fetch me pls</td>
            <td><div>ok</div></td>
          </tr>
          <tr>
            <th><div><label>NESTED</label></div></th>
            <td><div>fetch me pls</div></td>
          </tr>
        </tbody>
      </table>
      ">>,
      Replace = re:replace(RawHtml, "\\n|\\s{2,}", "", [global, {return, binary}]),
      HtmlBody = html_body(Replace),
      Html = document(HtmlBody),

    assert_find(Html, <<"tr:has(label)">>, [
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"label">>, [], [<<"TEST">>]}]},
         {<<"td">>, [], [<<"fetch me pls">>]},
         {<<"td">>, [], [{<<"div">>, [], [<<"ok">>]}]}
       ]},
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"div">>, [], [{<<"label">>, [], [<<"NESTED">>]}]}]},
         {<<"td">>, [], [{<<"div">>, [], [<<"fetch me pls">>]}]}
       ]}
    ]),

    assert_find(Html, <<"tr:has(th.empty)">>, [
      {<<"tr">>, [],
       [
         {<<"th">>, [{<<"class">>, <<"empty">>}], [<<"No Label">>]},
         {<<"td">>, [], [<<"some data">>]}
       ]}
    ]),

    assert_find(Html, <<"tr:has(h1, label)">>, [
      {<<"tr">>, [],
       [
         {<<"td">>, [], [{<<"h1">>, [], [<<"Header">>]}]},
         {<<"td">>, [], [<<"some data">>]}
       ]},
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"label">>, [], [<<"TEST">>]}]},
         {<<"td">>, [], [<<"fetch me pls">>]},
         {<<"td">>, [], [{<<"div">>, [], [<<"ok">>]}]}
       ]},
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"div">>, [], [{<<"label">>, [], [<<"NESTED">>]}]}]},
         {<<"td">>, [], [{<<"div">>, [], [<<"fetch me pls">>]}]}
       ]}
    ]),

    assert_find(Html, <<"tr:has(label):has(div)">>, [
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"label">>, [], [<<"TEST">>]}]},
         {<<"td">>, [], [<<"fetch me pls">>]},
         {<<"td">>, [], [{<<"div">>, [], [<<"ok">>]}]}
       ]},
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"div">>, [], [{<<"label">>, [], [<<"NESTED">>]}]}]},
         {<<"td">>, [], [{<<"div">>, [], [<<"fetch me pls">>]}]}
       ]}
    ]),

    assert_find(Html, <<"tr:has(*:fl-contains('TEST'))">>, [
      {<<"tr">>, [],
       [
         {<<"th">>, [], [{<<"label">>, [], [<<"TEST">>]}]},
         {<<"td">>, [], [<<"fetch me pls">>]},
         {<<"td">>, [], [{<<"div">>, [], [<<"ok">>]}]}
       ]}
    ]),

    assert_find(Html, <<"tr:has(*:fl-contains('TEST')) th ~ td">>, [
      {<<"td">>, [], [<<"fetch me pls">>]},
      {<<"td">>, [], [{<<"div">>, [], [<<"ok">>]}]}
    ]).

    %% NOTE: this parses incorrectly, parses as:
    %%   %PseudoClass{name: "has", value: [%Selector{type: "label", pseudo_classes: [%PseudoClass{name: "has", value: []}]}]}
    %% but would expect to parse as:
    %%   %PseudoClass{name: "has", value: [%Selector{type: "div", pseudo_classes: [%PseudoClass{name: "has", value: [%Selector{type: "label"}]}]}]}
    % assert_find(html, "tr:has(div:has(label))", [
    %   {"tr", [],
    %    [
    %      {"th", [], [{"div", [], [{"label", [], ["NESTED"]}]}]},
    %      {"td", [], [{"div", [], ["fetch me pls"]}]}
    %    ]}
    % ])

    %% NOTE: this does not parse, because "only simple selectors are allowed in :has() pseudo-class"
    % assert_find(html, "th:has(> label)", [
    %   {"th", [], [{"label", [], ["TEST"]}]}
    % ])

    %% NOTE: this does not parse, because "only simple selectors are allowed in :has() pseudo-class"
    % assert_find(html, "th:has(> div > label)", [
    %   {"th", [], [{"div", [], [{"label", [], ["NESTED"]}]}]}
    % ])

    %% NOTE: this parses incorrectly, because "only simple selectors are allowed in :has() pseudo-class"
    % assert_find(html, "tr:has(td + td)", [
    %   {"tr", [],
    %    [
    %      {"td", [], [{"h1", [], ["Header"]}]},
    %      {"td", [], ["some data"]}
    %    ]},
    %   {"tr", [],
    %    [
    %      {"th", [], [{"label", [], ["TEST"]}]},
    %      {"td", [], ["fetch me pls"]},
    %      {"td", [], [{"div", [], ["ok"]}]}
    %    ]}
    % ])

    %% NOTE: this parses incorrectly, parses as:
    %%  %PseudoClass{name: "not", value: [%Selector{type: "label", pseudo_classes: [%PseudoClass{name: "has", value: []}]}]}
    %% but would expect to parse as:
    %%  %PseudoClass{name: "not", value: [%Selector{type: "*", pseudo_classes: [%PseudoClass{name: "has", value: [%Selector{type: "label"}]}]}]}
    % assert_find(html, "tr:not(:has(label))", [
    %   {"tr", [], [{"th", [], ["No Label"]}, {"td", [], ["some data"]}]}
    % ])

has_pseudo_class_edge_cases_test() ->
    RawHtml =
      <<"
      <div>
        <div>
          <div>foo</div>
          <div>bar</div>
        </div>
        <div>baz</div>
      </div>
      ">>,
      Replace = re:replace(RawHtml, "\\n|\\s{2,}", "", [global, {return, binary}]),
      HtmlBody = html_body(Replace),
      Html = document(HtmlBody),

    % `:has` without any selector doesn't match any nodes (Enum.any? on an empty array returns false).
    % Firefox ignores this case, warning of a bad selector due to a dangling combinator.
    assert_find(Html, <<"div:has()">>, []),

    % `:has` with * as the selector matches all HTML nodes with HTML nodes as children.
    % This matches the behaviour of `:has(*)` in Firefox.
    assert_find(Html, <<"div:has(*)">>, [
      {<<"div">>, [],
       [
         {<<"div">>, [], [{<<"div">>, [], [<<"foo">>]}, {<<"div">>, [], [<<"bar">>]}]},
         {<<"div">>, [], [<<"baz">>]}
       ]},
      {<<"div">>, [], [{<<"div">>, [], [<<"foo">>]}, {<<"div">>, [], [<<"bar">>]}]}
    ]),

    % In this case, both the top-level div and the second-level div match the selector.
    assert_find(Html, <<"div:has(div:fl-contains('foo'))">>, [
      {<<"div">>, [],
       [
         {<<"div">>, [],
          [
            {<<"div">>, [], [<<"foo">>]},
            {<<"div">>, [], [<<"bar">>]}
          ]},
         {<<"div">>, [], [<<"baz">>]}
       ]},
      {<<"div">>, [],
       [
         {<<"div">>, [], [<<"foo">>]},
         {<<"div">>, [], [<<"bar">>]}
       ]}
    ]).

% Floki.find/2 - XML and invalid HTML

get_elements_inside_a_xml_structure_test() ->
    Xml = [
      {pi, <<"xml">>, [{<<"version">>, <<"1.0">>}, {<<"encoding">>, <<"UTF-8">>}]},
      {<<"rss">>, [{<<"version">>, <<"2.0">>}],
       [
         {<<"channel">>, [], [{<<"title">>, [], [<<"A podcast">>]}, {<<"link">>, [], [<<"www.foo.bar.com">>]}]},
         {<<"channel">>, [], [{<<"title">>, [], [<<"Another podcast">>]}, {<<"link">>, [], [<<"www.baz.com">>]}]}
       ]}
    ],

    assert_find(Xml, <<"title">>, [
      {<<"title">>, [], [<<"A podcast">>]},
      {<<"title">>, [], [<<"Another podcast">>]}
    ]).

find_elements_inside_namespaces_test() ->
    {ok, Xml} = floki:parse_fragment(<<"<x:foo><x:bar>42</x:bar></x:foo>">>),
    assert_find(Xml, <<"x | bar">>, [{<<"x:bar">>, [], [<<"42">>]}]).

find_an_inexistent_element_inside_a_invalid_html_test() ->
    {ok, Doc} = floki:parse_fragment(<<"foobar<a">>),
    assert_find(Doc, <<"a">>, []).

% Floki.find/2 - Raw selector structs

find_single_selector_structs_test() ->
    Html = document(?html),
    HtmlTree = html_tree:build(Html),

    SelectorStruct = #selector{type = <<"a">>},

    ?assertEqual(floki:find(Html, <<"a">>), floki:find(Html, SelectorStruct)),
    ?assertEqual(finder:find(HtmlTree, <<"a">>), finder:find(HtmlTree, SelectorStruct)).

find_multiple_selector_structs_test() ->
    Html = document(?html),
    HtmlTree = html_tree:build(Html),

    SelectorStruct1 = #selector{type = <<"a">>},
    SelectorStruct2 = #selector{type = <<"div">>},

    ?assertEqual(floki:find(Html, <<"a,div">>), floki:find(Html, [SelectorStruct1, SelectorStruct2])),

    ?assertEqual(finder:find(HtmlTree, <<"a,div">>),
             finder:find(HtmlTree, [SelectorStruct1, SelectorStruct2])).

% Floki.find/2 - Empty case

find_with_an_empty_selector_test() ->
    Html = document(?html),
    ?assertEqual(floki:find(Html, <<>>), []).

% Floki.get_by_id/2

get_by_id_finds_element_with_special_characters_test() ->
    Html =
      document(
        html_body(<<"
        <div id=\"my-id?with_special!char:acters\">Hello</div>
        ">>)
      ),

    ?assert({<<"div">>, [{<<"id">>, <<"my-id?with_special!char:acters">>}], [<<"Hello">>]} =
             floki:get_by_id(Html, <<"my-id?with_special!char:acters">>)),

    ?assertEqual(floki:get_by_id(Html, <<"doesn't exist">>), false).

% Floki.children/2

returns_the_children_elements_of_an_element_including_the_text_test() ->
    HtmlNode = {<<"div">>, [], [<<"a parent">>, {<<"span">>, [], [<<"a child">>]}]},

    Expected = [
      <<"a parent">>,
      {<<"span">>, [], [<<"a child">>]}
    ],

    ?assertEqual(floki:children(HtmlNode), Expected),
    ?assertEqual(floki:children(HtmlNode, [{include_text, true}]), Expected).

%    assert_raise ArgumentError, fn ->
%      Floki.children(html_node, include_text: true, unknown_option: true)
%    end
%
%    assert_raise ArgumentError, fn ->
%      Floki.children(html_node, unknown_option: true)
%    end

returns_the_children_elements_of_an_element_without_the_text_test() ->
    Html =
      document(html_body(<<"<div>a parent<span>child 1</span>some text<span>child 2</span></div>">>)),

    [Elements | _] = floki:find(Html, <<"body > div">>),

    Expected = [
      {<<"span">>, [], [<<"child 1">>]},
      {<<"span">>, [], [<<"child 2">>]}
    ],

    ?assertEqual(floki:children(Elements, [{include_text, false}]), Expected).

%%    assert_raise ArgumentError, fn ->
%%      Floki.children(elements, include_text: false, unknown_option: true)
%%    end

returns_nil_if_the_given_html_is_not_a_valid_tuple_test() ->
    ?assertEqual(floki:children([]), undefined),
    ?assertEqual(floki:children([], [{include_text, true}]), undefined),
    ?assertEqual(floki:children([], [{include_text, false}]), undefined).

  % Floki.attribute/3

get_attribute_values_from_elements_with_a_given_class_test() ->
    ClassSelector = <<".js-cool">>,
    ExpectedHrefs = [<<"http://google.com">>, <<"http://elixir-lang.org">>],

    ?assertEqual(floki:attribute(document(?html), ClassSelector, <<"href">>), ExpectedHrefs).

get_attributes_from_elements_test() ->
    ClassSelector = <<".js-cool">>,
    ExpectedHrefs = [<<"http://google.com">>, <<"http://elixir-lang.org">>],
    Elements = floki:find(document(?html), ClassSelector),

    ?assertEqual(floki:attribute(Elements, "href"), ExpectedHrefs).

  % Floki.attribute/2

get_attributes_from_an_element_found_by_id_test() ->
    Html = document(html_body(<<"<div id=important-el></div>">>)),
    Elements = floki:find(Html, "#important-el"),

    ?assertEqual(floki:attribute(Elements, <<"id">>), [<<"important-el">>]).

returns_an_empty_list_when_attribute_does_not_exist_test() ->
    ClassSelector = <<".js-cool">>,
    Elements = floki:find(document(?html), ClassSelector),

    ?assertEqual(floki:attribute(Elements, <<"title">>),  []).

 % find_and_update/3
transforms_attributes_from_selected_nodes_test() ->
      Transformation = fun
        ({<<"a">>, [{<<"href">>, Href} | Attrs]}) ->
          {<<"a">>, [{<<"href">>, binary:replace(Href, [<<"http://">>], <<"https://">>)} | Attrs]};

        (Other) -> Other
      end,

      RawChunk = <<"<div class=\"content\"><a href=\"http://elixir-lang.org\">Elixir</a><a href=\"http://github.com\">GitHub</a></div>">>,
      HtmlBody = html_body(RawChunk),
      HtmlTree = document(HtmlBody),

      Result = floki:find_and_update(HtmlTree, <<".content a">>, Transformation),

      ?assertEqual(Result, [
               {<<"html">>, [],
                [
                  {<<"head">>, [], []},
                  {<<"body">>, [],
                   [
                     {<<"div">>, [{<<"class">>, <<"content">>}],
                      [
                        {<<"a">>, [{<<"href">>, <<"https://elixir-lang.org">>}], [<<"Elixir">>]},
                        {<<"a">>, [{<<"href">>, <<"https://github.com">>}], [<<"GitHub">>]}
                      ]}
                   ]}
                ]}
             ]).

changes_the_type_of_a_given_tag_test() ->
    HtmlChunk = <<"<div><span class=\"strong\">Hello</span><span>world</span></div>">>,
    HtmlBody = html_body(HtmlChunk),
    Html = document(HtmlBody),

    UpdateFun = fun({"span", Attrs}) -> {<<"strong">>, Attrs};
               (Other) -> Other
             end,
    UpdateAndFind = floki:find_and_update(Html, <<"span.strong">>, UpdateFun),

    Result = [
              {
               <<"html">>,
               [],
               [
                {<<"head">>, [], []},
                {<<"body">>, [],
                 [
                  {<<"div">>, [],
                   [{<<"strong">>, [{<<"class">>, <<"strong">>}], [<<"Hello">>]}, {<<"span">>, [], [<<"world">>]}]}
                 ]}
               ]
              }
             ],

      ?assertEqual(UpdateAndFind,  Result).

removes_a_node_from_HTML_tree_test() ->
    RawChunk = <<"<div><span class=\"remove-me\">Hello</span><span>world</span></div>">>,
    HtmlBody = html_body(RawChunk),
    Html = document(HtmlBody),

    UpdateFun = fun({<<"span">>, [{<<"class">>, <<"remove-me">>}]}) -> delete;
                   (Other) -> Other
                end,

    FindAndUpdate = floki:find_and_update(Html, <<"span">>, UpdateFun),

    Result = [
              {
               <<"html">>,
               [],
               [
                {<<"head">>, [], []},
                {<<"body">>, [], [{<<"div">>, [], [{<<"span">>, [], [<<"world">>]}]}]}
               ]
              }
             ],

    ?assertEqual(FindAndUpdate, Result).

finding_leaf_nodes_test() ->
    Html = <<"
    <html>
    <body>
    <div id=\"messageBox\" class=\"legacyErrors\">
      <div class=\"messageBox error\">
        <h2 class=\"accessAid\">Error Message</h2>
        <p>There has been an error in your account.</p>
      </div>
    </div>
    <div id=\"main\" class=\"legacyErrors\"><p>Separate Error Message</p></div>
    </body>
    </html>
    ">>,

    assert_find(document(Html), <<".messageBox p">>, [
      {<<"p">>, [], [<<"There has been an error in your account.">>]}
    ]).

descendant_matches_are_returned_in_order_and_without_duplicates_test() ->
    Html =
      document(<<"
      <!doctype html>
      <html>
        <body>
          <div class=\"data-view\">Foo</div>
          <table summary=\"license-detail\">
            <tbody>
              <tr>
                <th>
                  <span>Name:</span>
                </th>
                <td class=\"data-view\"><span class=\"surname\">Silva</span>, <span>Joana</span><span>Maria</span></td>
              </tr>
              <tr>
                <th scope=\"row\">
                  <span>License Type:</span>
                </th>
                <td class=\"data-view\">Naturopathic Doctor</td>
              </tr>
              <tr>
                <th scope=\"row\">
                  <span>Expiration Date:</span>
                </th>
                <td class=\"data-view\">06/30/2017</td>
              </tr>
            </tbody>
          </table>
          <div class=\"data-view\">Bar</div>
        </body>
      </html>
      ">>),

    Expected = [
      {
        <<"td">>,
        [{<<"class">>, <<"data-view">>}],
        [
          {<<"span">>, [{<<"class">>, <<"surname">>}], [<<"Silva">>]},
          <<", ">>,
          {<<"span">>, [], [<<"Joana">>]},
          {<<"span">>, [], [<<"Maria">>]}
        ]
      },
      {<<"td">>, [{<<"class">>, <<"data-view">>}], [<<"Naturopathic Doctor">>]},
      {<<"td">>, [{<<"class">>, <<"data-view">>}], [<<"06/30/2017">>]}
    ],

    assert_find(Html, <<"table[summary='license-detail'] td.data-view">>, Expected).

finding_doesnt_fail_when_body_includes_unencoded_angles_test() ->
    HtmlWithWrongAnglesEncoding =
      document(
        html_body(<<"<span class=\"method-callseq\">mark # => #<Psych::Parser::Mark></span>">>)
      ),

    assert_find(HtmlWithWrongAnglesEncoding, <<"span">>, [
      {<<"span">>, [{<<"class">>, <<"method-callseq">>}], [<<"mark # => #">>, {<<"psych::parser::mark">>, [], []}]}
    ]).

html_with_xml_definition_tag_in_it_test() ->
    Html = <<"
      <!DOCTYPE html>
      <html>
      <head>
      </head>
      <body>
        <div class=\"text\">test</div>
        <?xml version=\"1.0\" encoding=\"utf-8\"?>
      </div>
      </body>
      </html>
    ">>,

    assert_find(document(Html), <<".text">>, [{<<"div">>, [{<<"class">>, <<"text">>}], [<<"test">>]}]).

finding_doesnt_fail_when_body_includes_xml_version_prefix_test() ->
    HtmlWithXmlPrefix = <<"
    <?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
    <head>
    </head>
    <body>
      <a id=\"anchor\" href=\"\">useless link</a>
    </body>
    </html>
    ">>,

    {ok, Html} = floki:parse_document(HtmlWithXmlPrefix),

    assert_find(Html, <<"#anchor">>, [{<<"a">>, [{<<"id">>, <<"anchor">>}, {<<"href">>, <<"">>}], [<<"useless link">>]}]).

we_can_produce_raw_html_if_it_has_an_xml_version_prefix_test() ->
    HtmlTree = [
      {pi, <<"xml">>, [{<<"version">>, <<"1.0">>}, {<<"encoding">>, <<"UTF-8">>}]},
      {<<"html">>,
       [
         {<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
         {<<"xml:lang">>, <<"en">>},
         {<<"lang">>, <<"en">>}
       ],
       [
         {<<"head">>, [], []},
         {<<"body">>, [], [{<<"a">>, [{<<"id">>, <<"anchor">>}, {<<"href">>, <<"">>}], [<<"useless link">>]}]}
       ]}
    ],

    ?assert(<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>", _/binary>> = floki:raw_html(HtmlTree)).

change_tag_attributes_test() ->
    Html =
      document(
        html_body(
          binary:join([
            <<"<a class=\"change\" href=\"http://not.url/changethis/\">link</a>">>,
            <<"<a href=\"http://not.url/changethisbutnotrly/\">link</a>">>,
            <<"<a class=\"change\" href=\"http://not.url/changethis/\">link</a>">>
          ], <<>>)
        )
      ),

    Expects =
      html_body(
        binary:join([
          <<"<a class=\"change\" href=\"http://not.url/changed/\">link</a>">>,
          <<"<a href=\"http://not.url/changethisbutnotrly/\">link</a>">>,
          <<"<a class=\"change\" href=\"http://not.url/changed/\">link</a>">>
        ], <<>>)
      ),

      AttrFun = fun(InnerHtml) ->
        binary:replace(InnerHtml, [<<"changethis">>], <<"changed">>)
      end,

      HtmlAttr = floki:attr(Html, <<".change">>, <<"href">>, AttrFun),
      Result = floki:raw_html(HtmlAttr),

    ?assertEqual(Result, iolist_to_binary(Expects)).

changing_attribute_dont_change_the_order_of_nodes_test() ->
    Html =
      document(
        html_body(
          <<"<p>a<em>b</em>c<a href=\"z\">d</a></p><p>e</p><p><a href=\"f\"><strong>g</strong></a>.<em>h</em>i</p><p><strong>j</strong>k<a href=\"m\">n</a>o</p><p><em>p</em>q<em>r</em>s<a href=\"t\">u</a></p>">>
        )
      ),

      HtmlAttr = floki:attr(Html, <<"a">>, <<"href">>, fun(Href) -> Href end),
      Result = hd(HtmlAttr),

    ?assertEqual(Result, Html).

 % is_html_node/1 guard
returns_true_when_html_tag_is_passed_test() ->
    ?assert(floki:is_html_node({<<"div">>, [], []})).

returns_true_when_html_comment_is_passed_test() ->
    ?assert(floki:is_html_node({comment, <<"Ok">>})).

returns_true_when_html_doctype_is_passed_test() ->
    ?assert(floki:is_html_node({doctype, <<"html">>, undefined, undefined})).

returns_true_when_html_declaration_is_passed_test() ->
    ?assert(floki:is_html_node({pi, <<"xml">>, [{<<"version">>, <<"1.0">>}]})).

returns_true_when_html_text_is_passed_test() ->
    ?assert(floki:is_html_node(<<"I am html_text">>)).

returns_false_when_ok_val_error_reason_is_supplied_test() ->
      ?assertEqual(floki:is_html_node({ok, 1}), false),
      ?assertEqual(floki:is_html_node({error, reason}), false).

parse_document_with_attributes_as_map_option_enabled_test() ->
    Html =
      html_body(<<"
      <div class=\"container\">
        <ul>
          <li class=\"link active\"><a href=\"/\">Home</a></li>
          <li class=\"link\"><a href=\"/about-us\">About us</a></li>
        </ul>
      </div>
      ">>),

    {ok, HtmlTree} = floki:parse_document(Html, [{attributes_as_maps, true}]),

    ?assertEqual(HtmlTree, [
             {<<"html">>, #{},
              [
                {<<"head">>, #{}, []},
                {<<"body">>, #{},
                 [
                   {<<"div">>, #{<<"class">> => <<"container">>},
                    [
                      {<<"ul">>, #{},
                       [
                         {<<"li">>, #{<<"class">> => <<"link active">>}, [{<<"a">>, #{<<"href">> => <<"/">>}, [<<"Home">>]}]},
                         {<<"li">>, #{<<"class">> => <<"link">>},
                          [{<<"a">>, #{<<"href">> => <<"/about-us">>}, [<<"About us">>]}]}
                       ]}
                    ]}
                 ]}
              ]}
           ]).

parse_document_with_attributes_as_map_option_enabled_and_duplicated_attributes_test() ->
    Html =
      html_body(<<"
      <div class=\"container\">
        <ul>
          <li class=\"link active\"><a href=\"/\">Home</a></li>
          <li class=\"link\" id=\"about-us\" class=\"link company\"><a href=\"/about-us\">About us</a></li>
        </ul>
      </div>
      ">>),

    {ok, HtmlTree} = floki:parse_document(Html, [{attributes_as_maps, true}]),

    % It takes the first attribute and ignores the second one.
    ?assertEqual(HtmlTree, [
             {<<"html">>, #{},
              [
                {<<"head">>, #{}, []},
                {<<"body">>, #{},
                 [
                   {<<"div">>, #{<<"class">> => <<"container">>},
                    [
                      {<<"ul">>, #{},
                       [
                         {<<"li">>, #{<<"class">> => <<"link active">>}, [{<<"a">>, #{<<"href">> => <<"/">>}, [<<"Home">>]}]},
                         {<<"li">>, #{<<"class">> => <<"link">>, <<"id">> => <<"about-us">>},
                          [{<<"a">>, #{<<"href">> => <<"/about-us">>}, [<<"About us">>]}]}
                       ]}
                    ]}
                 ]}
              ]}
           ]).

parse_fragment_containing_malformed_HTML_with_mochiweb_test() ->
    Html = <<"<spanclass=\"trade\">™ curl gelée<br><br><br></spanclass=\"trade\">">>,

    {ok, Tree} = floki:parse_fragment(Html),

    ?assertEqual(Tree, [
             {<<"spanclass">>, [{<<"=">>, <<"=">>}, {<<"\"trade\"">>, <<"\"trade\"">>}],
              [<<"™ curl gelée">>, {<<"br">>, [], []}, {<<"br">>, [], []}, {<<"br">>, [], []}]}
           ]).
