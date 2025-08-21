
-module(raw_html).

-export([default_self_closing_tags/0,
        self_closing_tags/0,
        raw_html/2]).

-define(default_self_closing_tags, [
                                   <<"area">>,
                                   <<"base">>,
                                   <<"br">>,
                                   <<"col">>,
                                   <<"command">>,
                                   <<"embed">>,
                                   <<"hr">>,
                                   <<"img">>,
                                   <<"input">>,
                                   <<"keygen">>,
                                   <<"link">>,
                                   <<"menuitem">>,
                                   <<"meta">>,
                                   <<"param">>,
                                   <<"source">>,
                                   <<"track">>,
                                   <<"wbr">>
                                  ]).

default_self_closing_tags() -> ?default_self_closing_tags.

self_closing_tags() ->
    CustomSelfClosingTags = application:get_env(floki, self_closing_tags),

    case is_list(CustomSelfClosingTags) of
      true -> CustomSelfClosingTags;
      false -> ?default_self_closing_tags
  end.

encoder() -> fun(Item) -> entities:encode(Item) end.
no_encoder() -> fun(Item) -> Item end.
-define(noop, "").
-define(pad_increase, 2).

raw_html(HtmlTree, Opts) ->
    Default = [{encode, use_default_encoder()},
               {pretty, false}],
    {ok, ValidOpts} = floki:validate(Opts, Default),
    HtmlTreeWrapped = finder:list_wrap(HtmlTree),

    Encoder =
    case proplists:get_value(encode, ValidOpts) of
        true -> encoder();
        false -> no_encoder()
      end,

    Pretty = proplists:get_value(pretty, ValidOpts) == true,

    Pad =
    case Pretty of
        true -> <<>>;
        false -> ?noop
    end,

    LineEnding =
      case Pretty of
        true -> <<"\n">>;
        false -> ?noop
      end,

    SelfClosingTags = self_closing_tags(),


    RawHtml = build_raw_html(HtmlTreeWrapped, [], Encoder, Pad, SelfClosingTags, LineEnding),
    Reversed = lists:reverse(RawHtml),
    list_to_binary(Reversed).

build_raw_html([], Acc, _Ancoder, _Pad, _SelfClosingTags, _LineEnding) -> Acc;

build_raw_html([String | Tail], Acc, Encoder, Pad, SelfClosingTags, LineEnding)
       when is_binary(String) ->
    Content = leftpad_content(Pad, Encoder(String), LineEnding),
    NewAcc = [Content | Acc],
    build_raw_html(Tail, NewAcc, Encoder, Pad, SelfClosingTags, LineEnding);

build_raw_html([{comment, Comment} | Tail],
               Acc, Encoder, Pad, SelfClosingTags, LineEnding) ->
    Content = [Pad, <<"<!--">>, Comment, <<"-->">>],
    NewAcc = [Content | Acc],
    build_raw_html(Tail, NewAcc, Encoder, Pad, SelfClosingTags, LineEnding);

build_raw_html([{pi, Tag, Attrs} | Tail],
               Acc, Encoder, Pad, SelfClosingTags, LineEnding) ->
    Content = [Pad, <<"<?">>, Tag, tag_attrs(Attrs, Encoder), <<"?>">>],
    NewAcc = [Content | Acc],
    build_raw_html(Tail, NewAcc, Encoder, Pad, SelfClosingTags, LineEnding);

build_raw_html([{doctype, Type, Public, System} | Tail],
               Acc, Encoder, Pad, SelfClosingTags, LineEnding) ->
    Attr =
    case {Public, System} of
        {<<"">>, <<"">>} -> [];
        {<<"">>, System} -> [<<" SYSTEM \"">>, System | <<"\"">>];
        {Public, System} -> [<<" PUBLIC \"">>, Public, <<"\" \"">>, System | <<"\"">>]
      end,

    Content = [Pad, <<"<!DOCTYPE ">>, Type, Attr, <<">">>],
    NewAcc = [Content | Acc],
    build_raw_html(Tail, NewAcc, Encoder, Pad, SelfClosingTags, LineEnding);

build_raw_html([{Type, Attrs, Children} | Tail],
               Acc, Encoder, Pad, SelfClosingTags, LineEnding) ->
    Open_tag_content = [
      tag_with_attrs(Type, Attrs, Children, Pad, Encoder, SelfClosingTags),
      LineEnding
    ],

    NewAcc = [Open_tag_content | Acc],

    FinalAcc =
      case Children of
        [] -> NewAcc;
        _ ->
          ChildrenWrapped = finder:list_wrap(Children),

          CurrEncoder =
            case type of
              <<"script">> -> no_encoder();
              <<"style">> -> no_encoder();
              <<"title">> -> no_encoder();
              _ -> encoder()
            end,

          build_raw_html(
            ChildrenWrapped,
            NewAcc,
            % Need to make sure to pass the encoder for the current node
            CurrEncoder,
            pad_increase(Pad),
            SelfClosingTags,
            LineEnding
          )
      end,

    CloseTagContent = close_end_tag(Type, Children, Pad, SelfClosingTags, LineEnding),
    ResultAcc = [CloseTagContent | FinalAcc],
    % Return the original encoder here, we don't want to propagate that
    build_raw_html(Tail, ResultAcc, Encoder, Pad, SelfClosingTags, LineEnding).

tag_attrs(AttrList, Encoder) ->
    F = fun(Item) -> build_attrs(Item, Encoder) end,
    lists:map(F, AttrList).

tag_with_attrs(Type, [], Children, Pad, _Encoder, SelfClosingTags) ->
     [Pad, <<"<">>, Type | close_open_tag(Type, Children, SelfClosingTags)];

tag_with_attrs(Type, Attrs, Children, Pad, Encoder, SelfClosingTags) ->
    [Pad, <<"<">>, Type,
     tag_attrs(Attrs, Encoder) | close_open_tag(Type, Children, SelfClosingTags)
    ].

close_open_tag(Type, [], SelfClosingTags) ->
    case lists:member(Type, SelfClosingTags) of
      true -> <<"/>">>;
      false -> <<">">>
    end;

close_open_tag(_Type, _Children, _SelfClosingTags) -> <<">">>.

close_end_tag(Type, [], Pad, SelfClosingTags, LineEnding) ->
    case lists:member(Type, SelfClosingTags) of
      true -> [];
      false -> [Pad, <<"</">>, Type, <<">">>, LineEnding]
    end;

close_end_tag(Type, _Children, Pad, _SelfClosingTags, LineEnding) ->
    [Pad, <<"</">>, Type, <<">">>, LineEnding].

build_attrs({Attr, Value}, Encoder) ->
    [$\s, Attr, <<"=\"">>, Encoder(Value) | <<"\"">>];

build_attrs(Attr, _Encoder) -> [$\s, Attr].

use_default_encoder() ->
    application:get_env(floki, encode_raw_html, true).

  % helpers
leftpad_content(?noop, Content, _LineEnding) -> Content;

leftpad_content(Pad, Content, LineEnding) ->
    Trimmed = string:trim(Content),
    ContentBin = list_to_binary(Trimmed),

    case ContentBin == <<"">> of
      true -> <<"">>;
      false -> [Pad, ContentBin, LineEnding]
    end.

pad_increase(?noop) -> ?noop;

pad_increase(<<"">>) -> <<"  ">>;
pad_increase(<<"  ">>) -> <<"    ">>;
pad_increase(<<"    ">>) -> <<"      ">>;
pad_increase(<<"      ">>) -> <<"        ">>;
pad_increase(<<"        ">>) -> <<"          ">>;
pad_increase(<<"          ">>) -> <<"            ">>;
pad_increase(<<"            ">>) -> <<"              ">>;
pad_increase(<<"              ">>) -> <<"                ">>;
pad_increase(<<"                ">>) -> <<"                  ">>;
pad_increase(<<"                  ">>) -> <<"                    ">>;
pad_increase(<<"                    ">>) -> <<"                      ">>;
pad_increase(<<"                      ">>) -> <<"                        ">>;
pad_increase(<<"                        ">>) -> <<"                          ">>;
pad_increase(<<"                          ">>) -> <<"                            ">>;
pad_increase(<<"                            ">>) -> <<"                              ">>;
pad_increase(<<"                              ">>) -> <<"                                ">>;
pad_increase(<<"                                ">>) -> <<"                                  ">>;
pad_increase(<<"                                  ">>) -> <<"                                    ">>;
pad_increase(<<"                                    ">>) -> <<"                                      ">>;
pad_increase(<<"                                      ">>) -> <<"                                        ">>;
pad_increase(<<"                                        ">>) -> <<"                                          ">>;
pad_increase(<<"                                          ">>) -> <<"                                            ">>;
pad_increase(<<"                                            ">>) -> <<"                                              ">>;
pad_increase(<<"                                              ">>) -> <<"                                                ">>;
pad_increase(<<"                                                ">>) -> <<"                                                  ">>;
pad_increase(<<"                                                  ">>) -> <<"                                                    ">>;
pad_increase(<<"                                                    ">>) -> <<"                                                      ">>;
pad_increase(<<"                                                      ">>) -> <<"                                                        ">>;
pad_increase(<<"                                                        ">>) -> <<"                                                          ">>;
pad_increase(<<"                                                          ">>) -> <<"                                                            ">>;
pad_increase(<<"                                                            ">>) -> <<"                                                              ">>;
pad_increase(<<"                                                              ">>) -> <<"                                                                ">>;
pad_increase(<<"                                                                ">>) -> <<"                                                                  ">>;
pad_increase(<<"                                                                  ">>) -> <<"                                                                    ">>;
pad_increase(<<"                                                                    ">>) -> <<"                                                                      ">>;
pad_increase(<<"                                                                      ">>) -> <<"                                                                        ">>;
pad_increase(<<"                                                                        ">>) -> <<"                                                                          ">>;
pad_increase(<<"                                                                          ">>) -> <<"                                                                            ">>;
pad_increase(<<"                                                                            ">>) -> <<"                                                                              ">>;
pad_increase(<<"                                                                              ">>) -> <<"                                                                                ">>;
pad_increase(<<"                                                                                ">>) -> <<"                                                                                  ">>;
pad_increase(<<"                                                                                  ">>) -> <<"                                                                                    ">>;
pad_increase(<<"                                                                                    ">>) -> <<"                                                                                      ">>;
pad_increase(<<"                                                                                      ">>) -> <<"                                                                                        ">>;
pad_increase(<<"                                                                                        ">>) -> <<"                                                                                          ">>;
pad_increase(<<"                                                                                          ">>) -> <<"                                                                                            ">>;
pad_increase(<<"                                                                                            ">>) -> <<"                                                                                              ">>;
pad_increase(<<"                                                                                              ">>) -> <<"                                                                                                ">>;
pad_increase(<<"                                                                                                ">>) -> <<"                                                                                                  ">>;
pad_increase(<<"                                                                                                  ">>) -> <<"                                                                                                    ">>;
pad_increase(<<"                                                                                                    ">>) -> <<"                                                                                                      ">>;
pad_increase(<<"                                                                                                      ">>) -> <<"                                                                                                        ">>;
pad_increase(<<"                                                                                                        ">>) -> <<"                                                                                                          ">>;
pad_increase(<<"                                                                                                          ">>) -> <<"                                                                                                            ">>;
pad_increase(<<"                                                                                                            ">>) -> <<"                                                                                                              ">>;
pad_increase(<<"                                                                                                              ">>) -> <<"                                                                                                                ">>;
pad_increase(<<"                                                                                                                ">>) -> <<"                                                                                                                  ">>;
pad_increase(<<"                                                                                                                  ">>) -> <<"                                                                                                                    ">>;
pad_increase(<<"                                                                                                                    ">>) -> <<"                                                                                                                      ">>;
pad_increase(<<"                                                                                                                      ">>) -> <<"                                                                                                                        ">>;
pad_increase(<<"                                                                                                                        ">>) -> <<"                                                                                                                          ">>;
pad_increase(<<"                                                                                                                          ">>) -> <<"                                                                                                                            ">>;
pad_increase(<<"                                                                                                                            ">>) -> <<"                                                                                                                              ">>;
pad_increase(<<"                                                                                                                              ">>) -> <<"                                                                                                                                ">>;
pad_increase(<<"                                                                                                                                ">>) -> <<"                                                                                                                                  ">>;
pad_increase(<<"                                                                                                                                  ">>) -> <<"                                                                                                                                    ">>;
pad_increase(<<"                                                                                                                                    ">>) -> <<"                                                                                                                                      ">>;
pad_increase(<<"                                                                                                                                      ">>) -> <<"                                                                                                                                        ">>;
pad_increase(<<"                                                                                                                                        ">>) -> <<"                                                                                                                                          ">>;
pad_increase(<<"                                                                                                                                          ">>) -> <<"                                                                                                                                            ">>;
pad_increase(<<"                                                                                                                                            ">>) -> <<"                                                                                                                                              ">>;
pad_increase(<<"                                                                                                                                              ">>) -> <<"                                                                                                                                                ">>;
pad_increase(<<"                                                                                                                                                ">>) -> <<"                                                                                                                                                  ">>;
pad_increase(<<"                                                                                                                                                  ">>) -> <<"                                                                                                                                                    ">>;
pad_increase(<<"                                                                                                                                                    ">>) -> <<"                                                                                                                                                      ">>;
pad_increase(<<"                                                                                                                                                      ">>) -> <<"                                                                                                                                                        ">>;
pad_increase(<<"                                                                                                                                                        ">>) -> <<"                                                                                                                                                          ">>;
pad_increase(<<"                                                                                                                                                          ">>) -> <<"                                                                                                                                                            ">>;
pad_increase(<<"                                                                                                                                                            ">>) -> <<"                                                                                                                                                              ">>;
pad_increase(<<"                                                                                                                                                              ">>) -> <<"                                                                                                                                                                ">>;
pad_increase(<<"                                                                                                                                                                ">>) -> <<"                                                                                                                                                                  ">>;
pad_increase(<<"                                                                                                                                                                  ">>) -> <<"                                                                                                                                                                    ">>;
pad_increase(<<"                                                                                                                                                                    ">>) -> <<"                                                                                                                                                                      ">>;
pad_increase(<<"                                                                                                                                                                      ">>) -> <<"                                                                                                                                                                        ">>;
pad_increase(<<"                                                                                                                                                                        ">>) -> <<"                                                                                                                                                                          ">>;
pad_increase(<<"                                                                                                                                                                          ">>) -> <<"                                                                                                                                                                            ">>;
pad_increase(<<"                                                                                                                                                                            ">>) -> <<"                                                                                                                                                                              ">>;
pad_increase(<<"                                                                                                                                                                              ">>) -> <<"                                                                                                                                                                                ">>;
pad_increase(<<"                                                                                                                                                                                ">>) -> <<"                                                                                                                                                                                  ">>;
pad_increase(<<"                                                                                                                                                                                  ">>) -> <<"                                                                                                                                                                                    ">>;
pad_increase(<<"                                                                                                                                                                                    ">>) -> <<"                                                                                                                                                                                      ">>;
pad_increase(<<"                                                                                                                                                                                      ">>) -> <<"                                                                                                                                                                                        ">>;
pad_increase(<<"                                                                                                                                                                                        ">>) -> <<"                                                                                                                                                                                          ">>;
pad_increase(<<"                                                                                                                                                                                          ">>) -> <<"                                                                                                                                                                                            ">>;
pad_increase(<<"                                                                                                                                                                                            ">>) -> <<"                                                                                                                                                                                              ">>;
pad_increase(<<"                                                                                                                                                                                              ">>) -> <<"                                                                                                                                                                                                ">>;
pad_increase(<<"
               ">>) -> <<"
               ">>;

pad_increase(Pad) ->
  PadSize = byte_size(Pad),
  binary:copy(<<" ">>, PadSize + ?pad_increase).
