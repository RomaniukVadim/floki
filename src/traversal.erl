-module(traversal).

-export([traverse_and_update/2,
         traverse_and_update/3]).

traverse_and_update([], _Fun) -> [];
traverse_and_update(Text, _Fun) when is_binary(Text) -> Text;
traverse_and_update({pi, _, _} = XmlTag, Fun) -> Fun(XmlTag);
traverse_and_update({comment, _Children} = Comment, Fun) -> Fun(Comment);
traverse_and_update({doctype, _, _, _} = Doctype, Fun) -> Fun(Doctype);

traverse_and_update([Head | Tail], Fun) ->
    case traverse_and_update(Head, Fun) of
        undefined -> traverse_and_update(Tail, Fun);
        MappedHead ->
            MappedTail = traverse_and_update(Tail, Fun),

            Mapped =
            case is_list(MappedHead) of
                true -> MappedHead ++ MappedTail;
                false -> [MappedHead | MappedTail]
            end,

            Mapped
    end;

traverse_and_update({Elem, Attrs, Children}, Fun) ->
    MappedChildren = traverse_and_update(Children, Fun),
    Fun({Elem, Attrs, MappedChildren}).

traverse_and_update([], Acc, _Fun) -> {[], Acc};
traverse_and_update(Text, Acc, _Fun) when is_binary(Text) -> {Text, Acc};
traverse_and_update({pi, _, _} = XmlTag, Acc, Fun) -> Fun(XmlTag, Acc);
traverse_and_update({comment, _Children} = Comment, Acc, Fun) -> Fun(Comment, Acc);
traverse_and_update({doctype, _, _, _} = Doctype, Acc, Fun) -> Fun(Doctype, Acc);

traverse_and_update([Head | Tail], Acc, Fun) ->
    case traverse_and_update(Head, Acc, Fun) of
        {undefined, NewAcc} ->
            traverse_and_update(Tail, NewAcc, Fun);
        {MappedHead, NewAcc} ->
            {MappedTail, NewAcc2} = traverse_and_update(Tail, NewAcc, Fun),

            Mapped =
            case is_list(MappedHead) of
                true -> MappedHead ++ MappedTail;
                false -> [MappedHead | MappedTail]
            end,

            {Mapped, NewAcc2}
    end;

traverse_and_update({Elem, Attrs, Children}, Acc, Fun) ->
    {MappedChildren, NewAcc} = traverse_and_update(Children, Acc, Fun),
    Fun({Elem, Attrs, MappedChildren}, NewAcc).

