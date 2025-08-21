-record(html_node, {
    type = <<>> :: binary(),
    attributes = [] :: [{binary(), binary()}],
    children_nodes_ids = [] :: [pos_integer()],
    node_id = undefined :: pos_integer() | undefined,
    parent_node_id = undefined :: pos_integer() | undefined
}).

-type html_attribute() :: {binary(), binary()}.
-type html_attributes() :: [html_attribute()] | html_attributes_map().
-type html_attributes_map() :: #{binary() => binary()}.
-type html_declaration() :: {pi, binary(), html_attributes()}.
-type html_comment() :: {comment, binary()}.
-type html_doctype() :: {doctype, binary(), binary(), binary()}.
-type html_text() :: binary().
-type html_tag() :: {binary(), html_attributes(), [html_node()]}.
-type html_node() :: html_tag() | html_comment() | html_doctype() | html_declaration() | html_text().
