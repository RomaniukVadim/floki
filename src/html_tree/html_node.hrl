-record(html_node, {
    type = <<>> :: binary(),
    attributes = [] :: [{binary(), binary()}],
    children_nodes_ids = [] :: [pos_integer()],
    node_id = undefined :: pos_integer() | undefined,
    parent_node_id = undefined :: pos_integer() | undefined
}).

%% Type specification for the record.
-type html_node() :: #html_node{}.
