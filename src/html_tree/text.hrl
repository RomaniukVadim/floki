% Represents a text node inside an HTML tree with reference to its parent node id.
-record(text, {content = <<>> :: binary(),
              node_id = undefined :: pos_integer() | undefined,
              parent_node_id = undefined :: pos_integer() | undefined }).
