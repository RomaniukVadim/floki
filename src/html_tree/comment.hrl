
-record(comment, {content = <<>> :: binary(),
                  node_id = undefined :: pos_integer() | undefined,
                  parent_node_id = undefined :: pos_integer() | undefined }).
