-include("html_tree/html_node.hrl").

-type text() :: any().
-type comment() :: any().


-record(html_tree, {
    nodes = #{} :: #{pos_integer() => html_node() | text() | comment()},
    root_nodes_ids = [] :: [pos_integer()],
    node_ids = [] :: [pos_integer()]
}).

%% Optionally, define a type for the record itself.
-type html_tree() :: #html_tree{}.
