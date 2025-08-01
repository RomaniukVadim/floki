-include("../selector.hrl").
-record(pseudo_class, {name = <<>> :: binary(),
                      value = undefined :: binary() | #selector{}}).
