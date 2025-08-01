-type combinator_match_type() ::
          descendant
          | child
          | adjacent_sibling
          | general_sibling.


-record(combinator, {match_type = undefined :: combinator_match_type(),
                    selector = undefined :: any()}).
