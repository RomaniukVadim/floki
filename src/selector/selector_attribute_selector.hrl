-type attribute_selector_match_type() :: undefined
| equal
| includes
| dash_match
| prefix_match
| suffix_match
| substring_match.

-record(attribute_selector, {match_type = undefined :: attribute_selector_match_type(),
                             attribute = undefined :: binary() | undefined, 
                             value = undefined :: binary() | undefiend,
                             flag = undefined :: binary() | undefined
                            }).
