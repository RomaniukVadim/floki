-module(id_seeder).
-export([seed/1]).

seed([]) -> 1;
seed([H | _]) -> H + 1.
