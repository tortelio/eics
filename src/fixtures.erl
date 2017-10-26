-module(fixtures).
-include("eics.hrl").

-export([calendar/1,
         todo/1]).

calendar(simple) ->
    #calendar{
       version = <<"2.0">>,
       production_id = <<"-//ABC Corporation//NONSGML My Product//EN">>,
       todos = [todo(simple)]
      }.

todo(simple) ->
    #todo{
       created_at = {{1998, 1, 30}, {13, 45, 00}},
       sequence = 1,
       id = <<"uid4@example.com">>,
       due = {{1998, 4, 15}, {23, 59, 59}},
       status = needs_action,
       summary = <<"Submit Income Taxes">>
      }.
