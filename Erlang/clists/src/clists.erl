-module(clists).

%% API exports
-export([main/0]).

%%====================================================================
%% API functions
%%====================================================================
main() ->
    run_01(),
    run_02(),
    run_03(),
    run_04().

%%====================================================================
%% Internal functions
%%====================================================================

%%  NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN].
run_01() ->
    RestaurantMenu = [{steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}],
    NewList = [{Item, Price*1.07} || {Item, Price} <- RestaurantMenu, Price >= 3, Price =< 10],
    io:fwrite("Results: ~p~n", [NewList]).

run_02() ->
    Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}],
    FoggyPlaces = [ X || {X, fog} <- Weather],
    io:fwrite("Results: ~p~n", [FoggyPlaces]).

run_03() ->
    CityLocation = [{newyork,us}, {losangeles,us}, {paris,france},
        {madrid,spain}], [{newyork,us},{losangeles,us},{paris,france},{madrid,spain}],
    USACyties = [C || {C,us} <- CityLocation],
    io:fwrite("Results: ~p~n", [USACyties]),
    Cities = [C || {newyork,C} <- CityLocation],
    io:fwrite("Results: ~p~n", [Cities]).

run_04() ->
    io:fwrite("~p~n", [[Element || Element <- [1,2,3,4], Element rem 2 == 0]]).


