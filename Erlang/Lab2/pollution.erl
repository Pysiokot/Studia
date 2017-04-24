-module(pollution).
-author("Bart").

-export([createMonitor/0]).
-export([addStation/3]).
-export([addValue/5]).
-export([removeValue/4]).
-export([getOneValue/4]).
-export([getStationMean/3]).
-export([getDailyMean/3]).

-record(coordinates, {horizontal, vertical}).
-record(measurement, {type, value, date}).

%działa
createMonitor() -> #{}.

%działa
addStation(Name, {X, Y}, Monitor) ->
    case maps:find(Name, Monitor) of
        {ok, _}     -> throw("Station with this name exists");
        _           -> case findCoords({X,Y}, maps:values(Monitor)) of
            false   -> maps:put(Name, {#coordinates{horizontal=X,vertical=Y}, []}, Monitor);
            true    -> throw("Station with these coordinates exists")
        end
    end.

%działa
addValue(Station, Date, Type, Value, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> 
            Name = Station,
            Val = maps:get(Station, Monitor),
            maps:remove(Station, Monitor),
            Monitor#{Name => addMeasurement(Value, Date, Type, Val)};
        false -> 
            case findCoords(Station, maps:values(Monitor)) of
            true -> 
                Val = findName(maps:to_list(Monitor), Station),
                Measurements = maps:get(Val, Monitor),
                Monitor2 = maps:remove(Val, Monitor),
                maps:put(Station, addMeasurement(Value, Date, Type, Measurements), Monitor2);
            false -> throw("not defined Station")
            end
        end.

%działa
removeValue(Station, Date, Type, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> 
            Name = Station,
            Val = maps:get(Station, Monitor),
            maps:remove(Station, Monitor),
            Monitor#{Name => removeMeasurement(Val, Date, Type)}; %poprawić XDDDDDDDDD
        false -> 
            case findCoords(Station, maps:values(Monitor)) of
            true -> 
                Val = findName(maps:to_list(Monitor), Station),
                Value = maps:get(Val, Monitor),
                Monitor2 = maps:remove(Val, Monitor),
                maps:put(Val, removeMeasurement(Value, Date, Type), Monitor2);
            false -> throw("Not defined Station")
        end
    end.

%działa
getOneValue(Station, Date, Type, Monitor) ->
    case maps:is_key(Station,Monitor) of
        true -> getMeasurement(Date, Type, maps:get(Station,Monitor));
        false -> throw("Not defined Station")
    end.

%działa
getStationMean(Station, Type, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> getMeanTypeMeasurement(maps:get(Station, Monitor), Type);
        false -> throw("Not defined Station")
    end.

getDailyMean(Type, Day, Monitor) -> getMeanDayMeasurement(Type, Day, maps:values(Monitor), 0, 0). %"PM2,5", {{2017,4,24},{11,49,8}}, [{#coord{},[]}|Tail], 0, 0

%działa
addMeasurement(Value, Date, Type, {Coords, List}) -> 
    case isDate(Date, List) of
        false   -> {Coords, lists:merge(List, [#measurement{type = Type, value = Value, date = Date}])};
        _       -> throw("Date exists")
    end.

%działa
removeMeasurement({Coords, Measurements}, Date, Type) ->
    case isDate(Date, Measurements) of
        true   -> {Coords, deleteMeasurement(Measurements, Date, Type)};
        _       -> throw("Date or Type doesn't exist!")
    end.

%działa
getMeanDayMeasurement(_, _, [], _, Count) when Count == 0 -> throw("No measures :c");
getMeanDayMeasurement(_, _, [{_,[]}|[]], _, 0) -> throw("No measures :c");
getMeanDayMeasurement(_, _, [], Sum, Count) -> Sum/Count;
getMeanDayMeasurement(_, _, [{_,[]}|[]], Sum, Count) -> Sum/Count;
getMeanDayMeasurement(Type, {{Y,M,D},_}, [{_,[]}|Next], Sum, Count) -> getMeanDayMeasurement(Type, {{Y,M,D},{}}, Next, Sum, Count);
getMeanDayMeasurement(Type, {{Y,M,D},_}, [{Coord,[#measurement{type=Type, value = Value, date ={{Y,M,D},_}} | T]} | Next], Sum, Count) -> getMeanDayMeasurement(Type, {{Y,M,D},{}}, lists:merge([{Coord, T}],Next), Sum + Value, Count +1);
getMeanDayMeasurement(Type, {{Y,M,D},_}, [{Coord,[#measurement{type=_, value = _, date = _} | T]} | Next], Sum, Count) -> getMeanDayMeasurement(Type, {{Y,M,D},{}}, lists:merge([{Coord, T}],Next), Sum, Count).

%działa
getMeanTypeMeasurement({_, Measurements}, Type) -> getMeanTypeMeasurement2(Measurements, Type, 0, 0).
%działa
getMeanTypeMeasurement2([], _, _, Count) when Count == 0 -> throw("No measures :c");
getMeanTypeMeasurement2([], _, Sum, Count) -> Sum/Count;
getMeanTypeMeasurement2([#measurement{type = Type, value = Value, date = _}|T], Type, Sum, Count) -> getMeanTypeMeasurement2(T, Type, Sum + Value, Count + 1);
getMeanTypeMeasurement2([#measurement{type = _, value = _, date = _}|T], Type, Sum, Count) -> getMeanTypeMeasurement2(T, Type, Sum, Count).

%działa
findCoords(_,[]) -> false;
findCoords({X,Y}, [{#coordinates{horizontal = X, vertical = Y},_}|_]) -> true;
findCoords({X,Y}, [_|List]) -> findCoords({X,Y}, List).

%działa
deleteMeasurement([#measurement{type = Type, value = _, date = Date}| Tail], Date, Type) -> Tail;
deleteMeasurement([Head | Tail], Date, Type) -> lists:merge([Head], deleteMeasurement(Tail, Date, Type));
deleteMeasurement([], _, _) -> "Haven't found measurement".

%działa
getMeasurement(_, _, {_, []}) -> throw("Haven't found measurement");
getMeasurement(Date, Type, {_, [#measurement{type = Type, value = Value, date = Date}|_]}) -> Value;
getMeasurement(Date, Type, {Coords, [#measurement{type = _, value = _, date = _}|T]}) -> getMeasurement(Date, Type, {Coords, T}).

%działa
isDate(_, []) -> false;
isDate(Date, [#measurement{type = _, value = _, date = Date}|_]) -> true;
isDate(Date, [#measurement{type = _, value = _, date = _}|T]) -> isDate(Date, T).

%działa
findName([],_) -> throw("Unexpected error");
findName([{Key, {#coordinates{horizontal = X, vertical = Y},_}} | _], {X, Y}) -> Key;
findName([_|Tail], {X,Y}) -> findName(Tail, {X,Y}).