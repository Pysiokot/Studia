-module(pollution).
-author("Bart").

-export([createMonitor/0]).
-export([addStation/3]).
-export([addValue/5]).
-export([removeValue/4]).
-export([getOneValue/4]).
-export([getStationMean/3]).
-export([getDailyMean/3]).
-export([getMovingMean/4]).

-record(coordinates, {horizontal, vertical}).
-record(measurement, {type, value, date}).

%działa
createMonitor() -> #{}.

%działa
addStation(Name, {X, Y}, Monitor) ->
    case maps:find(Name, Monitor) of
        {ok, _}     -> {error, "Station with these coordinates exists"};
        _           -> case findCoords({X,Y}, maps:values(Monitor)) of
            false   -> maps:put(Name, {#coordinates{horizontal=X,vertical=Y}, []}, Monitor);
            true    -> {error, "Station with these coordinates exists"}
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
            false -> {error,"not defined Station"}
            end
        end.

%działa
removeValue(Station, Date, Type, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> 
            Name = Station,
            Val = maps:get(Station, Monitor),
            maps:remove(Station, Monitor),
            Monitor#{Name => removeMeasurement(Val, Date, Type)};
        false -> 
            case findCoords(Station, maps:values(Monitor)) of
            true -> 
                Val = findName(maps:to_list(Monitor), Station),
                Value = maps:get(Val, Monitor),
                Monitor2 = maps:remove(Val, Monitor),
                maps:put(Val, removeMeasurement(Value, Date, Type), Monitor2);
            false -> {error,"Not defined Station"}
        end
    end.

%działa
getOneValue(Station, Date, Type, Monitor) ->
    case maps:is_key(Station,Monitor) of
        true -> getMeasurement(Date, Type, maps:get(Station,Monitor));
        false -> {error,"Not defined Station"}
    end.

%działa
getStationMean(Station, Type, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> getMeanTypeMeasurement(maps:get(Station, Monitor), Type);
        false -> {error,"Not defined Station"}
    end.

getDailyMean(Type, Day, Monitor) -> getMeanDayMeasurement(Type, Day, maps:values(Monitor), 0, 0). %"PM2,5", {{2017,4,24},{11,49,8}}, [{#coord{},[]}|Tail], 0, 0

getMovingMean(Type, Date, Coords, Monitor) ->
    case findCoords(Coords, maps:values(Monitor)) of
        true -> getStationMovingMean2(Type, Date, maps:get(findName(maps:to_list(Monitor), Coords), Monitor));
        _ -> {error,"Station with these coords doesn't exist"}
    end.


%private functions

getStationMovingMean2(Type, Date, {_,Measurements}) -> getStationMovingMean(Type, Date, 
    lists:sort(fun(#measurement{type = _, value = _, date = {_,{H1,_,_}}}, #measurement{type = _, value = _, date = {_,{H2,_,_}}}) -> H1 =< H2 end, Measurements), 0, 0, 24).

getStationMovingMean(_,_,[],_,_,24) -> {error,"No measures :c"};
getStationMovingMean(_,_,[],0,0,_) -> {error,"No measures :c"};
getStationMovingMean(_,_,[], Counter, Denominator, _) when Denominator /= 0 -> round(Counter/Denominator);
getStationMovingMean(_,_,_,Counter, Denominator, Steps) when Steps == 0 -> round(Counter/Denominator);
getStationMovingMean(Type, {{Y,M,D},{H,Min,S}}, [#measurement{type = Type, value = Value, date = {{Y,M,D},{H,_,_}}} | Tail], Counter, Denominator, Steps) ->
    case H - 1 of
        -1 -> getStationMovingMean(Type, {{Y,M,D},{H,Min,S}}, Tail, Counter + (Steps*Value), Denominator+Steps, 0);
        _ -> getStationMovingMean(Type, {{Y,M,D},{H-1,Min,S}}, Tail, Counter + (Steps*Value), Denominator+Steps, Steps-1)
    end;
getStationMovingMean(Type, {{Y,M,D},{H,Min,S}}, [_|Tail], Counter, Denominator, Steps) -> 
    case H - 1 of
        -1 -> getStationMovingMean(Type, {{Y,M,D},{H,Min,S}}, Tail, Counter, Denominator, 0);
        _ -> getStationMovingMean(Type, {{Y,M,D},{H-1,Min,S}}, Tail, Counter, Denominator, Steps-1)
    end.

%działa
addMeasurement(Value, Date, Type, {Coords, List}) -> 
    case isDate(Date, List) of
        false   -> {Coords, lists:merge(List, [#measurement{type = Type, value = Value, date = Date}])};
        _       -> {error,"Date exists"}
    end.

%działa
removeMeasurement({Coords, Measurements}, Date, Type) ->
    case isDate(Date, Measurements) of
        true   -> {Coords, deleteMeasurement(Measurements, Date, Type)};
        _       -> {error,"Date or Type doesn't exist!"}
    end.

%działa
getMeanDayMeasurement(_, _, [], _, Count) when Count == 0 -> {error,"No measures :c"};
getMeanDayMeasurement(_, _, [{_,[]}|[]], _, 0) -> {error,"No measures :c"};
getMeanDayMeasurement(_, _, [], Sum, Count) -> Sum/Count;
getMeanDayMeasurement(_, _, [{_,[]}|[]], Sum, Count) -> Sum/Count;
getMeanDayMeasurement(Type, {{Y,M,D},_}, [{_,[]}|Next], Sum, Count) -> getMeanDayMeasurement(Type, {{Y,M,D},{}}, Next, Sum, Count);
getMeanDayMeasurement(Type, {{Y,M,D},_}, [{Coord,[#measurement{type=Type, value = Value, date ={{Y,M,D},_}} | T]} | Next], Sum, Count) -> getMeanDayMeasurement(Type, {{Y,M,D},{}}, lists:merge([{Coord, T}],Next), Sum + Value, Count +1);
getMeanDayMeasurement(Type, {{Y,M,D},_}, [{Coord,[#measurement{type=_, value = _, date = _} | T]} | Next], Sum, Count) -> getMeanDayMeasurement(Type, {{Y,M,D},{}}, lists:merge([{Coord, T}],Next), Sum, Count).

%działa
getMeanTypeMeasurement({_, Measurements}, Type) -> getMeanTypeMeasurement2(Measurements, Type, 0, 0).
%działa
getMeanTypeMeasurement2([], _, _, Count) when Count == 0 -> {error,"No measures :c"};
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
deleteMeasurement([], _, _) -> {error,"Haven't found measurement"}.

%działa
getMeasurement(_, _, {_, []}) -> {error,"Haven't found measurement"};
getMeasurement(Date, Type, {_, [#measurement{type = Type, value = Value, date = Date}|_]}) -> Value;
getMeasurement(Date, Type, {Coords, [#measurement{type = _, value = _, date = _}|T]}) -> getMeasurement(Date, Type, {Coords, T}).

%działa
isDate(_, []) -> false;
isDate(Date, [#measurement{type = _, value = _, date = Date}|_]) -> true;
isDate(Date, [#measurement{type = _, value = _, date = _}|T]) -> isDate(Date, T).

%działa
findName([],_) -> {error,"Unexpected error"};
findName([{Key, {#coordinates{horizontal = X, vertical = Y},_}} | _], {X, Y}) -> Key;
findName([_|Tail], {X,Y}) -> findName(Tail, {X,Y}).