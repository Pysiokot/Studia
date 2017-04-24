-module(pollution).
-author("Bart").

-export([createMonitor/0]).
-export([addStation/3]).
-export([addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3]).

-record(coordinates, {vertical, horizontal}).
-record(measurement, {type, value, date}).

createMonitor() -> #{}.

addStation(Name, Coords, Monitor) ->
    case maps:find(Name) of
        {ok, _}     -> Monitor;
        _           -> Monitor#{Name => {Coords, []}}
    end.

addValue(Station, Date, Type, Value, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> 
            Name = Station,
            Val = maps:get(Station, Monitor),
            maps:remove(Station, Monitor),
            Monitor#{Name => addMeasurement(Value, Date, Type, Val)};
        false -> 
            case findCoords(maps:values(Monitor), Station) of
            true -> 
                Val = maps:get(Station, Monitor),
                Monitor2 = maps:remove(Station, Monitor),
                Monitor2#{Station => addMeasurement(Value, Date, Type, Val)};
            false -> throw("not defined Station")
            end
        end.

removeValue(Station, Date, Type, Monitor) ->
    case maps:is_key(Station, Monitor) of
        true -> 
            Name = Station,
            Val = maps:get(Station, Monitor),
            maps:remove(Station, Monitor),
            Monitor#{Name => removeMeasurement(Val, Date, Type)};
        false -> 
            case findCoords(maps:values(Monitor), Station) of
            true -> 
                Val = maps:get(Station, Monitor),
                Monitor2 = maps:remove(Station, Monitor),
                Monitor2#{Station => removeMeasurement(Val, Date, Type)};
            false -> throw("Not defined Station")
        end
    end.

getOneValue(Station, Date, Type, Monitor) ->
    case maps:is_key(Station) of
        true -> getMeasurement(Date, Type, maps:get(Station,Monitor));
        false -> throw("Not defined Station")
    end.

getStationMean(_, _, #{}) -> throw("No stations added");
getStationMean(Station, Type, Monitor) ->
    case maps:is_key(Station) of
        true -> getMeanTypeMeasurement(maps:get(Station, Monitor), Type);
        false -> throw("Not defined Station")
    end.

getDailyMean(_, _, #{}) -> throw("No stations added");
getDailyMean(Type, Day, Monitor) -> getMeanDayMeasurement(Type, Day, maps:values(Monitor), 0, 0).

addMeasurement(Value, Date, Type, {Coords, [List]}) -> 
    case isDate(Date, [List]) of
        false   -> {Coords, [List | #measurement{type = Type, value = Value, date = Date}]};
        _       -> throw("Date exists")
    end.

removeMeasurement({Coords, Measurements}, Date, Type) ->
    case isDate(Date, Measurements) of
        true   -> {Coords, deleteMeasurement(Measurements, Date, Type)};
        _       -> throw("Date or Type doesn't exist!")
    end.

getMeanDayMeasurement(_, _, [], _, Count) when Count == 0 -> throw("No measures :c");
getMeanDayMeasurement(_, _, [], Sum, Count) -> Sum/Count;
getMeanDayMeasurement(Type, Day, {Coords, [#measurement{type = Type1, value = Value, date = Day1}|T]}, Sum, Count) when (Type == Type1 andalso Day == Day1) -> getMeanDayMeasurement(Type, Day, {Coords, T}, Sum + Value, Count + 1);
getMeanDayMeasurement(Type, Day, {Coords, [#measurement{type = _, value = _, date = _}|T]}, Sum, Count) -> getMeanDayMeasurement(Type, Day, {Coords, T}, Sum, Count).

getMeanTypeMeasurement({_, [H | T]}, Type) ->
    case isType(Type, [H|T]) of
        true    -> getMeanTypeMeasurement2([H|T], Type, 0, 0);
        _       -> throw("No measurements of this type")
    end.

getMeanTypeMeasurement2([], _, _, Count) when Count == 0 -> throw("No measures :c");
getMeanTypeMeasurement2([], _, Sum, Count) -> Sum/Count;
getMeanTypeMeasurement2([#measurement{type = Type1, value = Value, date = _}|T], Type, Sum, Count) when Type =:= Type1 -> getMeanTypeMeasurement2(T, Type, Sum + Value, Count + 1);
getMeanTypeMeasurement2([#measurement{type = _, value = _, date = _}|T], Type, Sum, Count) -> getMeanTypeMeasurement2(T, Type, Sum, Count).

findCoords([{SearchFor, _}|_], SearchFor)   -> true;
findCoords([{_, _}|Tail], SearchFor)        -> findCoords(Tail, SearchFor);
findCoords([],_)                            -> flase.

deleteMeasurement([#measurement{type = Type, value = _, date = Date}| Tail], Date, Type) -> Tail;
deleteMeasurement([Head | Tail], Date, Type) -> lists:merge(Head, deleteMeasurement(Tail, Date, Type));
deleteMeasurement([], _, _) -> throw("Haven't found measurement").