-module(pollution_server).


%% API
%% crashuje sie przy dodaniu nowej stacji bo monitor w serwerze zamienia sie na string błędu
-export([loop/1, start/0,getState/0, addStation1/2, receiveData/0, addValue1/4, addValue2/4, removeValue2/3, getOneValue1/3, getStationMean1/2, stop/0, getDailyMean1/2,
  getMovingMean1/3, createStations1/0]).


start()->
  Monitor=pollution:createMonitor(),
  register(pServer,spawn(pollution_server,loop,[Monitor])).

loop(Monitor)->
  receive
    {request,Pid,addStation, Name, Coords}->
      case pollution:addStation(Name,Coords, Monitor) of ->
        {error, Msg} -> Pid ! Msg, loop(Monitor);
        NewMonitor -> Pid ! ok, loop(NewMonitor)
      end;
    {request,Pid, addValue1, Name, Type, Date, Value}->
      case pollution:addValue(Name, Type, Date, Value, Monitor) of ->
        {error, Msg} -> Pid ! Msg, loop(Monitor);
        NewMonitor -> Pid ! ok, loop(NewMonitor)
      end;
    {request,Pid,removeValue1, Name, Type, Date}->
      case pollution:removeValue(Name, Type, Date, Monitor) of ->
        {error, Msg} -> Pid ! Msg, loop(Monitor);
        NewMonitor -> Pid ! ok, loop(NewMonitor)
      end;
    {request,Pid,getOneValue, Name, Type, Date}->
      Pid!{response,pollution:getOneValue(Name, Type, Date, Monitor)},
      loop(Monitor);
    {request,Pid,getStationMean, Name, Type} ->
      Pid ! {response,pollution:getStationMean(Name, Type, Monitor)},
      loop(Monitor);
    {request,Pid,getDailyMean, Type, Date} ->
      Pid ! {response,pollution:getDailyMean(Type, Date, Monitor)},
      loop(Monitor);
    {request,Pid,getMovingMean,Type, Date, Coords, Monitor} ->
      Pid ! {response,pollution:getMovingMean(Type, Date, Coords, Monitor)},
      loop(Monitor);
    {request,Pid,getState} ->
      Pid ! {response, Monitor},
      loop(Monitor);
    {request,Pid,stop} -> ok;
    _ -> io:format("Error occured"),
      loop(Monitor)
  end.
receiveData() ->
  receive
    {response, Data} -> Data;
    _ -> io:format("Error occured")
  after
    500 -> io:format("No data recieved")
  end.

addStation1(Name,Coords)->
  pServer ! {request,self(),addStation, Name,Coords}.
addValue1(Name, Type, Date, Value) ->
  pServer ! {request,self(),addValue1, Name, Type, Date, Value}.
addValue2(Coords, Type, Date, Value) ->
  pServer ! {request,self(),addValue1,Coords, Type, Date, Value}.
removeValue2(Name, Type, Date) ->
  pServer ! {request,self(),removeValue1, Name, Type, Date}.
getOneValue1(Name, Type, Date) ->
  pServer ! {request,self(),getOneValue, Name, Type, Date},
  receiveData().
getStationMean1(Name, Type)->
  pServer ! {request,self(),getStationMean, Name, Type},
  receiveData().
getDailyMean1(Type, Date) ->
  pServer ! {request,self(),getDailyMean, Type, Date},
  receiveData().
getMovingMean1(Type,Date,Coords) ->
  pServer ! {request,self(),getMovingMean,Type, Date, Coords},
  receiveData().
stop() ->
  pServer ! {request,self(),stop}.
getState() ->
  pServer ! {request,self(),getState},
  receiveData().
createStations1() ->
  start(),
  addStation1("cba",{33,7}),
  addStation1("Krakow",{610,98}),
  addValue1("cba","PM10",{{2016,3,12},{04,56,23}},5),
  addValue1("cba",'PM2',{{2016,3,11},{12,32,54}},76),
  addValue2({610,98},'PM2',{{1996,7,10},{18,22,43}},34),
  addValue2({610,98},'PM2',{{1996,4,12},{11,21,12}},2).