% src/request_counter.erl
-module(request_counter).
-behaviour(gen_server).

-export([start_link/0, increment/0, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SAVE_INTERVAL, 60000). % Save every 60 seconds
-define(UPDATE_INTERVAL, 5000). % Update stats every 5 seconds
-define(COUNTER_FILE, "request_count.dat").

-record(state, {
    count = 0,
    requests_per_second = 0
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    case persistent_term:get(counter, not_found) of
        not_found ->
            % If counter is not initialized, increment later
            gen_server:cast(?MODULE, increment);
        CounterRef ->
            atomics:add(CounterRef, 1, 1)
    end.

get_stats() ->
    case persistent_term:get(counter, not_found) of
        not_found ->
            {0, 0.0};
        CounterRef ->
            {atomics:get(CounterRef, 1), atomics:get(CounterRef, 2) / 10}
    end.

init([]) ->
    CounterRef = atomics:new(2, [{signed, false}]),
    persistent_term:put(counter, CounterRef),
    Count = case file:read_file(?COUNTER_FILE) of
                {ok, Binary} -> binary_to_integer(Binary);
                {error, _} -> 0
            end,
    atomics:put(CounterRef, 1, Count),
    erlang:send_after(?SAVE_INTERVAL, self(), save_count),
    erlang:send_after(?UPDATE_INTERVAL, self(), update_stats),
    {ok, #state{count = Count}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(increment, State) ->
    % Handle increments that came in before initialization
    CounterRef = persistent_term:get(counter),
    atomics:add(CounterRef, 1, 1),
    {noreply, State}.

handle_info(save_count, State) ->
    CounterRef = persistent_term:get(counter),
    NewCount = atomics:get(CounterRef, 1),
    ok = file:write_file(?COUNTER_FILE, integer_to_binary(NewCount)),
    erlang:send_after(?SAVE_INTERVAL, self(), save_count),
    {noreply, State#state{count = NewCount}};

handle_info(update_stats, State) ->
    CounterRef = persistent_term:get(counter),
    NewCount = atomics:get(CounterRef, 1),
    Diff = NewCount - State#state.count,
    RPS = Diff / (?UPDATE_INTERVAL / 1000),
    atomics:put(CounterRef, 2, round(RPS * 10)),  % Store RPS * 10 for one decimal place
    erlang:send_after(?UPDATE_INTERVAL, self(), update_stats),
    {noreply, State#state{count = NewCount, requests_per_second = RPS}}.

terminate(_Reason, State) ->
    ok = file:write_file(?COUNTER_FILE, integer_to_binary(State#state.count)).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.