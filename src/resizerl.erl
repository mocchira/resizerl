-module(resizerl).

-export([start/0, stop/0, init/1]).
-export([resize/4]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec start() -> pid() | {error, any()}.
start() ->
    Exe = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", ?MODULE_STRING]);
                         _ ->
                             filename:join("../priv", ?MODULE_STRING)
                     end;
                 Dir ->
                     filename:join(Dir, ?MODULE_STRING)
             end,
    spawn(?MODULE, init, [Exe]).


-spec stop() -> any().
stop() ->
    ?MODULE ! stop.

-spec init(string()) -> any().
init(Exe) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Exe}, [{packet, 4}, binary, exit_status]),
    loop(Port).

-spec resize(string(), binary(), integer(), integer()) -> {ok, binary()} | {error, any()}.
resize(Path, ImageBin, Width, Height) ->
    call_port({resize_opencv, Path, ImageBin, Width, Height}).

%%--------------------------------------------------------------------
%% INNER FUNCTIONS
%%--------------------------------------------------------------------
-spec call_port(any()) -> any().
call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
        {?MODULE, Result} ->
            Result
    end.

-spec loop(port()) -> any().
loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {?MODULE, binary_to_term(Data)};
                {Port, {exit_status, Status}} ->
                    exit({port_terminated, Status});
                {'EXIT', Port, Reason} ->
                    exit(Reason);
		Other ->
		    io:format("received: ~p~n", [Other])
            after 5000 ->
                Caller ! {?MODULE, {error, timeout}}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', _Port, _Reason} ->
            exit(port_terminated)
    end.
