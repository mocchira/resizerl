-module(resizerl).

-export([start/1, stop/1, init/2]).
-export([resize/5]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec start(atom()) -> pid() | {error, any()}.
start(Id) ->
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
    spawn(?MODULE, init, [Exe, Id]).


-spec stop(atom()) -> any().
stop(Id) ->
    Id ! stop.

-spec init(string(), atom()) -> any().
init(Exe, Id) ->
    register(Id, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Exe}, [{packet, 4}, binary, exit_status]),
    loop(Port, Id).

-spec resize(atom(), binary(), binary(), integer(), integer()) -> {ok, binary()} | {error, any()}.
resize(Id, Path, ImageBin, Width, Height) ->
    call_port(Id, {resize_opencv, Path, ImageBin, Width, Height}).

%%--------------------------------------------------------------------
%% INNER FUNCTIONS
%%--------------------------------------------------------------------
-spec call_port(atom(), any()) -> any().
call_port(Id, Msg) ->
    Id ! {call, self(), Msg},
    receive
        {Id, Result} ->
            Result
    end.

-spec loop(port(), atom()) -> any().
loop(Port, Id) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {Id, binary_to_term(Data)};
                {Port, {exit_status, Status}} ->
                    exit({port_terminated, Status});
                {'EXIT', Port, Reason} ->
                    exit(Reason);
                Other ->
                    io:format("received: ~p~n", [Other])
            after 5000 ->
                Caller ! {Id, {error, timeout}}
            end,
            loop(Port, Id);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', _Port, _Reason} ->
            exit(port_terminated)
    end.
