-module(basho_bench_driver_resizerl).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, {id, test_images, width, height}).

%% ====================================================================
%% API
%% ====================================================================
new(Id) ->
    %% The IPs, port and path we'll be testing
    SrcDir = basho_bench_config:get(src_dir, "~/test/images/"),
    Width  = basho_bench_config:get(width,   800),
    Height = basho_bench_config:get(height,  600),
    case is_integer(Width) of
        true  -> void;
        false -> ?FAIL_MSG("Invalid configuration for width: ~p~n", [Width])
    end,
    case is_integer(Height) of
        true  -> void;
        false -> ?FAIL_MSG("Invalid configuration for height: ~p~n", [Height])
    end,
    TestImages = case file:list_dir(SrcDir) of
        {ok, Filenames} ->
            lists:foldl(
                fun(Filename, Acc) ->
                    case file:read_file(SrcDir ++ "/" ++ Filename) of
                        {ok, Bin} -> [{list_to_binary(Filename), Bin}|Acc];
                        _ -> Acc
                    end
                end, [], Filenames);
        {error, Reason} ->
            ?FAIL_MSG("Invalid configuration list_dir error for src_dir: ~p reason:~p ~n", [SrcDir, Reason])
    end,
    case length(TestImages) of
        0 -> ?FAIL_MSG("Invalid configuration file missing for src_dir: ~p~n", [SrcDir]);
        _ -> void
    end,
    random:seed(),
    ResId = list_to_atom(lists:concat(['resizerl_', Id])),
    resizerl:start(ResId),
    {ok, #state { test_images = TestImages,
                  id = ResId,
                  width = Width,
                  height = Height }}.

%% public interface
run(_, _KeyGen, _ValueGen, State) ->
    {Filename, ImgBin} = next_image(State#state.test_images),
    case resizerl:resize(State#state.id, Filename, ImgBin, State#state.width, State#state.height) of
        {ok, _Bin} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

%% inner
next_image(Images) ->
    Size = length(Images),
    Index = random:uniform(Size),
    lists:nth(Index, Images).

