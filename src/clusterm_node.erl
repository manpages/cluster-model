-module(clusterm_node).

%% Possible parameters to include in this module:
%%
%%% Failure probability — node will fail now and then.
%%%                       We will test system resilience
%%%                       against random failure this way.
%%%
%%% Multiple ops and op weight — for a more complex system
%%%                              we model nodes that do a
%%%                              lot of different operations
%%%                              whith more costy ops and
%%%                              less expensive ones.
%%%
%%% Heterogenous processing power — allow cluster supervisor
%%%                                 to start nodes of different
%%%                                 "strength" as opposed to
%%%                                 having identical processing
%%%                                 power configured globally
%% 
%% Possible way of integration into existing system:
%%
%%% Multiplex requests to the system to a HTTP router of
%%% a cluster model and "play" them back against the model,
%%% collecting data about % of requests we managed to respond
%%% to. This should be doable with some tricky (?) nginx config.
%%
%% We also might want to test the same cluster configurations 
%% against different strategies of load balancing/pooling.

-behaviour(gen_server).

%% Behaviour exports
-export([ init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,start_link/0 ]).

%% API
-export([ compute/2 ]).

-type mfargs()     :: { module(), atom(), [term()] }. % applicable notation, 
                                                      % see: http://erlang.org/doc/man/erlang.html#apply-3
-type operations() :: non_neg_integer().
-type period()     :: erlang:timestamp().

-record(load,  { pl    :: { operations(), period() }    % Processing limit: operations / period
                ,frame :: { operations(), period() | undefined }}). % Current frame: remaining operations,
                                                                    %                time of first operation
                                                                    %                of the frame.

-record(state, { f    :: mfargs()
                ,load :: #load{} }).


%%% API

-spec compute(pid(), mfargs()) -> term().
compute(Pid, MFA) ->
  gen_server:cast(Pid, {compute, MFA}).

%%% OTP

-spec init(mfargs()) -> {ok, #state{}}.
init(MFA) ->
  OpsCount = 1,
  Period   = {0, 1, 0},
  {ok, #state{ f=MFA, load=#load{ pl={OpsCount, Period}
                                 ,frame={OpsCount, undefined} } }}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

handle_call(_, _From, State) ->
  { reply 
   ,"Model of a node should operate in asynchronous mode only, attempted to handle a synchronous call"
   ,State }.
handle_info(_, State) ->
  {noreply, State}.

-spec handle_cast({compute, mfargs()}, #state{}) -> {noreply, #state{}}.
% Pattern-match node model state that corresponds to the beginning of
% a new processing frame
handle_cast({ compute, {M, F, A} }
             ,#state{
                load=#load{
                  pl={Operations, _}
  %
  % If current frame timestamp is undefined
  %             `--v         v---------”
                 ,frame={_, undefined}} = Load} = State) ->
  % Execute payload
  spawn_link(M, F, A),
  % And start new frame
  Load1 = Load#load{ frame={Operations, os:timestamp()} },
  {noreply, State#state{load=Load1}};

% Pattern-match node model state that corresponds to trying to make a
% computation when the processing resources of the node are exhausted
handle_cast({ compute, _ }
             ,#state{
                load=#load{
                  frame={0, _}} } = State) ->
  handle_exhaustion(State);

% Pattern-match node model state that corresponds to trying to a 
% computation within a frame
handle_cast({ compute, {M, F, A} }
             ,#state{
               load=#load{
                 pl={_, Period}
                ,frame={Operations, FrameStart}} = Load } = State) ->
  Load1 = 
    case spawn_maybe(fits_period(os:timestamp(), FrameStart, Period), Operations, {M, F, A}) of
      {ok, inside}  -> Load#load{ frame={Operations - 1, FrameStart} };
      {ok, outside} -> Load#load{ frame={Operations - 1, undefined}  };
      _             -> handle_exhaustion(State)
    end,
    {noreply, State#state{load=Load1}}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

%%% Private functions

handle_exhaustion(_) ->
  io:format("Node model running at ~p is exhausted~n", [self()]).

fits_period({NowM, NowS, NowI}, {StartM, StartS, StartI}, {_, _, _} = L) ->
  Delta = {NowM - StartM, NowS - StartS, NowI - StartI},
  case Delta < {0, 0, 0} of
    true -> error("Pime taradox!");
    _    -> ok
  end,
  Delta < L.

spawn_maybe(true, 0, _) ->
  exhausted;
spawn_maybe(true, _, {M, F, A}) ->
  spawn_link(M, F, A),
  {ok, inside};
spawn_maybe(false, _, {M, F, A}) ->
  spawn_link(M, F, A),
  {ok, outside}.
