%%% led_controller is a module to toggle LEDs wired to gpio pins on the raspberry pi
-module(led_controller).
-behavior(gen_server).
-export([start_link/1]).
% standard gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% Public Interface
-export([on/1, off/1, blink/2]).
% Don't call these directly
-export([blink_cast/2]).
% orddicts, Color atoms to Pins, Color atom to TRef, Color atom to state (on/off) not blink
-record(state, {led_pins=[], blinking_led=[], led_state=[]}).
% Time between on/off state changes
-define(BLINK_DELAY, 250).

%% gen_server specfic
%% Pass in proplist of {Color, GPIO_Pin} Example: {red, 17}
start_link(Color2Pin) -> 
    gen_server:start_link({local, led_svc}, ?MODULE, Color2Pin, []).

init(Color2Pin) ->
    %% Know when parent shuts down
    process_flag(trap_exit, true),
    io:format("[~s] started.~n", [?MODULE]),
    % Init state of LEDs to off
    LedState = [ {X, off} || X <- orddict:fetch_keys(Color2Pin)],
    %Color2Pin = [{green, 22}, {red, 17}],
    {ok, #state{led_pins=Color2Pin, led_state=LedState}}.

%% Public Interface

%% Turn Color LED ON
on(Color) ->
    toggle(Color, on).

%% Turn Color LED OFF 
off(Color) ->
    toggle(Color, off).

toggle(Color, LedState) ->
    gen_server:cast(led_svc, {LedState, Color}).

%% Blink Color LED Times 
blink(Color, Times) ->
    % A blink is 1 on/off cycle, we translate this to state changes
    StateChanges = Times * 2,
    blink_cast(Color, StateChanges).

%% Private 

%% Used to turn an led on and off
toggle(Color, Pin, LedState) ->
    io:format("[~s] ~p ~p~n", [?MODULE, Color, LedState]),
    File = get_value_file(Pin),
    case LedState of
        on -> Value = "1";
        off -> Value = "0"
    end,
    ok = file:write_file(File, Value ++ "\n").

%% Gets called by public interface and via apply_after 
blink_cast(Color, StateChangesLeft) ->
    gen_server:cast(led_svc, {blink, Color, StateChangesLeft}).

% 0 state changes means we're done
blink(_Color, _Pin, StateChangesLeft) when StateChangesLeft =< 0 -> done;
blink(Color, Pin, StateChangesLeft) -> 
    io:format("[~s] Blink ~p ~p~n", [?MODULE, Color, StateChangesLeft]),
    % On even states on, odd Off
    case StateChangesLeft rem 2 of
        0 -> toggle(Color, Pin, on);
        _ -> toggle(Color, Pin, off)
    end,
    {ok, _TRef} = timer:apply_after(?BLINK_DELAY, ?MODULE, blink_cast, [Color, StateChangesLeft - 1]).


%% Return the file that we are writing to to toggle value
get_value_file(Pin) ->
    "/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/value".

% Get the pin number for a color atom
get_pin_for_color(Color, Color2Pin) ->
    orddict:fetch(Color, Color2Pin).

% Remove TRef and cancel apply timer if it exists
cancel_blink(Color, Blinking2TRef) ->
    case orddict:find(Color, Blinking2TRef) of 
        error -> ok;
        {ok, TRef} -> timer:cancel(TRef)
    end,
    orddict:erase(Color, Blinking2TRef).

%% Event Loop

handle_cast({blink, Color, 0}, S = #state{blinking_led=BlinkingLed2TRef, led_state=TrackLedState}) -> 
    % No more blinks, clear our timer ref
    NewBlinkingLed2TRef = cancel_blink(Color, BlinkingLed2TRef),
    % Now that the blinking is done, restore to our previous steady state
    PrevState = orddict:fetch(Color, TrackLedState),
    toggle(Color, PrevState),
    {noreply, S#state{blinking_led=NewBlinkingLed2TRef}};
handle_cast({blink, Color, StateChangesLeft}, S = #state{led_pins=Color2Pin, blinking_led=BlinkingLed2TRef}) -> 
    Pin = get_pin_for_color(Color, Color2Pin),
    {ok, TRef} = blink(Color, Pin, StateChangesLeft),
    NewBlinkingLed2TRef = orddict:store(Color, TRef, BlinkingLed2TRef),
    {noreply, S#state{blinking_led=NewBlinkingLed2TRef}};
%% LedState should be either on or off
handle_cast({LedState, Color}, S = #state{led_pins=Color2Pin, blinking_led=BlinkingLed2TRef, led_state=TrackLedState}) -> 
    Pin = get_pin_for_color(Color, Color2Pin),
    % Cancel blinking in case we were blinking
    NewBlinkingLed2TRef = cancel_blink(Color, BlinkingLed2TRef),
    toggle(Color, Pin, LedState),
    NewTrackLedState = orddict:store(Color, LedState, TrackLedState),
    {noreply, S#state{blinking_led=NewBlinkingLed2TRef, led_state=NewTrackLedState}};
handle_cast(_Msg, S) -> 
    {noreply, S}.

handle_info(Msg, S) ->
    io:format("[~s] Unknown info ~p~n", [?MODULE, Msg]),
    {noreply, S}.

handle_call(terminate, _From, S) ->
    {stop, normal, ok, S};
handle_call(Msg, From, S = #state{}) ->
    io:format("[~s] Unknown call from ~p message ~p~n", [?MODULE, From, Msg]),
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(Reason, _S) ->
    io:format("[~s] Terminate Reason: ~p.~n", [?MODULE, Reason]),
    ok.

