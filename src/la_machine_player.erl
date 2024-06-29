%
% This file is part of La Machine
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0
%

%%-----------------------------------------------------------------------------
%% @doc La Machine player server
%% @end
%%-----------------------------------------------------------------------------

-module(la_machine_player).

-behaviour(gen_server).

-include("la_machine_definitions.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    start_link/0,
    stop/1,
    play/2
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type sequence() :: list().

% Internal state
-record(state, {
    audio_module :: atom(),
    servo_module :: atom(),
    servo_state = undefined :: undefined | term(),
    servo_end = undefined :: undefined | non_neg_integer(),
    wait_end = undefined :: undefined | non_neg_integer(),
    sequence = [] :: list(),
    audio_monitor = undefined :: undefined | {pid(), reference()},
    from = undefined :: undefined | gen_server:from()
}).

%%-----------------------------------------------------------------------------
%% @returns a reference to the server
%% @doc Start player
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, {la_machine_audio, la_machine_servo}, []).

%%-----------------------------------------------------------------------------
%% @param Pid reference to player server
%% @doc Stop player, synchronously.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%-----------------------------------------------------------------------------
%% @param Pid reference to player server
%% @param Sequence sequence to play
%% @doc Play a sequence, synchronously, with an infinity timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec play(pid(), sequence()) -> ok.
play(Pid, Sequence) ->
    gen_server:call(Pid, {play, Sequence}, infinity).

%%-----------------------------------------------------------------------------
%% @param Arg argument with audio and servo modules
%% @doc gen_server `init' callback
%% @end
%%-----------------------------------------------------------------------------
-spec init({atom(), atom()}) -> {ok, #state{}}.
init({AudioModule, ServoModule}) ->
    AudioModule:power_on(),
    ServoState = ServoModule:power_on(),
    {ok, #state{audio_module = AudioModule, servo_module = ServoModule, servo_state = ServoState}}.

%%-----------------------------------------------------------------------------
%% @param Message message to process
%% @param From caller reference for async result
%% @param State server state
%% @doc gen_server `handle_call' callback.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call({play, sequence()}, gen_server:from(), #state{}) -> {noreply, #state{}}.
handle_call({play, Sequence}, From, #state{sequence = []} = State0) ->
    State1 = State0#state{sequence = Sequence, from = From},
    play_next(State1).

%%-----------------------------------------------------------------------------
%% @param Message message to process
%% @param State server state
%% @doc gen_server `handle_cast' callback.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(Message :: term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @param Message message to process
%% @param State server state
%% @doc gen_server `handle_info' callback.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(Message :: term(), #state{}) -> {noreply, #state{}}.
handle_info(
    {'DOWN', MonitorRef, process, Pid, Reason}, #state{audio_monitor = {Pid, MonitorRef}} = State0
) ->
    case Reason of
        normal -> ok;
        _ -> exit(Reason)
    end,
    play_next(State0#state{audio_monitor = undefined});
handle_info(timeout, State0) ->
    play_next(State0).

%%-----------------------------------------------------------------------------
%% @param Reason termination reason
%% @param State server state
%% @doc gen_server `terminate' callback.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: #state{}) -> ok.
terminate(_Reason, #state{audio_module = AudioModule, servo_module = ServoModule} = _State) ->
    ServoModule:power_off(),
    AudioModule:power_off(),
    ok.

%%-----------------------------------------------------------------------------
%% @param OldVersion old version
%% @param State server state
%% @param Extra additional update script parameter
%% @doc gen_server `code_change' callback.
%% @end
%%-----------------------------------------------------------------------------
-spec code_change(OldVersion :: term(), State :: #state{}, Extra :: term()) -> {ok, #state{}}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% @param State server state
%% @doc Actually play next item in sequence
%% @end
%%-----------------------------------------------------------------------------
-spec play_next(#state{}) -> {noreply, #state{}} | {noreply, #state{}, timeout()}.
play_next(#state{wait_end = WaitEnd} = State0) when WaitEnd =/= undefined ->
    Now = erlang:system_time(millisecond),
    if
        Now >= WaitEnd ->
            play_next(State0#state{wait_end = undefined});
        true ->
            WaitMS = WaitEnd - Now,
            {noreply, State0, WaitMS}
    end;
play_next(#state{sequence = [{wait, sound} | Tail], audio_monitor = undefined} = State0) ->
    play_next(State0#state{sequence = Tail});
play_next(#state{sequence = [{wait, sound} | _]} = State0) ->
    {noreply, State0};
play_next(#state{sequence = [{wait, servo} | Tail], servo_end = undefined} = State0) ->
    play_next(State0#state{sequence = Tail});
play_next(#state{sequence = [{wait, WaitMS} | Tail]} = State0) when is_integer(WaitMS) ->
    Now = erlang:system_time(millisecond),
    WaitEnd = Now + WaitMS,
    {noreply, State0#state{sequence = Tail, wait_end = WaitEnd}, WaitMS};
play_next(
    #state{
        audio_module = AudioModule, sequence = [{aac, Filename} | Tail], audio_monitor = undefined
    } = State0
) ->
    {Pid, Ref} = spawn_opt(
        fun() ->
            AudioModule:play(Filename)
        end,
        [monitor]
    ),
    State1 = State0#state{sequence = Tail, audio_monitor = {Pid, Ref}},
    play_next(State1);
play_next(
    #state{
        servo_module = ServoModule,
        servo_state = ServoState0,
        sequence = [{servo, Target} | Tail],
        servo_end = undefined
    } = State0
) ->
    {TargetMS, ServoState1} = ServoModule:set_target(Target, ServoState0),
    Now = erlang:system_time(millisecond),
    State1 = State0#state{sequence = Tail, servo_state = ServoState1, servo_end = Now + TargetMS},
    play_next(State1);
play_next(
    #state{
        servo_module = ServoModule,
        servo_state = ServoState0,
        sequence = [{servo, Target, TimeMS} | Tail],
        servo_end = undefined
    } = State0
) ->
    {TargetMS, ServoState1} = ServoModule:set_target(Target, TimeMS, ServoState0),
    Now = erlang:system_time(millisecond),
    State1 = State0#state{sequence = Tail, servo_state = ServoState1, servo_end = Now + TargetMS},
    play_next(State1);
play_next(
    #state{sequence = [], from = From, audio_monitor = undefined, servo_end = undefined} = State0
) ->
    gen_server:reply(From, ok),
    {noreply, State0};
play_next(#state{servo_end = undefined} = State) ->
    {noreply, State};
play_next(
    #state{servo_module = ServoModule, servo_state = ServoState0, servo_end = ServoEnd} = State
) ->
    Now = erlang:system_time(millisecond),
    if
        Now < ServoEnd ->
            {noreply, State, ServoEnd - Now};
        true ->
            ServoState1 = ServoModule:timeout(ServoState0),
            play_next(State#state{servo_state = ServoState1, servo_end = undefined})
    end.

-ifdef(TEST).
start_stop_test_() ->
    {
        setup,
        fun() ->
            ServoMock = la_machine_servo_mock:new(),
            AudioMock = la_machine_audio_mock:new(),
            {ServoMock, AudioMock}
        end,
        fun({ServoMock, AudioMock}) ->
            la_machine_servo_mock:delete(ServoMock),
            la_machine_audio_mock:delete(AudioMock)
        end,
        fun({ServoMock, AudioMock}) ->
            ?_test(begin
                la_machine_servo_mock:expect(ServoMock, power_on, ok),
                la_machine_audio_mock:expect(AudioMock, power_on, ok),
                {ok, Pid} = gen_server:start_link(
                    ?MODULE, {la_machine_audio_mock, la_machine_servo_mock}, []
                ),
                la_machine_servo_mock:assert_called(ServoMock, power_on),
                la_machine_audio_mock:assert_called(AudioMock, power_on),

                la_machine_servo_mock:expect(ServoMock, power_off, ok),
                la_machine_audio_mock:expect(AudioMock, power_off, ok),
                ok = gen_server:stop(Pid),
                la_machine_servo_mock:assert_called(ServoMock, power_off),
                la_machine_audio_mock:assert_called(AudioMock, power_off)
            end)
        end
    }.

collect_messages(Ref, Acc) ->
    receive
        {Ref, Message} -> collect_messages(Ref, [Message | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

play_two_sounds_test_() ->
    {
        setup,
        fun() ->
            AudioMock = la_machine_audio_mock:new(),
            ServoMock = la_machine_servo_mock:new(),
            la_machine_servo_mock:expect(AudioMock, power_on, ok),
            la_machine_servo_mock:expect(ServoMock, power_on, ok),
            {ok, Pid} = gen_server:start_link(
                ?MODULE, {la_machine_audio_mock, la_machine_servo_mock}, []
            ),
            la_machine_audio_mock:assert_called(AudioMock, power_on),
            la_machine_servo_mock:assert_called(ServoMock, power_on),
            {ServoMock, AudioMock, Pid}
        end,
        fun({ServoMock, AudioMock, Pid}) ->
            la_machine_audio_mock:expect(AudioMock, power_off, ok),
            la_machine_servo_mock:expect(ServoMock, power_off, ok),
            ok = gen_server:stop(Pid),
            la_machine_audio_mock:assert_called(AudioMock, power_off),
            la_machine_servo_mock:assert_called(ServoMock, power_off),
            la_machine_audio_mock:delete(AudioMock),
            la_machine_servo_mock:delete(ServoMock)
        end,
        fun({ServoMock, AudioMock, Pid}) ->
            ?_test(begin
                Self = self(),
                Ref = make_ref(),
                la_machine_audio_mock:expect(AudioMock, play, fun(Filename) ->
                    Self ! {Ref, {play, Filename, erlang:system_time(millisecond)}},
                    timer:sleep(500)
                end),
                la_machine_servo_mock:expect(ServoMock, set_target, fun(Target, State) ->
                    Self ! {Ref, {set_target, Target, erlang:system_time(millisecond)}},
                    {100, State}
                end),
                la_machine_servo_mock:expect(ServoMock, timeout, fun(State) ->
                    Self ! {Ref, {timeout, erlang:system_time(millisecond)}},
                    State
                end),
                Before = erlang:system_time(millisecond),
                play(Pid, [
                    {aac, <<"filename_1.aac">>},
                    {servo, 20},
                    {wait, 200},
                    {servo, 30},
                    {aac, <<"filename_2.aac">>},
                    {wait, 100},
                    {servo, 100},
                    {servo, 0}
                ]),
                After = erlang:system_time(millisecond),
                la_machine_audio_mock:assert_called(AudioMock, play),
                ?assert(After - Before >= 1000),
                Messages = collect_messages(Ref, []),
                % Play and servo messages can arrive in a random order because
                % play are sent from another process
                PlayMessages = [Message || Message <- Messages, element(1, Message) =:= play],
                [
                    {play, <<"filename_1.aac">>, Filename1Timestamp},
                    {play, <<"filename_2.aac">>, Filename2Timestamp}
                ] = PlayMessages,
                ServoMessages = [Message || Message <- Messages, element(1, Message) =/= play],
                [
                    {set_target, 20, SetTarget1Timestamp},
                    {timeout, Timeout1Timestamp},
                    {set_target, 30, SetTarget2Timestamp},
                    {timeout, Timeout2Timestamp},
                    {set_target, 100, SetTarget3Timestamp},
                    {timeout, Timeout3Timestamp},
                    {set_target, 0, SetTarget4Timestamp},
                    {timeout, Timeout4Timestamp}
                ] = ServoMessages,
                % Ensure sounds are played one after the other
                ?assert(Filename2Timestamp - Filename1Timestamp >= 500),
                ?assert(After - Filename2Timestamp >= 500),
                % Ensure proper time between servo messages
                ?assert(Timeout1Timestamp - SetTarget1Timestamp >= 100),
                ?assert(SetTarget2Timestamp - SetTarget1Timestamp >= 200),
                ?assert(Timeout2Timestamp - SetTarget2Timestamp >= 100),
                ?assert(SetTarget3Timestamp - Filename2Timestamp >= 100),
                ?assert(Timeout3Timestamp - SetTarget3Timestamp >= 100),
                ?assert(SetTarget4Timestamp - SetTarget3Timestamp >= 100),
                ?assert(Timeout4Timestamp - SetTarget4Timestamp >= 100)
            end)
        end
    }.

play_wait_sound_test_() ->
    {
        setup,
        fun() ->
            AudioMock = la_machine_audio_mock:new(),
            ServoMock = la_machine_servo_mock:new(),
            la_machine_servo_mock:expect(AudioMock, power_on, ok),
            la_machine_servo_mock:expect(ServoMock, power_on, ok),
            {ok, Pid} = gen_server:start_link(
                ?MODULE, {la_machine_audio_mock, la_machine_servo_mock}, []
            ),
            la_machine_audio_mock:assert_called(AudioMock, power_on),
            la_machine_servo_mock:assert_called(ServoMock, power_on),
            {ServoMock, AudioMock, Pid}
        end,
        fun({ServoMock, AudioMock, Pid}) ->
            la_machine_audio_mock:expect(AudioMock, power_off, ok),
            la_machine_servo_mock:expect(ServoMock, power_off, ok),
            ok = gen_server:stop(Pid),
            la_machine_audio_mock:assert_called(AudioMock, power_off),
            la_machine_servo_mock:assert_called(ServoMock, power_off),
            la_machine_audio_mock:delete(AudioMock),
            la_machine_servo_mock:delete(ServoMock)
        end,
        fun({ServoMock, AudioMock, Pid}) ->
            ?_test(begin
                Self = self(),
                Ref = make_ref(),
                la_machine_audio_mock:expect(AudioMock, play, fun(Filename) ->
                    Self ! {Ref, {play, Filename, erlang:system_time(millisecond)}},
                    timer:sleep(500)
                end),
                la_machine_servo_mock:expect(ServoMock, set_target, fun(Target, State) ->
                    Self ! {Ref, {set_target, Target, erlang:system_time(millisecond)}},
                    {100, State}
                end),
                la_machine_servo_mock:expect(ServoMock, timeout, fun(State) ->
                    Self ! {Ref, {timeout, erlang:system_time(millisecond)}},
                    State
                end),
                Before = erlang:system_time(millisecond),
                play(Pid, [
                    {aac, <<"filename_1.aac">>},
                    {wait, sound},
                    {servo, 30}
                ]),
                After = erlang:system_time(millisecond),
                la_machine_audio_mock:assert_called(AudioMock, play),
                ?assert(After - Before >= 600),
                Messages = collect_messages(Ref, []),
                [
                    {play, <<"filename_1.aac">>, Filename1Timestamp},
                    {set_target, 30, SetTarget1Timestamp},
                    {timeout, Timeout1Timestamp}
                ] = Messages,
                % Ensure sounds are played one after the other
                ?assert(SetTarget1Timestamp - Filename1Timestamp >= 500),
                ?assert(After - SetTarget1Timestamp >= 100),
                % Ensure proper time between servo messages
                ?assert(Timeout1Timestamp - SetTarget1Timestamp >= 100)
            end)
        end
    }.

play_wait_servo_test_() ->
    {
        setup,
        fun() ->
            AudioMock = la_machine_audio_mock:new(),
            ServoMock = la_machine_servo_mock:new(),
            la_machine_servo_mock:expect(AudioMock, power_on, ok),
            la_machine_servo_mock:expect(ServoMock, power_on, ok),
            {ok, Pid} = gen_server:start_link(
                ?MODULE, {la_machine_audio_mock, la_machine_servo_mock}, []
            ),
            la_machine_audio_mock:assert_called(AudioMock, power_on),
            la_machine_servo_mock:assert_called(ServoMock, power_on),
            {ServoMock, AudioMock, Pid}
        end,
        fun({ServoMock, AudioMock, Pid}) ->
            la_machine_audio_mock:expect(AudioMock, power_off, ok),
            la_machine_servo_mock:expect(ServoMock, power_off, ok),
            ok = gen_server:stop(Pid),
            la_machine_audio_mock:assert_called(AudioMock, power_off),
            la_machine_servo_mock:assert_called(ServoMock, power_off),
            la_machine_audio_mock:delete(AudioMock),
            la_machine_servo_mock:delete(ServoMock)
        end,
        fun({ServoMock, AudioMock, Pid}) ->
            ?_test(begin
                Self = self(),
                Ref = make_ref(),
                la_machine_audio_mock:expect(AudioMock, play, fun(Filename) ->
                    Self ! {Ref, {play, Filename, erlang:system_time(millisecond)}},
                    timer:sleep(500)
                end),
                la_machine_servo_mock:expect(ServoMock, set_target, fun(Target, TimeMS, State) ->
                    Self ! {Ref, {set_target, Target, erlang:system_time(millisecond)}},
                    {TimeMS, State}
                end),
                la_machine_servo_mock:expect(ServoMock, timeout, fun(State) ->
                    Self ! {Ref, {timeout, erlang:system_time(millisecond)}},
                    State
                end),
                Before = erlang:system_time(millisecond),
                play(Pid, [
                    {servo, 30, 150},
                    {wait, servo},
                    {aac, <<"filename_1.aac">>}
                ]),
                After = erlang:system_time(millisecond),
                la_machine_audio_mock:assert_called(AudioMock, play),
                ?assert(After - Before >= 650),
                Messages = collect_messages(Ref, []),
                [
                    {set_target, 30, SetTarget1Timestamp},
                    {timeout, Timeout1Timestamp},
                    {play, <<"filename_1.aac">>, Filename1Timestamp}
                ] = Messages,
                % Ensure sounds are played one after the other
                ?assert(Filename1Timestamp - SetTarget1Timestamp >= 150),
                ?assert(After - Filename1Timestamp >= 500),
                % Ensure proper time between servo messages
                ?assert(Timeout1Timestamp - SetTarget1Timestamp >= 150)
            end)
        end
    }.
-endif.
