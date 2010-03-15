%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuel.rivas@lambdastream.com>
%%% @copyright (C) 2009, Samuel Rivas
%%% @doc Tests for ltest_mock.erl
%%%
%%% @end
%%% Created :  3 Dec 2009 by Samuel Rivas <samuel.rivas@lambdastream.com>
%%%-------------------------------------------------------------------
-module(ltest_mock_eunit).
-include_lib("eunit/include/eunit.hrl").

in_order_test_() ->
    {setup,
     fun () ->
             Mock = ltest_mock:new(),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),
             Mock
     end,
     mock_cleanup(),
     fun (Mock) ->
             [
              % XXX We cannot call replay in the setup since verify must be
              % called in the  same process
              ?_test(ltest_mock:replay(Mock)),

              ?_assertEqual(ok, testmodule1:mockme1(1,2)),
              ?_assertEqual(ok, testmodule1:mockme2(2,3)),
              ?_assertEqual(ok, testmodule2:mockme2(3,2)),
              ?_assertEqual(ok, testmodule2:mockme1(1,2)),

              % This funcion hangs when not called in the same process as verify
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

invalid_invocation_raises_error_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),

             % XXX We have to unlink the mock here or the faulty call in the
             % test will kill the testing process
             unlink(Mock),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_test(testmodule1:mockme1(1,2)),
              ?_test(testmodule1:mockme2(2,3)),

              % XXX The mock dies badly because of this call, thus killing the
              % testing process and ruining the test if not unlinked in the
              % setup
              ?_assertError(
                 {unexpected_invocation, {_Pid, testmodule2, mockme1, 2, [1,2]}},
                 testmodule2:mockme1(1,2))
             ]
     end}.

not_calling_expected_invocations_raises_error_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
             ltest_mock:expect(
               Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),
             unlink(Mock),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_test(testmodule1:mockme1(1,2)),
              ?_test(testmodule1:mockme2(2,3)),
              ?_test(testmodule2:mockme2(3,2)),

              ?_assertThrow(
                 {mock_failure, {expected_invocations_missing, _}},
                 ltest_mock:verify(Mock))
             ]
     end}.

o_o_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
             ltest_mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
             ltest_mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
             ltest_mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),

              ?_test(testmodule1:mockme1(1,2)),
              ?_test(testmodule2:mockme2(3,2)),
              ?_test(testmodule2:mockme1(1,2)),
              ?_test(testmodule1:mockme2(2,3)),

              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

o_o_not_calling_expected_invocations_raises_error_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
             ltest_mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
             ltest_mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
             ltest_mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),
             unlink(Mock),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),

              ?_test(testmodule1:mockme1(1,2)),
              ?_test(testmodule2:mockme2(3,2)),
              ?_test(testmodule2:mockme1(1,2)),

              ?_assertThrow(
                 {mock_failure, {expected_invocations_missing, _}},
                 ltest_mock:verify(Mock))
             ]
     end}.

o_o_bug252_test_() ->
    {setup,
     fun() ->
	     Mock = ltest_mock:new(),
	     ltest_mock:o_o(Mock, testmodule1, mockme1, [1,3], {return, ok}),
	     ltest_mock:o_o(Mock, testmodule1, mockme1, [1,2], {return, ok}),
	     ltest_mock:o_o(Mock, testmodule1, mockme1, [1,5], {return, ok}),
	     ltest_mock:o_o(Mock, testmodule1, mockme1, [1,1], {return, ok}),
	     unlink(Mock),
	     Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
	     [
	      ?_test(ltest_mock:replay(Mock)),
	      ?_test(testmodule1:mockme1(1,5)), 
	      ?_test(testmodule1:mockme1(1,3)),
	      ?_test(testmodule1:mockme1(1,1)),
	      ?_test(testmodule1:mockme1(1,2)),
	      ?_test(ltest_mock:verify(Mock))
	     ]
     end}.

stub_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
             ltest_mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
             ltest_mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
             ltest_mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

             ltest_mock:stub  (Mock, testmodule3, mockme, [666], {return, ok}),
             ltest_mock:stub  (Mock, testmodule3, mockme, [777], {return, ok}),
             ltest_mock:stub  (Mock, testmodule3, mockme, [888], {return, ok}),
             ltest_mock:stub  (Mock, testmodule3, mockme, [999], {return, ok}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),

              ?_assertMatch(ok, testmodule1:mockme1(1,2)),
              ?_assertMatch(ok, testmodule2:mockme2(3,2)),
              ?_assertMatch(ok, testmodule3:mockme(777)),
              ?_assertMatch(ok, testmodule2:mockme1(1,2)),
              ?_assertMatch(ok, testmodule3:mockme(777)),
              ?_assertMatch(ok, testmodule3:mockme(888)),
              ?_assertMatch(ok, testmodule1:mockme2(2,3)),
              ?_assertMatch(ok, testmodule3:mockme(777)),

              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

stub_bug257_test_() ->
    {setup,
     fun() ->
	     Mock = ltest_mock:new(),
	     ltest_mock:stub(Mock, testmodule, mockme, [1], {return, ok}),
	     unlink(Mock),
	     Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
	      ?_assertMatch(ok, testmodule:mockme(1)),
	      ?_assertMatch(ok, testmodule:mockme(1)),
	      ?_assertMatch(ok, testmodule:mockme(1)),
	      ?_assertError(
                 {unexpected_invocation, {_Pid, testmodule, mockme, 1, [2]}},
		 testmodule:mockme(2))
             ]
     end}.

stub_bug257_2_test_() ->
    {setup,
     fun() ->
	     Mock = ltest_mock:new(),
	     ltest_mock:expect(Mock, stub, testmodule, mockme, 2,
			       fun([_, 2]) ->
				       true;
				  ([_, _])->
				       false
			       end,
			       {return, ok}),
	     unlink(Mock),
	     Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
	      ?_assertMatch(ok, testmodule:mockme(2, 2)),
	      ?_assertError(
		 {unexpected_invocation, {_Pid, testmodule, mockme, 2, [1,4]}},
		 testmodule:mockme(1, 4))
             ]
     end}.

stub_bug257_3_test_() ->
    {setup,
     fun() ->
	     Mock = ltest_mock:new(),
	     ltest_mock:strict(Mock, testmodule, mockme2, [10], {return, ok}),
	     ltest_mock:stub(Mock, testmodule, mockme, [1], {return, ok}),
	     unlink(Mock),
	     Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
	      ?_assertMatch(ok, testmodule:mockme(1)),
	      ?_assertMatch(ok, testmodule:mockme(1)),
	      ?_assertMatch(ok, testmodule:mockme(1)),
	      ?_assertMatch(ok, testmodule:mockme2(10)),
	      ?_assertThrow({mock_failure, {invalid_state, _}},
			    ltest_mock:replay(Mock))
             ]
     end}.

stub_bug257_4_test_() ->
    {setup,
     fun() ->
	     Mock = ltest_mock:new(),
	     ltest_mock:expect(Mock, out_of_order, testmodule, mockme2, 1,
			       fun([a, b]) -> true end, {return, ok}),
	     unlink(Mock),
	     Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
	      ?_assertError({matching_function_is_incorrect, _, _},
			    testmodule:mockme2(3))
             ]
     end}.


verify_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:replay(Mock),
    ltest_mock:verify(Mock).

verify_after_fail_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:strict(Mock, testmodule, mockme, [1,2],{return, ok}),
             unlink(Mock),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertError(
		 {unexpected_invocation, {_Pid, testmodule, mockme, 2, [1,4]}},
		 testmodule:mockme(1, 4)),
	      ?_assertThrow({mock_failure,mock_failed_before_verify},
			    ltest_mock:verify(Mock))
             ]
     end}.

%error expected
strict_unexpected_invocation_throws_exception_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:strict(Mock, testmodule, mockme, [1,2],{return, ok}),
             unlink(Mock),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertThrow(
                 {mock_failure, {expected_invocations_missing, _}},
                 ltest_mock:verify(Mock))
             ]
     end}.

strict_error_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:strict(
               Mock, testmodule2, mockme1, [666], {error, end_of_times}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertError(end_of_times, testmodule2:mockme1(666)),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

strict_throw_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:o_o(
               Mock, testmodule2, mockme1, [666], {throw, end_of_times}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertThrow(end_of_times, testmodule2:mockme1(666)),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

strict_exit_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:o_o(
               Mock, testmodule2, mockme1, [666], {exit, end_of_times}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertExit(end_of_times, testmodule2:mockme1(666)),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

verify_without_spec_fails_test() ->
    Mock = ltest_mock:new(),
    ?assertThrow(
       {mock_failure, {invalid_state, verify}},ltest_mock:verify(Mock)).

strict_wrong_spec_raises_function_clause_() ->
    {setup,
     fun() ->
             ltest_mock:new()
     end,
     mock_cleanup(),
     fun(Mock) ->
             ?_assertError(
                function_clause,
                ltest_mock:strict(
                  Mock, testmodule1, mockme1, [1,2],
                  {hier_steht_was_falsches, xxx}))
     end}.

rec_msg_test_() ->
    {setup,
     fun() ->
             ltest_mock:new()
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(
                 ltest_mock:strict(
                   Mock, testmodule1, mockme1, [1,2], {rec_msg, self()})),
              ?_test(ltest_mock:replay(Mock)),
              ?_test(
                 begin
                     TestPid = spawn(testmodule1,mockme1,[1,2]),
                     TestPid ! test,
                     receive
                         test ->
                             ok
                     end
                 end),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.



o_o_fun_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:o_o(
               Mock, testmodule1, mockme1, [1,2],
               {function, fun(X,Y) -> X + Y end}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertEqual(3, testmodule1:mockme1(1,2)),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

stub_fun_can_raise_errors_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:stub(
               Mock, testmodule1, mockme1, [1,2],
               {function, fun(_,_) -> erlang:error(test) end}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertError(test, testmodule1:mockme1(1,2)),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

expect_matcher_fun_test_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme1, 1,
               fun([{qXYZ, D, B, A}]) when A >= B andalso B >= D ->
                       true
               end,
               {function, fun({qXYZ, D, B, A}) ->
                                  [B,D|A]
                          end}),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertMatch([2,1|3], testmodule1:mockme1({qXYZ, 1,2,3})),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

expect_matcher_fun_error_throws_exception_test_() ->
    {setup,
     fun() ->
             Mock = ltest_mock:new(),
             ltest_mock:expect(
               Mock, in_order, testmodule1, mockme1, 1,
               % hier fehlen die obligatorischen Klammern
               fun({qXYZ, D, B, A}) when A >= B andalso B >= D ->
                       true;
                  (_) -> false
               end,
               {function, fun({qXYZ, D, B, A}) -> [B,D|A] end}),
             unlink(Mock),
             Mock
     end,
     mock_cleanup(),
     fun(Mock) ->
             [
              ?_test(ltest_mock:replay(Mock)),
              ?_assertError(
                 {unexpected_invocation,
                  {_Pid, testmodule1, mockme1, 1, [{qXYZ, 1, 2, 3}]}},
                 testmodule1:mockme1({qXYZ, 1,2,3}))
              ]
     end}.

%%%-------------------------------------------------------------------
%%% Common functions
%%%-------------------------------------------------------------------

%% This cleanup function avoids leaking Mocks in some tests
mock_cleanup() ->
    fun (Mock) ->
            exit(Mock, kill)
    end.

