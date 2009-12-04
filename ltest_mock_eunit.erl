%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuel.rivas@lambdastream.com>
%%% @copyright (C) 2009, Samuel Rivas
%%% @doc Tests for ltest_mock.erl
%%%
%%% @end
%%% Created :  3 Dec 2009 by Samuel Rivas <samuel.rivas@lambdastream.com>
%%%-------------------------------------------------------------------
-module(ltest_mock_eunit).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

suite() ->
%    test0_test(),
    test0a_test(),
    test0b_test(),
    test0c_test(),
    test0d_test(),
    test0e_test(),
    test1_test(),
    test2_test(),
    test3_test(),
    test3a_test(),
    test3b_test(),
    test4_test(),
    test4a_test(),
    test5_test(),
    test6_test(),
    test6a_test(),
    test7_test(),
    test7a_test(),
    io:format("~n~nfinished without unexpected errors! error reports may be ignored!!~n~n~n").

test0_test_() ->
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
             ltest_mock:replay(Mock),
             Mock
     end,
     fun (_) -> ok end,
     fun (Mock) ->
             [
              ?_assertEqual(ok, testmodule1:mockme1(1,2)),
              ?_assertEqual(ok, testmodule1:mockme2(2,3)),
              ?_assertEqual(ok, testmodule2:mockme2(3,2)),
              ?_assertEqual(ok, testmodule2:mockme1(1,2)),
              ?_test(ltest_mock:verify(Mock))
             ]
     end}.

test0a_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:expect(Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
    ltest_mock:expect(Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
    ltest_mock:expect(Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
    ltest_mock:expect(Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),

    ltest_mock:replay(Mock),

    testmodule1:mockme1(1,2),
    testmodule1:mockme2(2,3),
    {'EXIT',_} = (catch testmodule2:mockme1(1,2)).

test0b_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:expect(Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
    ltest_mock:expect(Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
    ltest_mock:expect(Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
    ltest_mock:expect(Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),

    ltest_mock:replay(Mock),

    testmodule1:mockme1(1,2),
    testmodule1:mockme2(2,3),
    testmodule2:mockme2(3,2),

    {error, _} = ltest_mock:verify(Mock).

test0c_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
    ltest_mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
    ltest_mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
    ltest_mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

    ltest_mock:replay(Mock),

    testmodule1:mockme1(1,2),
    testmodule2:mockme2(3,2),
    testmodule2:mockme1(1,2),
    testmodule1:mockme2(2,3),

    ltest_mock:verify(Mock).

test0d_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
    ltest_mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
    ltest_mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
    ltest_mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

    ltest_mock:replay(Mock),

    testmodule1:mockme1(1,2),
    testmodule2:mockme2(3,2),
    testmodule2:mockme1(1,2),

    {error, _} = ltest_mock:verify(Mock).

test0e_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
    ltest_mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
    ltest_mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
    ltest_mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

    ltest_mock:stub  (Mock, testmodule3, mockme, [666], {return, ok}),
    ltest_mock:stub  (Mock, testmodule3, mockme, [777], {return, ok}),
    ltest_mock:stub  (Mock, testmodule3, mockme, [888], {return, ok}),
    ltest_mock:stub  (Mock, testmodule3, mockme, [999], {return, ok}),

    ltest_mock:replay(Mock),

    ok = testmodule1:mockme1(1,2),
    ok = testmodule2:mockme2(3,2),
    ok = testmodule3:mockme(777),
    ok = testmodule2:mockme1(1,2),
    ok = testmodule3:mockme(777),
    ok = testmodule3:mockme(888),
    ok = testmodule1:mockme2(2,3),
    ok = testmodule3:mockme(777),

    ltest_mock:verify(Mock).

test1_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:replay(Mock),
    ltest_mock:verify(Mock).

%error expected
test2_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule, mockme, [1,2],{return, ok}),
    ltest_mock:replay(Mock),
    {error, _} = ltest_mock:verify(Mock).


test3_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule2, mockme1, [666], {error, end_of_times}),
    ltest_mock:replay(Mock),
    try testmodule2:mockme1(666) of
        _ ->
            exit(error_expected)
    catch
        error:end_of_times ->
            ok
    end.

test3a_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:o_o(Mock, testmodule2, mockme1, [666], {throw, end_of_times}),
    ltest_mock:replay(Mock),
    try testmodule2:mockme1(666) of
        _ ->
            exit(error_expected)
    catch
        throw:end_of_times ->
            ok
    end.

test3b_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule2, mockme1, [666], {exit, end_of_times}),
    ltest_mock:replay(Mock),
    try testmodule2:mockme1(666) of
        _ ->
            exit(error_expected)
    catch
        exit:end_of_times ->
            ok
    end.

test4_test() ->
    Mock = ltest_mock:new(),
    {error,_} = ltest_mock:verify(Mock).

test4a_test() ->
    Mock = ltest_mock:new(),
    {'EXIT',_} =
        (catch ltest_mock:strict(
                 Mock, testmodule1, mockme1, [1,2],
                 {hier_steht_was_falsches, xxx})).

test5_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:strict(Mock, testmodule1, mockme1, [1,2], {rec_msg, self()}),
    ltest_mock:replay(Mock),
    TestPid = spawn(testmodule1,mockme1,[1,2]),
    TestPid ! test,
    receive
        test ->
            ok,
            ltest_mock:verify(Mock)
    after 1000 ->
            error
    end.

test6_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:o_o(
      Mock, testmodule1, mockme1, [1,2],
      {function, fun(X,Y) ->
                         X + Y
                 end}),
    ltest_mock:replay(Mock),
    R = testmodule1:mockme1(1,2),
    ltest_mock:verify(Mock),
    3 = R.

test6a_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:stub(
      Mock, testmodule1, mockme1, [1,2],
      {function, fun(_,_) ->
                         erlang:error(test)
                 end}),
    ltest_mock:replay(Mock),
    {'EXIT',_} = (catch testmodule1:mockme1(1,2)),
    ltest_mock:verify(Mock).

test7_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:expect(
      Mock, in_order, testmodule1, mockme1, 1,
      fun([{qXYZ, D, B, A}]) when A >= B andalso B >= D ->
              true
      end,
      {function, fun({qXYZ, D, B, A}) ->
                         [B,D|A]
                 end}),
    ltest_mock:replay(Mock),
    L = testmodule1:mockme1({qXYZ, 1,2,3}),
    ltest_mock:verify(Mock),
    [2,1|3] = L.

test7a_test() ->
    Mock = ltest_mock:new(),
    ltest_mock:expect(
      Mock, in_order, testmodule1, mockme1, 1,
      % hier fehlen die obligatorischen Klammern
      fun({qXYZ, D, B, A}) when A >= B andalso B >= D ->
              true
      end,
      {function,
       fun({qXYZ, D, B, A}) ->
               [B,D|A]
       end}),
    ltest_mock:replay(Mock),
    {'EXIT',_} = (catch testmodule1:mockme1({qXYZ, 1,2,3})).
