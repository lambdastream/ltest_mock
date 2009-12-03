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
  test0(),
  test0a(),
  test0b(),
  test0c(),
  test0d(),
  test0e(),
  test1(),
  test2(),
  test3(),
  test3a(),
  test3b(),
  test4(),
  test4a(),
  test5(),
  test6(),
  test6a(),
  test7(),
  test7a(),
  io:format("~n~nfinished without unexpected errors! error reports may be ignored!!~n~n~n").

test0() ->
  Mock = mock:new(),
  mock:expect(Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
  mock:expect(Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
  mock:expect(Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
  mock:expect(Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),

  mock:replay(Mock),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),

  mock:verify(Mock).

test0a() ->
  Mock = mock:new(),
  mock:expect(Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
  mock:expect(Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
  mock:expect(Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
  mock:expect(Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),

  mock:replay(Mock),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  {'EXIT',_} = (catch testmodule2:mockme1(1,2)).

test0b() ->
  Mock = mock:new(),
  mock:expect(Mock, in_order, testmodule1, mockme1, [1,2], {return, ok}),
  mock:expect(Mock, in_order, testmodule1, mockme2, [2,3], {return, ok}),
  mock:expect(Mock, in_order, testmodule2, mockme2, [3,2], {return, ok}),
  mock:expect(Mock, in_order, testmodule2, mockme1, [1,2], {return, ok}),

  mock:replay(Mock),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),

  {error, _} = mock:verify(Mock).

test0c() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
  mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
  mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
  mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

  mock:replay(Mock),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),
  testmodule1:mockme2(2,3),

  mock:verify(Mock).

test0d() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
  mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
  mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
  mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

  mock:replay(Mock),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),

  {error, _} = mock:verify(Mock).

test0e() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule1, mockme1, [1,2], {return, ok}),
  mock:o_o   (Mock, testmodule1, mockme2, [2,3], {return, ok}),
  mock:strict(Mock, testmodule2, mockme2, [3,2], {return, ok}),
  mock:o_o   (Mock, testmodule2, mockme1, [1,2], {return, ok}),

  mock:stub  (Mock, testmodule3, mockme, [666], {return, ok}),
  mock:stub  (Mock, testmodule3, mockme, [777], {return, ok}),
  mock:stub  (Mock, testmodule3, mockme, [888], {return, ok}),
  mock:stub  (Mock, testmodule3, mockme, [999], {return, ok}),

  mock:replay(Mock),

  ok = testmodule1:mockme1(1,2),
  ok = testmodule2:mockme2(3,2),
  ok = testmodule3:mockme(777),
  ok = testmodule2:mockme1(1,2),
  ok = testmodule3:mockme(777),
  ok = testmodule3:mockme(888),
  ok = testmodule1:mockme2(2,3),
  ok = testmodule3:mockme(777),

  mock:verify(Mock).

test1() ->
  Mock = mock:new(),
  mock:replay(Mock),
  mock:verify(Mock).

%error expected
test2() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule, mockme, [1,2],{return, ok}),
  mock:replay(Mock),
  {error, _} = mock:verify(Mock).


test3() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule2, mockme1, [666], {error, end_of_times}),
  mock:replay(Mock),
  try testmodule2:mockme1(666) of
 _ ->
      exit(error_expected)
  catch
 error:end_of_times ->
     ok
  end.  

test3a() ->
  Mock = mock:new(),
  mock:o_o(Mock, testmodule2, mockme1, [666], {throw, end_of_times}),
  mock:replay(Mock),
  try testmodule2:mockme1(666) of
 _ ->
      exit(error_expected)
  catch
 throw:end_of_times ->
     ok
  end.  

test3b() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule2, mockme1, [666], {exit, end_of_times}),
  mock:replay(Mock),
  try testmodule2:mockme1(666) of
 _ ->
      exit(error_expected)
  catch
 exit:end_of_times ->
     ok
  end.  

test4() ->
  Mock = mock:new(),
  {error,_} = mock:verify(Mock).

test4a() ->
  Mock = mock:new(),
  {'EXIT',_} = (catch mock:strict(Mock, testmodule1, mockme1, [1,2], {hier_steht_was_falsches, xxx})).

test5() ->
  Mock = mock:new(),
  mock:strict(Mock, testmodule1, mockme1, [1,2], {rec_msg, self()}),
  mock:replay(Mock),
  TestPid = spawn(testmodule1,mockme1,[1,2]),
  TestPid ! test,
  receive
 test ->
     ok,
     mock:verify(Mock)
  after 1000 ->
     error
  end.

test6() ->
  Mock = mock:new(),
  mock:o_o(Mock, testmodule1, mockme1, [1,2], {function, fun(X,Y) ->
            X + Y
           end}),
  mock:replay(Mock),
  R = testmodule1:mockme1(1,2),
  mock:verify(Mock),
  3 = R.

test6a() ->
  Mock = mock:new(),
  mock:stub(Mock, testmodule1, mockme1, [1,2], {function, fun(_,_) ->
            erlang:error(test)
           end}),
  mock:replay(Mock),
  {'EXIT',_} = (catch testmodule1:mockme1(1,2)),
  mock:verify(Mock).

test7() ->
  Mock = mock:new(),
  mock:expect(Mock, in_order, testmodule1, mockme1, 1,
  fun([{qXYZ, D, B, A}]) when A >= B andalso B >= D ->
   true
  end,
  {function, fun({qXYZ, D, B, A}) ->
   [B,D|A]
  end}
        ),
  mock:replay(Mock),
  L = testmodule1:mockme1({qXYZ, 1,2,3}),
  mock:verify(Mock),
  [2,1|3] = L.

test7a() ->
  Mock = mock:new(),
  mock:expect(Mock, in_order, testmodule1, mockme1, 1,
  %% hier fehlen die obligatorischen Klammern
  fun({qXYZ, D, B, A}) when A >= B andalso B >= D ->
   true
  end,
  {function, fun({qXYZ, D, B, A}) ->
   [B,D|A]
  end}
        ),
  mock:replay(Mock),
  {'EXIT',_}= (catch testmodule1:mockme1({qXYZ, 1,2,3})).
