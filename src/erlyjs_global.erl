%%%-------------------------------------------------------------------
%%% File:      erlyjs_global.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2008 Roberto Saccon
%%% @doc
%%%
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2008-02-17 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlyjs_global).
-author('rsaccon@gmail.com').

-define(SKIP_ENCODE_URI(C), ((C >= $a andalso C =< $z) orelse
                             (C >= $A andalso C =< $Z) orelse
                             (C >= $0 andalso C =< $9) orelse
                             (C =:= $- orelse
                              C =:= $_ orelse
                              C =:= $. orelse
                              C =:= $! orelse
                              C =:= $~ orelse
                              C =:= $* orelse
                              C =:= $' orelse
                              C =:= $( orelse
                              C =:= $) orelse
                              C =:= $; orelse
                              C =:= $, orelse
                              C =:= $/ orelse
                              C =:= $? orelse
                              C =:= $: orelse
                              C =:= $@ orelse
                              C =:= $& orelse
                              C =:= $= orelse
                              C =:= $+ orelse
                              C =:= $$ orelse
                              C =:= $#))).

-define(SKIP_ENCODE_URI_COMP(C), ((C >= $a andalso C =< $z) orelse
                                  (C >= $A andalso C =< $Z) orelse
                                  (C >= $0 andalso C =< $9) orelse
                                  (C =:= $- orelse
                                   C =:= $_ orelse
                                   C =:= $. orelse
                                   C =:= $! orelse
                                   C =:= $~ orelse
                                   C =:= $* orelse
                                   C =:= $' orelse
                                   C =:= $( orelse
                                   C =:= $)))).

%% API
-export([
    decodeURI/1,
    decodeURIComponent/1,
    encodeURI/1,
    encodeURIComponent/1,
    eval/1,
    eval/2,
    isFinite/1,
    isNaN/1,
    parseInt/1,
    parseInt/2,
    parseFloat/1,
    get_mod_func/3]).

-include("erlyjs.hrl").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec
%% @doc
%% @end
%%--------------------------------------------------------------------

decodeURI(_Str) ->
    exit(not_implemented_yet).


decodeURIComponent(_Str) ->
    exit(not_implemented_yet).


encodeURI(Str) ->
    ?l2b(encode(?b2l(Str), uri)).


encodeURIComponent(Str) ->
    ?l2b(encode(?b2l(Str), uri_component)).


%% TODO: variable mapping and bindings
eval(Str) ->
    case erlyjs_compiler:parse(Str) of
        {ok, JsParseTree} ->
            try erlyjs_compiler:parse_transform(JsParseTree) of
                {ErlAstList, _, _} ->
                    L = [erl_syntax:revert(X) || X <- ErlAstList],
                    case  erl_eval:exprs(L, erl_eval:new_bindings()) of
                        {value, Value, _Bindings} ->
                            Value;
                        Err ->
                            Err
                    end;
                Err ->
                    Err
            catch
                throw:Err ->
                    Err
            end;
        Err ->
            Err
    end.


eval(Str, _Object) when is_list(Str) ->
    exit(not_implemented_yet).


%% TODO: check for positive infinity or negative infinity
isFinite(X) ->
    case isNaN(X) of
        true ->
            false;
        _ ->
            true
    end.



%% TODO: check for positive infinity or negative infinity
isNaN('NaN') ->
    true;
isNaN(X) when is_number(X) ->
    false;
isNaN(X) ->
    case isNaN(parseInt(X)) of
        true ->
            isNaN(parseFloat(X));
        _ ->
            false
    end.


parseInt(Str) ->
    parseInt2(?b2l(Str)).


parseInt(_Str, _Radix) ->
    exit(not_implemented_yet).


parseFloat(Str) ->
    parseFloat2(?b2l(Str)).


get_mod_func(decodeURI, [], 1) -> {erlyjs_global, decodeURI};
get_mod_func(decodeURIComponent, [], 1) -> {erlyjs_global, decodeURIComponent};
get_mod_func(encodeURI, [], 1) -> {erlyjs_global, encodeURI};
get_mod_func(encodeURIComponent, [], 1) -> {erlyjs_global, encodeURIComponent};
get_mod_func(eval, [], 1) -> {erlyjs_global, eval};
get_mod_func(eval, [], 2) -> {erlyjs_global, eval};
get_mod_func(isFinite, [],1) -> {erlyjs_global, isFinite};
get_mod_func(isNaN, [], 1) -> {erlyjs_global, isNaN};
get_mod_func(parseInt, [], 1) -> {erlyjs_global, parseInt};
get_mod_func(parseInt, [], 2) -> {erlyjs_global, parseInt};
get_mod_func(parseFloat, [], 1) -> {erlyjs_global, parseFloat};
%%
get_mod_func('Date', [now], 0) -> {erlyjs_global_date, now};
get_mod_func('Date', [parse], 1) -> {erlyjs_global_date, parse};
%%
get_mod_func('Math', [abs], 1) -> {erlyjs_global_math, abs};
get_mod_func('Math', [acos], 1) -> {erlyjs_global_math, acos};
get_mod_func('Math', [asin], 1) -> {erlyjs_global_math, asin};
get_mod_func('Math', [atan], 1) -> {erlyjs_global_math, atan};
get_mod_func('Math', [atan2], 2) -> {erlyjs_global_math, atan2};
get_mod_func('Math', [ceil], 1) -> {erlyjs_global_math, ceil};
get_mod_func('Math', [cos], 1) -> {erlyjs_global_math, cos};
get_mod_func('Math', [exp], 1) -> {erlyjs_global_math, exp};
get_mod_func('Math', [floor], 1) -> {erlyjs_global_math, floor};
get_mod_func('Math', [log], 1) -> {erlyjs_global_math, log};
get_mod_func('Math', [max], 2) -> {erlyjs_global_math, max};
get_mod_func('Math', [min], 2) -> {erlyjs_global_math, min};
get_mod_func('Math', [pow], 2) -> {erlyjs_global_math, pow};
get_mod_func('Math', [random], 0) -> {erlyjs_global_math, random};
get_mod_func('Math', [round], 1) -> {erlyjs_global_math, round};
get_mod_func('Math', [sin], 1) -> {erlyjs_global_math, sin};
get_mod_func('Math', [sqrt], 1) -> {erlyjs_global_math, sqrt};
get_mod_func('Math', [tan], 1) -> {erlyjs_global_math, tan};
%%
get_mod_func('String', [charAt], 1) -> {erlyjs_global_string, charAt};
get_mod_func('String', [charCodeAt], 1) -> {erlyjs_global_string, charCodeAt};
get_mod_func('String', [concat], 1) -> {erlyjs_global_string, concat};
%% get_mod_func('String', [indexOf], 2) -> {erlyjs_global_string, indexOf};   % ??? delete ???
get_mod_func(Name, [indexOf], 1) -> {erlyjs_global_string, indexOf, Name};
get_mod_func('String', [indexOf], 3) -> {erlyjs_global_string, indexOf};
%% get_mod_func('String', [lastIndexOf], 2) -> {erlyjs_global_string, lastIndexOf};   % ??? delete ???
get_mod_func(Name, [lastIndexOf], 1) -> {erlyjs_global_string, lastIndexOf, Name};
get_mod_func('String', [lastIndexOf], 3) -> {erlyjs_global_string, lastIndexOf};
get_mod_func('String', [localeCompare], 1) -> {erlyjs_global_string, localeCompare};
get_mod_func('String', [match], 1) -> {erlyjs_global_string, match};
get_mod_func('String', [replace], 2) -> {erlyjs_global_string, replace};
get_mod_func('String', [search], 1) -> {erlyjs_global_string, search};
get_mod_func('String', [slice], 1) -> {erlyjs_global_string, slice};
get_mod_func('String', [slice], 2) -> {erlyjs_global_string, slice};
get_mod_func('String', [split], 0) -> {erlyjs_global_string, split};
get_mod_func('String', [split], 1) -> {erlyjs_global_string, split};
get_mod_func('String', [split], 2) -> {erlyjs_global_string, split};
get_mod_func('String', [substr], 1) -> {erlyjs_global_string, substr};
get_mod_func('String', [substr], 2) -> {erlyjs_global_string, substr};
get_mod_func('String', [substring], 1) -> {erlyjs_global_string, substring};
get_mod_func('String', [substring], 2) -> {erlyjs_global_string, substring};
get_mod_func('String', [toLocaleLowerCase], 0) -> {erlyjs_global_string, toLocaleLowerCase};
%% get_mod_func('String', [toLocaleUpperCase], 0) -> {erlyjs_global_string, toLocaleUpperCase};  % ??? delete ???
get_mod_func(Name, [toLowerCase], 0) -> {erlyjs_global_string, toLowerCase, Name};
get_mod_func('String', [toString], 0) -> {erlyjs_global_string, toString};
%% get_mod_func('String', [toUpperCase], 1) -> {erlyjs_global_string, toUpperCase};  % ??? delete ???
get_mod_func(Name, [toUpperCase], 0) -> {erlyjs_global_string, toUpperCase, Name};
get_mod_func('String', [valueOf], 0) -> {erlyjs_global_string, valueOf};
%%
get_mod_func(load, [], 1)  -> {erlyjs_api, load};
get_mod_func(print, [], 1)  -> {erlyjs_api, print};
%%
get_mod_func(_, _, _) -> err.


%%====================================================================
%% Internal functions
%%====================================================================

%% TODO: a lot, this is just the most simple case
parseInt2(Str) ->
    try list_to_integer(Str) of
        Val -> Val
    catch
        error:badarg -> 'NaN'
    end.


%% TODO: a lot, this is just the most simple case
parseFloat2(X) ->
    try list_to_float(X) of
        Val -> Val
    catch
        error:badarg -> 'NaN'
    end.


encode([Input], Type) when is_list(Input) or is_binary(Input) ->
    encode(Input, Type);
encode(Input, Type) when is_binary(Input) ->
    encode(Input, Type, 0);
encode(Input, Type) when is_list(Input) ->
    encode(Input, Type, []).


encode(Input, Type, Index) when is_binary(Input) ->
    case {Input, Type} of
        {<<_:Index/binary, Byte, _/binary>>, uri = Type} when ?SKIP_ENCODE_URI(Byte) ->
            encode(Input, Type, Index + 1);
        {<<_:Index/binary, Byte, _/binary>>, uri_component = Type} when ?SKIP_ENCODE_URI_COMP(Byte) ->
            encode(Input, Type, Index + 1);
        {<<Pre:Index/binary, Hi:4, Lo:4, Post/binary>>, Type} ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), encode(Post, Type, 0));
        {Input, _} ->
            Input
    end;
encode([], _, Acc) ->
    lists:reverse(Acc);
encode([C | Rest], uri = Type, Acc) when ?SKIP_ENCODE_URI(C) ->
    encode(Rest, Type, [C | Acc]);
encode([C | Rest], uri_component = Type, Acc) when ?SKIP_ENCODE_URI_COMP(C) ->
    encode(Rest, Type, [C | Acc]);
encode([C | Rest], Type, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    encode(Rest, Type, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).


process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
