%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        Javascript to Erlang compiler
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
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
%%%---------------------------------------------------------------------------------------
-module(erlyjs_compiler).
-author('rsaccon@gmail.com').

%% API
-export([parse/1, parse_transform/1, compile/2, compile/3]).

-import(erlyjs_global, [get_mod_func/3]).

-include("erlyjs.hrl").


compile(File, Module) ->
    compile(File, Module, []).

compile(File, Module, Options) ->
    Ctx = init_js_ctx(File, Module, Options),
    {M, F} = Ctx#js_ctx.reader,
    case catch M:F(File) of
    {ok, Data} ->
        crypto:start(),
        CheckSum = crypto:sha(Data),
        case parse(CheckSum, Data, Ctx) of
        ok ->
            ok;
        {ok, JsParseTree} ->
            trace(?MODULE, ?LINE, "JsParseTree", JsParseTree, Ctx),
            try p_t_root(JsParseTree, Ctx, #trav{}) of
                {AstList, Info, _} ->
                    Forms = forms(CheckSum, Module, AstList, Info),
                    RevertedForms = revert_forms(Forms),
                    case Ctx#js_ctx.verbose of
                    true ->
                        trace(?MODULE, ?LINE, "Forms", RevertedForms, Ctx),
                        io:format("Erlang source:~n~n"),
                        Hook = fun (Node, Ctxt, Cont) ->
                                    case type(Node) of
                                    binary ->
                                        case get_ann(Node) of
                                        [] -> Cont(Node, Ctxt);
                                        Anns ->  prettypr:text("<<\"" ++ hd(Anns) ++ "\">>")
                                        end;
                                    _ ->
                                        Cont(Node, Ctxt)
                                    end
                               end,
                        io:put_chars(erl_prettypr:format(form_list(Forms), [{hook, Hook}])),
                        io:format("~n~n");
                    _ ->
                        ok
                    end,
                    compile_forms(RevertedForms, Ctx)
            catch
                throw:Error ->
                    Error
            end;
        Error ->
            Error
        end;
    _ ->
        {error, "reading " ++ File ++ " failed "}
    end.


parse(Data) when is_binary(Data) ->
    parse(?b2l(Data));
parse(Data) ->
    case erlyjs_scan:string(Data) of
    {ok, Tokens, _} -> erlyjs_parser:parse(Tokens);
    Err -> Err
    end.


parse_transform(JsParseTree) ->
    parse_transform(JsParseTree, #js_ctx{}, #trav{}).


%%====================================================================
%% Internal functions
%%====================================================================

init_js_ctx(_File, Module, Options) ->
    Ctx = #js_ctx{},
    #js_ctx{
        module = list_to_atom(Module),
        out_dir = proplists:get_value(out_dir, Options,  Ctx#js_ctx.out_dir),
        verbose = proplists:get_value(verbose, Options, Ctx#js_ctx.verbose),
        reader = proplists:get_value(reader, Options, Ctx#js_ctx.reader),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#js_ctx.force_recompile)}.


parse(_, Data, #js_ctx{force_recompile = true}) ->
    parse(Data);
parse(CheckSum, Data, Ctx) ->
    Module = Ctx#js_ctx.module,
    case catch Module:checksum() of
    CheckSum -> ok;
    _ -> parse(Data)
    end.


forms(Checksum, Module, FuncAsts, Info) ->
    FuncAsts2 = lists:append([FuncAsts, Info#ast_inf.internal_func_asts]),
    InitFuncAstBody = case Info#ast_inf.global_asts of
    [] -> [tuple([atom("error"), atom("no_global_code")])];
    List -> lists:reverse([atom(ok) | List])
    end,
    InitFuncAst = function(atom("jsinit"), [clause([], none, InitFuncAstBody)]),
    ResetFuncAstFunBodyCase = application(atom(string), atom(str), [variable("KeyString"), string("js_")]),
    ResetFuncAstFunBody = [
        match_expr(tuple([variable("Key"), underscore()]), variable("X")),
        match_expr(variable("KeyString"), application(none, atom(atom_to_list), [variable("Key")])),
        case_expr(ResetFuncAstFunBodyCase, [
            clause([integer(1)], none, [application(none, atom(erase), [variable("Key")])]),
            clause([underscore()], none, [atom(ok)])])],
    ResetFuncAstBody = [
        application(atom(lists), atom(map), [
            fun_expr([clause([variable("X")], none, ResetFuncAstFunBody)]),
            application(none, atom(get), [])]),
        atom(ok)],
    ResetFuncAst = function(atom("jsreset"), [clause([], none, ResetFuncAstBody)]),
    ChecksumFuncAst = function(atom("checksum"), [clause([], none, [binary_ast(Checksum)])]),
    ModuleAst = attribute(atom(module), [atom(Module)]),
    ExportInit = arity_qualifier(atom("jsinit"), integer(0)),
    ExportReset = arity_qualifier(atom("jsreset"), integer(0)),
    ExportChecksum = arity_qualifier(atom("checksum"), integer(0)),
    ExportAst = attribute(atom(export), [list([ExportInit, ExportReset, ExportChecksum | Info#ast_inf.export_asts])]),
    [ModuleAst, ExportAst, InitFuncAst, ResetFuncAst, ChecksumFuncAst | FuncAsts2].


compile_forms(Forms, Ctx) ->
    CompileOptions = case Ctx#js_ctx.verbose of
    true -> [debug_info, verbose, report_errors, report_warnings];
    _ -> [debug_info]
    end,
    case compile:forms(Forms, CompileOptions) of
    {ok, Module1, Bin} ->
        Path = filename:join([Ctx#js_ctx.out_dir, atom_to_list(Module1) ++ ".beam"]),
        case file:write_file(Path, Bin) of
        ok ->
            code:purge(Module1),
            case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
            {module, _} -> ok;
            _ -> {error, "code reload failed"}
            end;
        {error, Reason} ->
            {error, lists:concat(["beam generation failed (", Reason, "): ", Path])}
        end;
    error ->
        {error, "compilation failed"}
    end.


p_t_root(JsParseTree, Ctx, Trav) ->
    parse_transform(JsParseTree, Ctx#js_ctx{global = true}, Trav).


p_t(JsParseTree, Ctx, Trav) ->
    parse_transform(JsParseTree, Ctx#js_ctx{global = false}, Trav).


parse_transform(JsParseTree, Ctx, Trav) when is_list(JsParseTree) ->
    {AstInfList, {_, Trav1}} = lists:mapfoldl(fun ast/2, {Ctx, Trav}, JsParseTree),
    {AstList, Inf} = lists:mapfoldl(
        fun({XAst, XInf}, InfAcc) ->
            {XAst, append_info(XInf, InfAcc)}
        end, #ast_inf{}, AstInfList),
    {lists:flatten(AstList), Inf, Trav1};
parse_transform(JsParseTree, Ctx, Trav) ->
    {{Ast, Inf}, {_, Trav1}} = ast(JsParseTree, {Ctx, Trav}),
    {Ast, Inf, Trav1}.


ast({identifier, _, true}, {Ctx, Trav}) ->
    {{atom(true), #ast_inf{}}, {Ctx, Trav}};
ast({identifier, _, false}, {Ctx, Trav}) ->
    {{atom(false), #ast_inf{}}, {Ctx, Trav}};
ast({identifier, _, null}, {Ctx, Trav}) ->
    {{atom(null), #ast_inf{}}, {Ctx, Trav}};
ast({{identifier, _, undefined}, _}, {Ctx, Trav}) ->
    {{atom(undefined), #ast_inf{}}, {Ctx, Trav}};
ast({{identifier, _, 'Infinity'}, _}, {Ctx, Trav}) ->
    {{atom('Infinity'), #ast_inf{}}, {Ctx, Trav}};
ast({{identifier, _, 'NaN'}, _}, {Ctx, Trav}) ->
    {{atom('NaN'), #ast_inf{}}, {Ctx, Trav}};
ast({identifier, _, Name}, {Ctx, Trav}) ->
    var_ast(Name, Ctx, Trav);
ast({integer, _, Value}, {Ctx, Trav}) ->
    {{integer(Value), #ast_inf{}}, {Ctx, Trav}};
ast({float, _, Value}, {Ctx, Trav}) ->
    {{float(Value), #ast_inf{}}, {Ctx, Trav}};
ast({string, _, Value}, {Ctx, Trav}) ->
    {{add_ann(Value, binary_ast(Value)), #ast_inf{}}, {Ctx, Trav}};
ast({'[', ','}, CtxTrav) ->
    {{atom(undefined), #ast_inf{}}, CtxTrav};
ast({'[', []}, CtxTrav) ->
    {{list([]), #ast_inf{}}, CtxTrav};
ast({'[', Values}, CtxTrav) ->
    Ast = list(lists:map(fun(E) -> element(1, element(1, ast(E, CtxTrav))) end, Values)),
    {{Ast, #ast_inf{}}, CtxTrav};
ast({{'[', Values}, [length]}, CtxTrav) ->
    ValuesAst = element(1, element(1, ast({'[', Values}, CtxTrav))),
    {{erlyjs_array:get_length(ValuesAst), #ast_inf{}}, CtxTrav};
ast({new, {identifier, _, 'Array'}}, CtxTrav) ->
    {{list([]), #ast_inf{}}, CtxTrav};
ast({new, {identifier, _, 'Array'}, {'(', [{integer, _, Length}]}}, CtxTrav) ->
    {{erlyjs_array:init_new(Length), #ast_inf{}}, CtxTrav};
ast({new, {identifier, _, 'Array'}, {'(', Values}}, CtxTrav) ->
    ast({'[', Values}, CtxTrav);
ast({{new, {identifier, _, 'Array'}}, [length]}, CtxTrav) ->
    {{integer(0), #ast_inf{}}, CtxTrav};
ast({{new, {identifier, L, 'Array'}, {'(', Values}}, [length]}, CtxTrav) ->
    ValuesAst = element(1, element(1, ast({new, {identifier, L, 'Array'}, {'(', Values}}, CtxTrav))),
    {{erlyjs_array:get_length(ValuesAst), #ast_inf{}}, CtxTrav};
ast({Object, {'[]', Value}}, {Ctx, Trav}) ->
    Value1 = case Value of
    {string, L, Val} ->
        try list_to_integer(Val) of
            Int -> {integer, L, Int}
        catch
            error:badarg -> Value
        end;
    _ ->
        Value
    end,
    ValueAst = element(1, element(1, ast(Value1, {Ctx, Trav}))),
    member(Object, ValueAst, Ctx, Trav);
ast({{{string, _, Value}, Names}, {'(', Args}}, {Ctx, Trav}) ->
    call(string, Value, Names, Args, Ctx, Trav);
ast({{{identifier, _, Name}, Names}, {'(', Args}}, {Ctx, Trav}) ->
    call(Name, Names, Args, Ctx, Trav);
ast({apply, {identifier, _, Name} , {'(', Args}}, {Ctx, Trav}) ->
    call(Name, [], Args, Ctx, Trav);
ast({apply, Call, {'(', Args}}, {Ctx, Trav}) ->
    {{Ast, Inf}, {Ctx1, Trav1}} = ast(Call, {Ctx#js_ctx{action = get}, Trav}),
    {Args1, _, _} = p_t(Args, Ctx1, Trav1),
    {{application(none, Ast, [list(Args1)]), Inf}, {Ctx1, Trav1}};
ast({{identifier, _, Name}, [Prop]}, {Ctx, Trav}) ->
    {{{Var, Metadata}, Inf}, _} = var_ast(Name, Ctx#js_ctx{action = get_all}, Trav),
    case Metadata of
    {array, _} when Prop =:= length ->
        {{erlyjs_array:get_length(Var), Inf}, {Ctx, Trav}};
    {string, _} when Prop =:= length ->
        {{application(none, atom(size), [Var]), Inf}, {Ctx, Trav}};
    {_, Props} ->
        case proplists:get_value(Prop, Props) of
        undefined -> {{atom(undefined), Inf}, {Ctx, Trav}};
        Value -> {{Value, Inf}, {Ctx, Trav}}
        end
    end;
ast({{identifier, _, Name}, Value}, {Ctx, Trav}) ->
    var_declare(Name, Value, Ctx, Trav);
ast({var, DeclarationList}, {Ctx, Trav}) ->
    {Ast, Inf, Trav1} = p_t(DeclarationList, Ctx#js_ctx{action = set}, Trav),
    maybe_global({{Ast, Inf}, {Ctx, Trav1}});
ast({return, Expression}, {Ctx, Trav}) ->
    ast(Expression, {Ctx, Trav});
ast({function, {params, Params, body, Body}}, {Ctx, Trav}) ->
    Body1 = case element(1, lists:last(Body)) of
    return -> Body;
    _ -> lists:append(Body, [{return, undefined}])
    end,
    func(Params, Body1, Ctx, Trav);
ast({function, {identifier, _L2, Name}, {params, Params, body, Body}}, {Ctx, Trav}) ->
    Body1 = case element(1, lists:last(Body)) of
    return -> Body;
    _ -> lists:append(Body, [{return, undefined}])
    end,
    func(Name, Params, Body1, Ctx, Trav);
ast({op, {Op, _}, In}, {Ctx, Trav}) ->
    {Out, _, #trav{var_counter = VarCounter}} = p_t(In, Ctx, Trav),
    {{erlyjs_operators:ast(Op, Out), #ast_inf{}}, {Ctx, Trav#trav{var_counter = VarCounter}}};
ast({op, {Op, _}, In1, In2}, {Ctx, Trav}) ->
    {Out1, _, #trav{var_counter = VarCounter1}} = p_t(In1, Ctx, Trav),
    {Out2, _, #trav{var_counter = VarCounter2}} = p_t(In2, Ctx, Trav#trav{var_counter = VarCounter1}),
    {{erlyjs_operators:ast(Op, Out1, Out2), #ast_inf{}}, {Ctx, Trav#trav{var_counter = VarCounter2}}};
ast({op, {Op, postfix, _}, {identifier, _, Name}}, {Ctx, Trav}) ->
    {{In, _}, _} = var_ast(Name, Ctx, Trav),
    {{In1, _}, {_, Trav1}} = var_ast(Name, Ctx#js_ctx{action = set}, Trav),
    case is_leaf(In) of
    true -> % must be a local variable
        Ast = match_expr(In1, erlyjs_operators:ast(Op, In)),
        {{block_expr([Ast, In]), #ast_inf{}}, {Ctx, Trav1}};
    false -> % must be a global variable then
        Ast1 = match_expr(In1, In),
        KV = [atom(global_prefix(Name)), erlyjs_operators:ast(Op, In1)],
        Ast2 = application(none, atom(put), KV),
        {{block_expr([Ast1, Ast2, In1]), #ast_inf{}}, {Ctx, Trav1}}
    end;
ast({op, Op, In1, In2, In3}, {Ctx, Trav}) ->
    {Out1, _, #trav{var_counter = VarCounter1}} = p_t(In1, Ctx, Trav),
    {Out2, _, #trav{var_counter = VarCounter2}} = p_t(In2, Ctx, Trav#trav{var_counter = VarCounter1}),
    {Out3, _, #trav{var_counter = VarCounter3}} = p_t(In3, Ctx, Trav#trav{var_counter = VarCounter2}),
    {{erlyjs_operators:ast(Op, Out1, Out2, Out3), #ast_inf{}}, {Ctx, Trav#trav{var_counter = VarCounter3}}};
ast({assign, {'=', _}, {identifier, _, Name}, In1}, {Ctx, Trav}) ->
    {{Out2, _}, {_, Trav1}} = var_ast(Name, Ctx#js_ctx{action = set}, Trav),
    {Out3, Inf, _} = p_t(In1, Ctx, Trav),
    assign_ast('=', Name, Out2, Out3, Inf, Ctx, Trav1);
ast({assign, {'=', _}, {{identifier, _, Name}, [Prop]}, Ast}, {Ctx, Trav}) ->
    {{{Var, Metadata}, Inf}, _} = var_ast(Name, Ctx#js_ctx{action = get_all}, Trav),
    case Metadata of
    {array, Props} when Prop =:= length ->
        {{Var1, _}, {_, Trav1}} = var_ast(Name, Ctx#js_ctx{action = set}, {array, Props}, Trav),
        {{match_expr(Var1, erlyjs_array:set_length(Var, Ast)), Inf}, {Ctx, Trav1}}
    end;
ast({assign, {Op, _}, {identifier, _, Name}, In1}, {Ctx, Trav}) ->
    {{Out2, _}, _} = var_ast(Name, Ctx, Trav),
    {Out3, Inf, Trav1} = p_t(In1, Ctx, Trav),
    {{Out4, _}, {_, Trav2}} = var_ast(Name, Ctx#js_ctx{action = set}, Trav1),
    assign_ast('=', Name, Out4, erlyjs_operators:ast(assign_to_op(Op), Out2, Out3), Inf, Ctx, Trav2);
ast({'if', Cond, Stmt}, {#js_ctx{global = true} = Ctx, Trav}) ->
    {Cond2, _, #trav{var_counter = VarCounter}} = p_t(Cond, Ctx, Trav),
    Trav2 = Trav#trav{var_counter = VarCounter, names = add_names(Trav)},
    {Stmt2, _, Trav3} = p_t(Stmt, Ctx, Trav2),
    ReturnVarsElse = get_vars_init(Trav, Trav3, Ctx),
    Ast = case_expr(Cond2, [
        clause([atom(true)], none, [Stmt2]),
        clause([underscore()], none, [ReturnVarsElse])]),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};
ast({'if', Cond, Stmt}, {Ctx, Trav}) ->
    {Cond2, _, #trav{var_counter = VarCounter}} = p_t(Cond, Ctx, Trav),
    TravIfIn = Trav#trav{var_counter = VarCounter, names = add_names(Trav)},
    {Stmt2, _, Trav2} = p_t(Stmt, Ctx, TravIfIn),
    NameKeys = get_name_keys(Trav2),
    [ReturnVarsIf] = get_vars_list(NameKeys, [Trav2], Trav, Ctx),
    ReturnVarsElse = get_vars_init(Trav, Trav2, Ctx),
    {Vars, Trav3} =  get_vars_result(NameKeys, Trav2, Trav2, Ctx),
    Ast = match_expr(Vars, case_expr(Cond2, [
        clause([atom(true)], none, append_asts(Stmt2, ReturnVarsIf)),
        clause([underscore()], none, [ReturnVarsElse])])),
     {{Ast, #ast_inf{}}, {Ctx, trav_clean(Trav3)}};
ast({'ifelse', Cond, StmtIf, StmtElse}, {#js_ctx{global = true} = Ctx, Trav}) ->
    {Cond2, _, #trav{var_counter = VarCounter}} = p_t(Cond, Ctx, Trav),
    TravIfIn = Trav#trav{var_counter = VarCounter, names = add_names(Trav)},
    {StmtIf2, _, #trav{var_counter = VarCounter2}} = p_t(StmtIf, Ctx, TravIfIn),
    TravElseIn = Trav#trav{var_counter = VarCounter2, names = add_names(Trav)},
    {StmtElse2, _, Trav3} = p_t(StmtElse, Ctx, TravElseIn),
    Ast = case_expr(Cond2, [
        clause([atom(true)], none, [StmtIf2]),
        clause([underscore()], none, [StmtElse2])]),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};
ast({'ifelse', Cond, StmtIf, StmtElse}, {Ctx, Trav}) ->
    {Cond2, _, #trav{var_counter = VarCounter}} = p_t(Cond, Ctx, Trav),
    TravIfIn = Trav#trav{var_counter = VarCounter, names = add_names(Trav)},
    {StmtIf2, _, #trav{var_counter = VarCounter1} = Trav2} = p_t(StmtIf, Ctx, TravIfIn),
    TravElseIn = Trav#trav{var_counter = VarCounter1, names = add_names(Trav)},
    {StmtElse2, _, Trav3} = p_t(StmtElse, Ctx, TravElseIn),
    NameKeys = get_name_keys(Trav2, Trav3),
    [ReturnVarsIf, ReturnVarsElse] = get_vars_list(NameKeys, [Trav2, Trav3], Trav, Ctx),
    {Vars, Trav4} =  get_vars_result(NameKeys, Trav3, Trav3, Ctx),
    Ast = match_expr(Vars, case_expr(Cond2, [
        clause([atom(true)], none, append_asts(StmtIf2, ReturnVarsIf)),
        clause([underscore()], none, append_asts(StmtElse2, ReturnVarsElse))])),
    {{Ast, #ast_inf{}}, {Ctx, trav_clean(Trav4)}};
ast({do_while, Stmt, Cond}, {#js_ctx{global = true} = Ctx, Trav}) ->
    {OutStmt, _, Trav2} = p_t(Stmt, Ctx, trav_prepare_func(Trav)),
    {OutCond, _, Trav3} = p_t(Cond, Ctx, Trav2),
    AstFuncCond = case_expr(OutCond, [
        clause([atom(true)], none, [application(none, func_name(Trav2), [])]),
        clause([underscore()], none, [get_global_vars(Trav2)])]),
    Func = function(func_name(Trav2), [clause([], none, append_asts(OutStmt, AstFuncCond))]),
    Ast = application(none, func_name(Trav2), []),
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};
ast({do_while, Stmt, Cond}, {Ctx, Trav}) ->
    {OutStmt, _, Trav2} = p_t(Stmt, Ctx, trav_prepare_func(Trav)),
    {OutCond, _, Trav3} = p_t(Cond, Ctx, Trav2),
    VarsBefore = get_vars_init(Trav, Trav2, Ctx),
    VarsAfterStmt = get_vars_snapshot(Trav2),
    {VarsAfter, Trav4} = get_vars_result(Trav2, Trav3, Ctx),
    AstFuncCond = case_expr(OutCond, [
        clause([atom(true)], none, [application(none, func_name(Trav2), [VarsAfterStmt])]),
        clause([underscore()], none, [VarsAfterStmt])]),
    Func = function(func_name(Trav2), [clause([VarsBefore], none, append_asts(OutStmt, AstFuncCond))]),
    Ast = match_expr(VarsAfter, application(none, func_name(Trav2), [VarsBefore])),
    {{[Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, trav_clean(Trav4)}};
ast({while, Cond, Stmt}, {#js_ctx{global = true} = Ctx, Trav}) ->
    {OutCond, _, #trav{var_counter = VarCounter}} = p_t(Cond, Ctx, Trav),
    Trav2 = Trav#trav{
        var_counter = VarCounter,
        names = add_names(Trav),
        func_counter = Trav#trav.func_counter + 1},
    {OutStmt, _, Trav3} = p_t(Stmt, Ctx, Trav2),
    AstFuncCond = case_expr(OutCond, [
        clause([atom(true)], none, append_asts(OutStmt, application(none, func_name(Trav3), []))),
        clause([underscore()], none, [get_global_vars(Trav3)])]),
    Func = function(func_name(Trav3), [clause([], none, [AstFuncCond])]),
    Ast = application(none, func_name(Trav3), []),
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};
ast({while, Cond, Stmt}, {Ctx, Trav}) ->
    {OutCond, _, #trav{var_counter = VarCounter}} = parse_transform(Cond, Ctx, Trav),
    Trav2 = Trav#trav{
        var_counter = VarCounter,
        names = add_names(Trav),
        func_counter = Trav#trav.func_counter + 1},
    {OutStmt, _, Trav3} = p_t(Stmt, Ctx, Trav2),
    VarsBefore = get_vars_init(Trav, Trav3, Ctx),
    VarsAfterStmt = get_vars_snapshot(Trav3),
    {VarsAfter, Trav4} = get_vars_result(Trav3, Trav3, Ctx),
    AstFuncCond = case_expr(OutCond, [
        clause([atom(true)], none, append_asts(OutStmt, application(none, func_name(Trav2), [VarsAfterStmt]))),
        clause([underscore()], none, [VarsBefore])]),
    Func = function(func_name(Trav2), [clause([VarsBefore], none, [AstFuncCond])]),
    Ast = match_expr(VarsAfter, application(none, func_name(Trav3), [VarsBefore])),
    {{[Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, trav_clean(Trav4)}};
ast({for, Init, Cond, Final, Stmt}, {#js_ctx{global = true} = Ctx, Trav}) ->
    {OutInit, _, Trav2} = p_t(Init, Ctx, Trav),
    {OutCond, _, Trav3} = p_t(Cond, Ctx, trav_prepare_func(Trav2)),
    {OutStmt, _, Trav4} = p_t(Stmt, Ctx, Trav3),
    {FinalExpr, _, Trav5} = p_t(Final, Ctx, Trav4),
    Stmts = append_asts(OutStmt, FinalExpr), %%% currently only works with full assignment expression as FinalExpr
    AstFuncCond = case_expr(OutCond, [
        clause([atom(true)], none, append_asts(Stmts, application(none, func_name(Trav3), []))),
        clause([underscore()], none, [get_global_vars(Trav4)])]),
    Func = function(func_name(Trav4), [clause([], none, [AstFuncCond])]),
    Ast = application(none, func_name(Trav3), []),
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = append_asts(Ast, OutInit)}}, {Ctx, trav_clean(Trav5)}};
ast({for, Init, Cond, Final, Stmt}, {Ctx, Trav}) ->
    {OutInit, _, Trav2} = p_t(Init, Ctx, Trav),
    {OutCond, _, Trav3} = p_t(Cond, Ctx, trav_prepare_func(Trav2)),
    {OutStmt, _, Trav4} = p_t(Stmt, Ctx, Trav3),
    {FinalExpr, _, Trav5} = p_t(Final, Ctx, Trav4),
    VarsBefore = get_vars_init(Trav2, Trav5, Ctx),
    VarsAfterStmt = get_vars_snapshot(Trav5),
    {VarsAfter, Trav6} = get_vars_result(Trav5, Trav5, Ctx),
    Stmts = append_asts(OutStmt, FinalExpr), %%% currently only works with full assignment expression as FinalExpr
    AstFuncCond = case_expr(OutCond, [
        clause([atom(true)], none, append_asts(Stmts, application(none, func_name(Trav3), [VarsAfterStmt]))),
        clause([underscore()], none, [VarsBefore])]),
    Func = function(func_name(Trav3), [clause([VarsBefore], none, [AstFuncCond])]),
    Ast = match_expr(VarsAfter, application(none, func_name(Trav5), [VarsBefore])),
    {{append_asts(OutInit, Ast), #ast_inf{internal_func_asts = [Func]}}, {Ctx, trav_clean(Trav6)}};
ast({switch, Cond, CaseList, {_, DefaultStmts}}, {#js_ctx{global = true} = Ctx, Trav}) ->
   {Cond2, _, Trav2} = p_t(Cond, Ctx, Trav),
   CaseList2 = CaseList ++ [{default, DefaultStmts}],
   {List, Trav3} =  get_switch_clause_list(CaseList2, trav_prepare(Trav2), Ctx),
   Clauses = lists:map(
       fun({Label, Guard, Stmts, _, _}) ->
           clause([Label], Guard, Stmts)
       end, List),
   Ast = case_expr(Cond2, Clauses),
   {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};
ast({switch, Cond, CaseList, {_, DefaultStmts}}, {Ctx, Trav}) ->
    {Cond2, _, Trav2} = p_t(Cond, Ctx, Trav),
    CaseList2 = CaseList ++ [{default, DefaultStmts}],
    {List, Trav3} =  get_switch_clause_list(CaseList2, trav_prepare(Trav2), Ctx),
    NameKeys = get_name_keys(List),
    TravList = [ X || {_,_,_,_,X} <- List],
    StmtsReturnVarsList = get_vars_list(NameKeys, TravList, Trav, Ctx),
    {Vars, Trav4} =  get_vars_result(NameKeys, Trav3, Trav3, Ctx),
    Clauses = lists:map(
        fun({{Label, Guard, Stmts, _, _}, StmtsReturnVars}) ->
            clause([Label], Guard, append_asts(Stmts, StmtsReturnVars))
        end, lists:zip(List, StmtsReturnVarsList)),
    Ast = match_expr(Vars, case_expr(Cond2, Clauses)),
    {{Ast, #ast_inf{}}, {Ctx, trav_clean(Trav4)}};
ast(undefined, {Ctx, Trav}) ->
    {{atom(undefined), #ast_inf{}}, {Ctx, Trav}};
ast(Unknown, _) ->
    throw({error, lists:concat(["Unknown token: ", Unknown])}).


func_name(Trav) ->
    atom(lists:concat(["func_", Trav#trav.func_counter])).


var_ast(Key, #js_ctx{action = set} = Ctx, Metadata, Trav) ->
    Scope = hd(Trav#trav.js_scopes),
    {ErlName, Trav1} = {
        lists:concat(["V", Trav#trav.var_counter, "_", Key]),
        Trav#trav{var_counter = Trav#trav.var_counter + 1}},
    Dict = dict:store(Key, {ErlName, Metadata}, Scope#scope.var_dict),
    Names = case Trav1#trav.names of
    [] -> [];
    [H|T] -> [dict:store(Key, {ErlName, Metadata}, H) | T]
    end,
    Trav2 = Trav1#trav{
        js_scopes = [#scope{var_dict = Dict} | tl(Trav#trav.js_scopes)],
        names = Names},
    {{variable(ErlName), #ast_inf{}}, {Ctx, Trav2}}.

var_ast(Key, #js_ctx{action = set} = Ctx, Trav) ->
    var_ast(Key, #js_ctx{action = set} = Ctx, nil, Trav);
var_ast(undefined, #js_ctx{action = get} = Ctx, Trav) ->
    {{atom(undefined), #ast_inf{}}, {Ctx, Trav}};
var_ast('Infinity', #js_ctx{action = get} = Ctx, Trav) ->
    {{atom('Infinity'), #ast_inf{}}, {Ctx, Trav}};
var_ast('NaN', #js_ctx{action = get} = Ctx, Trav) ->
    {{atom('NaN'), #ast_inf{}}, {Ctx, Trav}};
var_ast(Key, #js_ctx{action = get} = Ctx, Trav) ->
    {{{Ast, _}, Inf}, _} = var_ast(Key, Ctx#js_ctx{action = get_all}, Trav),
    {{Ast, Inf}, {Ctx, Trav}};
var_ast(Key, #js_ctx{action = get_all} = Ctx, Trav) ->
    case name_search(Key, Trav#trav.js_scopes, []) of
    not_found ->
        throw({error, lists:concat(["ReferenceError: ", Key, " is not defined"])});
    {global, {Name, Metadata}} ->
        Args = [atom(Name)],
        Ast = application(none, atom(get), Args),
        {{{Ast, Metadata}, #ast_inf{}}, {Ctx, Trav}};
    {Name, Metadata} ->
        {{{variable(Name), Metadata}, #ast_inf{}}, {Ctx, Trav}}
    end.

var_declare(Key, [], Ctx, #trav{js_scopes = [GlobalScope]}=Trav) ->
    Dict = dict:store(Key, {global_prefix(Key), []}, GlobalScope#scope.var_dict),
    Args = [atom(global_prefix(Key)), atom(undefined)],
    Ast = application(none, atom(put), Args),
    Trav2 = Trav#trav{js_scopes=[#scope{var_dict = Dict}]},
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, undefined}, Ctx, #trav{js_scopes = [_]}=Trav) ->
    {_, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Args = [atom(global_prefix(Key)), atom(undefined)],
    Ast = application(none, atom(put), Args),
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, 'Infinity'}, Ctx,  #trav{js_scopes = [_]}=Trav) ->
    {_, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Args = [atom(global_prefix(Key)), atom('Infinity')],
    Ast = application(none, atom(put), Args),
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, 'NaN'}, Ctx,  #trav{js_scopes = [_]}=Trav) ->
    {_, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Args = [atom(global_prefix(Key)), atom('NaN')],
    Ast = application(none, atom(put), Args),
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};
var_declare(Key, Value, Ctx, #trav{js_scopes = [GlobalScope]}=Trav) ->
    Dict = dict:store(Key, {global_prefix(Key), []}, GlobalScope#scope.var_dict),
    Trav2 = Trav#trav{js_scopes=[#scope{var_dict = Dict}]},
    {ValueAst, Inf, Trav3} = parse_transform(Value, Ctx, Trav2),
    Args = [atom(global_prefix(Key)), ValueAst],
    Ast = application(none, atom(put), Args),
    {{append_asts(Inf#ast_inf.global_asts, Ast),  #ast_inf{}}, {Ctx, Trav3}};
var_declare(Key, [], Ctx, Trav) ->
    {{AstVariable, _}, {_, Trav2}} = var_ast(Key, Ctx, Trav),
    Ast = match_expr(AstVariable, atom(undefined)),
    {{Ast, #ast_inf{}}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, undefined}, Ctx, Trav) ->
    {{AstVar, Inf}, {_, Trav2}} = var_ast(Key, Ctx, Trav),
    {{match_expr(AstVar, atom(undefined)), Inf}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, 'Infinity'}, Ctx, Trav) ->
    {{AstVar, Inf}, {_, Trav2}} = var_ast(Key, Ctx, Trav),
    {{match_expr(AstVar, atom('Infinity')), Inf}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, 'NaN'}, Ctx, Trav) ->
    {{AstVar, Inf}, {_, Trav2}} = var_ast(Key, Ctx, Trav),
    {{match_expr(AstVar, atom('NaN')), Inf}, {Ctx, Trav2}};
var_declare(Key, Value, Ctx, Trav) ->
    Metadata = case element(1, Value) of
    new ->
        case element(3, element(2, Value)) of
        'Array' -> {array, []};
        _ -> nil
        end;
    '[' ->
        {array, []};
    function ->
        {function, {params, Params, body, _Body}} = Value,
        {function, [{length, integer(length(Params))}]};
    string ->
        {string, []};
    _ ->
        nil
    end,
    {{AstVariable, _}, {_, Trav2}} = var_ast(Key, Ctx#js_ctx{action = set}, Metadata, Trav),
    {AstValue, Inf, Trav3} = parse_transform(Value, Ctx, Trav2),
    Ast = match_expr(AstVariable, AstValue),
    {{Ast, Inf}, {Ctx, Trav3}}.


member(Object, ValueAst, Ctx, Trav) ->
    case Object of
    {identifier, _, Name} ->
        {{{Var, Metadata}, _}, _} = var_ast(Name, Ctx#js_ctx{action = get_all}, Trav),
        case Metadata of
        {array, _} -> array_member(Var, ValueAst, Ctx, Trav);
        {arguments, _} -> array_member(Var, ValueAst, Ctx, Trav);
        {string, _} -> string_member(Var, ValueAst, Ctx, Trav)
        end;
    {'[', Values} ->
        Array = element(1, element(1, ast({'[', Values}, {Ctx, Trav}))),
        array_member(Array, ValueAst, Ctx, Trav)
    end.

array_member(ArrayAst, ValueAst, Ctx, Trav) ->
    ValueAst1 = infix_expr(ValueAst, operator('+'), integer(1)),
    {{application(atom(lists), atom(nth), [ValueAst1, ArrayAst]), #ast_inf{}}, {Ctx, Trav}}.

string_member(StringAst, ValueAst, Ctx, Trav) ->
    ValueAst1 = infix_expr(ValueAst, operator('+'), integer(1)),
    StringAst1 = application(none, atom(binary_to_list), [StringAst]),
    ResultAst = application(none, atom(list_to_binary),
        [list([application(atom(lists), atom(nth), [ValueAst1, StringAst1])])]),
    {{ResultAst, #ast_inf{}}, {Ctx, Trav}}.


name_search(_, [], _) ->
    not_found;
name_search(Key, [H | T], Trav) ->
    case dict:find(Key, H#scope.var_dict) of
    {ok, {ErlName, Metadata}} ->
        case T of
        [] -> {global, {global_prefix(Key), Metadata}};
        _ -> {ErlName, Metadata}
        end;
    error ->
        name_search(Key, T, [H | Trav])
    end.


get_name_keys(L) when is_list(L) ->
    lists:usort([Key || {Key, _} <- lists:flatten([element(4, X) || X <- L])]);


get_name_keys(Trav) ->
    lists:usort([ Key || {Key, _} <- lists:flatten(
        dict:to_list(hd(Trav#trav.names)))]).

get_name_keys(Trav1, Trav2) ->
    lists:usort([ Key || {Key, _} <- lists:flatten(lists:append([
        dict:to_list(hd(Trav1#trav.names)),
        dict:to_list(hd(Trav2#trav.names))]))]).


get_vars_init(Trav1, Trav2, Ctx) ->
    tuple(sort_vars(dict:fold(
      fun(Key, _, AccTravIn) ->
          {{Ast, _}, {_, _}} = var_ast(Key, Ctx, Trav1),
          [Ast| AccTravIn]
      end, [], hd(Trav2#trav.names)))).


get_vars_snapshot(Trav) ->
    tuple(sort_vars(dict:fold(
        fun(_, {Val, _Metadata}, AccTravIn) ->
            [variable(Val) | AccTravIn]
        end, [], hd(Trav#trav.names)))).


get_vars_list(NameKeys, TravList, Trav, Ctx) ->
    lists:map(
      fun(X) ->
          tuple(lists:map(
              fun(Key) ->
                  case dict:find(Key, hd(X#trav.names)) of
                  {ok, {Val, _Metadata}} -> variable(Val);
                  error -> element(1, element(1, var_ast(Key, Ctx, Trav)))
                  end
              end, NameKeys))
      end, TravList).


get_global_vars(Trav) ->
    L = dict:to_list(hd(Trav#trav.names)),
    tuple([application(none, atom(get), [atom(global_prefix(Key))]) || {Key, _} <-  L]).


get_vars_result(Trav, TravSet, Ctx) ->
    get_vars_result(get_name_keys(Trav), Trav, TravSet, Ctx).

get_vars_result(NameKeys, Trav, TravInit, Ctx) ->
    TravInit = Trav#trav{var_counter = TravInit#trav.var_counter},
    {VarsAfter, Trav2} = lists:mapfoldl(
        fun(Key, AccTravIn) ->
            {{Ast, _}, {_, AccTravOut}} = var_ast(Key, Ctx#js_ctx{action = set}, AccTravIn),
            {Ast, AccTravOut}
        end,  TravInit, NameKeys),
    {tuple(sort_vars(VarsAfter)), Trav2}.


get_switch_clause_list(CaseList, Trav, Ctx) ->
    %% TODO: eliminate possibility of inner shadow variables
    lists:mapfoldl(
    fun
        ({default, StmtsIn}, AccTravIn) ->
            AccTravIn2 = trav_reset(AccTravIn),
            LabelOut = underscore(),
            {StmtsOut, _, AccTravOut} = p_t(StmtsIn, Ctx, AccTravIn2),
            Names = dict:to_list(hd(AccTravOut#trav.names)),
            {{LabelOut, none, StmtsOut, Names, AccTravOut}, AccTravOut};
        ({[LabelIn], StmtsIn}, AccTravIn) ->
            AccTravIn2 = trav_reset(AccTravIn),
            case lists:last(StmtsIn) of
            {break, _} ->
                StmtsIn2 = lists:reverse(tl(lists:reverse(StmtsIn))),
                {LabelOut, _, _} = p_t(LabelIn, Ctx, AccTravIn2),
                {StmtsOut, _, AccTravOut} = p_t(StmtsIn2, Ctx, AccTravIn2),
                Names = dict:to_list(hd(AccTravOut#trav.names)),
                {{LabelOut, none, StmtsOut, Names, AccTravOut}, AccTravOut};
             _ ->
                 exit(not_implemented_yet)
            end;
        ({LabelsIn, StmtsIn}, AccTravIn) ->
            AccTravIn2 = trav_reset(AccTravIn),
            case lists:last(StmtsIn) of
            {break, _} ->
                StmtsIn2 = lists:reverse(tl(lists:reverse(StmtsIn))),
                {LabelsOut, _, _} = p_t(LabelsIn, Ctx, AccTravIn2),
                Guards = disjunction(
                    lists:map(
                        fun(Label) ->
                            infix_expr(
                                variable("X"),
                                operator('=='), Label)
                        end, LabelsOut)),
                {StmtsOut, _, AccTravOut} = p_t(StmtsIn2, Ctx, AccTravIn2),
                Names = dict:to_list(hd(AccTravOut#trav.names)),
                {{variable("X"), Guards, StmtsOut, Names, AccTravOut}, AccTravOut};
             _ ->
                 exit(not_implemented_yet)
            end
    end, Trav, CaseList).


sort_vars(Vars) ->
    lists:sort(
        fun(VarNode0, VarNode1) ->
            Var0 = variable_literal(VarNode0),
            Var1 = variable_literal(VarNode1),
            string:substr(Var0, string:chr(Var0, $_) + 1)
                < string:substr(Var1, string:chr(Var1, $_) + 1)
        end, Vars).


func(Params, Body, Ctx, Trav) ->
    {ArgsVar, ArgsAst, Trav1} = arguments_p_t(Params, Ctx, Trav),
    {BodyAst, Inf, _} = p_t(Body, Ctx#js_ctx{action = get}, wrap_add_scope(Trav1)),
    Ast = fun_expr([clause([ArgsVar], none, append_asts(ArgsAst, BodyAst))]),
    {{Ast, Inf}, {Ctx, Trav1}}.

func(Name, Params, Body, Ctx, Trav) ->
    case Ctx#js_ctx.global of
    true->
        {ArgsVar, ArgsAst, Trav1} = arguments_p_t(Params, Ctx, Trav),
        {BodyAst, Inf, _} = p_t(Body, Ctx#js_ctx{action = get}, wrap_add_scope(Trav1)),
        Ast = function(atom(global_prefix(Name)),
                  [clause([ArgsVar], none, append_asts(ArgsAst, BodyAst))]),
        Export = arity_qualifier(atom(global_prefix(Name)), integer(1)),
        Exports = [Export | Inf#ast_inf.export_asts],
        {{Ast, Inf#ast_inf{export_asts = Exports}}, {Ctx, Trav1}};
    _ ->
        {{FunVar, _}, {_, Trav1}} = var_ast(Name, Ctx#js_ctx{action = set}, {function, [{length, integer(length(Params))}]}, Trav),
        {ArgsVar, ArgsAst, Trav2} = arguments_p_t(Params, Ctx, Trav1),
        {BodyAst, Inf, _} = p_t(Body, Ctx#js_ctx{action = get}, wrap_add_scope(Trav2)),
        Ast = fun_expr([clause([ArgsVar], none, append_asts(ArgsAst, BodyAst))]),
        {{match_expr(FunVar, Ast), Inf}, {Ctx, Trav2}}
    end.

arguments_p_t([], Ctx, Trav) ->
    {{Args, _}, {_, Trav1}} = var_ast(arguments, Ctx#js_ctx{action = set}, {arguments, [{length, integer(0)}]}, Trav),
    {Args, [], Trav1};
arguments_p_t(Params, Ctx, Trav) ->
    ArgsLen = ?gensym("ArgumentsLength"),
    {{Args, _}, {_, Trav1}} = var_ast(arguments, Ctx#js_ctx{action = set}, {arguments, [{length, ArgsLen}]}, Trav),
    {Params1, _, Trav2} = p_t(Params, Ctx#js_ctx{action = set}, Trav1),
    Clauses = lists:map(
        fun(I) ->
            Guard = infix_expr(ArgsLen, operator('>='), integer(I)),
            ArgVals = lists:map(
                fun(J)  ->
                    case J =< I of
                    true -> application(atom(lists), atom(nth), [integer(J), Args]);
                    false -> atom(undefined)
                    end
                end, lists:seq(1, length(Params1))),
            clause([ArgsLen], Guard, [list(ArgVals)])
        end, lists:seq(length(Params1), 0, -1)),
    Ast = match_expr(list(Params1), case_expr(application(none, atom(length), [Args]), Clauses)),
    {Args, Ast, Trav2}.


call(string, String, DotSepNames, Args, Ctx, Trav) ->
    Arity = length(Args),
    StringAst = binary_ast(String),
    case get_mod_func(String, DotSepNames, Arity) of
    {Mod, Func, _} ->
        call2(Mod, Func, add_ann(String, StringAst), Args, Ctx, Trav);
    _ ->
        throw({error, lists:concat(["No such function: ",
            pprint_name("String", DotSepNames, Arity)])})
    end.

call(Name, DotSepNames, Args, Ctx, Trav) ->
    Arity = length(Args),
    case get_mod_func(Name, DotSepNames, Arity) of
    {Mod, Func} ->
        call2(Mod, Func, Args, Ctx, Trav);
    {Mod, Func, Arg} ->
        {{VarArgs, _}, _} = var_ast(Arg, Ctx#js_ctx{action = get}, Trav),
        call2(Mod, Func, VarArgs, Args, Ctx, Trav);
    _ ->
        {{VarArgs, _}, _} = var_ast(Name, Ctx#js_ctx{action = get}, Trav),
        {Args1, _, Trav1} = p_t(Args, Ctx, Trav),
        Ast = application(none, VarArgs, [list(Args1)]),
        maybe_global({{Ast, #ast_inf{}}, {Ctx, Trav1}})
    end.

call2(Mod, Func, Args, Ctx, Trav) ->
    {Args2, _, Trav2} = p_t(Args, Ctx, Trav),
    FuncAst = application(atom(Mod), atom(Func), Args2),
    call3(FuncAst, Ctx, Trav2).

call2(Mod, Func, Arg, Args, Ctx, Trav) ->
    {Args2, _, Trav2} = p_t(Args, Ctx, Trav),
    FuncAst = application(atom(Mod), atom(Func), [Arg | Args2]),
    call3(FuncAst, Ctx, Trav2).

call3(FuncAst, Ctx, Trav) ->
    VarVal = ?gensym("Val"),
    VarErr = ?gensym("Err"),
    ClauseOk = clause([VarVal], none, [VarVal]),
    ClauseCatch = clause([VarErr], none, [tuple([atom(error), VarErr])]),
    maybe_global({{try_expr([FuncAst], [ClauseOk], [ClauseCatch]), #ast_inf{}}, {Ctx, Trav}}).


assign_ast('=', Name, _, Ast2, _, Ctx, #trav{js_scopes = [_]} = Trav) ->
    %% TODO: dynamic typechecking
    Ast = application(none, atom(put), [atom(global_prefix(Name)), Ast2]),
    maybe_global({{Ast, #ast_inf{}}, {Ctx, Trav}});
assign_ast('=', _, Ast1, Ast2, _, Ctx, Trav) ->
    %% TODO: dynamic typechecking
    Ast = match_expr(Ast1, Ast2),
    maybe_global({{Ast, #ast_inf{}}, {Ctx, Trav}});
assign_ast(Unknown, _, _, _, _, _, _) ->
    throw({error, lists:concat(["Unknown assignment operator: ", Unknown])}).


maybe_global({{Ast, Inf}, {#js_ctx{global = true} = Ctx, Trav}}) ->
    GlobalAsts = append_asts(Inf#ast_inf.global_asts, Ast),
    {{[], Inf#ast_inf{global_asts = GlobalAsts}}, {Ctx, Trav}};
maybe_global({AstInf, CtxTrav}) ->
    {AstInf, CtxTrav}.


append_asts(Ast1, Ast2) when is_list(Ast1), is_list(Ast2) ->
    lists:append(Ast1, Ast2);
append_asts(Ast1, Ast2) when is_list(Ast1) ->
    lists:append(Ast1, [Ast2]);
append_asts(Ast1, Ast2) when is_list(Ast2) ->
    lists:append([Ast1], Ast2);
append_asts(Ast1, Ast2) ->
    [Ast1, Ast2].


binary_ast(Value) when is_list(Value) ->
    binary(lists:map(fun (Val) -> binary_field(integer(Val)) end, Value));
binary_ast(Value) when is_binary(Value) ->
    binary(lists:map(fun (Val) -> binary_field(integer(Val)) end, binary:bin_to_list(Value))).


append_info(Info1, Info2) ->
    #ast_inf{
        export_asts = lists:append(Info1#ast_inf.export_asts, Info2#ast_inf.export_asts),
        global_asts = lists:append(Info1#ast_inf.global_asts, Info2#ast_inf.global_asts),
        internal_func_asts = lists:append(
            Info1#ast_inf.internal_func_asts, Info2#ast_inf.internal_func_asts)}.


assign_to_op(Assign) ->
    list_to_atom(tl(lists:reverse(string:strip(atom_to_list(Assign), both, $')))).


global_prefix(Name) ->
    lists:concat(["js_", Name]).


%% TODO: eliminate
add_names(Trav) -> [dict:new() | Trav#trav.names].

trav_prepare(Trav) -> Trav#trav{names = [dict:new() | Trav#trav.names]}.

trav_prepare_func(Trav) ->
    Trav#trav{
        names = [dict:new() | Trav#trav.names],
        func_counter = Trav#trav.func_counter + 1}.

trav_clean(Trav) -> Trav#trav{names = tl(Trav#trav.names)}.

trav_reset(Trav) -> Trav#trav{names = [dict:new() | tl(Trav#trav.names)]}.

wrap_add_scope(Trav) -> Trav#trav{js_scopes = [#scope{} | Trav#trav.js_scopes]}.


pprint_name(Name, [], Arity)  ->
    lists:concat([Name, "/", Arity]);
pprint_name(_, DotSepNames, Arity)  ->
    lists:concat([string:join([atom_to_list(X) || X <- DotSepNames], "."),"/", Arity]).


trace(Module, Line, Title, Content, Ctx) ->
    case Ctx#js_ctx.verbose of
    true -> io:format("TRACE ~p:~p ~p: ~p~n",[Module, Line, Title, Content]);
    _ -> ok
    end.
