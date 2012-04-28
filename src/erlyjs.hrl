-compile({no_auto_import, [float/1]}).

-import(erl_syntax, [atom/1, list/1, tuple/1, binary/1, binary_field/1,
    string/1, integer/1, float/1, disjunction/1, application/2, application/3,
    attribute/2, arity_qualifier/2, operator/1, infix_expr/3, try_expr/3,
    function/2, fun_expr/1, case_expr/2, match_expr/2, block_expr/1, clause/3,
    revert_forms/1, variable/1, variable_literal/1, underscore/0, form_list/1,
    type/1, is_leaf/1, add_ann/2, get_ann/1]).

-record(js_ctx, {
    out_dir = "ebin",
    global = false,
    args = [],
    reader = {file, read_file},
    action = get,
    force_recompile = false,
    module = [],
    verbose = true}).

-record(ast_inf, {              %%  additional info about a parse transfomed AST
    export_asts = [],           %%  Module header export statements
    global_asts = [],           %%  Exported Erlang functions
    internal_func_asts = []}).  %%  Internal Erlang functions

-record(scope, {                %%  represents a Javascript variable scope
    var_dict = dict:new()}).    %%  Key: JsName, Value: {ErlName, Metadata}

-record(trav, {                 %%  traverses the whole tree
    js_scopes = [#scope{}],     %%  for Javascript variables scopes
    names = [],                 %%  for temporary use: [{JsNameAsKey, {ErlName, Metadata}}, ...]
    var_counter = 0,            %%  for unique Erlang variable names
    func_counter = 0}).         %%  for unique internal Erlang function names

-define(b2l(Value), binary_to_list(Value)).
-define(l2b(Value), list_to_binary(Value)).

-define(gensym(Name), erl_syntax:variable(Name ++ "_" ++ lists:concat(tuple_to_list(now())))).
