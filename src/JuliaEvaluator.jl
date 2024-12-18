
mutable struct Binding
  name
  value
end

make_binding(name, value) = Binding(name, value)
binding_name(binding) = binding.name
binding_value(binding) = binding.value
set_binding_value!(binding, value) = binding.value = value

mutable struct Environment
 bindings
 parent
end

create_initial_environment(bindings...) =
  Environment(map(b -> make_binding(b.first, b.second), bindings),
              nothing)

augment_environment(names, values, env) =
 Environment(map(make_binding, names, values),
             env)

get_environment_binding(name, env) =
  isnothing(env) ?
    # if the binding can't be found, we might go down one language level
    if isdefined(KhepriBase, name)
      make_binding(
        name,
        let val = getproperty(KhepriBase, name)
          val isa Function ? make_primitive(val) : val
        end)
    elseif isdefined(Main, name)
      make_binding(
        name,
        let val = getproperty(Main, name)
          val isa Function ? make_primitive(val) : val
        end)
    else
      error("Unbound name: ", name)
    end :
    begin
      for binding in env.bindings
        if name === binding_name(binding)
          return binding
        end
      end
      get_environment_binding(name, env.parent)
    end

#=
Regarding the syntax:
=#

#=
Assignments, in Julia, are easily confused with definitions, because
they use the same operator =. Therefore, we process assignments in the
clause for definitions.

Here, however, we can process guaranteed assignments involving the
combination assignment/operation, such as +=.
=#

is_set(expr) = expr isa Expr && expr.head in [:(+=), :(-=), :(*=), :(/=)]
assignment_name(expr) = expr.args[1]
assignment_expression(expr) = expr.args[2]

eval_set(expr, env) =
  let op = Dict(:(+=)=>:(+), :(-=)=>:(-), :(*=)=>:(*), :(/=)=>:(/))[expr.head],
      name = assignment_name(expr),
      expression = assignment_expression(expr)
    eval_expr(:($(name) = $op($name, $expression)), env)
  end


define_name!(name, value, env) =
  env.bindings = [make_binding(name, value), env.bindings...]

update_name!(name, value, env) =
  let binding = get_environment_binding(name, env)
    set_binding_value!(binding, value)
  end

#=
eval_set(expr, env) =
  let value = eval_expr(assignment_expression(expr), env)
    update_name!(assignment_name(expr), value, env)
    value
  end
=#
is_name(expr) = expr isa Symbol
eval_name(name, env) =
  binding_value(get_environment_binding(name, env))

is_splat(expr) = expr isa Expr && expr.head === :...
is_vect(expr) = expr isa Expr && expr.head === :vect
eval_vect(expr, env) = eval_args(expr.args, env)
eval_args(exprs, env) =
  let r = []
    for expri in exprs
      if is_splat(expri)
        append!(r, eval_expr(expri.args[1], env))
      else
        push!(r, eval_expr(expri, env))
      end
    end
    r
  end
# The illustrations benefit from a strict separation between exprs and args 
eval_args_dont_splat(exprs, env) = 
  let r = []
    for expri in exprs
      if is_splat(expri)
        push!(r, eval_expr(expri.args[1], env))
      else
        push!(r, eval_expr(expri, env))
      end
    end
    r
  end
maybe_splat(exprs, args) =
  let r = []
    for (expri, argi) in zip(exprs, args)
      if is_splat(expri)
        append!(r, argi)
      else
        push!(r, argi)
      end
    end
    r
  end

is_tuple(expr) = expr isa Expr && expr.head === :tuple
eval_tuple(expr, env) = eval_args(expr.args, env)
    


is_ref(expr) = expr isa Expr && expr.head === :ref
eval_ref(expr, env) =
  let v = eval_expr(expr.args[1], env),
      env = augment_environment([:begin, :end], [1, length(v)], env),
      i = eval_expr(expr.args[2], env)
    v[i]
  end

#
is_dot(expr) = expr isa Expr && expr.head === :.
eval_dot(expr, env) =
  let v = eval_expr(expr.args[1], env),
      i = expr.args[2].value
    getproperty(v, i)
  end

#
export for_steps_limit
for_steps_limit = Parameter(1)

is_for(expr) = expr isa Expr && expr.head === :for
eval_for(expr, env) =
  let var = expr.args[1].args[1],
      init = eval_expr(expr.args[1].args[2], env),
      body = expr.args[2],
      recursive_level_limit_for(step) =
        step <= for_steps_limit() && current_recursive_level() <= recursive_levels_limit() ?
          recursive_levels_limit() :
          -10 #Just to be on the safe side
    for (step, val) in enumerate(init)
      with(recursive_levels_limit, recursive_level_limit_for(step)) do 
        eval_expr(:(let $(var) = $(Expr(:quote, val)); $(body) end), env)
        #eval_expr(:((($var) -> $(body))($(Expr(:quote, val)))), env)
      end
    end
    nothing
  end

is_generator(expr) = expr isa Expr && expr.head === :generator
eval_generator(expr, env) =
  let var = expr.args[2].args[1],
      vals = eval_expr(expr.args[2].args[2], env),
      body = expr.args[1],
      recursive_level_limit_for(step) =
        step <= for_steps_limit()+1 && current_recursive_level() <= recursive_levels_limit() ?
          recursive_levels_limit() :
          -10 #Just to be on the safe side
    var isa Expr && var.head === :tuple ?
      [with(recursive_levels_limit, recursive_level_limit_for(step)) do 
        eval_expr(body, augment_environment(var.args, val, env))
       end for (step, val) in enumerate(vals)] :
      [with(recursive_levels_limit, recursive_level_limit_for(step)) do
        eval_expr(body, augment_environment([var], [val], env))
      end for (step, val) in enumerate(vals)]
  #[eval_expr(:(let $(var) = $(Expr(:quote, v)); $(body) end), env) for v in vals]
  end

is_comprehension(expr) = expr isa Expr && expr.head === :comprehension
eval_comprehension(expr, env) =
  expr.args[1].head === :flatten ?
    reduce(vcat, eval_expr(expr.args[1].args[1], env)) :
    eval_expr(expr.args[1], env)

is_range(expr) = expr isa Expr && expr.head === :call && expr.args[1] == :(:)
eval_range(expr, env) =
  length(expr.args) == 3 ?
    (eval_expr(expr.args[2], env):eval_expr(expr.args[3], env)) :
    (eval_expr(expr.args[2], env):eval_expr(expr.args[3], env):eval_expr(expr.args[4], env))

#

# do syntax
is_do(expr) = expr isa Expr && expr.head === :do
eval_do(expr, env) =
  eval_expr(Expr(:call, expr.args[1].args[1], expr.args[2], expr.args[1].args[2:end]...), env)

eval_expr(expr, env) =
  if is_self_evaluating(expr)
    expr
  elseif is_name(expr)
    eval_name(expr, env)
  elseif is_quote(expr)
    eval_quote(expr, env)
  elseif is_vect(expr)
    eval_vect(expr, env)
  elseif is_tuple(expr)
    eval_tuple(expr, env)
  elseif is_ref(expr)
    eval_ref(expr, env)
  elseif is_dot(expr)
    eval_dot(expr, env)
  elseif is_generator(expr)
    eval_generator(expr, env)
  elseif is_comprehension(expr)
    eval_comprehension(expr, env)
  elseif is_range(expr)
    eval_range(expr, env) 
  elseif is_lambda(expr)
    eval_lambda(expr, env)
  elseif is_if(expr)
    eval_if(expr, env)
  elseif is_let(expr)
    eval_let(expr, env)
  elseif is_for(expr)
    eval_for(expr, env)
  elseif is_def(expr)
    eval_def(expr, env)
  elseif is_gdef(expr)
    eval_gdef(expr, env)
  elseif is_set(expr)
    eval_set(expr, env)
  elseif is_fdef(expr)
    eval_fdef(expr, env)
  elseif is_mdef(expr)
    eval_mdef(expr, env)
  elseif is_begin(expr)
    eval_begin(expr, env)
  elseif is_and(expr)
    eval_and(expr, env)
  elseif is_or(expr)
    eval_or(expr, env)
  elseif is_do(expr)
    eval_do(expr, env)
  elseif is_call(expr)
    eval_call(expr, env)
  else
    println("I will generate an error! Here is the dump of the expression:")
    dump(expr)
    for e in illustrations_stack
      println(e)
    end
    error("Unknown expression type: ", expr)
  end 

struct LexicalFunction
  parameters
  body
  environment
end

Base.show(io::IO, f::LexicalFunction) =
  print(io, "<function>")

make_function(parameters, body, env) = LexicalFunction(parameters, body, env)
is_function(obj) = obj isa LexicalFunction
function_parameters(f) = f.parameters
function_body(f) = f.body
function_environment(f) = f.environment

eval_lambda(expr, env) =
  make_function(lambda_parameters(expr), lambda_body(expr), env)

(f::LexicalFunction)(args...) = 
  let params = function_parameters(f),
      extended_env = augment_environment(params, args,  function_environment(f))
    eval_expr(function_body(f), extended_env)
  end
#=
Regarding the implementation of macros, they are just like functions (with
lexical environment, et al), but are called differently, as they receive the
operands unevaluated and the result needs further evaluation. We might distinguish
by using a special calling syntax, as it happens with Julia @macro or we might
just have a special kind of function that is not exactly like a function so that
we can distinguish them and make an appropriate call for each one.
Scheme follows this last tradition, so we will just mark each macro so that we
can recognize it.

The struct has the same fields as a (lexical) function so that the same selectors
can be used (lack of inheritance in Julia is understandable but still annoying.)
=#

struct MacroFunction
  parameters
  body
  environment
end

make_macro_function(parameters, body, env) = MacroFunction(parameters, body, env)
is_macro_function(obj) = obj isa MacroFunction

eval_mdef(expr, env) =
  let value = make_macro_function(mdef_parameters(expr), mdef_body(expr), env)
    define_name!(mdef_name(expr), value, env)
    value
  end

illustrate(func::Any, exprs, args...) =
  any(is_splat, exprs) ?
    let exs = [],
        ars = []
      for (expr, arg) in zip(exprs, args)
        if is_splat(expr)
          append!(exs, [:($(expr.args[1])[$i]) for i in 1:length(arg)])
          append!(ars, arg)
        else
          push!(exs, expr)
          push!(ars, arg)
        end
      end
      illustrate(func, exs, ars...)
    end :
    missing

const pause_illustration = Parameter(false)
export pause_illustration

illustrate_and_maybe_pause(func, exprs, args...) =
  begin
    illustrate(func, exprs, args...)
    if pause_illustration() && length(tikz.shapes) > 0
      render_view()
      readline()
    end
  end

export recursive_levels_limit, current_recursive_level, illustrations_stack, step_by_step
const illustrations_stack = []
const recursive_levels_limit = Parameter{Real}(5)
const current_recursive_level = Parameter(0)
without_illustration(f) = with(f, recursive_levels_limit, -1)
const step_by_step = Parameter(false)
render_n = 0
eval_call(expr, env) =
  let func = eval_expr(call_operator(expr), env),
      exprs = call_operands(expr)
    is_macro_function(func) ?
      let expansion = apply_function(func, exprs, env)
        replace(expr, expansion)
        eval_expr(expansion, env)
      end :
      let unsplatted_args = eval_args_dont_splat(exprs, env),
          args = maybe_splat(exprs, unsplatted_args),
          #prev = isempty(illustrations_stack) ? nothing : illustrations_stack[end], 
          recursive_level = length(illustrations_stack) #count(==(prev), illustrations_stack)
        #println(recursive_level, " ", illustrations_stack)
        try
          with(current_recursive_level, recursive_level) do
            let res = 
              is_primitive(func) ?
                func(args...) :
                let params = function_parameters(func),
                    extended_env = augment_environment(params, args, function_environment(func))
                  push!(illustrations_stack, call_operator(expr))
                  eval_expr(function_body(func), extended_env)
                end
              if (is_primitive(func) && current_recursive_level() == recursive_levels_limit() + 1) || # HACK Added to handle things like let q = p + vxy(...)
                current_recursive_level() <= recursive_levels_limit()
                if step_by_step()
                  if !ismissing(illustrate(func, exprs, unsplatted_args...))
                    global render_n += 1
                    render_view("IllustrationStep$render_n")
                  end
                else
                  illustrate(func, exprs, unsplatted_args...)
                end
              end
              res
            end
          end
        finally
          is_primitive(func) || pop!(illustrations_stack)
        end
      end
  end


make_primitive(f) = f
is_primitive(obj) = obj isa Function
apply_primitive(prim, args) = prim(args...)

apply_function(func, args, env) =
  is_primitive(func) ?
    apply_primitive(func, args) :
    let params = function_parameters(func),
        extended_env = augment_environment(params, args,  function_environment(func))
      eval_expr(function_body(func), extended_env)
    end

eval_if(expr, env) =
  is_true(eval_expr(if_condition(expr), env)) ?
    eval_expr(if_consequent(expr), env) :
    eval_expr(if_alternative(expr), env)

#=
For the initial environment, we also need to clean things a bit:
=#

macro predef(sym)
  :($(QuoteNode(sym)) => make_primitive($sym))
end

initial_environment =
  create_initial_environment(
    @predef(pi),
    @predef(π),
    @predef(nothing),
    @predef(+),
    @predef(*),
    @predef(-),
    @predef(/),
    @predef((==)),
    @predef(<),
    @predef(>),
    @predef(<=),
    @predef(>=),
    @predef(!),
    @predef(^),
    @predef(sqrt),
    @predef(cbrt),
    @predef(sin),
    @predef(asin),
    @predef(cos),
    @predef(acos),
    @predef(tan),
    @predef(atan),
    @predef(print),
    @predef(println),
    @predef(tuple),
    @predef(Expr),
    @predef(gensym),
    @predef(circle),
    @predef(line),
    @predef(polygon),
    @predef(arc),
    @predef(regular_polygon),
    @predef(regular_polygon_vertices),
    @predef(surface_regular_polygon),
    @predef(surface_circle),
    @predef(rectangle),
    @predef(u0),
    @predef(x),
    @predef(vx),
    @predef(y),
    @predef(vy),
    @predef(xy),
    @predef(vxy),
    @predef(z),
    @predef(vz),
    @predef(xz),
    @predef(vxz),
    @predef(xyz),
    @predef(vxyz),
    @predef(pol),
    @predef(vpol),
    #@predef(label),
    @predef(box),
    @predef(without_illustration)
    )

export eval_expr
eval_expr(expr) = eval_expr(expr, initial_environment)

#=
Self evaluation must also include those pesky LineNumberNodes generated by
Julia's parser:
=#
is_self_evaluating(expr) = expr isa Number || expr isa String || expr isa Bool || expr isa Tuple ||
  expr isa LineNumberNode


call_operator(expr) = expr.args[1]
call_operands(expr) = expr.args[2:end]

#=
Julia uses blocks a lot. These are the equivalent to the begin forms of Scheme.
=#

is_begin(expr) = expr isa Expr && expr.head === :block
begin_expressions(expr) = expr.args
eval_begin(expr, env) =
  let eval_sequentially(exprs) =
        length(exprs) == 1 ?
          eval_expr(exprs[1], env) :
          begin
            eval_expr(exprs[1], env)
            eval_sequentially(exprs[2:end])
          end
    eval_sequentially(begin_expressions(expr))
  end

#=
As we saw in the Scheme evaluators, an anonymous function is a fundamental
concept that can be used to support different language constructs.
=#

is_lambda(expr) = expr isa Expr && expr.head === :->
lambda_parameters(expr) =
  expr.args[1] isa Expr ?
    expr.args[1].args :
    [expr.args[1]]
lambda_body(expr) = expr.args[2]

#=
Julia does not have flet, only let but it also supports local function
definitions. Moreover, let is more like let*.
=#

is_let(expr) = expr isa Expr && expr.head === :let
let_scopes(expr) =
  expr.args[1].head === :block ?
    expr.args[1].args :
    [expr.args[1]]
let_body(expr) = expr.args[end]
let_scope_name(scope) =
  scope.args[1] isa Expr && scope.args[1].head === :tuple ?
    scope.args[1].args :
    let_binding_name(scope)
let_scope_init(scope) =
    let_binding_init(scope)

scope_names_values(scope, env) =
  let name = let_scope_name(scope),
      init = eval_expr(let_scope_init(scope), env)
    if name isa Vector
      @assert(length(name)==length(init))
      name, init
    else
      [name], [init]
    end
  end
eval_let(expr, env) =
  let scopes = let_scopes(expr),
      body = let_body(expr)
    if isempty(scopes)
      # Let always create an augmented environment
      eval_expr(body, augment_environment([], [], env))
    else
      let scope = scopes[1],
          (name, init) = scope_names_values(scope, env),
          extended_env = augment_environment(name, init, env)
      #println((current_recursive_level(), recursive_levels_limit()))
        if current_recursive_level() <= recursive_levels_limit()
          let res = eval_let(:(let $(scopes[2:end]...); $body end), extended_env)
            illustrate_bindings(name, init)
            res
          end
        else
          eval_let(:(let $(scopes[2:end]...); $body end), extended_env)
        end
      end
    end
  end

#
illustrate_bindings(names, inits) =
  if current_recursive_level() <= recursive_levels_limit()
    with_recursive_illustration() do
      for (name, init) in zip(names, inits)
        illustrate_binding(name, init)
      end
    end
  end

#=
Julia's bindings appear in lots of places, so let's generalize it:
=#

julia_binding_name(binding) =
  is_call(binding.args[1]) ?
    call_operator(binding.args[1]) :
    binding.args[1]
julia_binding_init(binding) =
  is_call(binding.args[1]) ?
    :(($(call_operands(binding.args[1])...),) -> $(binding.args[2])) :
    binding.args[2]

#=
For lets, we have:
=#

let_binding_name(binding) = julia_binding_name(binding)
let_binding_init(binding) = julia_binding_init(binding)

#=
Julia supports definitions (the previous def) and assignments (the previous set!).
However, they all use the same operator (=), which makes it difficult to decide
whether we are facing a definition or an assigment. We will start by considering
definitions. We will consider assigments later.
=#

is_def(expr) = expr isa Expr && expr.head === :(=)
def_name(expr) = julia_binding_name(expr)
def_init(expr) = julia_binding_init(expr)

#=
Assignments in Julia are a complete mess. According to the documentation, in a
local scope, all variables are inherited from its parent global scope block,
unless an assignment would result in a modified global variable, or a variable
is specifically marked with the keyword local. Thus global variables are only
inherited for reading, not for writing. In order to inherit them for writing, it
is necessary to mark them with the global keyword. This last case is identical to
the gdef form that we invented in the Scheme evaluator so we can take care of it
immediatly.
=#

is_gdef(expr) = expr isa Expr && expr.head === :global
gdef_name(expr) = julia_binding_name(expr.args[1])
gdef_init(expr) = julia_binding_init(expr.args[1])
eval_gdef(expr, env) =
  let value = eval_expr(gdef_init(expr), env)
    define_name!(gdef_name(expr), value, initial_environment)
    value
  end

#=
However, this does not solve the problem of inner scopes accessing variables in
outer non-global scopes. This is one of the situations where we need to have a
special evaluation rule.

These are the rules implemented for Scheme for, respetively, definitions and
assignments.

eval_def(expr, env) =
  let value = eval_expr(def_init(expr), env)
    define_name!(def_name(expr), value, env)
    value
  end

eval_set(expr, env) =
  let value = eval_expr(assignment_expression(expr), env)
    update_name!(assignment_name(expr), value, env)
    value
  end

Given the Julia uses the same operator for both definition and assignment, we
somehow need to merge those two definitions.
In the case of Julia, it seems that the semantics is that we only make a definition
if there is a global for the same name or if there is no variable at all. Otherwise
we must treat it as an assignment.
=#

exists_environment_binding(name, env) =
  isnothing(env) ?
    false :
    begin
      for binding in env.bindings
        if name === binding_name(binding)
          return true
        end
      end
      exists_environment_binding(name, env.parent)
    end

exists_non_global_environment_binding(name, env) =
  env === initial_environment ?
    false :
    begin
      for binding in env.bindings
        if name === binding_name(binding)
          return true
        end
      end
      exists_non_global_environment_binding(name, env.parent)
    end

eval_def(expr, env) =
  let name = def_name(expr)
    if env === initial_environment
      let value = eval_expr(def_init(expr), env)
        exists_environment_binding(name, env) ?
          update_name!(name, value, env) :
          define_name!(name, value, env)
        value
      end
    elseif exists_non_global_environment_binding(name, env)
      let value = eval_expr(def_init(expr), env)
        update_name!(name, value, env)
        value
      end
    elseif exists_environment_binding(name, initial_environment)
      define_name!(name, nothing, env)
      let value = eval_expr(def_init(expr), env)
        define_name!(name, value, env)
        value
      end
    else
      let value = eval_expr(def_init(expr), env)
        define_name!(name, value, env)
        value
      end
    end
  end

#=
The non-short form for function definitions can be handled by the previous fdef.
However, we do not need special evaluation rules because these forms can be
transformed into the corresponding short-form definitions using a source-to-source
transformation.
=#

is_fdef(expr) = expr isa Expr && expr.head === :function

fdef_name(expr) = call_operator(expr.args[1])
fdef_parameters(expr) = call_operands(expr.args[1])
fdef_body(expr) = expr.args[2]

eval_fdef(expr, env) =
  eval_expr(:($(fdef_name(expr)) =
                ($(fdef_parameters(expr)...),) -> $(fdef_body(expr))),
            env)

#=
Boolean values in Julia are more restrictive than in Scheme. In boolean contexts
only true and false are allowed.
=#

is_true(value) =
  value === true ?
    true :
    value === false ?
      false :
      error("Non boolean used in a boolean context")

if_condition(expr) = expr.args[1]
if_consequent(expr) = expr.args[2]
if_alternative(expr) = expr.args[3]

#=
We also need to support elseif. The simplest way, it seems, is to treat it as an if.
=#

is_if(expr) = expr isa Expr && (expr.head === :if || expr.head === :elseif)

#=
Logical operators need short-circuiting. Given that we ended up implementing
them in Scheme using macro transformations, we recover the original non-macro-based
implementations:
=#

eval_and(expr, env) =
  let exprs = and_expressions(expr)
    for i in 1:length(exprs)-1
      is_true(eval_expr(exprs[i], env)) || return false
    end
    eval_expr(exprs[end], env)
  end

eval_or(expr, env) =
  let exprs = or_expressions(expr)
    for i in 1:length(exprs)-1
      let res = eval_expr(exprs[i], env)
        is_true(res) && return res
      end
    end
    eval_expr(exprs[end], env)
  end

#=
The syntax of logical operators is different in Julia:
=#

is_and(expr) = expr isa Expr && expr.head === :&&
and_expressions(expr) = expr.args

is_or(expr) = expr isa Expr && expr.head === :||
or_expressions(expr) = expr.args

#=
Now, macros. They are just like functions except that their name starts with @
and Julia automatically provides an extra initial argument.
=#

is_mdef(expr) = expr isa Expr && expr.head === :macro
mdef_name(expr) = Symbol("@" * string(call_operator(expr.args[1])))
mdef_parameters(expr) = [:linenumbernode, fdef_parameters(expr)...]
mdef_body(expr) = fdef_body(expr)

is_quote(expr) = expr isa Expr && expr.head === :quote
quoted_form(expr) = expr.args[1]
#=
We need to implement the unquote form for interpolation:
=#

is_unquote(expr) = expr isa Expr && expr.head === :$
unquote_form(expr) = expr.args[1]

eval_quote(expr, env) = expand(quoted_form(expr), env)
#For one-level quote, unquote
expand(expr, env) =
  if is_unquote(expr)
    eval_expr(unquote_form(expr), env)
  elseif expr isa Expr
    Expr(expr.head, map(subform -> expand(subform, env), expr.args)...)
  else
    expr
  end

#=
Syntatically speaking, macro calls are just like function calls:
=#

is_call(expr) = expr isa Expr && (expr.head === :call || expr.head === :macrocall)

#=
Finally, we need to handle the replacement done at macro call sites.
=#

replace(source, target) =
  begin
    source.head = target.head
    source.args = target.args
  end

#=
@test eval_expr(jl"1") == 1
@test eval_expr(jl"\"Hello, World!\"") == "Hello, World!"
=#

julia_repl() =
  while true
    prompt_for_input()
    let expression = julia_read(stdin),
        value = eval_expr(expression)
      println(stdout, value)
    end
  end

macro illustrator(call)
  :(begin
    empty!(illustrations_stack)
    eval_expr($(QuoteNode(call)))
  end)
end

export @illustrator,
       illustrate,
       without_illustration
