# MiniPPL runtime and compiler in base R.
#
# Surface programs are ordinary, unevaluated R syntax captured by `ppl()`.
# The compiler rewrites that syntax into a trampolined continuation monad.  R's
# own closures and lexical frames implement object-language closures and `let`.
# Leading function assignments stand for top-level `defn`; formula syntax offers
# an optional statistical spelling for `sample` and `observe`.

# -----------------------------------------------------------------------------
# Continuation monad and probabilistic Step interface
#
#   P a = (a -> Bounce (Step r)) -> Bounce (Step r)
#
#   Step r = Done r
#          | Sample address distribution (value -> Step r)
#          | Observe address distribution value (() -> Step r)
# -----------------------------------------------------------------------------

More <- function(thunk) {
  force(thunk)
  structure(thunk, class = c("ppl_more", "function"))
}

trampoline <- function(x) {
  while (inherits(x, "ppl_more")) x <- x()
  x
}

Cont <- function(run) {
  force(run)
  structure(run, class = c("ppl_cont", "function"))
}

pure <- function(x) {
  # Force here: object-language values are call-by-value even though the host
  # language passes this argument as a promise.
  force(x)
  Cont(function(k) {
    force(k)
    More(function() k(x))
  })
}

bind <- function(m, f) {
  force(m)
  force(f)
  Cont(function(k) {
    force(k)
    More(function() {
      m(function(x) {
        # Do not let host promise chains accumulate across source-level calls.
        # This selective forcing is what makes the trampoline genuinely
        # stack-safe while retaining R's lazy syntax capture at the boundary.
        force(x)
        More(function() {
          next_computation <- f(x)
          force(next_computation)
          next_computation(k)
        })
      })
    })
  })
}

run_cont <- function(m, k) {
  force(m)
  force(k)
  trampoline(m(k))
}

Done <- function(value) {
  force(value)
  structure(list(tag = "done", value = value), class = "ppl_step")
}

Sample <- function(address, distribution, k) {
  force(address)
  force(distribution)
  force(k)
  structure(
    list(
      tag = "sample",
      address = address,
      distribution = distribution,
      k = k
    ),
    class = "ppl_step"
  )
}

Observe <- function(address, distribution, value, resume) {
  force(address)
  force(distribution)
  force(value)
  force(resume)
  structure(
    list(
      tag = "observe",
      address = address,
      distribution = distribution,
      value = value,
      resume = resume
    ),
    class = "ppl_step"
  )
}

sample_effect <- function(address, distribution) {
  if (!is_distribution(distribution)) {
    stop("sample: value is not a distribution", call. = FALSE)
  }

  Cont(function(k) {
    force(k)
    Sample(
      address,
      distribution,
      function(value) {
        force(value)
        trampoline(k(value))
      }
    )
  })
}

observe_effect <- function(address, distribution, value) {
  if (!is_distribution(distribution)) {
    stop("observe: value is not a distribution", call. = FALSE)
  }

  Cont(function(k) {
    force(k)
    Observe(
      address,
      distribution,
      value,
      function() trampoline(k(value))
    )
  })
}

address_at <- function(address, part) c(address, part)

address_key <- function(address) {
  if (length(address)) paste(address, collapse = "/") else "<root>"
}

# -----------------------------------------------------------------------------
# First-class source functions
#
# A source `function` compiles to an actual R closure.  The hidden first
# argument carries the dynamic call-site address; source arguments have already
# been evaluated by the monadic application compiler.
# -----------------------------------------------------------------------------

ppl_function <- function(f, arity, parameters) {
  force(f)
  force(arity)
  force(parameters)
  structure(
    f,
    class = c("ppl_function", "function"),
    arity = as.integer(arity),
    parameters = parameters
  )
}

ppl_args <- function(...) list(...)

apply_value <- function(address, f, args) {
  force(address)
  force(f)
  force(args)

  if (inherits(f, "ppl_function")) {
    expected <- attr(f, "arity", exact = TRUE)
    if (length(args) != expected) {
      stop(
        sprintf("arity mismatch: expected %d, got %d", expected, length(args)),
        call. = FALSE
      )
    }

    out <- do.call(f, c(list(address), args))
    if (!inherits(out, "ppl_cont")) {
      stop("internal error: source function did not return a computation", call. = FALSE)
    }
    return(out)
  }

  if (is.function(f)) return(pure(do.call(f, args)))
  stop("cannot apply a non-function value", call. = FALSE)
}

# -----------------------------------------------------------------------------
# Source-to-source CPS compiler
# -----------------------------------------------------------------------------

call_code <- function(name, args = list()) {
  as.call(c(list(as.name(name)), args))
}

function_code <- function(formals, body) {
  as.call(list(as.name("function"), as.pairlist(formals), body))
}

lambda1 <- function(name, body) {
  f <- formals(function(x) NULL)
  names(f) <- as.character(name)
  function_code(f, body)
}

block_code <- function(...) {
  as.call(c(list(as.name("{")), list(...)))
}

head_name <- function(expr) {
  if (is.call(expr) && is.symbol(expr[[1L]])) as.character(expr[[1L]]) else NULL
}

is_head <- function(expr, name) identical(head_name(expr), name)

block_items <- function(expr) {
  if (is_head(expr, "{")) as.list(expr)[-1L] else list(expr)
}

missing_formal <- function(x) {
  names(x) <- NULL
  missing <- alist(x = )
  names(missing) <- NULL
  identical(x, missing)
}

new_compiler <- function() {
  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state
}

gensym <- function(state, prefix = "..ppl_tmp_") {
  state$n <- state$n + 1L
  as.name(paste0(prefix, state$n))
}

validate_name <- function(name, where = "binding") {
  if (!nzchar(name)) stop(sprintf("empty %s name", where), call. = FALSE)
  if (startsWith(name, "..ppl_")) {
    stop(
      sprintf("%s name uses the reserved prefix '..ppl_': %s", where, name),
      call. = FALSE
    )
  }
  name
}

addr_code <- function(address, part) {
  call_code("..ppl_at", list(address, part))
}

compile_function_value <- function(expr, state) {
  if (!is_head(expr, "function") || length(expr) < 3L) {
    stop("malformed function expression", call. = FALSE)
  }

  source_formals <- expr[[2L]]
  parameters <- names(source_formals)
  if (is.null(parameters)) parameters <- character()

  unsupported <- any(parameters == "...") ||
    any(!vapply(
      seq_along(source_formals),
      function(i) missing_formal(source_formals[i]),
      logical(1)
    ))
  if (unsupported) {
    stop(
      "functions support positional parameters without defaults or `...`",
      call. = FALSE
    )
  }

  if (length(parameters)) {
    vapply(parameters, validate_name, character(1), where = "parameter")
  }

  address_name <- as.character(gensym(state, "..ppl_addr_"))
  hidden <- formals(function(.address) NULL)
  names(hidden) <- address_name
  all_formals <- as.pairlist(c(hidden, source_formals))

  body <- compile_sequence(
    block_items(expr[[3L]]),
    as.name(address_name),
    state
  )

  # Source arguments are call-by-value.  Force the host promises immediately so
  # a returned continuation never changes the source evaluation discipline.
  force_calls <- lapply(
    c(address_name, parameters),
    function(name) call_code("..ppl_force", list(as.name(name)))
  )
  body <- as.call(c(list(as.name("{")), force_calls, list(body)))

  fcode <- function_code(all_formals, body)
  call_code(
    "..ppl_function",
    list(fcode, length(parameters), parameters)
  )
}

compile_binding <- function(name, rhs_expr, exprs, address, state, i) {
  if (i == length(exprs)) {
    stop("a binding needs a following body expression", call. = FALSE)
  }

  name <- validate_name(name)
  rhs <- compile_expr(
    rhs_expr,
    addr_code(address, paste0("let:", i - 1L)),
    state
  )
  value <- gensym(state)
  rest <- compile_sequence(exprs, address, state, i + 1L)
  body <- block_code(
    call_code("<-", list(as.name(name), value)),
    rest
  )

  call_code("..ppl_bind", list(rhs, lambda1(value, body)))
}

compile_sequence <- function(exprs, address, state, i = 1L) {
  if (!length(exprs)) stop("empty body", call. = FALSE)

  expr <- exprs[[i]]
  last <- i == length(exprs)

  # A bare formula statement is an optional R/statistical spelling of
  #   name <- sample(distribution)
  # It is syntax only: no host formula object is constructed.
  if (is_head(expr, "~")) {
    if (length(expr) != 3L || !is.symbol(expr[[2L]])) {
      stop(
        "a sampling formula must have the form `name ~ distribution`",
        call. = FALSE
      )
    }

    sampled <- call_code("sample", list(expr[[3L]]))
    return(compile_binding(
      as.character(expr[[2L]]),
      sampled,
      exprs,
      address,
      state,
      i
    ))
  }

  # `<-` is the R spelling of a sequential MiniPPL `let` binding.  The
  # continuation function creates a fresh lexical frame, so rebinding shadows
  # rather than mutates an earlier captured environment.
  if (is_head(expr, "<-")) {
    if (length(expr) != 3L || !is.symbol(expr[[2L]])) {
      stop("a binding requires a simple name", call. = FALSE)
    }

    return(compile_binding(
      as.character(expr[[2L]]),
      expr[[3L]],
      exprs,
      address,
      state,
      i
    ))
  }

  current <- compile_expr(
    expr,
    addr_code(address, paste0("body:", i - 1L)),
    state
  )
  if (last) return(current)

  ignored <- gensym(state)
  call_code(
    "..ppl_bind",
    list(
      current,
      lambda1(ignored, compile_sequence(exprs, address, state, i + 1L))
    )
  )
}

compile_application <- function(expr, address, state) {
  parts <- as.list(expr)
  part_names <- names(parts)
  if (!is.null(part_names) && any(nzchar(part_names[-1L]))) {
    stop("named arguments are not part of MiniPPL", call. = FALSE)
  }

  function_value <- gensym(state)
  arguments <- parts[-1L]

  compile_arguments <- function(i, values) {
    if (i > length(arguments)) {
      argv <- call_code("..ppl_args", values)
      return(call_code(
        "..ppl_apply",
        list(address, function_value, argv)
      ))
    }

    argument_value <- gensym(state)
    argument <- compile_expr(
      arguments[[i]],
      addr_code(address, paste0("arg:", i - 1L)),
      state
    )

    call_code(
      "..ppl_bind",
      list(
        argument,
        lambda1(
          argument_value,
          compile_arguments(i + 1L, c(values, list(argument_value)))
        )
      )
    )
  }

  operator <- compile_expr(
    expr[[1L]],
    addr_code(address, "fn"),
    state
  )

  call_code(
    "..ppl_bind",
    list(operator, lambda1(function_value, compile_arguments(1L, list())))
  )
}

compile_expr <- function(expr, address, state) {
  if (is.symbol(expr)) {
    return(call_code("..ppl_pure", list(expr)))
  }
  if (!is.call(expr)) {
    return(call_code("..ppl_pure", list(expr)))
  }

  head <- head_name(expr)

  if (identical(head, "(")) {
    if (length(expr) != 2L) stop("malformed parentheses", call. = FALSE)
    return(compile_expr(expr[[2L]], address, state))
  }

  if (identical(head, "{")) {
    return(compile_sequence(block_items(expr), address, state))
  }

  if (identical(head, "if")) {
    if (length(expr) != 4L) {
      stop("if requires a test, then branch, and else branch", call. = FALSE)
    }

    condition <- gensym(state)
    branch <- call_code(
      "if",
      list(
        call_code("..ppl_truthy", list(condition)),
        compile_expr(expr[[3L]], addr_code(address, "then"), state),
        compile_expr(expr[[4L]], addr_code(address, "else"), state)
      )
    )

    return(call_code(
      "..ppl_bind",
      list(
        compile_expr(expr[[2L]], addr_code(address, "test"), state),
        lambda1(condition, branch)
      )
    ))
  }

  if (identical(head, "function")) {
    return(call_code(
      "..ppl_pure",
      list(compile_function_value(expr, state))
    ))
  }

  if (identical(head, "sample")) {
    if (length(expr) != 2L) {
      stop("sample requires one distribution", call. = FALSE)
    }

    distribution <- gensym(state)
    return(call_code(
      "..ppl_bind",
      list(
        compile_expr(expr[[2L]], addr_code(address, "dist"), state),
        lambda1(
          distribution,
          call_code("..ppl_sample", list(address, distribution))
        )
      )
    ))
  }

  if (identical(head, "observe")) {
    # Both spellings have the same semantics:
    #   observe(distribution, value)
    #   observe(value ~ distribution)
    if (length(expr) == 2L && is_head(expr[[2L]], "~") &&
        length(expr[[2L]]) == 3L) {
      formula <- expr[[2L]]
      distribution_expr <- formula[[3L]]
      value_expr <- formula[[2L]]
    } else if (length(expr) == 3L) {
      distribution_expr <- expr[[2L]]
      value_expr <- expr[[3L]]
    } else {
      stop(
        "observe requires `(distribution, value)` or `(value ~ distribution)`",
        call. = FALSE
      )
    }

    distribution <- gensym(state)
    value <- gensym(state)
    return(call_code(
      "..ppl_bind",
      list(
        compile_expr(distribution_expr, addr_code(address, "dist"), state),
        lambda1(
          distribution,
          call_code(
            "..ppl_bind",
            list(
              compile_expr(value_expr, addr_code(address, "value"), state),
              lambda1(
                value,
                call_code(
                  "..ppl_observe",
                  list(address, distribution, value)
                )
              )
            )
          )
        )
      )
    ))
  }

  if (identical(head, "~")) {
    stop(
      "sampling formulae are only allowed as statements in a block",
      call. = FALSE
    )
  }
  if (identical(head, "<<-")) {
    stop(
      "recursive definitions must be leading declarations in the program",
      call. = FALSE
    )
  }
  if (identical(head, "<-")) {
    stop("a binding is only allowed as a statement in a block", call. = FALSE)
  }

  compile_application(expr, address, state)
}

# Leading `name <- function(...)` declarations translate top-level `defn`.
# `<<-` is accepted as an explicit spelling, but is captured syntax only: the
# compiler never performs host-R superassignment.
is_top_definition <- function(expr) {
  head_name(expr) %in% c("<-", "<<-") && length(expr) == 3L &&
    is.symbol(expr[[2L]]) && is_head(expr[[3L]], "function")
}

runtime_internals <- function() {
  list(
    ..ppl_pure = pure,
    ..ppl_bind = bind,
    ..ppl_truthy = truthy,
    ..ppl_at = address_at,
    ..ppl_sample = sample_effect,
    ..ppl_observe = observe_effect,
    ..ppl_function = ppl_function,
    ..ppl_args = ppl_args,
    ..ppl_apply = apply_value,
    ..ppl_force = base::force,
    ..ppl_root = character()
  )
}

compile_ppl <- function(source) {
  exprs <- block_items(source)
  definitions <- list()

  while (length(exprs) && is_top_definition(exprs[[1L]])) {
    definitions[[length(definitions) + 1L]] <- exprs[[1L]]
    exprs <- exprs[-1L]
  }
  if (!length(exprs)) stop("program has no main expression", call. = FALSE)

  state <- new_compiler()
  prelude <- primitive_env(runtime_internals())
  global <- new.env(parent = prelude, hash = TRUE)
  definition_names <- character()

  if (length(definitions)) {
    for (definition in definitions) {
      name <- validate_name(as.character(definition[[2L]]), "definition")
      if (exists(name, envir = global, inherits = FALSE)) {
        stop(sprintf("duplicate definition: %s", name), call. = FALSE)
      }

      value <- eval(
        compile_function_value(definition[[3L]], state),
        envir = global
      )
      assign(name, value, envir = global)
      definition_names <- c(definition_names, name)
    }
  }

  code <- compile_sequence(exprs, as.name("..ppl_root"), state)
  computation <- eval(code, envir = global)
  if (!inherits(computation, "ppl_cont")) {
    stop("internal error: compiler did not produce a computation", call. = FALSE)
  }

  lockEnvironment(global, bindings = TRUE)
  structure(
    list(
      source = source,
      code = code,
      computation = computation,
      environment = global,
      definitions = definition_names
    ),
    class = "ppl_program"
  )
}

# `expr` is a promise.  substitute() observes its syntax without forcing it.
ppl <- function(expr) compile_ppl(substitute(expr))

# Low-level entry point for programmatically constructed R language objects.
ppl_ <- function(expr) {
  force(expr)
  compile_ppl(expr)
}

print.ppl_program <- function(x, ...) {
  defs <- if (length(x$definitions)) {
    paste(x$definitions, collapse = ", ")
  } else {
    "none"
  }
  cat("<MiniPPL program>\n")
  cat("  recursive definitions:", defs, "\n")
  cat("  source:\n")
  cat(paste0("    ", deparse(x$source, width.cutoff = 80L)), sep = "\n")
  cat("\n")
  invisible(x)
}

show_compiled <- function(program) {
  if (!inherits(program, "ppl_program")) {
    stop("expected a compiled MiniPPL program", call. = FALSE)
  }
  cat(paste(deparse(program$code, width.cutoff = 100L), collapse = "\n"), "\n")
  invisible(program$code)
}

as_program <- function(program) {
  if (!inherits(program, "ppl_program")) {
    stop("expected a program created by ppl()", call. = FALSE)
  }
  program
}

initial_step <- function(program) {
  program <- as_program(program)
  run_cont(program$computation, Done)
}

# -----------------------------------------------------------------------------
# Controllers over the same Done / Sample / Observe stream
# -----------------------------------------------------------------------------

softmax <- function(x) {
  if (!length(x)) return(numeric())
  m <- max(x)
  if (!is.finite(m)) stop("all weights are zero", call. = FALSE)
  z <- exp(x - m)
  z / sum(z)
}

simplify_values <- function(values) {
  scalar <- vapply(
    values,
    function(x) is.atomic(x) && length(x) == 1L,
    logical(1)
  )
  if (all(scalar)) unlist(values, use.names = FALSE) else values
}

run_lw_once <- function(program) {
  step <- initial_step(program)
  log_weight <- 0

  repeat {
    if (step$tag == "done") {
      return(list(value = step$value, log_weight = log_weight))
    }

    if (step$tag == "sample") {
      step <- step$k(draw(step$distribution))
    } else if (step$tag == "observe") {
      log_weight <- log_weight + log_prob(step$distribution, step$value)
      step <- step$resume()
    } else {
      stop("unknown Step", call. = FALSE)
    }
  }
}

run_lw <- function(program, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  run_lw_once(as_program(program))
}

likelihood_weighting <- function(program, N, seed = NULL) {
  if (N < 1L) stop("N must be positive", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  program <- as_program(program)

  runs <- lapply(seq_len(N), function(i) run_lw_once(program))
  log_weights <- vapply(runs, `[[`, numeric(1), "log_weight")

  list(
    values = simplify_values(lapply(runs, `[[`, "value")),
    weights = softmax(log_weights),
    log_weights = log_weights
  )
}

advance_particle <- function(particle) {
  step <- particle$step
  while (step$tag == "sample") {
    step <- step$k(draw(step$distribution))
  }
  particle$step <- step
  particle
}

smc <- function(program, N, seed = NULL) {
  if (N < 1L) stop("N must be positive", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  program <- as_program(program)

  particles <- lapply(
    seq_len(N),
    function(i) list(step = initial_step(program), log_weight = 0)
  )

  repeat {
    particles <- lapply(particles, advance_particle)
    tags <- unique(vapply(
      particles,
      function(particle) particle$step$tag,
      character(1)
    ))

    if (length(tags) == 1L && identical(tags, "done")) {
      return(simplify_values(lapply(
        particles,
        function(particle) particle$step$value
      )))
    }

    if (length(tags) != 1L || !identical(tags, "observe")) {
      stop(
        "particles reached different breakpoints: SMC requires a shared observe sequence",
        call. = FALSE
      )
    }

    increments <- vapply(
      particles,
      function(particle) {
        log_prob(particle$step$distribution, particle$step$value)
      },
      numeric(1)
    )

    paused <- lapply(seq_along(particles), function(i) {
      list(
        step = particles[[i]]$step$resume(),
        log_weight = particles[[i]]$log_weight + increments[[i]]
      )
    })

    ancestors <- base::sample.int(
      N,
      N,
      replace = TRUE,
      prob = softmax(increments)
    )
    particles <- lapply(ancestors, function(i) paused[[i]])
  }
}

trace_run <- function(program, redraw = NULL, cache = list()) {
  step <- initial_step(program)
  X <- list()
  S <- numeric()
  O <- numeric()

  repeat {
    if (step$tag == "sample") {
      key <- address_key(step$address)
      x <- if (identical(key, redraw) || !(key %in% names(cache))) {
        draw(step$distribution)
      } else {
        cache[[key]]
      }

      X[key] <- list(x)
      S[[key]] <- log_prob(step$distribution, x)
      step <- step$k(x)
    } else if (step$tag == "observe") {
      key <- address_key(step$address)
      O[[key]] <- log_prob(step$distribution, step$value)
      step <- step$resume()
    } else if (step$tag == "done") {
      return(list(value = step$value, X = X, S = S, O = O))
    } else {
      stop("unknown Step", call. = FALSE)
    }
  }
}

mh_log_alpha <- function(X, X2, S, S2, O, O2, redraw) {
  forward <- union(redraw, setdiff(names(X2), names(X)))
  reverse <- union(redraw, setdiff(names(X), names(X2)))

  numerator <- sum(S2[setdiff(names(S2), forward)]) + sum(O2)
  denominator <- sum(S[setdiff(names(S), reverse)]) + sum(O)

  log(length(X)) - log(length(X2)) + numerator - denominator
}

single_site_mh <- function(
  program,
  steps,
  warmup = 2000L,
  seed = NULL
) {
  if (steps < 1L) stop("steps must be positive", call. = FALSE)
  if (warmup < 0L) stop("warmup cannot be negative", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  program <- as_program(program)

  current <- trace_run(program)
  if (!length(current$X)) {
    stop("single-site MH requires at least one sample site", call. = FALSE)
  }

  chain <- vector("list", steps)
  kept <- 0L

  for (i in seq_len(steps + warmup)) {
    addresses <- names(current$X)
    redraw <- addresses[[base::sample.int(length(addresses), 1L)]]
    proposal <- trace_run(program, redraw, current$X)

    log_alpha <- mh_log_alpha(
      current$X,
      proposal$X,
      current$S,
      proposal$S,
      current$O,
      proposal$O,
      redraw
    )

    if (log(stats::runif(1L)) < min(0, log_alpha)) {
      current <- proposal
    }

    if (i > warmup) {
      kept <- kept + 1L
      chain[kept] <- list(current$value)
    }
  }

  simplify_values(chain)
}

# -----------------------------------------------------------------------------
# Exact enumeration for finite Bernoulli programs
#
# A Sample continuation is an ordinary reusable R closure.  Enumeration simply
# invokes it once for each support value; no evaluator state needs to be copied.
# This is the finite-discrete controller from the June 26 material.
# -----------------------------------------------------------------------------

finite_support <- function(distribution) {
  if (inherits(distribution, "ppl_bernoulli")) {
    values <- list(FALSE, TRUE)
    out <- lapply(values, function(value) {
      list(
        value = value,
        log_probability = log_prob(distribution, value)
      )
    })
    return(Filter(function(x) is.finite(x$log_probability), out))
  }

  stop(
    sprintf(
      "cannot enumerate %s; this minimal controller handles Bernoulli samples",
      distribution$name
    ),
    call. = FALSE
  )
}

enumerate_traces <- function(program, max_states = 10000000L) {
  if (max_states < 1L) stop("max_states must be positive", call. = FALSE)
  program <- as_program(program)

  stack <- list(list(step = initial_step(program), log_weight = 0))
  finished <- list()
  visited <- 0L

  while (length(stack)) {
    visited <- visited + 1L
    if (visited > max_states) {
      stop(sprintf("state budget exceeded: %d", max_states), call. = FALSE)
    }

    branch <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    step <- branch$step

    if (step$tag == "done") {
      finished[[length(finished) + 1L]] <- list(
        value = step$value,
        log_weight = branch$log_weight
      )
    } else if (step$tag == "observe") {
      stack[[length(stack) + 1L]] <- list(
        step = step$resume(),
        log_weight = branch$log_weight +
          log_prob(step$distribution, step$value)
      )
    } else if (step$tag == "sample") {
      support <- finite_support(step$distribution)
      for (candidate in support) {
        stack[[length(stack) + 1L]] <- list(
          step = step$k(candidate$value),
          log_weight = branch$log_weight + candidate$log_probability
        )
      }
    } else {
      stop("unknown Step", call. = FALSE)
    }
  }

  structure(finished, class = c("ppl_enumeration", "list"))
}

log_add_exp <- function(a, b) {
  if (is.infinite(a) && a < 0) return(b)
  if (is.infinite(b) && b < 0) return(a)
  m <- max(a, b)
  m + log(exp(a - m) + exp(b - m))
}

posterior_table <- function(runs, key = identity) {
  if (!length(runs)) stop("enumeration produced no complete traces", call. = FALSE)

  masses <- numeric()
  values <- list()

  for (run in runs) {
    value <- key(run$value)
    if (!is.atomic(value) || length(value) != 1L) {
      stop("posterior_table key must return one atomic value", call. = FALSE)
    }

    label <- paste0(typeof(value), ":", as.character(value))
    if (label %in% names(masses)) {
      masses[[label]] <- log_add_exp(masses[[label]], run$log_weight)
    } else {
      masses[[label]] <- run$log_weight
      values[[label]] <- value
    }
  }

  labels <- names(masses)
  m <- max(masses)
  log_evidence <- m + log(sum(exp(masses - m)))
  probability <- exp(masses - log_evidence)
  order_index <- order(simplify_values(unname(values[labels])))
  labels <- labels[order_index]

  table <- data.frame(
    value = simplify_values(unname(values[labels])),
    log_mass = unname(masses[labels]),
    probability = unname(probability[labels]),
    row.names = NULL,
    check.names = FALSE
  )

  list(table = table, log_evidence = log_evidence)
}
