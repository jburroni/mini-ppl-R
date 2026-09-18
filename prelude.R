# MiniPPL prelude: values, distributions, and deterministic primitives.
#
# This file contains no evaluator.  `primitive_env()` builds the closed lexical
# environment visible to a MiniPPL program.  The environment has no parent, so
# arbitrary host-R functions are not accidentally part of the source language.

# -----------------------------------------------------------------------------
# Runtime values
# -----------------------------------------------------------------------------

ppl_vector <- function(...) {
  structure(list(...), class = "ppl_vector")
}

vector_items <- function(x) {
  if (!inherits(x, "ppl_vector")) {
    stop("expected a MiniPPL vector", call. = FALSE)
  }
  unclass(x)
}

ppl_map <- function(keys = list(), values = list()) {
  structure(list(keys = keys, values = values), class = "ppl_map")
}

is_number <- function(x) {
  is.atomic(x) && length(x) == 1L &&
    (is.integer(x) || is.double(x)) && !is.logical(x)
}

num <- function(x) {
  if (is.logical(x) && length(x) == 1L) return(as.integer(x))
  x
}

truthy <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.logical(x) && length(x) == 1L) return(isTRUE(x))
  if ((is.integer(x) || is.double(x)) && length(x) == 1L) return(x != 0)
  if (is.character(x) && length(x) == 1L) return(nchar(x) > 0L)
  if (inherits(x, "ppl_vector")) return(length(vector_items(x)) > 0L)
  if (inherits(x, "ppl_map")) return(length(x$keys) > 0L)
  TRUE
}

value_equal <- function(a, b) {
  if ((is.numeric(a) || is.logical(a)) && length(a) == 1L &&
      (is.numeric(b) || is.logical(b)) && length(b) == 1L) {
    return(isTRUE(num(a) == num(b)))
  }

  if (inherits(a, "ppl_vector") && inherits(b, "ppl_vector")) {
    aa <- vector_items(a)
    bb <- vector_items(b)
    return(
      length(aa) == length(bb) &&
        all(vapply(
          seq_along(aa),
          function(i) value_equal(aa[[i]], bb[[i]]),
          logical(1)
        ))
    )
  }

  if (inherits(a, "ppl_map") && inherits(b, "ppl_map")) {
    if (length(a$keys) != length(b$keys)) return(FALSE)
    return(all(vapply(seq_along(a$keys), function(i) {
      j <- map_index(b, a$keys[[i]])
      !is.na(j) && value_equal(a$values[[i]], b$values[[j]])
    }, logical(1))))
  }

  if (is.matrix(a) || is.matrix(b) || is.array(a) || is.array(b)) {
    return(isTRUE(all.equal(a, b, check.attributes = TRUE)))
  }

  identical(a, b)
}

map_index <- function(m, key) {
  hits <- which(vapply(m$keys, function(k) value_equal(k, key), logical(1)))
  if (length(hits)) hits[[1L]] else NA_integer_
}

map_get <- function(m, key, default = NULL) {
  i <- map_index(m, key)
  if (is.na(i)) default else m$values[[i]]
}

map_put <- function(m, key, value) {
  keys <- m$keys
  values <- m$values
  i <- map_index(m, key)

  if (is.na(i)) {
    keys <- c(keys, list(key))
    values <- c(values, list(value))
  } else {
    values[i] <- list(value)
  }

  ppl_map(keys, values)
}

as_plain_numeric <- function(x) {
  if (inherits(x, "ppl_vector")) x <- vector_items(x)
  as.numeric(unlist(x, recursive = TRUE, use.names = FALSE))
}

as_matrix_value <- function(x) {
  if (is.matrix(x) || is.array(x)) return(x)

  if (inherits(x, "ppl_vector")) {
    xs <- vector_items(x)
    if (!length(xs)) return(numeric())

    scalar <- vapply(
      xs,
      function(z) is.atomic(z) && length(z) == 1L,
      logical(1)
    )
    if (all(scalar)) return(as_plain_numeric(x))

    rows <- lapply(xs, as_plain_numeric)
    widths <- vapply(rows, length, integer(1))
    if (length(unique(widths)) == 1L) return(do.call(rbind, rows))
  }

  as.matrix(x)
}

# -----------------------------------------------------------------------------
# Distributions
# -----------------------------------------------------------------------------

ppl_distribution <- function(name, draw, log_prob, parameters = list()) {
  force(name)
  force(draw)
  force(log_prob)
  force(parameters)

  structure(
    list(
      name = name,
      draw = draw,
      log_prob = log_prob,
      parameters = parameters
    ),
    class = c(paste0("ppl_", gsub("-", "_", name)), "ppl_distribution")
  )
}

is_distribution <- function(x) inherits(x, "ppl_distribution")

draw <- function(d) {
  if (!is_distribution(d)) {
    stop("sample: value is not a distribution", call. = FALSE)
  }
  d$draw()
}

log_prob <- function(d, x) {
  if (!is_distribution(d)) {
    stop("observe: value is not a distribution", call. = FALSE)
  }
  as.numeric(d$log_prob(x))
}

print.ppl_distribution <- function(x, ...) {
  args <- unname(x$parameters)
  text <- if (length(args)) {
    paste(vapply(args, function(v) paste(deparse(v), collapse = ""), character(1)), collapse = " ")
  } else {
    ""
  }
  cat(sprintf("(%s%s)\n", x$name, if (nzchar(text)) paste0(" ", text) else ""))
  invisible(x)
}

normal_dist <- function(mu, sigma) {
  mu <- as.numeric(num(mu))
  sigma <- as.numeric(num(sigma))
  if (sigma <= 0) stop("normal: sigma must be > 0", call. = FALSE)

  ppl_distribution(
    "normal",
    function() stats::rnorm(1L, mu, sigma),
    function(x) stats::dnorm(as.numeric(num(x)), mu, sigma, log = TRUE),
    list(mu = mu, sigma = sigma)
  )
}

log_normal_dist <- function(mu, sigma) {
  mu <- as.numeric(num(mu))
  sigma <- as.numeric(num(sigma))
  if (sigma <= 0) stop("log-normal: sigma must be > 0", call. = FALSE)

  ppl_distribution(
    "log-normal",
    function() stats::rlnorm(1L, mu, sigma),
    function(x) stats::dlnorm(as.numeric(num(x)), mu, sigma, log = TRUE),
    list(mu = mu, sigma = sigma)
  )
}

uniform_dist <- function(a, b) {
  a <- as.numeric(num(a))
  b <- as.numeric(num(b))
  if (b <= a) stop("uniform-continuous: requires b > a", call. = FALSE)

  ppl_distribution(
    "uniform-continuous",
    function() stats::runif(1L, a, b),
    function(x) stats::dunif(as.numeric(num(x)), a, b, log = TRUE),
    list(a = a, b = b)
  )
}

exponential_dist <- function(rate) {
  rate <- as.numeric(num(rate))
  if (rate <= 0) stop("exponential: rate must be > 0", call. = FALSE)

  ppl_distribution(
    "exponential",
    function() stats::rexp(1L, rate),
    function(x) stats::dexp(as.numeric(num(x)), rate, log = TRUE),
    list(rate = rate)
  )
}

beta_dist <- function(alpha, beta) {
  alpha <- as.numeric(num(alpha))
  beta <- as.numeric(num(beta))
  if (alpha <= 0 || beta <= 0) {
    stop("beta: parameters must be > 0", call. = FALSE)
  }

  ppl_distribution(
    "beta",
    function() stats::rbeta(1L, alpha, beta),
    function(x) {
      x <- as.numeric(num(x))
      if (x <= 0 || x >= 1) -Inf else stats::dbeta(x, alpha, beta, log = TRUE)
    },
    list(alpha = alpha, beta = beta)
  )
}

gamma_dist <- function(shape, rate) {
  shape <- as.numeric(num(shape))
  rate <- as.numeric(num(rate))
  if (shape <= 0 || rate <= 0) {
    stop("gamma: parameters must be > 0", call. = FALSE)
  }

  ppl_distribution(
    "gamma",
    function() stats::rgamma(1L, shape = shape, rate = rate),
    function(x) {
      x <- as.numeric(num(x))
      if (x <= 0) -Inf else stats::dgamma(x, shape = shape, rate = rate, log = TRUE)
    },
    list(shape = shape, rate = rate)
  )
}

poisson_dist <- function(lambda) {
  lambda <- as.numeric(num(lambda))
  if (lambda <= 0) stop("poisson: rate must be > 0", call. = FALSE)

  ppl_distribution(
    "poisson",
    function() as.integer(stats::rpois(1L, lambda)),
    function(x) stats::dpois(as.integer(num(x)), lambda, log = TRUE),
    list(lambda = lambda)
  )
}

bernoulli_dist <- function(p) {
  p <- as.numeric(num(p))
  if (p < 0 || p > 1) stop("bernoulli: p must be in [0,1]", call. = FALSE)

  ppl_distribution(
    "bernoulli",
    function() stats::runif(1L) < p,
    function(x) {
      if (truthy(x)) {
        if (p == 0) -Inf else log(p)
      } else {
        if (p == 1) -Inf else log1p(-p)
      }
    },
    list(p = p)
  )
}

discrete_dist <- function(...) {
  args <- list(...)
  probs <- if (length(args) == 1L && inherits(args[[1L]], "ppl_vector")) {
    as_plain_numeric(args[[1L]])
  } else {
    as.numeric(unlist(args, use.names = FALSE))
  }

  if (!length(probs) || any(probs < 0) || sum(probs) <= 0) {
    stop("discrete: invalid probability vector", call. = FALSE)
  }
  probs <- probs / sum(probs)

  ppl_distribution(
    "discrete",
    function() as.integer(base::sample.int(length(probs), 1L, prob = probs) - 1L),
    function(k) {
      k <- as.integer(num(k))
      if (k < 0L || k >= length(probs) || probs[[k + 1L]] == 0) {
        -Inf
      } else {
        log(probs[[k + 1L]])
      }
    },
    list(probabilities = probs)
  )
}

uniform_discrete_dist <- function(lo, hi) {
  lo <- as.integer(num(lo))
  hi <- as.integer(num(hi))
  if (hi <= lo) stop("uniform-discrete: requires hi > lo", call. = FALSE)

  ppl_distribution(
    "uniform-discrete",
    function() as.integer(base::sample.int(hi - lo, 1L) - 1L + lo),
    function(k) {
      k <- as.integer(num(k))
      if (k < lo || k >= hi) -Inf else -log(hi - lo)
    },
    list(lo = lo, hi = hi)
  )
}

dirichlet_dist <- function(...) {
  args <- list(...)
  alpha <- if (length(args) == 1L && inherits(args[[1L]], "ppl_vector")) {
    as_plain_numeric(args[[1L]])
  } else {
    as.numeric(unlist(args, use.names = FALSE))
  }

  if (!length(alpha) || any(alpha <= 0)) {
    stop("dirichlet: alphas must be > 0", call. = FALSE)
  }

  ppl_distribution(
    "dirichlet",
    function() {
      z <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
      do.call(ppl_vector, as.list(z / sum(z)))
    },
    function(x) {
      x <- as_plain_numeric(x)
      if (length(x) != length(alpha) || any(x <= 0)) return(-Inf)
      lgamma(sum(alpha)) - sum(lgamma(alpha)) + sum((alpha - 1) * log(x))
    },
    list(alpha = alpha)
  )
}

# -----------------------------------------------------------------------------
# Deterministic primitives
# -----------------------------------------------------------------------------

p_add <- function(...) {
  xs <- lapply(list(...), num)
  if (!length(xs)) return(0)
  Reduce(`+`, xs)
}

p_sub <- function(...) {
  xs <- lapply(list(...), num)
  if (!length(xs)) stop("-: expected at least one argument", call. = FALSE)
  if (length(xs) == 1L) return(-xs[[1L]])
  Reduce(`-`, xs)
}

p_mul <- function(...) {
  xs <- lapply(list(...), num)
  if (!length(xs)) stop("*: expected at least one argument", call. = FALSE)
  Reduce(`*`, xs)
}

p_div <- function(...) {
  xs <- lapply(list(...), num)
  if (!length(xs)) stop("/: expected at least one argument", call. = FALSE)
  if (length(xs) == 1L) return(1 / xs[[1L]])
  Reduce(`/`, xs)
}

zero_based_index <- function(i, n) {
  i <- as.integer(num(i))
  if (i < 0L) i <- n + i
  i + 1L
}

p_hash_map <- function(...) {
  xs <- list(...)
  if (length(xs) %% 2L) stop("hash-map: odd number of arguments", call. = FALSE)

  out <- ppl_map()
  if (length(xs)) {
    for (i in seq.int(1L, length(xs), by = 2L)) {
      out <- map_put(out, xs[[i]], xs[[i + 1L]])
    }
  }
  out
}

p_get <- function(coll, key, default = NULL) {
  if (inherits(coll, "ppl_map")) return(map_get(coll, key, default))

  if (is.character(coll) && length(coll) == 1L) {
    i <- zero_based_index(key, nchar(coll))
    if (i < 1L || i > nchar(coll)) default else substr(coll, i, i)
  } else if (inherits(coll, "ppl_vector")) {
    xs <- vector_items(coll)
    i <- zero_based_index(key, length(xs))
    if (i < 1L || i > length(xs)) default else xs[[i]]
  } else if (is.matrix(coll)) {
    i <- zero_based_index(key, nrow(coll))
    if (i < 1L || i > nrow(coll)) default else coll[i, , drop = TRUE]
  } else if (is.array(coll) || is.atomic(coll)) {
    i <- zero_based_index(key, length(coll))
    if (i < 1L || i > length(coll)) default else coll[[i]]
  } else {
    stop("get: unsupported collection", call. = FALSE)
  }
}

p_put <- function(coll, key, value) {
  if (inherits(coll, "ppl_map")) return(map_put(coll, key, value))

  if (inherits(coll, "ppl_vector")) {
    xs <- vector_items(coll)
    i <- zero_based_index(key, length(xs))
    if (i < 1L || i > length(xs)) stop("put: index out of range", call. = FALSE)
    xs[i] <- list(value)
    return(do.call(ppl_vector, xs))
  }

  if (is.matrix(coll)) {
    out <- coll
    i <- zero_based_index(key, nrow(out))
    if (i < 1L || i > nrow(out)) stop("put: index out of range", call. = FALSE)
    out[i, ] <- value
    return(out)
  }

  if (is.array(coll) || is.atomic(coll)) {
    out <- coll
    i <- zero_based_index(key, length(out))
    if (i < 1L || i > length(out)) stop("put: index out of range", call. = FALSE)
    out[[i]] <- value
    return(out)
  }

  stop("put: unsupported collection", call. = FALSE)
}

p_first <- function(v) p_get(v, 0L)
p_second <- function(v) p_get(v, 1L)
p_nth <- function(v, i) p_get(v, i)

p_last <- function(v) {
  if (inherits(v, "ppl_vector")) {
    xs <- vector_items(v)
    return(xs[[length(xs)]])
  }
  if (is.character(v) && length(v) == 1L) {
    return(substr(v, nchar(v), nchar(v)))
  }
  if (is.matrix(v)) return(v[nrow(v), , drop = TRUE])
  v[[length(v)]]
}

p_rest <- function(v) {
  xs <- vector_items(v)
  if (length(xs) <= 1L) ppl_vector() else do.call(ppl_vector, xs[-1L])
}

p_conj <- function(coll, ...) {
  do.call(ppl_vector, c(vector_items(coll), list(...)))
}

p_cons <- function(x, coll) {
  do.call(ppl_vector, c(list(x), vector_items(coll)))
}

p_append <- p_conj

p_concat <- function(...) {
  parts <- lapply(list(...), vector_items)
  xs <- if (length(parts)) do.call(c, parts) else list()
  do.call(ppl_vector, xs)
}

p_count <- function(coll) {
  if (inherits(coll, "ppl_map")) return(length(coll$keys))
  if (inherits(coll, "ppl_vector")) return(length(vector_items(coll)))
  if (is.character(coll) && length(coll) == 1L) return(nchar(coll))
  if (is.matrix(coll) || length(dim(coll)) > 1L) return(dim(coll)[[1L]])
  length(coll)
}

p_empty <- function(coll) p_count(coll) == 0L
p_peek <- p_last

p_range <- function(...) {
  a <- as.integer(vapply(list(...), num, numeric(1)))

  if (length(a) == 1L) {
    start <- 0L
    stop <- a[[1L]]
    step <- 1L
  } else if (length(a) == 2L) {
    start <- a[[1L]]
    stop <- a[[2L]]
    step <- 1L
  } else if (length(a) == 3L) {
    start <- a[[1L]]
    stop <- a[[2L]]
    step <- a[[3L]]
  } else {
    stop("range: expected 1, 2, or 3 arguments", call. = FALSE)
  }

  if (step == 0L) stop("range: step cannot be zero", call. = FALSE)
  if ((step > 0L && start >= stop) || (step < 0L && start <= stop)) {
    return(ppl_vector())
  }

  vals <- seq.int(start, stop - sign(step), by = step)
  do.call(ppl_vector, as.list(vals))
}

# Build the closed environment visible to generated MiniPPL code.  `internals`
# is supplied by runtime.R and contains the continuation machinery under names
# reserved by the compiler.
primitive_env <- function(internals = list()) {
  p <- new.env(parent = emptyenv(), hash = TRUE)
  add <- function(name, value) assign(name, value, envir = p)

  # R evaluator specials needed by generated code.  Source occurrences are
  # intercepted by the compiler; these bindings are implementation plumbing.
  add("function", get("function", envir = baseenv()))
  add("{", get("{", envir = baseenv()))
  add("<-", get("<-", envir = baseenv()))
  add("if", get("if", envir = baseenv()))

  if (length(internals)) {
    for (name in names(internals)) add(name, internals[[name]])
  }

  # Arithmetic.
  add("+", p_add)
  add("-", p_sub)
  add("*", p_mul)
  add("/", p_div)
  add("sqrt", function(x) sqrt(num(x)))
  add("exp", function(x) exp(num(x)))
  add("log", function(x) log(num(x)))
  add("pow", function(x, y) num(x) ^ num(y))
  add("^", function(x, y) num(x) ^ num(y))
  add("abs", function(x) abs(num(x)))
  add("floor", function(x) floor(num(x)))
  add("ceil", function(x) ceiling(num(x)))
  add("ceiling", function(x) ceiling(num(x)))
  add("tanh", function(x) tanh(num(x)))
  add("max", function(...) {
    xs <- list(...)
    if (!length(xs)) stop("max: expected at least one argument", call. = FALSE)
    max(vapply(xs, num, numeric(1)))
  })
  add("min", function(...) {
    xs <- list(...)
    if (!length(xs)) stop("min: expected at least one argument", call. = FALSE)
    min(vapply(xs, num, numeric(1)))
  })
  add("mod", function(a, b) num(a) %% num(b))
  add("%%", function(a, b) num(a) %% num(b))

  # Comparisons and logic.  Arguments are evaluated by the compiler before
  # these functions are called, preserving the original strict semantics.
  add("=", value_equal)
  add("==", value_equal)
  add("!=", function(a, b) !value_equal(a, b))
  add("<", function(a, b) num(a) < num(b))
  add(">", function(a, b) num(a) > num(b))
  add("<=", function(a, b) num(a) <= num(b))
  add(">=", function(a, b) num(a) >= num(b))
  add("and", function(...) all(vapply(list(...), truthy, logical(1))))
  add("or", function(...) any(vapply(list(...), truthy, logical(1))))
  add("not", function(x) !truthy(x))
  add("&", function(...) all(vapply(list(...), truthy, logical(1))))
  add("&&", function(...) all(vapply(list(...), truthy, logical(1))))
  add("|", function(...) any(vapply(list(...), truthy, logical(1))))
  add("||", function(...) any(vapply(list(...), truthy, logical(1))))
  add("!", function(x) !truthy(x))

  # Persistent data structures.
  add("vector", ppl_vector)
  add("list", ppl_vector)
  add("c", ppl_vector)
  add("hash-map", p_hash_map)
  add("get", p_get)
  add("put", p_put)
  add("assoc", p_put)
  add("first", p_first)
  add("second", p_second)
  add("last", p_last)
  add("rest", p_rest)
  add("nth", p_nth)
  add("conj", p_conj)
  add("cons", p_cons)
  add("append", p_append)
  add("concat", p_concat)
  add("count", p_count)
  add("empty?", p_empty)
  add("peek", p_peek)
  add("range", p_range)
  add("vector?", function(x) inherits(x, "ppl_vector"))
  add("map?", function(x) inherits(x, "ppl_map"))
  add("number?", is_number)

  # Matrices.
  add("mat-mul", function(a, b) as_matrix_value(a) %*% as_matrix_value(b))
  add("mat-add", function(a, b) as_matrix_value(a) + as_matrix_value(b))
  add("mat-transpose", function(a) t(as_matrix_value(a)))
  add("mat-tanh", function(a) tanh(as_matrix_value(a)))
  add("mat-relu", function(a) pmax(as_matrix_value(a), 0))
  add("mat-repmat", function(a, r, c) {
    kronecker(
      matrix(1, nrow = as.integer(num(r)), ncol = as.integer(num(c))),
      as_matrix_value(a)
    )
  })

  # Distribution constructors.
  add("normal", normal_dist)
  add("log-normal", log_normal_dist)
  add("beta", beta_dist)
  add("gamma", gamma_dist)
  add("exponential", exponential_dist)
  add("uniform-continuous", uniform_dist)
  add("uniform", uniform_dist)
  add("poisson", poisson_dist)
  add("bernoulli", bernoulli_dist)
  add("flip", bernoulli_dist)
  add("discrete", discrete_dist)
  add("categorical", discrete_dist)
  add("uniform-discrete", uniform_discrete_dist)
  add("dirichlet", dirichlet_dist)

  lockEnvironment(p, bindings = TRUE)
  p
}
