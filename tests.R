# Executable semantic and inference tests.

.this_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
.this_dir <- if (is.null(.this_file)) getwd() else dirname(normalizePath(.this_file))
source(file.path(.this_dir, "minippl.R"))
rm(.this_file, .this_dir)

value_of <- function(program, seed = 1) run_lw(program, seed = seed)$value

expect_error <- function(expr, pattern = NULL) {
  error <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  if (is.null(error)) stop("expected an error", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(error))) {
    stop(
      sprintf(
        "error did not match %s: %s",
        sQuote(pattern),
        conditionMessage(error)
      ),
      call. = FALSE
    )
  }
  invisible(error)
}

cat("syntax capture and closed source environment ... ")
.host_normal_was_forced <- FALSE
normal <- function(...) {
  .host_normal_was_forced <<- TRUE
  stop("host normal must not run")
}
captured <- ppl(sample(normal(0, 1)))
stopifnot(!.host_normal_was_forced, inherits(captured, "ppl_program"))
rm(normal, .host_normal_was_forced)
expect_error(ppl(sin(0)), "object 'sin' not found")
cat("ok\n")

cat("sequential let, shadowing, and lexical closures ... ")
sequential <- ppl({
  x <- 2
  y <- x + 3
  y
})
stopifnot(identical(value_of(sequential), 5))

shadowing <- ppl({
  x <- 10
  f <- function(y) x + y
  x <- 20
  f(1)
})
stopifnot(identical(value_of(shadowing), 11))

shift <- ppl({
  make_shift <- function(mu) function(x) x + mu
  f <- make_shift(10)
  f(3)
})
stopifnot(identical(value_of(shift), 13))
cat("ok\n")

cat("top-level recursion, mutual recursion, and trampoline ... ")
countdown <- ppl({
  down <- function(n) if (n == 0) 0 else down(n - 1)
  down(5000)
})
stopifnot(identical(value_of(countdown), 0))

mutual <- ppl({
  even <- function(n) if (n == 0) TRUE else odd(n - 1)
  odd <- function(n) if (n == 0) FALSE else even(n - 1)
  even(1001)
})
stopifnot(identical(value_of(mutual), FALSE))
cat("ok\n")

cat("strict left-to-right application on top of lazy R ... ")
strict <- ppl({
  constant <- function(x) 7
  constant(observe(2.3 ~ normal(0, 1)))
})
strict_run <- run_lw(strict, seed = 1)
stopifnot(
  identical(strict_run$value, 7),
  isTRUE(all.equal(strict_run$log_weight, dnorm(2.3, log = TRUE)))
)

strict_and <- ppl(FALSE && observe(0 ~ normal(0, 1)))
strict_and_run <- run_lw(strict_and, seed = 1)
stopifnot(
  identical(strict_and_run$value, FALSE),
  isTRUE(all.equal(strict_and_run$log_weight, dnorm(0, log = TRUE)))
)

lazy_branch <- ppl(if (TRUE) 1 else observe(100 ~ normal(0, 1)))
lazy_branch_run <- run_lw(lazy_branch, seed = 1)
stopifnot(identical(lazy_branch_run$value, 1), lazy_branch_run$log_weight == 0)

ordered <- ppl({
  ignore <- function(a, b) 0
  ignore(sample(bernoulli(0.5)), sample(bernoulli(0.5)))
})
first <- initial_step(ordered)
second <- first$k(FALSE)
stopifnot(
  first$tag == "sample",
  second$tag == "sample",
  grepl("arg:0", address_key(first$address), fixed = TRUE),
  grepl("arg:1", address_key(second$address), fixed = TRUE)
)
cat("ok\n")

cat("formula syntax and canonical effect syntax agree ... ")
formula_model <- ppl({
  mu ~ normal(0, 1)
  observe(2.3 ~ normal(mu, 1))
  mu
})
canonical_model <- ppl({
  mu <- sample(normal(0, 1))
  observe(normal(mu, 1), 2.3)
  mu
})
formula_run <- run_lw(formula_model, seed = 9)
canonical_run <- run_lw(canonical_model, seed = 9)
stopifnot(
  identical(formula_run$value, canonical_run$value),
  identical(formula_run$log_weight, canonical_run$log_weight)
)
cat("ok\n")

cat("sample continuations are reusable ... ")
coin <- ppl(sample(bernoulli(0.3)))
paused <- initial_step(coin)
false_branch <- paused$k(FALSE)
true_branch <- paused$k(TRUE)
stopifnot(
  paused$tag == "sample",
  false_branch$tag == "done",
  true_branch$tag == "done",
  identical(false_branch$value, FALSE),
  identical(true_branch$value, TRUE)
)
cat("ok\n")

cat("persistent source collections and zero-based indexing ... ")
collections <- ppl({
  v <- c(1, 2, 3)
  w <- put(v, 1, 9)
  m <- `hash-map`("answer", 42)
  c(nth(v, 1), nth(w, 1), get(m, "answer"))
})
collection_value <- vector_items(value_of(collections))
stopifnot(
  identical(collection_value[[1]], 2),
  identical(collection_value[[2]], 9),
  identical(collection_value[[3]], 42)
)
cat("ok\n")

cat("distribution families and matrix primitives ... ")
distributions <- ppl(c(
  normal(0, 1),
  `log-normal`(0, 1),
  beta(2, 3),
  gamma(2, 3),
  exponential(2),
  uniform(0, 1),
  poisson(3),
  bernoulli(0.4),
  discrete(c(0.2, 0.8)),
  `uniform-discrete`(2, 6),
  dirichlet(c(1, 2, 3))
))
distribution_values <- vector_items(value_of(distributions))
set.seed(10)
for (distribution in distribution_values) {
  x <- draw(distribution)
  lp <- log_prob(distribution, x)
  stopifnot(is_distribution(distribution), is.numeric(lp), length(lp) == 1L, !is.nan(lp))
}

matrix_program <- ppl(
  `mat-mul`(
    c(c(1, 2), c(3, 4)),
    c(c(1), c(1))
  )
)
matrix_value <- value_of(matrix_program)
stopifnot(
  is.matrix(matrix_value),
  isTRUE(all.equal(as.numeric(matrix_value), c(3, 7)))
)
cat("ok\n")

cat("compiler rejects syntax outside the source language ... ")
expect_error(ppl(normal(mu = 0, sigma = 1)), "named arguments")
expect_error(
  ppl({
    f <- function(x = 1) x
    f()
  }),
  "without defaults"
)
expect_error(ppl({ x ~ normal(0, 1) }), "following body")
expect_error(ppl(if (TRUE) 1), "then branch, and else branch")
cat("ok\n")

cat("exact Bernoulli enumeration ... ")
one_coin <- ppl({
  x ~ bernoulli(0.3)
  x
})
one_runs <- enumerate_traces(one_coin)
one_posterior <- posterior_table(one_runs)
stopifnot(
  length(one_runs) == 2L,
  abs(one_posterior$log_evidence) < 1e-12,
  isTRUE(all.equal(one_posterior$table$probability, c(0.7, 0.3)))
)

bits8 <- ppl({
  b1 ~ bernoulli(0.5)
  b2 ~ bernoulli(0.5)
  b3 ~ bernoulli(0.5)
  b4 ~ bernoulli(0.5)
  b5 ~ bernoulli(0.5)
  b6 ~ bernoulli(0.5)
  b7 ~ bernoulli(0.5)
  b8 ~ bernoulli(0.5)
  total <- b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8
  observe(total ~ normal(7, 1))
  total
})
bits_runs <- enumerate_traces(bits8)
bits_posterior <- posterior_table(bits_runs)
k <- 0:8
analytical_mass <- choose(8, k) * 2^-8 * dnorm(k, 7, 1)
analytical_probability <- analytical_mass / sum(analytical_mass)
stopifnot(
  length(bits_runs) == 256L,
  nrow(bits_posterior$table) == 9L,
  isTRUE(all.equal(
    bits_posterior$table$probability,
    analytical_probability,
    tolerance = 1e-12
  ))
)
cat("ok\n")

cat("LW, SMC, and single-site MH recover the conjugate posterior ... ")
lw <- likelihood_weighting(formula_model, N = 2000, seed = 22)
lw_mean <- sum(lw$values * lw$weights)
smc_draws <- smc(formula_model, N = 2000, seed = 23)
mh_draws <- single_site_mh(
  formula_model,
  steps = 4000,
  warmup = 1000,
  seed = 24
)
stopifnot(
  abs(lw_mean - 1.15) < 0.08,
  abs(mean(smc_draws) - 1.15) < 0.08,
  abs(mean(mh_draws) - 1.15) < 0.10,
  abs(sd(mh_draws) - sqrt(0.5)) < 0.08
)
cat("ok\n")

cat("\nAll MiniPPL tests passed.\n")
