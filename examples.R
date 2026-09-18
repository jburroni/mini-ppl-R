# Examples for the native-syntax R MiniPPL.

.this_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
.this_dir <- if (is.null(.this_file)) getwd() else dirname(normalizePath(.this_file))
source(file.path(.this_dir, "minippl.R"))
rm(.this_file, .this_dir)

cat("\n1. Lexical closure\n")
shift <- ppl({
  make_shift <- function(mu) function(x) x + mu
  f <- make_shift(10)
  f(3)
})
cat("   (make_shift(10))(3) =", run_lw(shift, seed = 1)$value, "\n")

cat("\n2. Recursive probabilistic function\n")
geometric <- ppl({
  geom <- function() {
    if (sample(bernoulli(0.3))) 0 else 1 + geom()
  }
  geom()
})
set.seed(2)
geometric_draws <- vapply(
  seq_len(2000),
  function(i) run_lw(geometric)$value,
  numeric(1)
)
cat("   empirical mean =", round(mean(geometric_draws), 3),
    "; exact mean =", round(0.7 / 0.3, 3), "\n")

cat("\n3. One model, three Monte Carlo controllers\n")
conjugate <- ppl({
  mu ~ normal(0, 1)
  observe(2.3 ~ normal(mu, 1))
  mu
})

lw <- likelihood_weighting(conjugate, N = 2000, seed = 22)
lw_mean <- sum(lw$values * lw$weights)
smc_draws <- smc(conjugate, N = 2000, seed = 23)
mh_draws <- single_site_mh(
  conjugate,
  steps = 4000,
  warmup = 1000,
  seed = 24
)
cat("   LW mean   =", round(lw_mean, 3), "\n")
cat("   SMC mean  =", round(mean(smc_draws), 3), "\n")
cat("   SSMH mean =", round(mean(mh_draws), 3), "\n")
cat("   exact     = 1.150\n")

cat("\n4. Exact enumeration of eight Bernoulli sites\n")
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

runs <- enumerate_traces(bits8)
posterior <- posterior_table(runs)
print(posterior$table, row.names = FALSE)
cat("   complete traces =", length(runs), "\n")
cat("   log evidence    =", posterior$log_evidence, "\n")
