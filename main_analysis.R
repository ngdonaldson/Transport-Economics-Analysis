rm(list = ls())

library(dplyr)

######## CHAPTER 4: CALIBRATION, ROBUSTNESS, AND SCENARIO ANALYSIS ############
# Numerical implementation of the welfare model
# described in Chapter 3 and calibrated in Chapter 4

############################
# 1. BASELINE CALIBRATION  #
############################

# Corridor market size
Y_T_base <- 27235
Y_M_base <- 25450
N_base   <- Y_T_base + Y_M_base

# Core parameter list
par <- list(
  # Demand
  N             = N_base,
  theta         = 0.001, #0.00015 to test less responsive case
  kappa_T       = 0,
  
  # Baseline shares
  share_T_base  = Y_T_base / N_base,
  share_M_base  = Y_M_base / N_base,
  
  # Baseline policy variables
  P_T_real      = 1000,
  P_M           = 1800,
  f_base        = 120,
  
  # Time values
  ivt           = 22,
  alpha_ivt     = 169,
  alpha_wait    = 331,
  gamma_M       = 206,
  t_M           = 18,
  
  # Fleet composition
  share_a       = 0.32,
  share_b       = 0.68,
  
  # Operating cost inputs
  days_month      = 22,
  operating_hours = 16,
  H               = 1,
  L_BRT           = 5,
  L_trunk         = 15,
  V               = 27,
  fee_a           = 0.0052,
  fee_b           = 0.0004,
  tariff_km_a     = 3458,
  tariff_km_b     = 3761,
  tariff_veh_a    = 23192949,
  tariff_veh_b    = 24885867,
  tariff_prov_a   = 19034219,
  tariff_prov_b   = 23697586,
  
  # Motorcycle externalities
  delta_acc       = 4.42,
  delta_air       = 0.32,
  delta_climate   = 1.27,
  delta_noise     = 7.40,
  delta_WTT       = 0.46,
  L_moto          = 5.2,
  euro_to_COP     = 4385,
  
  # Scenario C slackness
  Lambda          = 0.05
)

########################
# 2. CORE MODEL BLOCKS #
########################

# Binary logit demand
logit_YT <- function(gT, gM, par) {
  sT <- plogis(par$theta * (gM - gT) + par$kappa_T)
  par$N * sT
}

# Crowding multiplier
m_crowding <- function(YT, f, par) {
  phi <- YT / f
  da  <- (phi - 48) / 28
  db  <- (phi - 69) / 47.75
  d   <- par$share_a * pmax(da, 0) + par$share_b * pmax(db, 0)
  1 + 0.11 * d
}

pax_density <- function(YT, f, par) {
  (m_crowding(YT, f, par) - 1) / 0.11
}

# Generalized costs
gT_fun <- function(P_T, f, YT, par) {
  P_T + par$alpha_wait * (30 / f) + par$alpha_ivt * par$ivt * m_crowding(YT, f, par)
}

gM_fun <- function(par) {
  par$P_M + par$gamma_M * par$t_M
}

# Operating cost
K_f <- function(f, par) {
  2 * par$H * f * par$L_BRT
}

Q_t <- function(f, par) {
  cycle_time_h <- 2 * par$L_trunk / par$V
  f * cycle_time_h
}

C_f_fun <- function(f, par) {
  Kf <- K_f(f, par)
  Qt <- Q_t(f, par)
  
  denom <- par$days_month * par$operating_hours
  tau_veh_a  <- par$tariff_veh_a  / denom
  tau_veh_b  <- par$tariff_veh_b  / denom
  tau_prov_a <- par$tariff_prov_a / denom
  tau_prov_b <- par$tariff_prov_b / denom
  
  (1 + par$fee_a + par$fee_b) *
    (par$share_a * (par$tariff_km_a * Kf + (tau_veh_a + tau_prov_a) * Qt) +
       par$share_b * (par$tariff_km_b * Kf + (tau_veh_b + tau_prov_b) * Qt))
}

# Fixed-point demand solver
solve_YT <- function(P_T, f, par) {
  gM <- gM_fun(par)
  
  root_fun <- function(YT) {
    gT <- gT_fun(P_T, f, YT, par)
    YT - logit_YT(gT, gM, par)
  }
  
  uniroot(root_fun, lower = 0, upper = par$N)$root
}

###########################
# 3. WELFARE CALCULATION  #
###########################

CS_fun <- function(gT, gM, par) {
  th <- par$theta
  par$N * (1 / th) * log(exp(-th * gT) + exp(-th * gM))
}

MotoEx_fun <- function(YM, par) {
  delta <- (par$delta_acc + par$delta_air + par$delta_climate +
              par$delta_noise + par$delta_WTT) *
    0.01 * par$L_moto * par$euro_to_COP
  delta * YM
}

W_fun <- function(P_T, f, par) {
  YT <- solve_YT(P_T, f, par)
  YM <- par$N - YT
  gT <- gT_fun(P_T, f, YT, par)
  gM <- gM_fun(par)
  CS <- CS_fun(gT, gM, par)
  Pi <- P_T * YT - C_f_fun(f, par)
  ME <- MotoEx_fun(YM, par)
  
  CS + Pi - ME
}

gap_rev_cost <- function(P_T, f, par) {
  P_T * solve_YT(P_T, f, par) - C_f_fun(f, par)
}

###########################
# 4. ASC CALIBRATION      #
###########################

calibrate_kappa <- function(target_share, P_T, f, par) {
  f_k <- function(k) {
    par2 <- par
    par2$kappa_T <- k
    solve_YT(P_T, f, par2) / par2$N - target_share
  }
  uniroot(f_k, c(-20, 20))$root
}

# Baseline calibration for the main parameter set
par$kappa_T <- calibrate_kappa(
  target_share = par$share_T_base,
  P_T = par$P_T_real,
  f   = par$f_base,
  par = par
)

###############################
# 4A. CALIBRATION OUTPUTS     #
###############################

# Table 5: ASC for the less and more responsive baseline cases
theta_cases <- c(0.00015, 0.001)

asc_table <- bind_rows(lapply(theta_cases, function(th) {
  par_tmp <- par
  par_tmp$theta <- th
  
  kappa_star <- calibrate_kappa(
    target_share = par_tmp$share_T_base,
    P_T = par_tmp$P_T_real,
    f   = par_tmp$f_base,
    par = par_tmp
  )
  
  data.frame(
    theta = th,
    ASC_kappa = kappa_star
  )
}))

print(asc_table)

###############################
# 4B. ROBUSTNESS CHECKS       #
###############################

# Grids used in Section 4.3
P_grid   <- c(500, 1000, 1500)
f_grid   <- c(90, 120, 150)
P_M_grid <- c(1300, 1800, 2300)

make_par_theta <- function(theta_val, par_base) {
  par_tmp <- par_base
  par_tmp$theta <- theta_val
  par_tmp$kappa_T <- calibrate_kappa(
    target_share = par_tmp$share_T_base,
    P_T = par_tmp$P_T_real,
    f   = par_tmp$f_base,
    par = par_tmp
  )
  par_tmp
}

par_low  <- make_par_theta(0.00015, par)
par_high <- make_par_theta(0.001,   par)

# ---------- Tables 8 & 9: BRT share by fare and frequency ----------
make_share_table <- function(par_use, P_vals, f_vals) {
  out <- matrix(NA_real_,
                nrow = length(P_vals),
                ncol = length(f_vals),
                dimnames = list(
                  paste0("PT = ", P_vals),
                  paste0("f = ", f_vals)
                ))
  
  for (i in seq_along(P_vals)) {
    for (j in seq_along(f_vals)) {
      YT <- solve_YT(P_vals[i], f_vals[j], par_use)
      out[i, j] <- YT / par_use$N
    }
  }
  out
}

table8_share_low  <- round(make_share_table(par_low,  P_grid, f_grid), 4)
table9_share_high <- round(make_share_table(par_high, P_grid, f_grid), 4)

print(table8_share_low)
print(table9_share_high)

# ---------- Tables 10 & 11: passenger density by fare and frequency ----------
make_density_table <- function(par_use, P_vals, f_vals) {
  out <- matrix(NA_real_,
                nrow = length(P_vals),
                ncol = length(f_vals),
                dimnames = list(
                  paste0("PT = ", P_vals),
                  paste0("f = ", f_vals)
                ))
  
  for (i in seq_along(P_vals)) {
    for (j in seq_along(f_vals)) {
      YT <- solve_YT(P_vals[i], f_vals[j], par_use)
      out[i, j] <- pax_density(YT, f_vals[j], par_use)
    }
  }
  out
}

table10_density_low  <- round(make_density_table(par_low,  P_grid, f_grid), 3)
table11_density_high <- round(make_density_table(par_high, P_grid, f_grid), 3)

print(table10_density_low)
print(table11_density_high)

# ---------- Tables 12 & 13: changes under higher responsiveness ----------
table12_share_diff <- round(table9_share_high - table8_share_low, 4)
table13_density_diff <- round(table11_density_high - table10_density_low, 3)

print(table12_share_diff)
print(table13_density_diff)

# ---------- Tables 14 & 15: fleet composition ----------
fleet_cases <- list(
  list(label = "Baseline",             share_a = 0.32, share_b = 0.68),
  list(label = "100% Bi-Articulated",  share_a = 0.00, share_b = 1.00),
  list(label = "100% Articulated",     share_a = 1.00, share_b = 0.00)
)

table14_density_fleet <- sapply(fleet_cases, function(fc) {
  par_tmp <- par
  par_tmp$share_a <- fc$share_a
  par_tmp$share_b <- fc$share_b
  pax_density(Y_T_base, par$f_base, par_tmp)
})

names(table14_density_fleet) <- sapply(fleet_cases, function(fc) fc$label)
table14_density_fleet <- round(table14_density_fleet, 3)

print(table14_density_fleet)

table15_cost_fleet <- sapply(fleet_cases, function(fc) {
  par_tmp <- par
  par_tmp$share_a <- fc$share_a
  par_tmp$share_b <- fc$share_b
  C_f_fun(par$f_base, par_tmp)
})

names(table15_cost_fleet) <- sapply(fleet_cases, function(fc) fc$label)
table15_cost_fleet <- round(table15_cost_fleet, 2)

print(table15_cost_fleet)

# ---------- Tables 16 & 17: BRT share by motorcycle price and fare (f = f_base) ----------
# reported relative to the baseline cell (PT = 1000, PM = 1800)

make_share_PM_PT_table <- function(par_use, PM_vals, PT_vals, f_fixed) {
  out <- matrix(NA_real_,
                nrow = length(PM_vals),
                ncol = length(PT_vals),
                dimnames = list(
                  paste0("PM = ", PM_vals),
                  paste0("PT = ", PT_vals)
                ))
  
  for (i in seq_along(PM_vals)) {
    for (j in seq_along(PT_vals)) {
      par_tmp <- par_use
      par_tmp$P_M <- PM_vals[i]
      YT <- solve_YT(PT_vals[j], f_fixed, par_tmp)
      out[i, j] <- YT / par_tmp$N
    }
  }
  
  base_val <- out[which(PM_vals == 1800), which(PT_vals == 1000)]
  out - base_val
}

table16_share_PM_PT_low  <- round(make_share_PM_PT_table(par_low,  P_M_grid, P_grid, par$f_base), 4)
table17_share_PM_PT_high <- round(make_share_PM_PT_table(par_high, P_M_grid, P_grid, par$f_base), 4)

print(table16_share_PM_PT_low)
print(table17_share_PM_PT_high)

# ---------- Tables 18 & 19: passenger density by motorcycle price and fare (f = f_base) ----------
# also reported relative to the baseline cell (PT = 1000, PM = 1800)

make_density_PM_PT_table <- function(par_use, PM_vals, PT_vals, f_fixed) {
  out <- matrix(NA_real_,
                nrow = length(PM_vals),
                ncol = length(PT_vals),
                dimnames = list(
                  paste0("PM = ", PM_vals),
                  paste0("PT = ", PT_vals)
                ))
  
  for (i in seq_along(PM_vals)) {
    for (j in seq_along(PT_vals)) {
      par_tmp <- par_use
      par_tmp$P_M <- PM_vals[i]
      YT <- solve_YT(PT_vals[j], f_fixed, par_tmp)
      out[i, j] <- pax_density(YT, f_fixed, par_tmp)
    }
  }
  
  base_val <- out[which(PM_vals == 1800), which(PT_vals == 1000)]
  out - base_val
}

table18_density_PM_PT_low  <- round(make_density_PM_PT_table(par_low,  P_M_grid, P_grid, par$f_base), 3)
table19_density_PM_PT_high <- round(make_density_PM_PT_table(par_high, P_M_grid, P_grid, par$f_base), 3)

print(table18_density_PM_PT_low)
print(table19_density_PM_PT_high)

###########################
# 5. SCENARIO OPTIMIZERS  #
###########################

opt_fare <- function(f, par, lo = 0, hi = 10000) {
  optimize(function(P) -W_fun(P, f, par), interval = c(lo, hi))$minimum
}

opt_frequency <- function(P_T, par, lo = 1e-3, hi = 10000) {
  optimize(function(f) -W_fun(P_T, f, par), interval = c(lo, hi))$minimum
}

# ---------- Full cost recovery: fare case (B1) ----------
find_feasible_P <- function(f, par, lo = 0, hi = 10000, n = 2001) {
  P_grid <- seq(lo, hi, length.out = n)
  g_vals <- sapply(P_grid, function(P) gap_rev_cost(P, f, par))
  
  ok <- which(g_vals >= 0)
  if (length(ok) == 0) return(NULL)
  
  P_lo0 <- P_grid[min(ok)]
  P_hi0 <- P_grid[max(ok)]
  
  # refine lower boundary
  if (P_lo0 > lo) {
    i <- min(ok)
    P1 <- P_grid[i - 1]
    P2 <- P_grid[i]
    P_lo <- uniroot(function(P) gap_rev_cost(P, f, par), c(P1, P2))$root
  } else {
    P_lo <- lo
  }
  
  # refine upper boundary
  if (P_hi0 < hi) {
    i <- max(ok)
    P1 <- P_grid[i]
    P2 <- P_grid[i + 1]
    P_hi <- uniroot(function(P) gap_rev_cost(P, f, par), c(P1, P2))$root
  } else {
    P_hi <- hi
  }
  
  c(P_lo = P_lo, P_hi = P_hi)
}

opt_fare_CR <- function(f, par, lo = 0, hi = 10000) {
  int <- find_feasible_P(f, par, lo, hi)
  if (is.null(int)) return(NA_real_)
  optimize(function(P) -W_fun(P, f, par), interval = int)$minimum
}

# ---------- Full cost recovery: frequency case (B2) ----------
gap_rev_cost_f <- function(f, P_T, par) {
  P_T * solve_YT(P_T, f, par) - C_f_fun(f, par)
}

find_feasible_f <- function(P_T, par, lo = 1e-3, hi = 10000, n = 4001) {
  f_grid <- seq(lo, hi, length.out = n)
  g_vals <- sapply(f_grid, function(f) gap_rev_cost_f(f, P_T, par))
  
  ok <- which(g_vals >= 0)
  if (length(ok) == 0) return(NULL)
  
  f_lo0 <- f_grid[min(ok)]
  f_hi0 <- f_grid[max(ok)]
  
  # refine lower boundary
  if (f_lo0 > lo) {
    i <- min(ok)
    f1 <- f_grid[i - 1]
    f2 <- f_grid[i]
    f_lo <- uniroot(function(f) gap_rev_cost_f(f, P_T, par), c(f1, f2))$root
  } else {
    f_lo <- lo
  }
  
  # refine upper boundary
  if (f_hi0 < hi) {
    i <- max(ok)
    f1 <- f_grid[i]
    f2 <- f_grid[i + 1]
    f_hi <- uniroot(function(f) gap_rev_cost_f(f, P_T, par), c(f1, f2))$root
  } else {
    f_hi <- hi
  }
  
  c(f_lo = f_lo, f_hi = f_hi)
}

opt_frequency_CR <- function(P_T, par, lo = 1e-3, hi = 10000) {
  int <- find_feasible_f(P_T, par, lo, hi)
  if (is.null(int)) return(NA_real_)
  optimize(function(f) -W_fun(P_T, f, par), interval = int)$minimum
}

###########################
# 6. SCENARIOS A AND B    #
###########################

# A1: first-best fare
P_A1 <- opt_fare(par$f_base, par)

# B1: second-best fare under strict cost recovery
P_B1 <- opt_fare_CR(par$f_base, par)

# A2: first-best frequency
f_A2 <- opt_frequency(par$P_T_real, par)

# B2: second-best frequency under strict cost recovery
f_B2 <- opt_frequency_CR(par$P_T_real, par)

###########################
# 7. SCENARIO TABLES      #
###########################

scenario_row <- function(label, P_T, f, par) {
  YT <- solve_YT(P_T, f, par)
  data.frame(
    Scenario     = label,
    Fare         = P_T,
    f            = f,
    YT           = YT,
    ShareT       = YT / par$N,
    pax_density  = pax_density(YT, f, par),
    Gap_rev_cost = gap_rev_cost(P_T, f, par),
    Welfare      = W_fun(P_T, f, par)
  )
}

fare_table <- bind_rows(
  scenario_row("Baseline", par$P_T_real, par$f_base, par),
  scenario_row("A1: First-best fare", P_A1, par$f_base, par),
  scenario_row("B1: Second-best fare", P_B1, par$f_base, par)
)

freq_table <- bind_rows(
  scenario_row("Baseline", par$P_T_real, par$f_base, par),
  scenario_row("A2: First-best frequency", par$P_T_real, f_A2, par),
  scenario_row("B2: Second-best frequency", par$P_T_real, f_B2, par)
)

print(fare_table)
print(freq_table)

###########################
# 8. SCENARIO C: FET      #
###########################

Cf_base <- C_f_fun(par$f_base, par)

Fmax_from_slack <- function(Lambda, Cf_ref) {
  Lambda * Cf_ref
}

# ---------- Relaxed feasible interval: frequency case ----------
find_feasible_f_FET <- function(P_T, par, Lambda, lo = 1e-3, hi = 10000, n = 4001) {
  Fmax <- Fmax_from_slack(Lambda, Cf_base)
  
  f_grid <- seq(lo, hi, length.out = n)
  g_vals <- sapply(f_grid, function(f) gap_rev_cost(P_T, f, par) + Fmax)
  
  ok <- which(g_vals >= 0)
  if (length(ok) == 0) return(NULL)
  
  f_lo0 <- f_grid[min(ok)]
  f_hi0 <- f_grid[max(ok)]
  
  # refine lower boundary
  if (f_lo0 > lo) {
    i <- min(ok)
    f1 <- f_grid[i - 1]
    f2 <- f_grid[i]
    f_lo <- uniroot(function(f) gap_rev_cost(P_T, f, par) + Fmax, c(f1, f2))$root
  } else {
    f_lo <- lo
  }
  
  # refine upper boundary
  if (f_hi0 < hi) {
    i <- max(ok)
    f1 <- f_grid[i]
    f2 <- f_grid[i + 1]
    f_hi <- uniroot(function(f) gap_rev_cost(P_T, f, par) + Fmax, c(f1, f2))$root
  } else {
    f_hi <- hi
  }
  
  c(f_lo = f_lo, f_hi = f_hi)
}

opt_frequency_FET <- function(P_T, par, Lambda, lo = 1e-3, hi = 10000) {
  int <- find_feasible_f_FET(P_T, par, Lambda, lo, hi)
  if (is.null(int)) return(NA_real_)
  
  optimize(function(f) -W_fun(P_T, f, par), interval = int)$minimum
}

# ---------- Relaxed feasible interval: fare case ----------
find_feasible_P_FET <- function(f, par, Lambda, lo = 0, hi = 10000, n = 2001) {
  Fmax <- Fmax_from_slack(Lambda, Cf_base)
  
  P_grid <- seq(lo, hi, length.out = n)
  g_vals <- sapply(P_grid, function(P) gap_rev_cost(P, f, par) + Fmax)
  
  ok <- which(g_vals >= 0)
  if (length(ok) == 0) return(NULL)
  
  P_lo0 <- P_grid[min(ok)]
  P_hi0 <- P_grid[max(ok)]
  
  # refine lower boundary
  if (P_lo0 > lo) {
    i <- min(ok)
    P1 <- P_grid[i - 1]
    P2 <- P_grid[i]
    P_lo <- uniroot(function(P) gap_rev_cost(P, f, par) + Fmax, c(P1, P2))$root
  } else {
    P_lo <- lo
  }
  
  # refine upper boundary
  if (P_hi0 < hi) {
    i <- max(ok)
    P1 <- P_grid[i]
    P2 <- P_grid[i + 1]
    P_hi <- uniroot(function(P) gap_rev_cost(P, f, par) + Fmax, c(P1, P2))$root
  } else {
    P_hi <- hi
  }
  
  c(P_lo = P_lo, P_hi = P_hi)
}

opt_fare_FET <- function(f, par, Lambda, lo = 0, hi = 10000) {
  int <- find_feasible_P_FET(f, par, Lambda, lo, hi)
  if (is.null(int)) return(NA_real_)
  
  optimize(function(P) -W_fun(P, f, par), interval = int)$minimum
}

# Example sensitivity grid
slack_grid <- c(0, 0.01, 0.02, 0.05, 0.08, 0.10, 0.15, 0.20)

scenarioC_freq <- bind_rows(lapply(slack_grid, function(L) {
  f_star <- opt_frequency_FET(par$P_T_real, par, L)
  if (is.na(f_star)) return(NULL)
  
  row <- scenario_row("Scenario C (frequency)", par$P_T_real, f_star, par)
  row$Lambda <- L
  row$Fmax <- Fmax_from_slack(L, Cf_base)
  row$Deficit_used <- max(-row$Gap_rev_cost, 0)
  row
}))

scenarioC_fare <- bind_rows(lapply(slack_grid, function(L) {
  P_star <- opt_fare_FET(par$f_base, par, L)
  if (is.na(P_star)) return(NULL)
  
  row <- scenario_row("Scenario C (fare)", P_star, par$f_base, par)
  row$Lambda <- L
  row$Fmax <- Fmax_from_slack(L, Cf_base)
  row$Deficit_used <- max(-row$Gap_rev_cost, 0)
  row
}))

print(scenarioC_freq)
print(scenarioC_fare)
