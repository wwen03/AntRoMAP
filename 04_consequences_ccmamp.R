# ==============================================================================
# 04_consequences_ccmamp.R  —  CC-MAMP: True Chance-Constrained MAMP
#
# Implements the formulation from CC-MAMP_summary.md:
#   binary z^s per SCENARIO (not per feature as in 04_consequences_cc.R)
#
# COMPARISON:
#   04_consequences_cc.R   → M_f binary per FEATURE  → "how many targets can we drop?"
#   04_consequences_ccmamp → z^s binary per SCENARIO → "how many scenarios must we satisfy?"
#
# FORMULATION:
#   Shared variables (one set, used across all scenarios):
#     w_i  ∈ {0,1}  — monitor planning unit i
#     x_ia ∈ {0,1}  — apply action a in unit i
#
#   Per-scenario variables (one set per scenario s):
#     b^s_d ∈ [0,1]  — benefit of distribution d under scenario s
#     z^s   ∈ {0,1}  — 1 if ALL species targets are met in scenario s
#
#   Objective:
#     min  ∑_i(m_i × w_i)  +  ∑_ia(c_ia × x_ia)
#     [cost is shared — does not vary across scenarios]
#
#   Constraint A — Activation (shared, once):
#     x_ia ≤ w_i     ∀ i, a
#
#   Constraint B — Benefit calculation per scenario s:
#     Conservation:  b^s_d = w_pu          [Type 1: no threats]
#     Recovery:      b^s_d ≤ (δ2/K) × x_a  [Type 2: threat mitigated by action]
#
#   Constraint C — Target with BigM relaxation, per scenario s, per species f:
#     ∑_d (amount_d × b^s_d)  −  BigM × z^s  ≥  T_f − BigM
#     → if z^s = 1: ∑ amounts × b ≥ T_f  (target enforced)
#     → if z^s = 0: ∑ amounts × b ≥ T_f − BigM ≤ 0  (auto-satisfied)
#
#   Constraint D — Chance constraint (one, global):
#     ∑_s z^s  ≥  ⌈α × S⌉
#     → at least α fraction of scenarios must have ALL targets met
#
# SCENARIOS for Windmill Islands (two options):
#   Option A — SAA risk multipliers (used here):
#     K scenarios from Beta(3,3) on [0.5, 1.5], scaling dist_risks amounts
#   Option B — ProACT Models B–E as scenarios:
#     Sensitivity matrices from each model = different threat weightings
#
# VARIABLE LAYOUT:
#   [w: 1..n_pu]  [x: n_pu+1..n_pu+n_act]
#   [b^1: ..]  [b^2: ..]  ...  [b^S: ..]    (n_benefit_vars each)
#   [z^1 .. z^S]                              (S binary variables)
#   Total: n_pu + n_act + S×n_bvars + S
# ==============================================================================

# ==============================================================================
# STEP 1: Generate SAA scenarios from Beta(3,3) risk multipliers
# (Same distribution as the SAA loop in proact_windmill_islands_main.R)
# ==============================================================================

generate_saa_scenarios <- function(data,
                                   K           = 9,
                                   shape1      = 3,
                                   shape2      = 3,
                                   mult_lo     = 0.5,
                                   mult_hi     = 1.5,
                                   risk_threshold = 0.1) {
    quantiles    <- seq(1 / (K + 1), K / (K + 1), length.out = K)
    beta_q       <- qbeta(quantiles, shape1 = shape1, shape2 = shape2)
    multipliers  <- mult_lo + beta_q * (mult_hi - mult_lo)

    cat("Generating", K, "SAA scenarios\n")
    cat("Distribution: Beta(", shape1, ",", shape2, ") on [",
        mult_lo, ",", mult_hi, "]\n")
    cat("Multipliers:", paste(round(multipliers, 3), collapse = ", "), "\n\n")

    lapply(seq_len(K), function(k) {
        dr <- data$dist_risks
        dr$amount <- pmin(pmax(dr$amount * multipliers[k], 0), 1)
        dr[!is.na(dr$amount) & dr$amount > risk_threshold, ]
    })
}

# ==============================================================================
# STEP 2: Build the CC-MAMP MIP
# ==============================================================================

build_ccmamp_model <- function(problem,
                                scenario_dr_list,
                                cc_alpha,
                                risk_threshold = 0.1,
                                verbose        = TRUE) {

    cat("=== Building CC-MAMP Model ===\n")

    data           <- problem$data
    n_pu           <- data$n_pu
    n_features     <- data$n_features
    n_actions      <- nrow(data$dist_risks)   # base action space
    n_benefit_vars <- nrow(data$dist_features)
    S              <- length(scenario_dr_list)

    # --- Variable layout ---
    w_idx    <- seq_len(n_pu)
    x_idx    <- n_pu + seq_len(n_actions)
    b_offset <- n_pu + n_actions              # b^s starts at b_offset + (s-1)*n_bvars + 1
    z_idx    <- n_pu + n_actions + S * n_benefit_vars + seq_len(S)
    n_vars   <- n_pu + n_actions + S * n_benefit_vars + S

    cat("Variable layout:\n")
    cat("  w (monitoring):          ", n_pu, "\n")
    cat("  x (actions):             ", n_actions, "\n")
    cat("  b (benefits ×", S, "scen):", S * n_benefit_vars, "\n")
    cat("  z (scenario success):    ", S, "\n")
    cat("  Total variables:         ", n_vars, "\n\n")

    # Objective: shared cost only (b^s and z^s have zero cost)
    obj <- c(
        data$pu$monitoring_cost,
        data$dist_risks$action_cost,
        rep(0.0, S * n_benefit_vars),
        rep(0.0, S)
    )

    # BigM: must be large enough to relax any target when z^s = 0
    all_targets <- c(data$features$target_recovery,
                     data$features$target_conservation)
    BigM <- max(all_targets, na.rm = TRUE) * 2.0
    cat("BigM =", round(BigM, 4), "\n\n")

    # Static lookups (shared across scenarios)
    pu_lookup      <- match(data$dist_risks$pu, data$pu$id)
    pu_lookup_feat <- match(data$dist_features$pu, data$pu$id)

    df_d <- data.frame(
        d       = seq_len(n_benefit_vars),
        pu      = data$dist_features$pu,
        feature = data$dist_features$feature,
        amount  = data$dist_features$amount,
        stringsAsFactors = FALSE
    )

    # Base action key (maps (pu, risk) → index in x vector)
    base_action_key <- paste(data$dist_risks$pu, data$dist_risks$risk, sep = ":")

    # --- Sparse matrix accumulators ---
    all_i <- integer(0); all_j <- integer(0); all_x_vals <- numeric(0)
    sense_vec <- character(0); rhs_vec <- numeric(0)
    constraint_count <- 0L

    # =========================================================================
    # CONSTRAINT A: ACTIVATION — x_ia ≤ w_i   (shared, built once)
    # =========================================================================
    rows_act <- rep(seq_len(n_actions), each = 2)
    cols_act <- c(rbind(x_idx, pu_lookup))
    vals_act <- rep(c(1, -1), n_actions)
    all_i     <- c(all_i,     constraint_count + rows_act)
    all_j     <- c(all_j,     cols_act)
    all_x_vals <- c(all_x_vals, vals_act)
    sense_vec <- c(sense_vec, rep("<=", n_actions))
    rhs_vec   <- c(rhs_vec,   rep(0.0,  n_actions))
    constraint_count <- constraint_count + n_actions

    cat("Built activation constraints:", n_actions, "\n")

    # =========================================================================
    # CONSTRAINTS B + C: PER SCENARIO LOOP
    # Each scenario s gets its own:
    #   B — benefit calculation (conservation + recovery)
    #   C — target constraints with BigM z^s relaxation
    # =========================================================================
    for (s in seq_len(S)) {

        if (verbose) cat(sprintf("  Building scenario %d / %d ...\n", s, S))

        # Benefit variable indices for scenario s
        b_s_idx <- b_offset + (s - 1) * n_benefit_vars + seq_len(n_benefit_vars)

        # z^s column index
        z_s_col <- z_idx[s]

        # ----- Scenario-specific action lookup --------------------------------
        # Filter this scenario's dist_risks above threshold
        dr_s <- scenario_dr_list[[s]]

        # Map scenario action keys → base action indices (shared x vector)
        action_key_s <- paste(dr_s$pu, dr_s$risk, sep = ":")
        base_positions <- match(action_key_s, base_action_key)
        valid_mask <- !is.na(base_positions)
        action_lookup_s <- setNames(base_positions[valid_mask],
                                    action_key_s[valid_mask])

        # Relevant (d, action) pairs for scenario s
        relevant_s <- merge(df_d,
            data$sensitivity[, c("feature", "risk"), drop = FALSE],
            by = "feature", all.x = FALSE)
        relevant_s$action_i <- action_lookup_s[
            paste(relevant_s$pu, relevant_s$risk, sep = ":")]
        relevant_s <- relevant_s[!is.na(relevant_s$action_i), ]

        d_has_risks_s <- sort(unique(relevant_s$d))
        d_no_risks_s  <- setdiff(seq_len(n_benefit_vars), d_has_risks_s)

        # Add delta coefficients
        relevant_s <- merge(relevant_s,
            data$sensitivity[, c("feature", "risk", "delta1", "delta2")],
            by = c("feature", "risk"), all.x = TRUE)
        relevant_s$delta1[is.na(relevant_s$delta1)] <- 0
        relevant_s$delta2[is.na(relevant_s$delta2)] <- 1

        # n_rel: number of relevant risks per distribution d (for normalisation)
        n_rel_per_d  <- tabulate(match(relevant_s$d, d_has_risks_s))
        n_rel_lookup <- n_rel_per_d[match(relevant_s$d, d_has_risks_s)]

        # --- Constraint B1: Conservation benefit = monitoring -----------------
        # b^s_d - w_pu = 0  for d in d_no_risks_s
        n_ben1 <- length(d_no_risks_s)
        if (n_ben1 > 0) {
            row_off1 <- constraint_count + seq_len(n_ben1)
            rows_b1  <- rep(row_off1, each = 2)
            cols_b1  <- c(rbind(b_s_idx[d_no_risks_s],
                                pu_lookup_feat[d_no_risks_s]))
            vals_b1  <- rep(c(1, -1), n_ben1)
            all_i     <- c(all_i, rows_b1)
            all_j     <- c(all_j, cols_b1)
            all_x_vals <- c(all_x_vals, vals_b1)
            sense_vec <- c(sense_vec, rep("==", n_ben1))
            rhs_vec   <- c(rhs_vec,   rep(0.0,  n_ben1))
            constraint_count <- constraint_count + n_ben1
        }

        # --- Constraint B2: Recovery benefit ≤ action contributions ----------
        # b^s_d ≤ (δ2/n_rel) × x_action  →  b^s_d - (δ2/n_rel) × x_a ≤ 0
        n_ben2 <- length(d_has_risks_s)
        if (n_ben2 > 0) {
            d_row_map_s <- setNames(constraint_count + seq_len(n_ben2),
                                    as.character(d_has_risks_s))
            constraint_count <- constraint_count + n_ben2

            all_i     <- c(all_i,
                           unname(d_row_map_s[as.character(d_has_risks_s)]),
                           unname(d_row_map_s[as.character(relevant_s$d)]))
            all_j     <- c(all_j,
                           b_s_idx[d_has_risks_s],
                           x_idx[relevant_s$action_i])
            all_x_vals <- c(all_x_vals,
                            rep(1.0, n_ben2),
                            -(relevant_s$delta2 / n_rel_lookup))
            sense_vec <- c(sense_vec, rep("<=", n_ben2))
            rhs_vec   <- c(rhs_vec,   rep(0.0,  n_ben2))
        }

        # --- Constraint C: Target constraints with BigM z^s relaxation --------
        # ∑_d(amount_d × b^s_d)  −  BigM × z^s  ≥  T_f − BigM
        # Rearranged for standard form (coefficients on LHS, rhs on RHS)
        for (f in seq_len(n_features)) {
            feature_id <- data$features$id[f]
            target_rec <- data$features$target_recovery[f]
            target_con <- data$features$target_conservation[f]
            all_dists  <- which(data$dist_features$feature == feature_id)

            # Recovery target
            if (target_rec > 0) {
                rec_dists <- intersect(all_dists, d_has_risks_s)
                if (length(rec_dists) > 0) {
                    constraint_count <- constraint_count + 1
                    all_i     <- c(all_i,
                                   rep(constraint_count, length(rec_dists)),
                                   constraint_count)
                    all_j     <- c(all_j,
                                   b_s_idx[rec_dists],
                                   z_s_col)
                    all_x_vals <- c(all_x_vals,
                                   df_d$amount[rec_dists],
                                   -BigM)          # − BigM × z^s on LHS
                    sense_vec <- c(sense_vec, ">=")
                    rhs_vec   <- c(rhs_vec, target_rec - BigM)  # T_f − BigM
                }
            }

            # Conservation target
            if (target_con > 0) {
                con_dists <- intersect(all_dists, d_no_risks_s)
                if (length(con_dists) > 0) {
                    constraint_count <- constraint_count + 1
                    all_i     <- c(all_i,
                                   rep(constraint_count, length(con_dists)),
                                   constraint_count)
                    all_j     <- c(all_j,
                                   b_s_idx[con_dists],
                                   z_s_col)
                    all_x_vals <- c(all_x_vals,
                                   df_d$amount[con_dists],
                                   -BigM)
                    sense_vec <- c(sense_vec, ">=")
                    rhs_vec   <- c(rhs_vec, target_con - BigM)
                }
            }
        }
    } # end scenario loop

    # =========================================================================
    # CONSTRAINT D: CHANCE CONSTRAINT (one global constraint)
    # ∑_s z^s ≥ ⌈α × S⌉
    # =========================================================================
    min_successes <- ceiling(cc_alpha * S)
    constraint_count <- constraint_count + 1
    all_i      <- c(all_i,      rep(constraint_count, S))
    all_j      <- c(all_j,      z_idx)
    all_x_vals <- c(all_x_vals, rep(1.0, S))
    sense_vec  <- c(sense_vec, ">=")
    rhs_vec    <- c(rhs_vec,   min_successes)

    cat("Total constraints:", constraint_count, "\n")
    cat("  Chance constraint requires", min_successes, "of", S, "scenarios\n\n")

    # Assemble sparse matrix
    A <- Matrix::sparseMatrix(
        i = all_i, j = all_j, x = all_x_vals,
        dims = c(constraint_count, n_vars)
    )

    row_lb <- ifelse(sense_vec == "<=", -Inf,
                ifelse(sense_vec == ">=", rhs_vec, rhs_vec))
    row_ub <- ifelse(sense_vec == ">=",  Inf,
                ifelse(sense_vec == "<=", rhs_vec, rhs_vec))

    list(
        obj           = obj,
        A             = A,
        row_lb        = row_lb,
        row_ub        = row_ub,
        n_vars        = n_vars,
        w_idx         = w_idx,
        x_idx         = x_idx,
        b_offset      = b_offset,
        z_idx         = z_idx,
        n_benefit_vars = n_benefit_vars,
        S             = S,
        BigM          = BigM,
        min_successes = min_successes,
        is_maximize   = FALSE
    )
}

# ==============================================================================
# STEP 3: Solve the CC-MAMP model
# ==============================================================================

solveCCMAMP <- function(problem,
                         scenario_dr_list,
                         cc_alpha       = 0.90,
                         risk_threshold = 0.1,
                         solver         = "gurobi",
                         gap            = 0.1,
                         time_limit     = 600,
                         cores          = 2,
                         verbose        = TRUE) {

    S <- length(scenario_dr_list)
    cat("\n=== CC-MAMP: Chance-Constrained Multi-Action Management Planning ===\n")
    cat("Scenarios S:", S, "\n")
    cat("Confidence level alpha:", cc_alpha, "\n")
    cat("Minimum scenarios to satisfy:", ceiling(cc_alpha * S), "of", S, "\n\n")

    model <- build_ccmamp_model(problem, scenario_dr_list, cc_alpha,
                                 risk_threshold, verbose)

    # Variable types:
    #   w, x → binary; b^s → continuous [0,1]; z^s → binary
    is_integer <- c(
        rep(TRUE,  length(model$w_idx) + length(model$x_idx)),  # w + x
        rep(FALSE, S * model$n_benefit_vars),                    # b^s
        rep(TRUE,  S)                                            # z^s
    )

    # --- Solve with Gurobi ---
    if (solver != "gurobi") stop("Only 'gurobi' solver supported for CC-MAMP.")

    gurobi_model <- list(
        obj        = model$obj,
        A          = model$A,
        sense      = ifelse(model$row_lb == model$row_ub, "=",
                        ifelse(model$row_lb == -Inf, "<", ">")),
        rhs        = ifelse(model$row_lb == model$row_ub, model$row_lb,
                        ifelse(model$row_lb == -Inf, model$row_ub, model$row_lb)),
        vtype      = ifelse(is_integer, "B", "C"),
        lb         = rep(0, model$n_vars),
        ub         = rep(1, model$n_vars),
        modelsense = "min"
    )

    params <- list(
        MIPGap      = gap,
        Threads     = cores,
        OutputFlag  = ifelse(verbose, 1, 0)
    )
    if (!is.null(time_limit)) params$TimeLimit <- time_limit

    result  <- gurobi::gurobi(gurobi_model, params)
    status  <- result$status
    obj_val <- if (!is.null(result$objval)) result$objval else NA_real_
    sol_vec <- if (!is.null(result$x)) result$x else rep(0, model$n_vars)

    # --- Report z^s outcomes ---
    z_vals <- round(sol_vec[model$z_idx])
    cat("\n--- Scenario outcomes (z^s) ---\n")
    for (s in seq_len(S)) {
        cat(sprintf("  Scenario %2d: %s\n", s,
                    ifelse(z_vals[s] == 1L, "SATISFIED (all targets met)",
                           "failed     (allowed to fail)")))
    }
    cat(sprintf("\nSatisfied: %d / %d  (required >= %d, alpha = %.2f)\n",
                sum(z_vals), S, model$min_successes, cc_alpha))
    cat("Status:", status, "\n")
    if (!is.na(obj_val))
        cat("Total cost:", round(obj_val, 2), "\n")

    solution <- list(
        problem         = problem,
        model           = model,
        status          = status,
        objective_value = obj_val,
        solution        = sol_vec,
        solver          = solver,
        gap             = gap,
        cc_alpha        = cc_alpha,
        S               = S,
        z_vals          = z_vals
    )
    class(solution) <- "PrOACT_CCMAMP"
    invisible(solution)
}

# ==============================================================================
# STEP 4: Extract actions (compatible with existing getActions())
# Uses only the shared w/x variables — same format as PrOACT_Consequences
# ==============================================================================

getActions_CCMAMP <- function(sol, format = "wide") {
    model   <- sol$model
    problem <- sol$problem
    vec     <- sol$solution

    w_vals <- round(vec[model$w_idx])
    x_vals <- round(vec[model$x_idx])

    acts <- data.frame(
        pu      = problem$data$pu$id,
        monitor = w_vals
    )

    risk_ids <- problem$data$dist_risks$risk
    pu_ids   <- problem$data$dist_risks$pu
    for (r in unique(risk_ids)) {
        col_name <- as.character(r)
        acts[[col_name]] <- 0L
        idx <- which(risk_ids == r)
        pu_match <- match(pu_ids[idx], acts$pu)
        acts[[col_name]][pu_match] <- x_vals[idx]
    }
    acts
}

print.PrOACT_CCMAMP <- function(x, ...) {
    cat("CC-MAMP Solution\n")
    cat("  Status:", x$status, "\n")
    if (!is.na(x$objective_value))
        cat("  Cost:", round(x$objective_value, 2), "\n")
    cat("  Scenarios satisfied:", sum(x$z_vals), "/", x$S,
        "  (alpha =", x$cc_alpha, ")\n")
    cat("  Solver:", x$solver, "\n")
}

# ==============================================================================
# USAGE EXAMPLE (run after proact_windmill_islands_main.R has loaded data)
# ==============================================================================
#
#   source("analysis/scripts/helpers/04_consequences_ccmamp.R")
#
#   # Generate 9 SAA scenarios (same as existing SAA loop)
#   saa_scenarios <- generate_saa_scenarios(data_multi, K = 9)
#
#   # Solve CC-MAMP: at least 90% of scenarios must have ALL targets met
#   sol_ccmamp <- solveCCMAMP(
#       problem          = altC,
#       scenario_dr_list = saa_scenarios,
#       cc_alpha         = 0.90,
#       solver           = "gurobi",
#       gap              = 0.1,
#       time_limit       = 600,
#       cores            = 2
#   )
#
#   # Compare costs
#   cat("Model C (deterministic):", getCost(conC)$total, "\n")
#   cat("CC-MAMP (alpha=0.90):   ", sol_ccmamp$objective_value, "\n")
#
#   # See which scenarios succeeded
#   print(sol_ccmamp$z_vals)
#
#   # Extract actions (compatible format)
#   actions_ccmamp <- getActions_CCMAMP(sol_ccmamp)
