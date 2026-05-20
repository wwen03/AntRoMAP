# ==============================================================================
# PrOACT Step 5: TRADE-OFFS - Extract and Compare Results
# ==============================================================================

getActions <- function(solution, format = "long") {
    if (solution$status == "INFEASIBLE") {
        cat("Cannot extract actions: model is infeasible\n")
        return(data.frame())
    }
    x_sol <- solution$solution[solution$model$var_idx$x]
    actions <- solution$problem$data$dist_risks
    actions$selected <- x_sol > 0.5
    actions$value <- x_sol

    result <- actions[actions$selected, ]

    if (format == "wide") {
        # Pivot to wide format - include all planning units
        all_pu <- solution$problem$data$pu$id
        wide <- data.frame(pu = all_pu)

        # Add column for each risk/action (0 or 1)
        for (r in unique(solution$problem$data$risks$id)) {
            risk_pu <- result$pu[result$risk == r & result$selected]
            wide[[as.character(r)]] <- as.integer(wide$pu %in% risk_pu)
        }

        # Get selected planning units
        w_sol <- solution$solution[solution$model$var_idx$w]
        selected_pu <- solution$problem$data$pu$id[w_sol > 0.5]
        action_pu <- unique(result$pu)

        # Identify non-threatened feature-PU pairs (matching prioriactions logic):
        # a distribution entry is non-threatened when that feature has no
        # co-occurring risk in that PU.
        df   <- solution$problem$data$dist_features
        sens <- solution$problem$data$sensitivity
        dr   <- solution$problem$data$dist_risks
        feat_risks_map <- tapply(sens$risk, sens$feature, c, simplify = FALSE)
        pu_risks_map   <- tapply(dr$risk,   dr$pu,        c, simplify = FALSE)
        is_threatened  <- mapply(function(feat, pu_id) {
            f_risks <- feat_risks_map[[as.character(feat)]]
            if (is.null(f_risks) || length(f_risks) == 0) return(FALSE)
            p_risks <- pu_risks_map[[as.character(pu_id)]]
            if (is.null(p_risks)) return(FALSE)
            any(f_risks %in% p_risks)
        }, df$feature, df$pu)

        # Conservation: selected PU that contains ANY non-threatened feature
        # distribution — can coexist with threat actions in the same PU
        pus_with_nonthreat <- unique(df$pu[!is_threatened])
        conservation_pu    <- intersect(selected_pu, pus_with_nonthreat)

        # Connectivity: selected PU with neither conservation nor threat actions
        connectivity_pu <- setdiff(selected_pu, union(conservation_pu, action_pu))

        wide$conservation <- as.integer(wide$pu %in% conservation_pu)
        wide$connectivity <- as.integer(wide$pu %in% connectivity_pu)

        return(wide)
    }

    return(result)
}

getCost <- function(solution) {
    if (solution$status == "INFEASIBLE") {
        cat("Cannot calculate cost: model is infeasible\n")
        return(list(monitoring = NA, actions = NA, total = NA))
    }
    data <- solution$problem$data
    w_sol <- solution$solution[solution$model$var_idx$w]
    x_sol <- solution$solution[solution$model$var_idx$x]

    monitoring <- sum(data$pu$monitoring_cost * w_sol)
    actions <- sum(data$dist_risks$action_cost * x_sol)

    return(list(
        monitoring = monitoring,
        actions = actions,
        total = monitoring + actions
    ))
}

getBenefit <- function(solution) {
    if (solution$status == "INFEASIBLE") {
        cat("Cannot calculate benefits: model is infeasible\n")
        return(data.frame())
    }
    data <- solution$problem$data
    b_sol <- solution$solution[solution$model$var_idx$b]

    benefits <- data.frame(
        feature = numeric(),
        benefit = numeric(),
        target_recovery = numeric(),
        target_conservation = numeric(),
        met = logical()
    )

    for (f in 1:data$n_features) {
        feature_id <- data$features$id[f]
        feature_dists <- which(data$dist_features$feature == feature_id)

        total_benefit <- sum(data$dist_features$amount[feature_dists] * b_sol[feature_dists])
        target_rec <- data$features$target_recovery[f]
        target_con <- data$features$target_conservation[f]
        target_total <- target_rec + target_con

        benefits <- rbind(benefits, data.frame(
            feature = feature_id,
            benefit = total_benefit,
            target_recovery = target_rec,
            target_conservation = target_con,
            met = total_benefit >= target_total
        ))
    }

    return(benefits)
}

getPerformance <- function(solution) {
    if (solution$status == "INFEASIBLE") {
        cat("Cannot calculate performance: model is infeasible\n")
        return(list(
            n_units = NA,
            n_actions = NA,
            cost = NA,
            n_features = NA,
            n_targets_met = NA,
            pct_targets_met = NA
        ))
    }
    w_sol <- solution$solution[solution$model$var_idx$w]
    x_sol <- solution$solution[solution$model$var_idx$x]

    costs <- getCost(solution)
    benefits <- getBenefit(solution)

    list(
        n_units = sum(w_sol > 0.5),
        n_actions = sum(x_sol > 0.5),
        cost = costs$total,
        n_features = nrow(benefits),
        n_targets_met = sum(benefits$met),
        pct_targets_met = mean(benefits$met) * 100
    )
}

getConnectivityPenalty <- function(solution) {
    if (is.null(solution$problem$data$boundary)) {
        return(0)
    }

    w_sol <- solution$solution[solution$model$var_idx$w]
    boundary <- solution$problem$data$boundary
    pu_ids <- solution$problem$data$pu$id

    penalty <- 0
    for (i in 1:nrow(boundary)) {
        pu1_i <- which(pu_ids == boundary$id1[i])
        pu2_i <- which(pu_ids == boundary$id2[i])

        if (xor(w_sol[pu1_i] > 0.5, w_sol[pu2_i] > 0.5)) {
            penalty <- penalty + boundary$boundary[i]
        }
    }

    return(penalty * solution$problem$blm)
}

getModelInfo <- function(solution) {
    list(
        model_type = solution$problem$model_type,
        n_variables = solution$model$n_vars,
        n_constraints = length(solution$model$row_lb),
        blm = solution$problem$blm,
        curve = solution$problem$curve,
        solver = solution$solver,
        status = solution$status
    )
}
