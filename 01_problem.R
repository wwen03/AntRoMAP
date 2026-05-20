# ==============================================================================
# PrOACT Step 1: PROBLEM - Define the Decision Context
# ==============================================================================

defineProblem <- function(pu, features, dist_features, risks, dist_risks,
                          sensitivity, boundary = NULL) {
    cat("\n=== PrOACT Step 1: Defining the Problem ===\n")

    # Validate planning units
    stopifnot(
        "PU needs: id, monitoring_cost" =
            all(c("id", "monitoring_cost") %in% names(pu))
    )
    # Validate conservation features (values at risk)
    stopifnot("Features needs: id" = "id" %in% names(features))
    stopifnot(
        "dist_features needs: feature, pu, amount" =
            all(c("feature", "pu", "amount") %in% names(dist_features))
    )
    # Validate risks
    stopifnot(
        "Risks needs: id, action_cost" =
            all(c("id", "action_cost") %in% names(risks))
    )
    stopifnot(
        "dist_risks needs: risk, pu, action_cost" =
            all(c("risk", "pu", "action_cost") %in% names(dist_risks))
    )
    # Validate sensitivity (which features are sensitive to which risks)
    stopifnot(
        "Sensitivity needs: feature, risk" =
            all(c("feature", "risk") %in% names(sensitivity))
    )

    # Process targets (objectives for each feature)
    if ("target" %in% names(features)) {
        features$target_recovery <- features$target
        # Only default conservation to 0 if not already set by the caller
        if (!"target_conservation" %in% names(features)) {
            features$target_conservation <- 0
        }
    }
    if (!"target_recovery" %in% names(features)) {
        features$target_recovery <- 0
    }
    if (!"target_conservation" %in% names(features)) {
        features$target_conservation <- 0
    }

    # Add sensitivity parameters if missing (use defaults)
    if (!"delta1" %in% names(sensitivity)) sensitivity$delta1 <- 0
    if (!"delta2" %in% names(sensitivity)) sensitivity$delta2 <- 1
    if (!"delta3" %in% names(sensitivity)) sensitivity$delta3 <- 0
    if (!"delta4" %in% names(sensitivity)) sensitivity$delta4 <- 1

    # Create problem definition object
    data <- list(
        pu = pu,
        features = features,
        dist_features = dist_features,
        risks = risks,
        dist_risks = dist_risks,
        sensitivity = sensitivity,
        boundary = boundary,
        n_pu = nrow(pu),
        n_features = nrow(features),
        n_risks = nrow(risks),
        n_actions = nrow(dist_risks)
    )

    class(data) <- "PrOACT_Problem"

    cat("Planning units:", data$n_pu, "\n")
    cat("Conservation features:", data$n_features, "\n")
    cat("Risks:", data$n_risks, "\n")
    cat("Management actions:", data$n_actions, "\n")
    cat("Spatial connectivity:", !is.null(boundary), "\n\n")

    return(data)
}

print.PrOACT_Problem <- function(x, ...) {
    cat("PrOACT Problem Definition\n")
    cat("  Planning units:", x$n_pu, "\n")
    cat("  Conservation features:", x$n_features, "\n")
    cat("  Risks:", x$n_risks, "\n")
    cat("  Management actions:", x$n_actions, "\n")
    cat("  Spatial connectivity:", !is.null(x$boundary), "\n")
}
