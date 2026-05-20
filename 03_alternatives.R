# ==============================================================================
# PrOACT Step 3: ALTERNATIVES - Formulate the Optimization Problem
# ==============================================================================

formulateAlternatives <- function(x, model_type = "minimizeCosts", budget = NULL,
                                  blm = 0, blm_actions = 0, curve = 1, segments = 3,
                                  locked_in = NULL, locked_out = NULL) {
    cat("\n=== PrOACT Step 3: Formulating Alternatives ===\n")
    cat("Decision model:", model_type, "\n")
    cat("Spatial connectivity (BLM):", blm, "\n")
    cat("Action connectivity:", blm_actions, "\n")
    cat("Benefit-response curve:", curve, "(1=linear, 2=quadratic, 3=cubic)\n")

    if (curve > 1) {
        cat("Segments for linearization:", segments, "\n")
    }

    # Validate
    stopifnot(inherits(x, "PrOACT_Problem"))
    stopifnot(model_type %in% c("minimizeCosts", "maximizeBenefits"))
    stopifnot(curve %in% c(1, 2, 3))
    stopifnot(segments %in% c(1, 2, 3, 4, 5))

    if (model_type == "maximizeBenefits" && is.null(budget)) {
        stop("Budget required for maximizeBenefits model")
    }

    # Auto-derive locked_in/locked_out from pu$status if not explicitly provided
    # Marxan convention: status=1 -> locked in, status=2 -> locked out
    if (is.null(locked_in) && "status" %in% names(x$pu)) {
        from_status <- x$pu$id[x$pu$status == 1]
        if (length(from_status) > 0) {
            locked_in <- from_status
            cat("Locked-in from pu$status:", length(locked_in), "PUs\n")
        }
    }
    if (is.null(locked_out) && "status" %in% names(x$pu)) {
        from_status <- x$pu$id[x$pu$status == 2]
        if (length(from_status) > 0) {
            locked_out <- from_status
            cat("Locked-out from pu$status:", length(locked_out), "PUs\n")
        }
    }

    problem <- list(
        data = x,
        model_type = model_type,
        budget = budget,
        blm = blm,
        blm_actions = blm_actions,
        curve = curve,
        segments = segments,
        locked_in = locked_in,
        locked_out = locked_out
    )

    class(problem) <- "PrOACT_Alternatives"
    cat("Alternatives formulated successfully\n\n")

    return(problem)
}

print.PrOACT_Alternatives <- function(x, ...) {
    cat("PrOACT Alternatives (Optimization Problem)\n")
    cat("  Decision model:", x$model_type, "\n")
    cat("  Planning units:", x$data$n_pu, "\n")
    cat("  Features:", x$data$n_features, "\n")
    cat("  BLM:", x$blm, "\n")
    cat("  Curve:", x$curve, "\n")
}
