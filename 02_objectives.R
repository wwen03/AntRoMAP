# ==============================================================================
# PrOACT Step 2: OBJECTIVES - Estimate Potential Benefit
# ==============================================================================

estimatePotentialBenefit <- function(data) {
    cat("\n=== PrOACT Step 2: Assessing Potential Benefit (Objectives) ===\n")

    potential <- data.frame(
        feature = numeric(),
        dist = numeric(),
        dist_threatened = numeric(),
        maximum.conservation.benefit = numeric(),
        maximum.recovery.benefit = numeric(),
        maximum.benefit = numeric()
    )

    for (f in 1:data$n_features) {
        feature_id <- data$features$id[f]
        feature_dists <- data$dist_features[data$dist_features$feature == feature_id, ]

        # Total distribution
        total_dist <- sum(feature_dists$amount)

        # Find which risks affect this feature
        risks_affecting_feature <- data$sensitivity$risk[data$sensitivity$feature == feature_id]

        # Find planning units with those risks
        pu_with_risks <- unique(data$dist_risks$pu[data$dist_risks$risk %in% risks_affecting_feature])

        # Calculate threatened vs. non-threatened amounts
        dist_threatened <- sum(feature_dists$amount[feature_dists$pu %in% pu_with_risks])
        dist_conservation <- total_dist - dist_threatened

        potential <- rbind(potential, data.frame(
            feature = feature_id,
            dist = total_dist,
            dist_threatened = dist_threatened,
            maximum.conservation.benefit = dist_conservation,
            maximum.recovery.benefit = dist_threatened,
            maximum.benefit = total_dist
        ))
    }

    return(potential)
}
