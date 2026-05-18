test_that("hybrid_score multiplicative formula matches EF^w1 * Stab^w2", {
    edges <- data.frame(
        source = c("A", "B", "C"),
        target = c("B", "C", "D"),
        EdgeFrequency = c(0.8, 0.5, 0.9),
        Stability.dcor.scaled = c(0.7, 0.3, 0.85)
    )

    scored <- hybrid_score(edges,
        w1 = 0.3, w2 = 0.7,
        score_type = "multiplicative"
    )

    expect_true("HybridScore" %in% colnames(scored))
    expect_equal(
        scored$HybridScore,
        (edges$EdgeFrequency^0.3) *
            (edges$Stability.dcor.scaled^0.7)
    )
})

test_that("hybrid_score additive mode uses w1*EF + w2*Stab", {
    edges <- data.frame(
        EdgeFrequency         = c(0.8, 0.5),
        Stability.dcor.scaled = c(0.6, 0.2)
    )

    scored <- hybrid_score(edges,
        w1 = 0.3, w2 = 0.7,
        score_type = "additive"
    )

    expect_equal(
        scored$HybridScore,
        0.3 * edges$EdgeFrequency +
            0.7 * edges$Stability.dcor.scaled
    )
})

test_that("hybrid_score errors on unknown score_type", {
    edges <- data.frame(
        EdgeFrequency = 0.5,
        Stability.dcor.scaled = 0.5
    )
    expect_error(hybrid_score(edges, score_type = "geometric"))
})

test_that("hybrid_score dCor rescue raises low EF for high-stability edges", {
    set.seed(1)
    edges <- data.frame(
        EdgeFrequency         = c(0.01, 0.5, 0.8, 0.9, 0.7, 0.02),
        Stability.dcor.scaled = c(0.95, 0.3, 0.4, 0.5, 0.6, 0.98)
    )
    base <- hybrid_score(edges, w1 = 0.3, w2 = 0.7)
    rescue <- hybrid_score(edges,
        w1 = 0.3, w2 = 0.7,
        dcor_rescue_percentile = 0.8
    )

    # The low-EF rows with high stability should score strictly higher with rescue
    expect_gt(rescue$HybridScore[1], base$HybridScore[1])
    expect_gt(rescue$HybridScore[6], base$HybridScore[6])
})

test_that("threshold_edges top_n returns at most N edges sorted descending", {
    scored <- data.frame(
        source = paste0("MAG", seq_len(10)),
        target = paste0("MAG", 11:20),
        HybridScore = seq(0.1, 1.0, by = 0.1)
    )

    top5 <- threshold_edges(scored, method = "top_n", value = 5)

    expect_equal(nrow(top5), 5)
    expect_true(all(diff(top5$HybridScore) <= 0))
    expect_equal(top5$HybridScore[1], 1.0)
})

test_that("threshold_edges score method filters by hard cutoff", {
    scored <- data.frame(
        source = paste0("S", seq_len(4)),
        target = paste0("T", seq_len(4)),
        HybridScore = c(0.2, 0.5, 0.8, 0.9)
    )
    out <- threshold_edges(scored, method = "score", value = 0.5)
    expect_s3_class(out, "data.frame")
    expect_true(all(out$HybridScore >= 0.5))
    expect_equal(nrow(out), 3)
})

test_that("threshold_edges quantile method keeps the top fraction", {
    scored <- data.frame(
        source = paste0("S", seq_len(100)),
        target = paste0("T", seq_len(100)),
        HybridScore = seq(0, 1, length.out = 100)
    )
    out <- threshold_edges(scored, method = "quantile", value = 0.7)
    expect_s3_class(out, "data.frame")
    expect_gte(min(out$HybridScore), quantile(scored$HybridScore, 0.7) - 1e-9)
})

test_that("threshold_edges rejects unknown methods", {
    scored <- data.frame(HybridScore = c(0.5, 0.7))
    expect_error(threshold_edges(scored, method = "magic"))
})
