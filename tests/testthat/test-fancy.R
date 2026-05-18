test_that("fancy() runs end-to-end on a tiny CLR matrix", {
    skip_on_cran()
    data(fancy_tiny_clr)

    # 10 MAGs x 60 samples is enough to exercise the full pipeline
    # while keeping the test under ~20 seconds.
    small <- t(fancy_tiny_clr[seq_len(10), seq_len(60)])

    result <- fancy(small,
        n_bootstrap = 2L, k = 3L, cpus = 1L,
        verbose = FALSE
    )

    expect_s3_class(result, "fancy")
    expect_named(result,
        c(
            "edges", "all_edges", "all_edges_unfiltered",
            "k", "n_bootstrap", "params"
        ),
        ignore.order = TRUE
    )
    expect_s3_class(result$edges, "data.frame")
    expect_true("HybridScore" %in% colnames(result$edges))
    expect_true(all(c("source", "target") %in% colnames(result$edges)))
    expect_equal(result$n_bootstrap, 2L)
    expect_equal(result$k, 3L)
})

test_that("fancy() additive scoring produces a different ranking from multiplicative", {
    skip_on_cran()
    data(fancy_tiny_clr)
    small <- t(fancy_tiny_clr[seq_len(10), seq_len(60)])

    set.seed(1)
    res_mult <- fancy(small,
        n_bootstrap = 2L, k = 3L, cpus = 1L,
        score_type = "multiplicative", verbose = FALSE
    )
    set.seed(1)
    res_add <- fancy(small,
        n_bootstrap = 2L, k = 3L, cpus = 1L,
        score_type = "additive", verbose = FALSE
    )

    # Both should produce a non-empty edge table with HybridScore present
    expect_gt(nrow(res_mult$all_edges), 0)
    expect_gt(nrow(res_add$all_edges), 0)
    # The two scoring schemes don't agree numerically on the same edges
    m_sub <- res_mult$all_edges[order(
        res_mult$all_edges$source,
        res_mult$all_edges$target
    ), ]
    a_sub <- res_add$all_edges[order(
        res_add$all_edges$source,
        res_add$all_edges$target
    ), ]
    expect_false(isTRUE(all.equal(m_sub$HybridScore, a_sub$HybridScore)))
})

test_that("find_optimal_k returns a results data.frame and a chosen k", {
    skip_on_cran()
    set.seed(1)
    m <- matrix(rnorm(200), nrow = 40, ncol = 5)

    res <- find_optimal_k(m, k_values = c(2L, 3L, 4L), cpus = 1L)

    expect_true(is.list(res))
    expect_true("results" %in% names(res))
    expect_s3_class(res$results, "data.frame")
    expect_equal(res$results$k, c(2L, 3L, 4L))
    expect_true(all(c("k", "avg_mi") %in% colnames(res$results)))
})

test_that("bootstrap_networks returns one named entry per (method, iter)", {
    skip_on_cran()
    data(fancy_tiny_clr)
    small <- t(fancy_tiny_clr[seq_len(10), seq_len(60)])

    res <- bootstrap_networks(small,
        n_bootstrap = 2L, k = 3L, cpus = 1L,
        use_mrnet = TRUE
    )

    expect_true(is.list(res))
    expect_true(any(grepl("^MRNET_", names(res))))
    expect_true(any(grepl("^dCor_", names(res))))
})
