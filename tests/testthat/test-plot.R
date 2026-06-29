test_that("plot_network runs end-to-end on a fancy result", {
    skip_on_cran()
    skip_if_not_installed("igraph")
    data(fancy_tiny_clr)
    data(fancy_tiny_taxonomy)
    small <- t(fancy_tiny_clr[seq_len(10), seq_len(60)])

    result <- fancy(small,
        n_bootstrap = 2L, k = 3L, cpus = 1L,
        verbose = FALSE
    )

    tax <- fancy_tiny_taxonomy[seq_len(10), , drop = FALSE]

    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    grDevices::png(tmp, width = 600, height = 600)
    expect_error(plot_network(result, tax, community = FALSE), NA)
    grDevices::dev.off()
    expect_true(file.exists(tmp))
})

test_that("plot() method dispatches on FancyResult and draws a histogram", {
    edges <- data.frame(
        source = paste0("M", seq_len(20)),
        target = paste0("N", seq_len(20)),
        HybridScore = seq(0.05, 1.0, length.out = 20)
    )
    fake_result <- methods::new("FancyResult",
        edges = edges, all_edges = edges,
        k = 5, n_bootstrap = 2L,
        params = list(
            threshold_method = "quantile",
            threshold_value = 0.7
        )
    )

    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    grDevices::png(tmp, width = 500, height = 400)
    expect_error(plot(fake_result), NA)
    grDevices::dev.off()
    expect_true(file.exists(tmp))
})

test_that("plot_k_elbow draws a curve from a find_optimal_k result", {
    fake_k_result <- list(
        results = data.frame(
            k = c(2, 3, 4, 5, 6),
            avg_mi = c(0.10, 0.18, 0.22, 0.24, 0.245)
        ),
        best_k = 4L
    )

    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    grDevices::png(tmp, width = 500, height = 400)
    expect_error(plot_k_elbow(fake_k_result), NA)
    grDevices::dev.off()
    expect_true(file.exists(tmp))
})
