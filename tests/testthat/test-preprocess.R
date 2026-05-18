test_that("filter_mags applies coverage mask and prevalence filter", {
    data(fancy_tiny_counts)
    data(fancy_tiny_coverage)

    filt <- filter_mags(fancy_tiny_counts, fancy_tiny_coverage,
        min_coverage = 0.3, min_samples = 50L
    )

    expect_s3_class(filt, "data.frame")
    expect_equal(ncol(filt), ncol(fancy_tiny_counts))
    expect_lte(nrow(filt), nrow(fancy_tiny_counts))
    expect_true(all(filt >= 0, na.rm = TRUE))
})

test_that("filter_mags zeroes counts where coverage falls below threshold", {
    counts <- data.frame(
        s1 = c(100, 200), s2 = c(50, 80),
        row.names = c("MAG1", "MAG2")
    )
    cov <- data.frame(
        s1 = c(0.5, 0.1), s2 = c(0.9, 0.4),
        row.names = c("MAG1", "MAG2")
    )

    filt <- filter_mags(counts, cov, min_coverage = 0.3, min_samples = 1L)

    expect_equal(filt["MAG2", "s1"], 0)
    expect_equal(filt["MAG1", "s1"], 100)
})

test_that("filter_mags drops MAGs below prevalence threshold", {
    counts <- data.frame(
        s1 = c(100, 200, 50),
        s2 = c(80, 120, 60),
        s3 = c(70, 100, 55),
        row.names = c("MAG1", "MAG2", "MAG3")
    )
    cov <- data.frame(
        s1 = c(0.9, 0.1, 0.1),
        s2 = c(0.9, 0.1, 0.1),
        s3 = c(0.9, 0.1, 0.9),
        row.names = c("MAG1", "MAG2", "MAG3")
    )

    filt <- filter_mags(counts, cov, min_coverage = 0.3, min_samples = 2L)

    expect_true("MAG1" %in% rownames(filt))
    expect_false("MAG2" %in% rownames(filt))
})

test_that("clr_normalize produces CLR-transformed values centred near zero", {
    data(fancy_tiny_counts)

    clr_df <- clr_normalize(fancy_tiny_counts[seq_len(10), seq_len(20)])

    expect_s3_class(clr_df, "data.frame")
    expect_equal(dim(clr_df), c(10, 20))
    # compositions::clr() treats each ROW as a composition, so each MAG's
    # CLR-transformed row sums (and therefore means) to zero across samples.
    row_means <- rowMeans(clr_df)
    expect_true(all(abs(row_means) < 1e-8))
})

test_that("clr_normalize accepts a pseudocount and preserves names", {
    counts <- data.frame(
        s1 = c(0, 5, 10),
        s2 = c(2, 0, 8),
        row.names = c("A", "B", "C")
    )

    out <- clr_normalize(counts, pseudocount = 1)

    expect_equal(rownames(out), c("A", "B", "C"))
    expect_equal(colnames(out), c("s1", "s2"))
    expect_true(all(is.finite(as.matrix(out))))
})
