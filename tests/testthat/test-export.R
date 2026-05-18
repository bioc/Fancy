test_that("phyla_palette returns one unique colour per unique phylum", {
    cols <- phyla_palette(c(
        "Bacillota", "Pseudomonadota",
        "Bacillota", "Bacteroidota"
    ))

    expect_named(cols)
    expect_equal(
        sort(names(cols)),
        c("Bacillota", "Bacteroidota", "Pseudomonadota")
    )
    expect_equal(length(unique(cols)), 3L)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}", cols)))
})

test_that("phyla_palette is deterministic across calls", {
    expect_equal(
        phyla_palette(c("Bacillota", "Pseudomonadota")),
        phyla_palette(c("Pseudomonadota", "Bacillota"))
    )
})

test_that("phyla_palette handles > 12 phyla by falling back to hcl.colors", {
    many <- paste0("Phylum_", LETTERS[seq_len(15)])
    cols <- phyla_palette(many)
    expect_equal(length(cols), 15L)
    expect_equal(length(unique(cols)), 15L)
})

test_that("export_cytoscape writes node and edge tables for a thresholded result", {
    data(fancy_tiny_taxonomy)

    edges <- data.frame(
        source = c("MAG1", "MAG2", "MAG3"),
        target = c("MAG2", "MAG3", "MAG4"),
        EdgeFrequency = c(0.8, 0.5, 0.9),
        Stability.dcor.scaled = c(0.7, 0.3, 0.85),
        HybridScore = c(0.75, 0.4, 0.85)
    )
    fake_result <- structure(
        list(
            edges = edges, all_edges = edges,
            k = 5L, n_bootstrap = 2L,
            params = list(
                threshold_method = "quantile",
                threshold_value = 0.7
            )
        ),
        class = "fancy"
    )

    tax <- fancy_tiny_taxonomy[seq_len(4), , drop = FALSE]
    rownames(tax) <- c("MAG1", "MAG2", "MAG3", "MAG4")

    tmp <- file.path(tempdir(), "fancy_export_test")
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    out <- export_cytoscape(fake_result,
        taxonomy = tax,
        file_prefix = file.path(tmp, "test_net"),
        edges = "thresholded"
    )

    expect_true(file.exists(file.path(tmp, "test_net_edges.tsv")))
    expect_true(file.exists(file.path(tmp, "test_net_nodes.tsv")))
    expect_type(out, "list")
    expect_true(all(c("nodes", "edges") %in% names(out)))
})
