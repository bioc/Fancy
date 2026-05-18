test_that("clean_sample_names strips the X prefix and dot suffixes", {
    raw <- c(
        "X12.34.AB.QUALITY_PASSED_R1.fastq.Read.Count",
        "X99.YY.QUALITY_PASSED_R1.fastq.Covered.Fraction"
    )
    cleaned <- clean_sample_names(raw)

    expect_equal(cleaned, c("12-34-AB", "99-YY"))
})

test_that("clean_sample_names honours remove_prefix and dot_to_dash flags", {
    raw <- "X12.34.QUALITY_PASSED_R1.fastq.Read.Count"

    expect_equal(
        clean_sample_names(raw, remove_prefix = FALSE),
        "X12-34"
    )
    expect_equal(
        clean_sample_names(raw, dot_to_dash = FALSE),
        "12.34"
    )
})

test_that("parse_count_tables reads per-sample TSVs into combined matrices", {
    tmp <- file.path(tempdir(), "fancy_tsv_dir")
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    writeLines(
        c(
            "MAG\tcount\trel_abund\tcov_frac",
            "MAG1\t100\t0.5\t0.8",
            "MAG2\t200\t0.3\t0.6"
        ),
        file.path(tmp, "sample1.tsv")
    )
    writeLines(
        c(
            "MAG\tcount\trel_abund\tcov_frac",
            "MAG1\t150\t0.55\t0.75",
            "MAG2\t220\t0.32\t0.65"
        ),
        file.path(tmp, "sample2.tsv")
    )

    tables <- parse_count_tables(tmp,
        drop_first_row = FALSE,
        clean_names = FALSE
    )

    expect_named(tables, c("count", "relative_abundance", "covered_fraction"),
        ignore.order = TRUE
    )
    expect_equal(nrow(tables$count), 2L)
    expect_equal(ncol(tables$count), 2L)
})

test_that("parse_gtdb_taxonomy strips rank prefixes and sub-designations", {
    tmp <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp), add = TRUE)

    writeLines(
        c(
            "Strain\tDomain\tPhyla\tClass\tOrder\tFamily\tGenus\tSpecies",
            paste("MAG1", "d__Bacteria", "p__Bacillota_C", "c__Clostridia",
                "o__Lachnospirales", "f__Lachnospiraceae",
                "g__Butyrivibrio", "s__Butyrivibrio sp1",
                sep = "\t"
            ),
            paste("MAG2", "d__Archaea", "p__Methanobacteriota",
                "c__Methanobacteria", "o__Methanobacteriales",
                "f__Methanobacteriaceae", "g__Methanobrevibacter",
                "s__",
                sep = "\t"
            )
        ),
        tmp
    )

    tax <- parse_gtdb_taxonomy(tmp)

    expect_true(all(c(
        "Domain", "Phyla", "Class", "Order",
        "Family", "Genus", "Species"
    ) %in% colnames(tax)))
    expect_equal(nrow(tax), 2L)
    expect_equal(tax["MAG1", "Domain"], "Bacteria")
    expect_equal(tax["MAG1", "Phyla"], "Bacillota")
    expect_equal(tax["MAG1", "Species"], "sp")
    expect_equal(tax["MAG2", "Species"], "sp")
})
