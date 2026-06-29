# Fancy 0.99.4

* `fancy()` now returns a formal S4 `FancyResult` object (with slots,
  validity, and `show`/`plot`/`$` methods) in place of the previous S3
  list. Slots remain accessible with `$` (e.g. `result$edges`) or `@`.
* Reformatted all R sources to 4-space indentation
  (`styler::style_pkg(indent_by = 4)`).
* Lowered the default number of parallel workers to `cpus = 1L` in
  `fancy()`, `bootstrap_networks()`, and `find_optimal_k()`; users can
  raise it for real datasets.
* Replaced `(i + 1L):n` index ranges with `seq_len()`-based forms for
  zero-length safety.
* Split the bundled example data into one `.rda` file per dataset so each
  is loadable with `data(<name>)`.

# Fancy 0.0.1

* Initial release.
* Hybrid network inference combining mutual information (MRNET) and distance
  correlation with bootstrap resampling.
* Exported functions: `fancy()`, `bootstrap_networks()`, `clr_normalize()`,
  `filter_mags()`, `find_optimal_k()`, `hybrid_score()`, `threshold_edges()`,
  `parse_count_tables()`, `parse_gtdb_taxonomy()`, `clean_sample_names()`,
  `export_cytoscape()`, `phyla_palette()`, `plot_k_elbow()`, `plot_network()`.
* Bundled example dataset (`fancy_tiny`) with 100 MAGs x 321 rumen samples.
* Vignette: `Fancy_workflow` demonstrating the full pipeline.
