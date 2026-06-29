#' FancyResult: S4 container for Fancy pipeline output
#'
#' A formal S4 class holding the results of the [fancy()] hybrid network
#' inference pipeline. Use the [fancy()] wrapper to construct objects of
#' this class; slots can be read with the `$` operator (e.g.
#' `result$edges`) or with standard slot access (`result@edges`).
#'
#' @slot edges A data.frame of thresholded edges (the final network).
#' @slot all_edges A data.frame of all scored edges before thresholding
#'   (useful for re-thresholding).
#' @slot all_edges_unfiltered A data.frame of all scored pairs before the
#'   hard frequency/stability pre-filter.
#' @slot k Numeric; the kNN parameter used for MI estimation.
#' @slot n_bootstrap Integer; number of bootstrap iterations run.
#' @slot params A list of all pipeline parameters, for reproducibility.
#'
#' @return An object of class `FancyResult`.
#'
#' @seealso [fancy()]
#'
#' @export
setClass(
    "FancyResult",
    slots = c(
        edges = "data.frame",
        all_edges = "data.frame",
        all_edges_unfiltered = "data.frame",
        k = "numeric",
        n_bootstrap = "integer",
        params = "list"
    ),
    prototype = list(
        edges = data.frame(),
        all_edges = data.frame(),
        all_edges_unfiltered = data.frame(),
        k = NA_real_,
        n_bootstrap = NA_integer_,
        params = list()
    )
)

setValidity("FancyResult", function(object) {
    msgs <- character()

    required <- c("source", "target", "HybridScore")
    if (nrow(object@all_edges) > 0L &&
        !all(required %in% colnames(object@all_edges))) {
        msgs <- c(msgs, paste0(
            "'all_edges' must contain columns: ",
            paste(required, collapse = ", ")
        ))
    }
    if (nrow(object@edges) > 0L &&
        !all(c("source", "target") %in% colnames(object@edges))) {
        msgs <- c(msgs, "'edges' must contain 'source' and 'target' columns")
    }
    if (length(object@n_bootstrap) == 1L && !is.na(object@n_bootstrap) &&
        object@n_bootstrap < 0L) {
        msgs <- c(msgs, "'n_bootstrap' must be non-negative")
    }

    if (length(msgs) == 0L) TRUE else msgs
})

#' Access FancyResult slots with `$`
#'
#' Convenience accessor so that `result$edges`, `result$all_edges`,
#' `result$params`, etc. retrieve the corresponding slot of a
#' [FancyResult-class] object.
#'
#' @param x A [FancyResult-class] object.
#' @param name Slot name to extract.
#'
#' @return The contents of the named slot.
#'
#' @importFrom methods slot
#' @export
setMethod("$", "FancyResult", function(x, name) {
    slot(x, name)
})

#' Display a FancyResult
#'
#' Compact summary method for [FancyResult-class] objects.
#'
#' @param object A [FancyResult-class] object.
#'
#' @return `object`, invisibly.
#'
#' @importFrom methods show
#' @export
setMethod("show", "FancyResult", function(object) {
    cat("<FancyResult>\n")
    cat("  Bootstrap iterations :", object@n_bootstrap, "\n")
    cat("  k (kNN MI)           :", object@k, "\n")
    cat("  Thresholded edges    :", nrow(object@edges), "\n")
    cat("  Scored edges         :", nrow(object@all_edges), "\n")
    if (!is.null(object@params$score_type)) {
        cat("  Score type           :", object@params$score_type, "\n")
    }
    invisible(object)
})
