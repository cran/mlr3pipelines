#' @title Split Numeric Features into Quantile Bins
#'
#' @usage NULL
#' @name mlr_pipeops_quantilebin
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Splits numeric features into quantile bins.
#'
#' @section Construction:
#' ```
#' PipeOpQuantileBin$new(id = "quantilebin", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"quantilebin"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their binned versions.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `bins` :: `list` \cr
#'   List of intervals representing the bins for each numeric feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `numsplits`  :: `integer(1)` \cr
#'   Number of bins to create. Default is `2`.
#'
#' @section Internals:
#' Uses the [`stats::quantile`] function.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("quantilebin")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpQuantileBin = R6Class("PipeOpQuantileBin",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "quantilebin", param_vals = list()) {
      ps = ps(
        numsplits = p_int(lower = 2, special_vals = list(NULL), tags = "train")
      )
      ps$values = list(numsplits = 2L)
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      bins = lapply(dt, function(d)
        unique(c(-Inf, stats::quantile(d, (1:(self$param_set$values$numsplits - 1)) /
            self$param_set$values$numsplits, na.rm = TRUE), Inf)))
      list(bins = bins)
    },

    .transform_dt = function(dt, levels) {
      as.data.frame(mapply(function(d, b) ordered(cut(d, breaks = b)), d = dt,
        b = self$state$bins, SIMPLIFY = FALSE), row.names = rownames(dt))
    }
  )
)

mlr_pipeops$add("quantilebin", PipeOpQuantileBin)
