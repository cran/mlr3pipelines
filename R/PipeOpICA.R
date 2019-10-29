#' @title PipeOpICA
#'
#' @usage NULL
#' @name mlr_pipeops_ica
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Extracts statistically independent components from data.  Only affects numerical features.
#' See [fastICA::fastICA] for details.
#'
#' @section Construction:
#' ```
#' PipeOpICA$new(id = "ica", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"ica"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric parameters replaced by independent components.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the elements of the [`"fastICA"` function][fastICA::fastICA],
#' with the exception of the `$X` and `$S` slots. These are in particular:
#' * `K` :: `matrix`\cr
#'   Matrix that projects data onto the first `n.comp` principal components.
#'   See [`fastICA()`][fastICA::fastICA].
#' * `W` :: `matrix`\cr
#'   Estimated un-mixing matrix. See [`fastICA()`][fastICA::fastICA].
#' * `A` :: `matrix`\cr
#'   Estimated mixing matrix. See [`fastICA()`][fastICA::fastICA].
#' * `center` :: `numeric`\cr
#'   The mean of each numeric feature during training.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as the following parameters
#' based on [`fastICA()`][fastICA::fastICA]:
#' * `n.comp` :: `numeric(1)`\cr
#'   Number of components to extract. Default is `NULL`, which sets it
#'   to the number of available numeric columns.
#' * `alg.typ`:: `character(1)`\cr
#'   Algorithm type. One of \dQuote{parallel} (default) or \dQuote{deflation}.
#' * `fun` :: `character(1)`\cr
#'   One of \dQuote{logcosh} (default) or \dQuote{exp}.
#' * `alpha` :: `numeric(1)`\cr
#'   In range `[1, 2]`, Used for negentropy calculation when `fun` is \dQuote{logcosh}.
#'   Default is 1.0.
#' * `method` :: `character(1)`\cr
#'   Internal calculation method. \dQuote{C} (default) or \dQuote{R}.
#'   See [`fastICA()`][fastICA::fastICA].
#' * `row.norm` :: `logical(1)`\cr
#'   Logical value indicating whether rows should be standardized beforehand.
#'   Default is `FALSE`.
#' * `maxit` :: `numeric(1)`\cr
#'   Maximum number of iterations. Default is 200.
#' * `tol` :: `numeric(1)`\cr
#'   Tolerance for convergence, default is `1e-4`.
#' * `verbose` `logical(1)`\cr
#'   Logical value indicating the level of output during the run of the algorithm.
#'   Default is `FALSE`.
#' * `w.init`:: `matrix`\cr
#'   Initial un-mixing matrix. See [`fastICA()`][fastICA::fastICA].
#'   Default is `NULL`.
#'
#' @section Internals:
#' Uses the [`fastICA()`][fastICA::fastICA] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("ica")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpICA = R6Class("PipeOpICA",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "ica", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("n.comp", lower = 1, upper = Inf, tags = c("train", "ica")),
        ParamFct$new("alg.typ", levels = c("parallel", "deflation"),
          default = "parallel", tags = c("train", "ica")),
        ParamFct$new("fun", default = "logcosh", levels = c("logcosh", "exp"), tags = c("train", "ica")),
        ParamDbl$new("alpha", default = 1.0, lower = 1, upper = 2, tags = c("train", "ica")),
        ParamFct$new("method", default = "R", levels = c("C", "R"), tags = c("train", "ica")),
        ParamLgl$new("row.norm", default = FALSE, tags = c("train", "ica")),
        ParamInt$new("maxit", default = 200, lower = 1, tags = c("train", "ica")),
        ParamDbl$new("tol", default = 1e-04, lower = 0, tags = c("train", "ica")),
        ParamLgl$new("verbose", default = FALSE, tags = c("train", "ica")),
        ParamUty$new("w.init", default = NULL, tags = c("train", "ica"))
      ))
      ps$values = list(method = "C")
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "fastICA")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    train_dt = function(dt, levels, target) {

      params = insert_named(list(n.comp = ncol(dt)), self$param_set$get_values(tags = "ica"))

      ica = invoke(fastICA::fastICA, as.matrix(dt), .args = params)

      self$state = ica
      self$state$S = NULL
      self$state$X = NULL
      self$state$center = map_dbl(dt, mean)
      ica$S
    },

    predict_dt = function(dt, levels) {
      scale(as.matrix(dt), scale = FALSE, center = self$state$center) %*%
        (self$state$K %*% self$state$W)
    }
  )
)

mlr_pipeops$add("ica", PipeOpICA)