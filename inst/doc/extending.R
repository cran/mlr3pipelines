## ----extending-020------------------------------------------------------------
library("mlr3")
task = tsk("iris")
task$data()

## ----extending-022, eval = FALSE, tidy = FALSE--------------------------------
#  PipeOpCopyTwo = R6::R6Class("PipeOpCopyTwo",
#    inherit = mlr3pipelines::PipeOp,
#    public = list(
#      initialize = function(id = "copy.two") {
#        ....
#      },
#    ),
#    private == list(
#      .train = function(inputs) {
#        ....
#      },
#  
#      .predict = function(inputs) {
#        ....
#      }
#    )
#  )

## ----extending-023, eval = FALSE----------------------------------------------
#  initialize = function(id = "copy.two") {
#    input = data.table::data.table(name = "input", train = "*", predict = "*")
#    # the following will create two rows and automatically fill the `train`
#    # and `predict` cols with "*"
#    output = data.table::data.table(
#      name = c("output1", "output2"),
#      train = "*", predict = "*"
#    )
#    super$initialize(id,
#      input = input,
#      output = output
#    )
#  }

## ----extending-024, eval = FALSE----------------------------------------------
#  .train = function(inputs) {
#    self$state = list()
#    c(inputs, inputs)
#  }

## ----extending-025, eval = FALSE----------------------------------------------
#  .predict = function(inputs) {
#    c(inputs, inputs)
#  }

## ----extending-026, tidy = FALSE----------------------------------------------
PipeOpCopyTwo = R6::R6Class("PipeOpCopyTwo",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(id = "copy.two") {
      super$initialize(id,
        input = data.table::data.table(name = "input", train = "*", predict = "*"),
        output = data.table::data.table(name = c("output1", "output2"),
                            train = "*", predict = "*")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      c(inputs, inputs)
    },

    .predict = function(inputs) {
      c(inputs, inputs)
    }
  )
)

## ----extending-027------------------------------------------------------------
library("mlr3pipelines")
poct = PipeOpCopyTwo$new()
gr = Graph$new()
gr$add_pipeop(poct)

print(gr)

result = gr$train(task)

str(result)

## ----extending-028, tidy = FALSE----------------------------------------------
PipeOpDropNA = R6::R6Class("PipeOpDropNA",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "drop.na") {
      super$initialize(id)
    }
  ),

  private = list(
    .train_task = function(task) {
      self$state = list()
      featuredata = task$data(cols = task$feature_names)
      exclude = apply(is.na(featuredata), 1, any)
      task$filter(task$row_ids[!exclude])
    },

    .predict_task = function(task) {
      # nothing to be done
      task
    }
  )
)

## ----extending-029------------------------------------------------------------
smalliris = iris[(1:5) * 30, ]
smalliris[1, 1] = NA
smalliris[2, 2] = NA
sitask = as_task_classif(smalliris, target = "Species")
print(sitask$data())

## ----extending-030------------------------------------------------------------
gr = Graph$new()
gr$add_pipeop(PipeOpDropNA$new())

filtered_task = gr$train(sitask)[[1]]
print(filtered_task$data())

## ----extending-031, tidy = FALSE----------------------------------------------
PipeOpScaleAlways = R6::R6Class("PipeOpScaleAlways",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "scale.always") {
      super$initialize(id = id)
    }
  ),

  private = list(
    .select_cols = function(task) {
      task$feature_types[type == "numeric", id]
    },

    .train_dt = function(dt, levels, target) {
      sc = scale(as.matrix(dt))
      self$state = list(
        center = attr(sc, "scaled:center"),
        scale = attr(sc, "scaled:scale")
      )
      sc
    },

    .predict_dt = function(dt, levels) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)

## ----extending-032------------------------------------------------------------
gr = Graph$new()
gr$add_pipeop(PipeOpScaleAlways$new())

result = gr$train(task)

result[[1]]$data()

## ----extending-033, tidy = FALSE----------------------------------------------
PipeOpDropConst = R6::R6Class("PipeOpDropConst",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "drop.const") {
      super$initialize(id = id)
    }
  ),

  private = list(
    .get_state = function(task) {
      data = task$data(cols = task$feature_names)
      nonconst = sapply(data, function(column) length(unique(column)) > 1)
      list(cnames = colnames(data)[nonconst])
    },

    .transform = function(task) {
      task$select(self$state$cnames)
    }
  )
)

## ----extending-034------------------------------------------------------------
irishead = task$clone()$filter(1:5)
irishead$data()

## ----extending-035------------------------------------------------------------
gr = Graph$new()$add_pipeop(PipeOpDropConst$new())
dropped_task = gr$train(irishead)[[1]]

dropped_task$data()

## ----extending-036------------------------------------------------------------
gr$pipeops$drop.const$state

## ----extending-037------------------------------------------------------------
dropped_predict = gr$predict(task)[[1]]

dropped_predict$data()

## ----extending-038, tidy = FALSE----------------------------------------------
PipeOpScaleAlwaysSimple = R6::R6Class("PipeOpScaleAlwaysSimple",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "scale.always.simple") {
      super$initialize(id = id)
    }
  ),

  private = list(
    .select_cols = function(task) {
      task$feature_types[type == "numeric", id]
    },

    .get_state_dt = function(dt, levels, target) {
      list(
        center = sapply(dt, mean),
        scale = sapply(dt, sd)
      )
    },

    .transform_dt = function(dt, levels) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)

## ----extending-039------------------------------------------------------------
gr = Graph$new()$add_pipeop(PipeOpScaleAlways$new())
result_posa = gr$train(task)[[1]]

gr = Graph$new()$add_pipeop(PipeOpScaleAlwaysSimple$new())
result_posa_simple = gr$train(task)[[1]]

## ----extending-040------------------------------------------------------------
result_posa$data()

## ----extending-041------------------------------------------------------------
result_posa_simple$data()

## ----extending-042------------------------------------------------------------
PipeOpScale$public_methods$initialize

## ----extending-043------------------------------------------------------------
pss = po("scale")
print(pss$param_set)

## ----extending-044------------------------------------------------------------
pss$param_set$values$center = FALSE
print(pss$param_set$values)

## ----extending-045, error = TRUE----------------------------------------------
pss$param_set$values$scale = "TRUE" # bad input is checked!

## ----extending-046------------------------------------------------------------
PipeOpScale$private_methods$.train_dt

## ----extending-047------------------------------------------------------------
pss$param_set$values$scale = FALSE
pss$param_set$values$center = FALSE

gr = Graph$new()
gr$add_pipeop(pss)

result = gr$train(task)

result[[1]]$data()

