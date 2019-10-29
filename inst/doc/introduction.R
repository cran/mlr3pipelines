## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  comment = "#>"
)
set.seed(8008135)
compiler::enableJIT(0)
library("mlr3")
library("mlr3pipelines")


## ------------------------------------------------------------------------
task = TaskClassif$new("iris", as_data_backend(iris), "Species")

lrn = mlr_learners$get("classif.rpart")

rsmp = mlr_resamplings$get("holdout")

resample(task, lrn, rsmp)

## ------------------------------------------------------------------------
po = mlr_pipeops$get("pca")

po$train(list(task))[[1]]$data()

## ------------------------------------------------------------------------
single_line_task = task$clone()$filter(1)

po$predict(list(single_line_task))[[1]]$data()

## ------------------------------------------------------------------------
po$state

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics("figures/po_viz.png")

## ------------------------------------------------------------------------
as.data.table(mlr_pipeops)[, c("key", "input.num", "output.num")]

## ------------------------------------------------------------------------
mlr_pipeops$get("pca", param_vals = list(rank. = 3))

## ------------------------------------------------------------------------
iris_first_half = task$clone()$select(c("Petal.Length", "Petal.Width"))
iris_second_half = task$clone()$select(c("Sepal.Length", "Sepal.Width"))

pofu = mlr_pipeops$get("featureunion", innum = 2)

pofu$train(list(iris_first_half, iris_second_half))[[1]]$data()

## ------------------------------------------------------------------------
pofu$input

## ------------------------------------------------------------------------
# TODO this is an important case to handle here, do not delete unless there is a better example.
# mlr_pipeops$get("backuplearner")$input

## ------------------------------------------------------------------------
mlr_pipeops$get("chunk", outnum = 3)$output

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics("figures/po_multi_alone.png")

## ------------------------------------------------------------------------
# mlr_pipeops$get("backuplearner")

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics("figures/po_multi_viz.png")

## ------------------------------------------------------------------------
gr = Graph$new()

gr$add_pipeop(mlr_pipeops$get("scale"))

gr$add_pipeop(mlr_pipeops$get("subsample", param_vals = list(frac = 0.1)))

gr$add_edge("scale", "subsample")

## ------------------------------------------------------------------------
print(gr)

## ---- fig.width = 8, fig.height = 8--------------------------------------
gr$plot(html = TRUE)

## ------------------------------------------------------------------------
gr$train(task)[[1]]$data()

## ------------------------------------------------------------------------
gr$predict(single_line_task)[[1]]$data()

## ------------------------------------------------------------------------
gr = Graph$new()$
  add_pipeop(mlr_pipeops$get("copy", outnum = 2))$
  add_pipeop(mlr_pipeops$get("scale"))$
  add_pipeop(mlr_pipeops$get("pca"))$
  add_pipeop(mlr_pipeops$get("featureunion", innum = 2))

gr$
  add_edge("copy", "scale", src_channel = 1)$        # designating channel by index
  add_edge("copy", "pca", src_channel = "output2")$  # designating channel by name
  add_edge("scale", "featureunion", dst_channel = 1)$
  add_edge("pca", "featureunion", dst_channel = 2)

gr$plot(html = TRUE)

## ------------------------------------------------------------------------
gr$train(iris_first_half)[[1]]$data()

## ------------------------------------------------------------------------
gr = mlr_pipeops$get("copy", outnum = 2) %>>%
  gunion(list(mlr_pipeops$get("scale"), mlr_pipeops$get("pca"))) %>>%
  mlr_pipeops$get("featureunion", innum = 2)

gr$plot(html = TRUE)

## ---- error = TRUE-------------------------------------------------------
po1 = mlr_pipeops$get("scale")
po2 = mlr_pipeops$get("scale")
po1 %>>% po2  # name clash

## ------------------------------------------------------------------------
po2$id = "scale2"
gr = po1 %>>% po2
gr

## ------------------------------------------------------------------------
# Alternative ways of getting new ids:
mlr_pipeops$get("scale", id = "scale2")
PipeOpScale$new(id = "scale2")

## ---- error = TRUE-------------------------------------------------------
# sometimes names of PipeOps within a Graph need to be changed
gr2 = mlr_pipeops$get("scale") %>>% mlr_pipeops$get("pca")
gr %>>% gr2

## ------------------------------------------------------------------------
gr2$set_names("scale", "scale3")
gr %>>% gr2

## ------------------------------------------------------------------------
gr = mlr_pipeops$get("pca") %>>% mlr_pipeops$get("learner",
  mlr_learners$get("classif.rpart"))

## ------------------------------------------------------------------------
gr$train(task)
gr$predict(task)

## ------------------------------------------------------------------------
lrngrph = GraphLearner$new(gr)

resample(task, lrngrph, rsmp)

## ------------------------------------------------------------------------
op_pca = mlr_pipeops$get("pca")
op_pca$param_set

## ------------------------------------------------------------------------
op_pca$param_set$values$center = FALSE
op_pca$param_set$values

## ------------------------------------------------------------------------
op_pca = mlr_pipeops$get("pca", param_vals = list(center = TRUE))
op_pca$param_set$values

## ------------------------------------------------------------------------
gr = op_pca %>>% mlr_pipeops$get("scale")
gr$param_set

## ------------------------------------------------------------------------
gr$param_set$values

## ------------------------------------------------------------------------
op_rpart = mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"))
op_rpart$param_set

## ------------------------------------------------------------------------
glrn = GraphLearner$new(gr %>>% op_rpart)
glrn$param_set

