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
  library("mlr3")
  task = mlr_tasks$get("iris")

## ------------------------------------------------------------------------
  test.idx = sample(seq_len(task$nrow), 30)
  train.idx = setdiff(seq_len(task$nrow), test.idx)
  # Set task to only use train indexes
  task$row_roles$use = train.idx

## ------------------------------------------------------------------------
  library("mlr3pipelines")
  mlr_pipeops$keys()

## ------------------------------------------------------------------------
  op1 = PipeOpScale$new()
  op2 = PipeOpPCA$new()
  op3 = PipeOpLearner$new(learner = mlr_learners$get("classif.rpart"))

## ------------------------------------------------------------------------
  linear_pipeline = op1 %>>% op2 %>>% op3

## ------------------------------------------------------------------------
  linear_pipeline$train(task)

## ------------------------------------------------------------------------
  # predict on test.idx
  task$row_roles$use = test.idx
  linear_pipeline$predict(task)

## ---- eval = FALSE-------------------------------------------------------
#  library("mlr")
#  # We first create a learner
#  lrn = makeLearner("classif.rpart")
#  # Wrap this learner in a FilterWrapper
#  lrn.wrp = makeFilterWrapper(lrn, fw.abs = 2L)
#  # And wrap the resulting wrapped learner into an ImputeWrapper.
#  lrn.wrp = makeImputeWrapper(lrn.wrp)
#  
#  # Afterwards, we can train the resulting learner on a task
#  train(lrn, iris.task)

## ---- eval = FALSE-------------------------------------------------------
#  library("mlr3")
#  library("mlr3pipelines")
#  library("mlr3filters")
#  
#  impute = PipeOpImpute$new()
#  filter = PipeOpFilter$new(filter = FilterVariance$new(), param_vals = list(filter.nfeat = 2L))
#  rpart = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
#  
#  # Assemble the Pipeline
#  pipeline = impute %>>% filter %>>% rpart
#  # And convert to a 'GraphLearner'
#  learner = GraphLearner$new(id = "Pipeline", pipeline)

## ---- eval = FALSE-------------------------------------------------------
#  from sklearn.pipeline import Pipeline, FeatureUnion
#  from sklearn.model_selection import GridSearchCV
#  from sklearn.svm import SVC
#  from sklearn.datasets import load_iris
#  from sklearn.decomposition import PCA
#  from sklearn.feature_selection import SelectKBest
#  
#  iris = load_iris()
#  
#  X, y = iris.data, iris.target
#  
#  # This dataset is way too high-dimensional. Better do PCA:
#  pca = PCA(n_components=2)
#  
#  # Maybe some original features where good, too?
#  selection = SelectKBest(k=1)
#  
#  # Build estimator from PCA and Univariate selection:
#  combined_features = FeatureUnion([("pca", pca), ("univ_select", selection)])
#  
#  # Use combined features to transform dataset:
#  X_features = combined_features.fit(X, y).transform(X)
#  
#  svm = SVC(kernel="linear")
#  
#  # Do grid search over k, n_components and C:
#  pipeline = Pipeline([("features", combined_features), ("svm", svm)])
#  
#  param_grid = dict(features__pca__n_components=[1, 2, 3],
#                    features__univ_select__k=[1, 2],
#                    svm__C=[0.1, 1, 10])
#  
#  grid_search = GridSearchCV(pipeline, param_grid=param_grid, cv=5, verbose=10)
#  grid_search.fit(X, y)

## ---- eval = FALSE-------------------------------------------------------
#  library("mlr3verse")
#  iris = mlr_tasks$get("iris")
#  
#  # Build the steps
#  copy = PipeOpCopy$new(2)
#  pca = PipeOpPCA$new()
#  selection = PipeOpFilter$new(filter = FilterVariance$new())
#  union = PipeOpFeatureUnion$new(2)
#  svm = PipeOpLearner$new(mlr_learners$get("classif.svm", param_vals = list(kernel = "linear")))
#  
#  # Assemble the Pipeline
#  pipeline = copy %>>% gunion(list(pca, selection)) %>>% union %>>% svm
#  learner = GraphLearner$new(id = "Pipeline", pipeline)
#  
#  # For tuning, we define the resampling and the Parameter Space
#  resampling = mlr3::mlr_resamplings$get("cv", param_vals = list(folds = 5L))
#  
#  param_set = paradox::ParamSet$new(params = list(
#    paradox::ParamDbl$new("classif.svm.cost", lower = 0.1, upper = 1),
#    paradox::ParamInt$new("pca.rank.",  lower = 1, upper = 3),
#    paradox::ParamInt$new("variance.filter.nfeat",  lower = 1, upper = 2)
#  ))
#  
#  pe = PerformanceEvaluator$new(iris, learner, resampling, param_set)
#  terminator = TerminatorEvaluations$new(60)
#  tuner = TunerGridSearch$new(pe, terminator, resolution = 10)$tune()
#  
#  # Set the learner to the optimal values and train
#  learner$param_set$values = tuner$tune_result()$values

## ---- eval = FALSE-------------------------------------------------------
#  library("tidymodels")
#  library("rsample")
#  data("credit_data")
#  
#  set.seed(55)
#  train_test_split = initial_split(credit_data)
#  credit_train = training(train_test_split)
#  credit_test = testing(train_test_split)
#  
#  rec = recipe(Status ~ ., data = credit_train) %>%
#    step_knnimpute(all_predictors()) %>%
#    step_dummy(all_predictors(), -all_numeric()) %>%
#    step_center(all_numeric()) %>%
#    step_scale(all_numeric())
#  
#  trained_rec = prep(rec, training = credit_train)
#  
#  # Apply to train and test set
#  train_data <- bake(trained_rec, new_data = credit_train)
#  test_data  <- bake(trained_rec, new_data = credit_test)

## ---- eval = FALSE-------------------------------------------------------
#  # Train
#  rf = rand_forest(mtry = 12, trees = 200) %>%
#    set_engine("ranger", importance = 'impurity') %>%
#    fit(Status ~ ., data = train_data)
#  
#  # Predict
#  prds = predict(rf, test_data)

## ---- eval = FALSE-------------------------------------------------------
#  library("data.table")
#  library("mlr3")
#  library("mlr3learners")
#  library("mlr3pipelines")
#  data("credit_data", package = "recipes")
#  set.seed(55)
#  
#  # Create the task
#  tsk = TaskClassif$new(id = "credit_task", target = "Status",
#    backend = as_data_backend(data.table(credit_data)))
#  
#  # Build up the Pipeline:
#  g = PipeOpImpute$new() %>>%
#    # PipeOpEncode$new() %>>%
#    # PipeOpScale$new() %>>%
#    PipeOpLearner$new(mlr_learners$get("classif.ranger"))
#  
#  # We can visualize what happens to the data using the `plot` function:
#  g$plot()
#  
#  # And we can use `mlr3's` full functionality be wrapping the Graph into a GraphLearner.
#  glrn = GraphLearner$new(g)
#  resample(tsk, glrn, mlr_resamplings$get("holdout"))

