% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/po.R
\name{po}
\alias{po}
\alias{pos}
\title{Shorthand PipeOp Constructor}
\usage{
po(.obj, ...)

pos(.objs, ...)
}
\arguments{
\item{.obj}{\verb{[any]}\cr
The object from which to construct a \code{PipeOp}. If this is a
\code{character(1)}, it is looked up in the \code{\link{mlr_pipeops}} dictionary.
Otherwise, it is converted to a \code{\link{PipeOp}}.}

\item{...}{\code{any}\cr
Additional parameters to give to constructed object.
This may be an argument of the constructor of the
\code{PipeOp}, in which case it is given to this constructor;
or it may be a parameter value, in which case it is
given to the \code{param_vals} argument of the constructor.}

\item{.objs}{\code{character} | \code{list}\cr
Either a \code{character} of \code{\link{PipeOp}}s to look up in \code{\link{mlr_pipeops}},
or a list of other objects to be converted to a \code{\link{PipeOp}}.
If this is a named \code{list}, then the names are used as \verb{$id} slot for the resulting
\code{\link{PipeOp}}s.}
}
\value{
A \code{\link{PipeOp}} (for \code{po()}), or a \code{list} of \code{\link{PipeOp}}s (for \code{pos()}).
}
\description{
Create
\itemize{
\item a \code{PipeOp} from \code{mlr_pipeops} from given ID
\item a \code{PipeOpLearner} from a \code{Learner} object
\item a \code{PipeOpFilter} from a \code{Filter} object
\item a \code{PipeOpSelect} from a \code{Selector} object
\item a clone of a \code{PipeOp} from a given \code{PipeOp} (possibly with changed settings)
}

The object is initialized with given parameters and \code{param_vals}.

\code{po()} taks a single \code{obj} (\code{PipeOp} id, \code{\link[mlr3:Learner]{Learner}}, ...) and converts
it to a \code{\link{PipeOp}}. \code{pos()} (with plural-s) takes either a \code{character}-vector, or a
list of objects, and creates a \code{list} of \code{\link{PipeOp}}s.
}
\examples{
\dontshow{ if (requireNamespace("rpart")) \{ }
library("mlr3")

po("learner", lrn("classif.rpart"), cp = 0.3)

po(lrn("classif.rpart"), cp = 0.3)

# is equivalent with:
mlr_pipeops$get("learner", lrn("classif.rpart"),
  param_vals = list(cp = 0.3))

mlr3pipelines::pos(c("pca", original = "nop"))
\dontshow{ \} }
}
