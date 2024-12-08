% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/branch_model.R
\name{branch_model}
\alias{branch_model}
\title{Branch (i.e. copy) a parsli SLiM model.}
\usage{
branch_model(slim_model, branch_name)
}
\arguments{
\item{slim_model}{An EidosModel object.}

\item{branch_name}{Name for the branched model.}
}
\value{
A complete copy of the slim_model, with all references to the
original model severed. Branched models are entirely separate.
}
\description{
Branches (copies) a parsli SLiM model with all its components.
}
