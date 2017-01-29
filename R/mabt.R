#' @import methods
#' @import knitr
#' @importFrom GoogleGenomics authenticate
#' @importFrom Biobase selectSome
#' @import SummarizedExperiment
#' @import bigrquery
#' @importFrom BiocGenerics nrow
#' @import dplyr
#' @import magrittr
#'
setOldClass("tbl_bigquery")
setClassUnion("intOrNULL", c("integer", "NULL"))
#'
#' A class for manipulating BigTable-resident assay and range metadata on features.
#'
#' @slot assay a \code{tbl_bigquery} instance/reference for a rectangular table
#' @slot rangeData a \code{tbl_bigquery} instance/reference for a rectangular table with information about rows of \code{assay}, including all information required to define a \code{\link[GenomicRanges]{GRanges-class}} instance
#' @slot project a character string naming the Google Cloud Platform project in use
#' @slot cached_nrow Because counting rows can be expensive, this integer slot is available for setting by \code{\link{set_cached_nrow}}
#' @export
#'
setClass("RangedBT", representation(assay="tbl_bigquery",
    rangeData="tbl_bigquery", project="character", cached_nrow="intOrNULL"))
#'
#' Simple constructor for RangedBT
#' @param project character tag for Google Cloud Platform project
#' @param src 'src_bigquery' instance
#' @param assayname name of table with assay data
#' @param rangename name of table with range meta data
#' @export
RangedBT = function(project, src, assayname, rangename) {
 new("RangedBT", assay=src %>% tbl(assayname), rangeData=
       src %>% tbl(rangename), project=project, cached_nrow=NULL)
}


#' @title 
#' Methods for RangedBT
#' @description 
#' Basic accessors
#' @name RangedBTmethods
NULL

#' simple accessors for assay and metadata components
#' @docType methods
#' @rdname RangedBTmethods 
#' @param x RangedBT instance
#' @param i ignored
#' @param \dots ignored
#' @export
setMethod("assay", "RangedBT", function(x, i, ...) x@assay)


#' @rdname RangedBTmethods 
#' @docType methods
#' @export
setMethod("rowRanges", "RangedBT", function(x, ...) x@rangeData)

#' @rdname RangedBTmethods 
#' @docType methods
#' @export
setGeneric("project", function(x) standardGeneric("project"))

#' @docType methods
#' @rdname RangedBTmethods 
#' @export
setMethod("project", "RangedBT", function(x) x@project)
#'

#' @rdname RangedBTmethods 
#' @export
assayname = function(x) as.character(assay(x)$ops[["x"]])
#'
#' extract character name of dataset in which RangedBT is defined
#' @rdname RangedBTmethods 
#' @export
datasetname = function(x) as.character(assay(x)$src$con@dataset)




#' @rdname RangedBTmethods 
#' @param object a RangedBT instance
#' @export
setMethod("show", "RangedBT", function(object) {
 cat("rangedBT instance.\n")
 cat("assay colnames include:\n")
 cat("   ", selectSome(assay(object) %>% colnames()), "\n")
 cat("rangeData colnames include:\n")
 cat("   ", selectSome(rowRanges(object) %>% colnames()), "\n")
})

#' @rdname RangedBTmethods
#' @export
setMethod("nrow", "RangedBT", function(x) {
 if (!is.null(x@cached_nrow)) return(x@cached_nrow)
 ans <- query_exec( query=paste("select count(*) from",  assayname(x), collapse=" "),
   project=project(x), default_dataset=paste(c(project(x), datasetname(x)), collapse=":"))
 ans = as.integer(unlist(ans))
 ans
})

#' @rdname RangedBTmethods
#' @param nr integer row count, not checked
#' @export
set_cached_nrow = function(x, nr) {
  x@cached_nrow = as.integer(nr)
  x
}
#
#bq = getBQ2()
#bano = RangedBT("cgc-05-0009", bq, "banovichSE_expressionData", "banovichSE_rowRanges")
#bano
#
#query_exec(query="select count(*) from banovichSE_expressionData", project="cgc-05-0009", default_dataset="cgc-05-0009:yriMulti")
#
