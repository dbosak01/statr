# Freq Function -----------------------------------------------------------


#' @title Creates frequency statistics
#' @description The \code{freq} function creates frequency statistics in a
#' manner similar to the SAS® procedure PROC FREQ.
#' @param data The input data frame.
#' @param by The by variable for the procedure, wrapped in a \code{vars} function.
#' @param ... A set of \code{\link{exact}} functions defining the statistical
#' procedures to use.
#' @param tables The variables to use for the tables parameter, wrapped in
#' a \code{\link{requests}} object.
#' @param test The tests to use for this procedure.
#' @param weight Which if any variables to weight the procedure.  Wrap in
#' a \code{vars} function if there is more than one.
#' @param options The options to use for this procedure, wrapped in an
#' \code{\link{opts}} function.
#' @return A frequency object.
#' @examples
#' # freq (data=d1,
#' #       by=vars(v1, v2),
#' #       exact(fisher, fisherout, opts(ci)),
#' #       exact(chisq, chisqout),
#' #       tables = requests(v1, v2, v1 * v2),
#' #       test=list(),
#' #       weight=vars(cnt1),
#' #       options=opts()
#' # )
#' @export
freq <- function(data,
                 by = NULL,
                 ...,
                 tables = NULL,
                 test = NULL,
                 weight = NULL,
                 options = NULL) {

  fr  <- structure(list(), class = c("freq", "list"))

  exts <- list(...)

  fr$data <- data
  fr$by <- getVars(by)
  fr$exacts <- exts
  fr$tables <- tables
  fr$test <- test
  fr$weight <- getVars(weight)
  fr$options <- options
  fr$results <- getFrequencies(fr)

  return(fr)
}



# Calculation Functions ----------------------------------------------------


#' @noRd
getFrequencies <- function(obj) {

  if (!"freq" %in% class(obj))
    stop("Input object must be of class 'freq'")


 ret <- table(obj$data[ , obj$by])


 return(ret)

}


# Input Objects -----------------------------------------------------------


#' @title Define a set of requests for a statistical function
#' @description The \code{requests} function is used to define
#' requests for the \code{table} parameter on the \code{freq} function.
#' @param ... An indefinite number of variables and/or variable interactions.
#' @return A character vector of variable names and/or variable interactions.
#' @export
requests <- function(...) {

  lst <- as.character(substitute(c(...), env = environment()))
  lst <- lst[2:length(lst)]


  return(lst)

}



#' @title Define options for a statistical function
#' @description The \code{opts} function is used to define
#' options for a number of different functions.
#' @param ... An indefinite number of unquoted options.
#' @return The a character vector of options.
#' @export
opts <- function(...) {
  lst <- as.character(substitute(c(...), env = environment()))
  lst <- lst[2:length(lst)]

  ret <- structure(lst, class = c("opts", "list"))

  return(ret)
}


#' @title Define statistics for a statistical procedure
#' @description The \code{exact} function is used to define
#' statistics for a statistical procedure.
#' @param name The name of the statistical function
#' @param output The name of the output data frame
#' @param options The options to use for this procedure, wrapped in
#' an \code{\link{opts}} function.
#' @return An "exact" object.
#' @export
exact <- function(name, output = NULL, options = NULL) {

  if (!is.null(options))
    if (!is.opts(options))
      stop("Invalid value for options parameter.")

  ex <- structure(list(), class = c("exact", "list"))

  ex$name <- deparse1(substitute(name, env = environment()))
  ex$options <- options
  ex$output <- deparse1(substitute(output, env = environment()))

  return(ex)
}



# Utilities ---------------------------------------------------------------

#' @title Test whether object is of class "exact"
#' @param x The object to test
#' @return A TRUE or FALSE value, indicating whether the object is
#' of class "exact"
#' @export
is.exact <- function(x) {

  ret <- FALSE
  if ("exact" %in% class(x))
    ret <- TRUE

  return(ret)

}

#' @title Test whether object is of class "opts"
#' @param x The object to test
#' @return A TRUE or FALSE value, indicating whether the object is
#' of class "opts"
#' @export
is.opts <- function(x) {

  ret <- FALSE
  if ("opts" %in% class(x))
    ret <- TRUE

  return(ret)

}


#' @title Turn vars() object into vector of variable strings
#' @param x vars() object
#' @return Vector of variable names as strings
#' @import rlang
#' @noRd
getVars <- function(x) {

  ret <- c()
  for (i in seq_along(x)) {
    ret[[length(ret) + 1]] <- as.character(quo_get_expr(x[[i]]))

  }
  ret <- unlist(ret)

  return(ret)
}


#' @title Print a freq class
#' @param x The freq object to print.
#' @param ... Any follow on parameters.
#' @param verbose Whether to print the object in list form.
#' @return The freq object, invisibly
#' @export
print.freq <- function(x, ..., verbose = FALSE) {

  if (verbose == TRUE)
    print(unclass(x))
  else {

    cat("# class 'freq':\n")
    cat(paste0("- by: ", paste(x$by, collapse = " "), "\n"))

  }

  invisible(x)
}

#' @title Print an exact class
#' @param x The object to print.
#' @param ... Any follow on parameters.
#' @param verbose Whether to print the object in list form.
#' @return The exact object, invisibly
#' @export
print.exact <- function(x, ..., verbose = FALSE) {

  print(unclass(x))

  invisible(x)

}

