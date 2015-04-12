#' Parse, retaining comments.
#'
#' Works very similarly to parse, but also keeps original formatting and
#' comments.
#'
#' @param x object to parse.  Can be a string, a file connection, or a
#'   function
#' @return a data.frame with columns \code{src}, the source code, and
#'   \code{expr}
#' @export
parse_all <- function(x) UseMethod("parse_all")

#' @export
parse_all.character <- function(x) {
  if (length(x) == 0) {
    return(data.frame(
      src = character(), expr = I(expression()),
      stringsAsFactors = FALSE
    ))
  }
  if (length(grep("\n", x)))
    x <- unlist(str_split(x, "\n"), recursive = FALSE, use.names = FALSE)
  x <- group_src(x)
  exprs <- lapply(x, parse_only)
  x <- unlist(x, recursive = FALSE, use.names = FALSE)
  n <- length(x)
  if (n > 1) {
    x <- paste(x, rep(c("\n", ""), c(n - 1, 1)), sep = "")
    if (x[n] == "") {
      x <- x[-n]
      exprs <- exprs[-n]
    }
  }
  res <- data.frame(src = x, stringsAsFactors = FALSE)
  res$expr <- exprs
  res
}

# group source lines into complete expressions (copied from highr:::group_src)
group_src <- function(code) {
  n <- length(code)
  if (n < 1) return(list(code))
  i <- i1 <- i2 <- 1
  x <- list()
  while (i2 <= n) {
    piece <- code[i1:i2]
    if (try_parse(piece)) {
      x[[i]] <- paste(piece, collapse = "\n")
      i <- i + 1
      i1 <- i2 + 1 # start from the next line
    }
    i2 <- i2 + 1
  }
  if (i1 <= n) parse(text = piece)  # must be an error there
  x
}

# whether a code expression can be parsed (copied from highr:::try_parse)
try_parse <- function(code) {
  !inherits(try(parse_only(code), silent = TRUE), "try-error")
}

# parse the code without storing srcref (copied from formatR:::parse_only)
parse_only <- function(code) {
  if (length(code) == 0) return(expression())
  op <- options(keep.source = FALSE); on.exit(options(op))
  base::parse(text = code, srcfile = NULL)
}

#' @export
parse_all.connection <- function(x) {
  if (!isOpen(x, "r")) {
      open(x, "r")
      on.exit(close(x))
  }
  text <- readLines(x)
  parse_all(text)
}

#' @export
parse_all.function <- function(x) {
  src <- attr(x, "srcref", exact = TRUE)
  if (is.null(src)) {
    src <- deparse(body(x))
    # Remove { and }
    n <- length(src)
    if (n >= 2) src <- src[-c(1, n)]
    parse_all(src)
  } else {
    src2 <- attr(body(x), "srcref", exact = TRUE)
    n <- length(src2)
    if (n >= 2) {
      parse_all(unlist(lapply(src2[-1], as.character)))
    } else if (n == 1) {
      # f <- function(...) {}
      parse_all(character(0))
    } else if (n == 0) {
      parse_all(deparse(body(x)))
    }
  }
}

#' @export
parse_all.default <- function(x) {
  parse_all(deparse(x))
}
