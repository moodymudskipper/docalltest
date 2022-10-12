#' @import rlang
#' @importFrom utils head
NULL

#' @export
make_dots <- function(...) environment()$...

#' @export
update_dots <- function(...) {
  assign("...", environment()$..., parent.frame())
}

#' @export
as_dots <- function(x) {
  do.call(function(...) environment()$..., x)
}

#' @export
unroll <- function(call) {
  call <- substitute(call)
  dots <- call[["..."]]
  i <- which(names(call) == "...")
  if (length(i) != 1) stop("A named `...` arg is expected once and only once")
  call1 <- as.list(call)[1:(i-1)]
  call2 <- eval.parent(call[["..."]])
  l <- length(call)
  call3 <- if (i < l) as.list(call)[(i+1):l]
  new_call <- as.call(c(call1, call2, call3))
  rlang::eval_bare(new_call, parent.frame())
}

docall_benchmark <- function() {
  cars <- data.frame(
    speed = c(
      4, 4, 7, 7, 8, 9, 10, 10, 10, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14,
      14, 14, 15, 15, 15, 16, 16, 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 20, 20,
      20, 20, 20, 22, 23, 24, 24, 24, 24, 25
    ),
    dist = c(
      2, 10, 4, 22, 16, 10, 18, 26, 34, 17, 28, 14, 20, 24, 28, 26, 34, 34, 46, 26,
      36, 60, 80, 20, 26, 54, 32, 40, 32, 40, 50, 42, 56, 76, 84, 36, 46, 68, 32,
      48, 52, 56, 64, 66, 54, 70, 92, 93, 120, 85
    )
  )

  # rlang
  f_invoke1 <- function(...) rlang::invoke(head, list(cars, n=1))
  f_invoke2 <- function(...) purrr::invoke(head, list(cars, n=1))
  f_inject <- function(...) inject(head(!!!list(cars, n=1)))
  f_exec <- function(...) exec(head, !!!list(cars, n=1))

  # base
  f_as.call <- function(...) eval(as.call(c(quote(head), list(cars, n=1))))
  f_do.call <- function(...) do.call(head, list(cars, n=1))

  # experiment
  f_unroll <- function(...) unroll(head(... = list(cars, n=1)))
  f_update_dots <- function(...) {update_dots(cars, n=1) ; head(...)}
  f_as_dots <- function(...) {`...` <- as_dots(list(cars, n=1)) ; head(...)}
  f_make_dots <- function(...) {`...` <- make_dots(cars, n=1) ; head(...)}


  bench::mark(
    `rlang::invoke` =  f_invoke1(),
    "\u001b[103munroll\u001b[49m" =  f_unroll(),
    `purrr::invoke` =  f_invoke2(),
    `rlang::inject` =  f_inject(),
    `rlang::exec` = f_exec(),
    "\u001b[103mas_dots\u001b[49m" = f_as_dots(),
    as.call = f_as.call(),
    "\u001b[103mupdate_dots\u001b[49m" = f_update_dots(),
    do.call = f_do.call(),
    "\u001b[103mmake_dots\u001b[49m" = f_make_dots(),
    min_time = Inf, max_iterations = 10000, relative = TRUE
  ) [-(10:13)]
}

#' @export
docall_benchmark_head <- function() {
  diamonds <- ggplot2::diamonds
  g <- head

  # rlang
  f_invoke1 <- function(...) rlang::invoke(g, list(diamonds))
  f_invoke2 <- function(...) purrr::invoke(g, list(diamonds))
  f_inject <- function(...) inject(g(!!!list(diamonds)))
  f_exec <- function(...) exec(g, !!!list(diamonds))

  # base
  f_as.call <- function(...) eval(as.call(c(quote(g), list(diamonds))))
  f_do.call <- function(...) do.call(g, list(diamonds))

  # experiment
  f_unroll <- function(...) unroll(g(... = list(diamonds)))
  f_update_dots <- function(...) {update_dots(diamonds) ; g(...)}
  f_as_dots <- function(...) {`...` <- as_dots(list(diamonds)) ; g(...)}
  f_make_dots <- function(...) {`...` <- make_dots(diamonds) ; g(...)}


  bench::mark(
    `rlang::invoke` =  f_invoke1(),
    "\u001b[103munroll\u001b[49m" =  f_unroll(),
    `purrr::invoke` =  f_invoke2(),
    `rlang::inject` =  f_inject(),
    `rlang::exec` = f_exec(),
    "\u001b[103mas_dots\u001b[49m" = f_as_dots(),
    "\u001b[103mupdate_dots\u001b[49m" = f_update_dots(),
    as.call = f_as.call(),
    do.call = f_do.call(),
    "\u001b[103mmake_dots\u001b[49m" = f_make_dots(),
    min_time = Inf, max_iterations = 10000, relative = TRUE
  ) [-(10:13)]
}

#' @export
docall_benchmark_nrow <- function() {
  diamonds <- ggplot2::diamonds
  g <- nrow

  # rlang
  f_invoke1 <- function(...) rlang::invoke(g, list(diamonds))
  f_invoke2 <- function(...) purrr::invoke(g, list(diamonds))
  f_inject <- function(...) inject(g(!!!list(diamonds)))
  f_exec <- function(...) exec(g, !!!list(diamonds))

  # base
  f_as.call <- function(...) eval(as.call(c(quote(g), list(diamonds))))
  f_do.call <- function(...) do.call(g, list(diamonds))

  # experiment
  f_unroll <- function(...) unroll(g(... = list(diamonds)))
  f_update_dots <- function(...) {update_dots(diamonds) ; g(...)}
  f_as_dots <- function(...) {`...` <- as_dots(list(diamonds)) ; g(...)}
  f_make_dots <- function(...) {`...` <- make_dots(diamonds) ; g(...)}


  bench::mark(check = FALSE,
              `rlang::invoke` =  f_invoke1(),
              "\u001b[103munroll\u001b[49m" =  f_unroll(),
              `purrr::invoke` =  f_invoke2(),
              `rlang::inject` =  f_inject(),
              `rlang::exec` = f_exec(),
              "\u001b[103mas_dots\u001b[49m" = f_as_dots(),
              "\u001b[103mupdate_dots\u001b[49m" = f_update_dots(),
              as.call = f_as.call(),
              do.call = f_do.call(),
              "\u001b[103mmake_dots\u001b[49m" = f_make_dots(),
              min_time = Inf, max_iterations = 10000, relative = TRUE
  ) [-(10:13)]
}

#' @export
docall_benchmark_match.call <- function() {
  diamonds <- ggplot2::diamonds
  g <- function(x, ...) {
    match.call()
  }

  # rlang
  f_invoke1 <- function(...) rlang::invoke(g, list(diamonds))
  f_invoke2 <- function(...) purrr::invoke(g, list(diamonds))
  f_inject <- function(...) inject(g(!!!list(diamonds)))
  f_exec <- function(...) exec(g, !!!list(diamonds))

  # base
  f_as.call <- function(...) eval(as.call(c(quote(g), list(diamonds))))
  f_do.call <- function(...) do.call(g, list(diamonds))

  # experiment
  f_unroll <- function(...) unroll(g(... = list(diamonds)))
  f_update_dots <- function(...) {update_dots(diamonds) ; g(...)}
  f_as_dots <- function(...) {`...` <- as_dots(list(diamonds)) ; g(...)}
  f_make_dots <- function(...) {`...` <- make_dots(diamonds) ; g(...)}


  bench::mark(check = FALSE,
    `rlang::invoke` =  f_invoke1(),
    "\u001b[103munroll\u001b[49m" =  f_unroll(),
    `purrr::invoke` =  f_invoke2(),
    `rlang::inject` =  f_inject(),
    "\u001b[103mas_dots\u001b[49m" = f_as_dots(),
    `rlang::exec` = f_exec(),
    "\u001b[103mupdate_dots\u001b[49m" = f_update_dots(),
    as.call = f_as.call(),
    do.call = f_do.call(),
    "\u001b[103mmake_dots\u001b[49m" = f_make_dots(),
    min_time = Inf, max_iterations = 10000, relative = TRUE
  ) [-(10:13)]
}
