#' Generate ngrams from tokenlist
#'
#' `step_ngram` creates a *specification* of a recipe step that
#'  will convert a [tokenlist] into a list of ngram of tokens.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_no-new
#' @template args-trained
#' @template args-columns
#' @param num_tokens The number of tokens in the n-gram. This must be an integer
#'  greater than or equal to 1. Defaults to 3.
#' @param min_num_tokens The minimum number of tokens in the n-gram.
#'  This must be an integer greater than or equal to 1 and smaller than `n`.
#'  Defaults to 3.
#' @param delim The separator between words in an n-gram. Defaults to "_".
#' @template args-skip
#' @template args-id
#'
#' @template returns
#' 
#' @details
#'  The use of this step will leave the ordering of the tokens meaningless.
#'  If `min_num_tokens <  num_tokens` then the tokens order in increasing
#'  fashion with respect to the number of tokens in the n-gram. If
#'  `min_num_tokens = 1` and `num_tokens = 3` then the output contains all the
#'  1-grams followed by all the 2-grams followed by all the 3-grams.
#' 
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to tokenlist steps
#' 
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(okc_text)
#'
#' okc_rec <- recipe(~., data = okc_text) %>%
#'   step_tokenize(essay0) %>%
#'   step_ngram(essay0)
#'
#' okc_obj <- okc_rec %>%
#'   prep()
#'
#' bake(okc_obj, new_data = NULL, essay0) %>%
#'   slice(1:2)
#'
#' bake(okc_obj, new_data = NULL) %>%
#'   slice(2) %>%
#'   pull(essay0)
#'
#' tidy(okc_rec, number = 2)
#' tidy(okc_obj, number = 2)
#' 
#' @export
step_ngram <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           columns = NULL,
           num_tokens = 3L,
           min_num_tokens = 3L,
           delim = "_",
           skip = FALSE,
           id = rand_id("ngram")) {
    add_step(
      recipe,
      step_ngram_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_tokens = num_tokens,
        min_num_tokens = min_num_tokens,
        delim = delim,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_ngram_new <-
  function(terms, role, trained, columns, num_tokens, min_num_tokens, delim,
           skip, id) {
    step(
      subclass = "ngram",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      num_tokens = num_tokens,
      min_num_tokens = min_num_tokens,
      delim = delim,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_ngram <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_ngram_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    num_tokens = x$num_tokens,
    min_num_tokens = x$min_num_tokens,
    delim = x$delim,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ngram <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    ngrammed_tokenlist <- tokenlist_ngram(
      x = new_data[, col_names[i], drop = TRUE],
      n = object$num_tokens,
      n_min = object$min_num_tokens,
      delim = object$delim
    )

    new_data[, col_names[i]] <- tibble(ngrammed_tokenlist)
  }
  new_data <- factor_to_text(new_data, col_names)
  as_tibble(new_data)
}

#' @export
print.step_ngram <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("ngramming for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_ngram
#' @param x A `step_ngram` object.
#' @export
tidy.step_ngram <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$terms)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_chr
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_ngram <- function(x, ...) {
  c("textrecipes")
}

#' @rdname tunable.step
#' @export
tunable.step_ngram <- function(x, ...) {
  tibble::tibble(
    name = c("num_tokens"),
    call_info = list(
      list(pkg = "dials", fun = "num_tokens", range = c(1, 3))
    ),
    source = "recipe",
    component = "step_ngram",
    component_id = x$id
  )
}
