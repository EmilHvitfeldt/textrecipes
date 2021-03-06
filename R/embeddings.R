#' Pretrained word embeddings of tokens
#'
#' `step_word_embeddings` creates a *specification* of a recipe step that will
#' convert a [tokenlist] into word-embedding dimensions by aggregating the
#' vectors of each token from a pre-trained embedding.
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param embeddings A tibble of pre-trained word embeddings, such as those
#'   returned by the embedding_glove function function from the textdata
#'   package The first column should contain tokens, and additional columns
#'   should contain embeddings vectors.
#' @param aggregation A character giving the name of the aggregation function to
#'   use. Must be one of "sum", "mean", "min", and "max". Defaults to "sum".
#' @param aggregation_default A numeric denoting the default value for case with
#'   no words are matched in embedding. Defaults to 0.
#' @template args-prefix
#' @template args-skip
#' @template args-id
#'
#' @template returns
#' 
#' @details Word embeddings map words (or other tokens) into a high-dimensional
#'   feature space. This function maps pre-trained word embeddings onto the
#'   tokens in your data.
#'
#'   The argument `embeddings` provides the pre-trained vectors. Each dimension
#'   present in this tibble becomes a new feature column, with each column
#'   aggregated across each row of your text using the function supplied in the
#'   `aggregation` argument.
#'
#'   The new components will have names that begin with `prefix`, then the name
#'   of the aggregation function, then the name of the variable from the
#'   embeddings tibble (usually something like "d7"). For example, using the
#'   default "word_embeddings" prefix, the "sum" aggregation, and the GloVe
#'   embeddings from the textdata package (where the column names are `d1`,
#'   `d2`, etc), new columns would be `word_embeddings_sum_d1`,
#'   `word_embeddings_sum_d2`, etc.
#'   
#' @seealso [step_tokenize()] to turn character into tokenlist.
#' @family tokenlist to numeric steps
#' 
#' @examples
#' library(recipes)
#'
#' embeddings <- tibble(
#'   tokens = c("the", "cat", "ran"),
#'   d1 = c(1, 0, 0),
#'   d2 = c(0, 1, 0),
#'   d3 = c(0, 0, 1)
#' )
#'
#' sample_data <- tibble(
#'   text = c(
#'     "The.",
#'     "The cat.",
#'     "The cat ran."
#'   ),
#'   text_label = c("fragment", "fragment", "sentence")
#' )
#'
#' rec <- recipe(text_label ~ ., data = sample_data) %>%
#'   step_tokenize(text) %>%
#'   step_word_embeddings(text, embeddings = embeddings)
#'
#' obj <- rec %>%
#'   prep()
#'
#' bake(obj, sample_data)
#'
#' tidy(rec, number = 2)
#' tidy(obj, number = 2)
#' 
#' @export
step_word_embeddings <- function(recipe,
                                 ...,
                                 role = "predictor",
                                 trained = FALSE,
                                 columns = NULL,
                                 embeddings,
                                 aggregation = c("sum", "mean", "min", "max"),
                                 aggregation_default = 0,
                                 prefix = "w_embed",
                                 skip = FALSE,
                                 id = rand_id("word_embeddings")) {
  # Validate the special inputs here to make sure nothing goes haywire further
  # downstream.
  if (
    !tibble::is_tibble(embeddings) ||
      !any(inherits(embeddings[[1]], c("character", "factor"), which = TRUE)) ||
      ncol(embeddings) == 1 ||
      !all(map_lgl(embeddings[, 2:ncol(embeddings)], is.numeric))
  ) {
    embeddings_message <- paste(
      "embeddings should be a tibble with 1 character or factor column and",
      "additional numeric columns."
    )
    rlang::abort(
      embeddings_message,
      .subclass = "bad_embeddings"
    )
  }

  aggregation <- match.arg(aggregation)

  add_step(
    recipe,
    step_word_embeddings_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      embeddings = embeddings,
      aggregation = aggregation,
      aggregation_default = aggregation_default,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

step_word_embeddings_new <- function(terms, role, trained, columns, embeddings,
                                     aggregation, aggregation_default, prefix,
                                     skip, id) {
  recipes::step(
    subclass = "word_embeddings",
    terms = terms,
    role = role,
    trained = trained,
    columns = columns,
    embeddings = embeddings,
    aggregation = aggregation,
    aggregation_default = aggregation_default,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_word_embeddings <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  check_list(training[, col_names])

  step_word_embeddings_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    embeddings = x$embeddings,
    aggregation = x$aggregation,
    aggregation_default = x$aggregation_default,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_word_embeddings <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    arggregation_fun <- switch(
      object$aggregation,
      sum = function(x, ...) {
        if (length(x) == 0) {
          return(object$aggregation_default)
        }
        sum(x, ...)
      },
      mean = function(x, ...) {
        if (length(x) == 0) {
          return(object$aggregation_default)
        }
        mean(x, ...)
      },
      min = function(x, ...) {
        if (length(x) == 0) {
          return(object$aggregation_default)
        }
        min(x, ...)
      },
      max = function(x, ...) {
        if (length(x) == 0) {
          return(object$aggregation_default)
        }
        max(x, ...)
      }
    )

    embeddings_columns <- tokenlist_embedding(
      new_data[, col_names[i], drop = TRUE],
      object$embeddings,
      arggregation_fun
    )

    colnames(embeddings_columns) <- paste(
      object$prefix,
      object$aggregation,
      colnames(embeddings_columns),
      sep = "_"
    )

    new_data <- vctrs::vec_cbind(new_data, embeddings_columns)

    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]
  }

  as_tibble(new_data)
}

#' @export
print.step_word_embeddings <- function(x,
                                       width = max(20, options()$width - 30),
                                       ...) {
  cat("Word embeddings aggregated from ", sep = "")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_word_embeddings
#' @param x A `step_word_embeddings` object.
#' @export
tidy.step_word_embeddings <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
      embeddings_rows = nrow(x$embeddings),
      aggregation = x$aggregation
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      embeddings_rows = nrow(x$embeddings),
      aggregation = x$aggregation
    )
  }
  res$id <- x$id
  res
}
