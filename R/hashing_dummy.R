#' Indicator variables via feature hashing
#'
#' `step_dummy_hash` creates a *specification* of a recipe step that
#'  will convert factors or character columns into a series of binary
#'  (or signed binary) indicator columns. 
#'
#' @template args-recipe
#' @template args-dots
#' @template args-role_predictors
#' @template args-trained
#' @template args-columns
#' @param signed A logical, indicating whether to use a signed
#' hash-function to reduce collisions when hashing. Defaults to TRUE.
#' @param num_terms An integer, the number of variables to output.
#'  Defaults to 32.
#' @template args-prefix
#' @template args-skip
#' @template args-id
#' 
#' @template returns
#' 
#' @details
#'  Feature hashing, or the hashing trick, is a transformation of a
#'  text variable into a new set of numerical variables. This is done by
#'  applying a hashing function over the values of the factor levels and 
#'  using the hash values as feature indices. This allows for a low memory 
#'  representation of the data and can be very helpful when a qualitative 
#'  predictor has many levels or is expected to have new levels during 
#'  prediction. This implementation is done using the MurmurHash3 method.
#' 
#'  The argument `num_terms` controls the number of indices that the hashing
#'  function will map to. This is the tuning parameter for this
#'  transformation. Since the hashing function can map two different tokens
#'  to the same index, will a higher value of `num_terms` result in a lower
#'  chance of collision.
#'  
#' @template details-prefix
#' 
#' @references Kilian Weinberger; Anirban Dasgupta; John Langford;
#'  Alex Smola; Josh Attenberg (2009).
#'  
#' @seealso [recipes::step_dummy()]
#' 
#' @examples
#' if (requireNamespace("text2vec", quietly = TRUE)) {
#'   library(recipes)
#'   library(modeldata)
#'   data(grants)
#'
#'   grants_rec <- recipe(~ sponsor_code, data = grants_other) %>%
#'     step_dummy_hash(sponsor_code)
#'
#'   grants_obj <- grants_rec %>%
#'     prep()
#'
#'   bake(grants_obj, grants_test)
#'
#'   tidy(grants_rec, number = 1)
#'   tidy(grants_rec, number = 1)
#' }
#' 
#' @export
step_dummy_hash <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           columns = NULL,
           signed = TRUE,
           num_terms = 32,
           prefix = "hash",
           skip = FALSE,
           id = rand_id("dummy_hash")) {
    recipes::recipes_pkg_check(required_pkgs.step_dummy_hash())
    
    add_step(
      recipe,
      step_dummy_hash_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        columns = columns,
        signed = signed,
        num_terms = num_terms,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_dummy_hash_new <-
  function(terms, role, trained, columns, signed, num_terms, prefix, skip,
           id) {
    step(
      subclass = "dummy_hash",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      signed = signed,
      num_terms = num_terms,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy_hash <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  
  recipes::check_type(training[, col_names], quant = FALSE)
  
  step_dummy_hash_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    signed = x$signed,
    num_terms = x$num_terms,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dummy_hash <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat
  
  for (i in seq_along(col_names)) {
    tf_text <- hashing_function(
      as.character(new_data[[ col_names[i] ]]),
      paste0(
        col_names[i], "_",
        names0(object$num_terms, object$prefix)
      ),
      object$signed,
      object$num_terms
    )
    
    new_data <-
      new_data[, !(colnames(new_data) %in% col_names[i]), drop = FALSE]
    
    new_data <- vctrs::vec_cbind(tf_text, new_data)
  }
  
  as_tibble(new_data)
}

#' @export
print.step_dummy_hash <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Feature hashing with ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_dummy_hash
#' @param x A `step_dummy_hash` object.
#' @export
tidy.step_dummy_hash <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$terms,
      value = x$signed,
      length = x$num_terms
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_lgl,
      length = na_int
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @keywords internal
#' @export
required_pkgs.step_dummy_hash <- function(x, ...) {
  c("text2vec", "textrecipes")
}

#' @rdname tunable.step
#' @keywords internal
#' @export
tunable.step_dummy_hash <- function(x, ...) {
  tibble::tibble(
    name = c("signed", "num_terms"),
    call_info = list(
      list(pkg = "dials", fun = "signed_hash"),
      list(pkg = "dials", fun = "num_hash", range = c(8, 12))
    ),
    source = "recipe",
    component = "step_dummy_hash",
    component_id = x$id
  )
}
