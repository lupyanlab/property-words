#' Tidy up the model predictions for plotting.
#'
#' @importFrom dplyr `%>%`
#' @param mod Model (lmer, glmer) to pass to predict function.
#' @param response_type prompt or word.
#' @return a tidy data frame containing all x predictors and y predictions.
#' @export
format_mod_preds <- function(mod, response_type) {
  stopifnot(response_type %in% c("prompt", "word"))
  if (response_type == "prompt") {
    x_preds <- prompt_x_preds()
  } else {
    x_preds <- word_x_preds()
  }

  x_preds <- x_preds %>% recode

  y_preds <- AICcmodavg::predictSE(mod, x_preds, se = TRUE, print.matrix = TRUE)
  preds <- cbind(x_preds, y_preds) %>%
    dplyr::rename(estimate = fit, se = se.fit)
  preds
}

prompt_x_preds <- function() {
  expand.grid(
    feat_type = c("visual", "nonvisual"),
    mask_type = c("mask", "nomask"),
    stringsAsFactors = FALSE
  )
}

word_x_preds <- function() {
  # In V2, the prompt is replaced with dashes so there is no
  # feat_type on those trials.
  expand.grid(
    response_type = "word",
    feat_type = NA,
    mask_type = c("mask", "nomask"),
    correct_response = c("no", "yes"),
    stringsAsFactors = FALSE
  )
}
