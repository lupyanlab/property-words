#' Recode all of the factors in the experiment.
#'
#' @param frame A data.frame with columns: feat_type, mask_type, response_type
#' @importFrom dplyr `%>%`
#' @export
recode <- function(frame) {

  frame <- frame %>%
    recode_feat_type %>%
    recode_mask_type %>%
    recode_response_type %>%
    recode_correct_response %>%
    recode_version

  # Combine feat_type and mask_type for colors in the plot
  try({
    frame$feat_mask <- with(frame, paste(feat_type, mask_type, sep = ":"))
  }, silent = TRUE)

  frame
}

recode_feat_type <- function(frame) {
  feat_type_map <- dplyr::data_frame(
    feat_type = c("nonvisual", "visual"),
    feat_label = c("Encyclopedic Knowledge", "Visual Knowledge"),
    feat_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, feat_type_map), silent = TRUE)
  frame
}

recode_mask_type <- function(frame) {
  mask_type_map <- dplyr::data_frame(
    mask_type = c("nomask", "mask"),
    mask_c = c(-0.5, 0.5),
    mask_ordered = factor(mask_type, levels = c("nomask", "mask"))
  )
  try(frame <- dplyr::left_join(frame, mask_type_map), silent = TRUE)
  frame
}

recode_response_type <- function(frame) {
  response_type_map <- dplyr::data_frame(
    response_type = c("prompt", "word"),
    response_label = c("Answer proposition", "Word repetition"),
    response_c = c(-0.5, 0.5)
  )
  try(frame <- dplyr::left_join(frame, response_type_map), silent = TRUE)
  frame
}

recode_correct_response <- function(frame) {
  correct_response_map <- dplyr::data_frame(
    correct_response = c("no", "yes"),
    correct_response_c = c(-0.5, 0.5),
    correct_response_label = c("Different word", "Same word")
  )
  
  try(frame <- dplyr::left_join(frame, correct_response_map), silent = TRUE)
  frame
}

recode_version <- function(frame) {
  subj_id_nums <- as.numeric(stringr::str_extract(frame$subj_id, "[[:digit:]]{3}"))
  try(frame$version <- ifelse(subj_id_nums < 200, 1, 2), silent = TRUE)
  frame
}

add_sig_stars <- function(frame) {
  frame %>% mutate(
    sig = ifelse(p.value > 0.05, "",
          ifelse(p.value > 0.01, "*",
          ifelse(p.value > 0.001, "**",
                 "***"))))
}
