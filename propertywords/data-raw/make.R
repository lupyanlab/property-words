library(dplyr)
library(devtools)
load_all()

make <- function(data_dir) {
  compile(data_dir) %>% clean %>% recode
}

make_printed <- function(overwrite = FALSE) {
  printed <- make("data-raw/printed-word")
  use_data(printed, overwrite = overwrite)
}

make_audio <- function(overwrite = FALSE) {
  audio <- make("data-raw/audio-word")
  use_data(audio, overwrite = overwrite)
}

make_propertywords <- function(overwrite = FALSE) {
  printed <- make("data-raw/printed-word")
  audio <- make("data-raw/audio-word")
  propertywords <- plyr::rbind.fill(printed, audio)
  use_data(propertywords, overwrite = overwrite)
}

make_all <- function(overwrite = FALSE) {
  make_printed(overwrite)
  make_audio(overwrite)
  make_propertywords(overwrite)
}
