

#  ------------------------------------------------------------------------
#
# Title : Import ACR 90
#    By : PhM
#  Date : 2024-09-09
#
#  ------------------------------------------------------------------------


importph <- function() {
  rm(list = ls())
  library(readODS)
  library(tidyverse)
  library(janitor)
  library(baseph)
  library(lubridate)
  library(labelled)
  #
  nan <- c("NA", "ND", "", " ", "non demandé")

  tt <- read_ods("datas/ACR90.ods", sheet = 1, na = nan)
  bn <- read_ods("datas/ACR90.ods", sheet = 2)
  var_label(tt) <- bn$bnom
  #
  tt <-  tt |>
    janitor::clean_names() |>
    remove_constant() |>
    dplyr::mutate_if(is.character, as.factor) |>
    dplyr::select(!starts_with("temoin")) |>
    mutate(delai_mce = as.numeric(as.character(delai_mce))) |>
    mutate(horaire = as.numeric(str_split(horaire, "h", simplify = TRUE)[, 1])) |>
    mutate(horaire = as.factor(
      cut(
        horaire,
        include.lowest = TRUE,
        right = FALSE,
        dig.lab = 4,
        breaks = c(0, 8, 22, 24),
        labels = c("nuit", "jour", "nuit")
      )
    )) |>
    mutate(experience = fct_relevel(experience,
                                    "1-4 ans", "5-9 ans", ">10 ans"))
#
  var_label(tt$delai_mce) <- "Délais Initiation MCE"

  save(tt, bn, file = "datas/acr90.RData")
}

load(file = "datas/acr90.RData")



