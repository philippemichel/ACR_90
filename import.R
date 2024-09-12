

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
  #
  tt <-  tt |>
    janitor::clean_names() |>
    remove_constant() |>
    mutate(across(where(is.character) & !starts_with("temoin"), as.factor)) |>
    mutate(delai_mce = as.numeric(as.character(delai_mce))) |>
    mutate(periode = as.numeric(str_split(horaire, "h", simplify = TRUE)[, 1])) |>
    mutate(periode = as.factor(
      cut(
        periode,
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
  bn <- c("id", "Délai decrochage/ reconnaissance ACR", "Motif de l'appel", "En faveur d'un ACR", "En défaveur d'un ACR", "Délais d'initiation du MCE",  "Ton de l'appel", "Sexe de l'ARM", "ancienneté de l'ARM", "Horaire", "Période")
  var_label(tt) <- bn

  save(tt, bn, file = "datas/acr90.RData")
}

importph()
load(file = "datas/acr90.RData")



