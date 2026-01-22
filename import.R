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
  nan <- c("NA", "ND", "", "non connu")

  tt <- read_ods("datas/ACR90_BDD_20251218.ods", sheet = 1, na = nan)
  #
  tt <- tt |>
    janitor::clean_names() |>
    remove_constant() |>
    dplyr::rename(
      delai_mce = delais_initiation_mce,
      delai_reconnaissance = delai_decrochage_reconnaissance_acr,
      en_faveur_acr = donnees_du_temoin_en_faveur_acr,
      en_defaveur_acr = donnees_du_temoin_en_defaveur_acr,
      delai_dae = delai_demande_dae,
      lien_appelant_patient = lien_entre_lappelant_et_le_patient,
      delai_reconnaissance = delai_decrochage_reconnaissance_acr
    ) |>
    mutate(age = as.numeric(age)) |>
    mutate(across(where(is.character), as.factor)) |>
    mutate(experience_arm = fct_relevel(
      experience_arm,
      "1-4 ans", "5-9 ans", ">10 ans"
    )) |>
    mutate(delai90 = as.factor(ifelse(delai_reconnaissance < 91,
      "< 90 sec", "> 90 sec"
    )))

  bn <- c(
    "id", "Délai decrochage/ reconnaissance ACR", "Motif de l'appel", "En faveur d'un ACR", "En défaveur d'un ACR",
    "DAE", "Initiation du MCE",
    "Délais d'initiation du MCE", "Ton de l'appel",
    "Sexe de l'ARM", "Expérience de l'ARM",
    "Localisation du patient", "Sexe du patient", "Âge du patient",
    "Lien", "Lien entre l'appelant et le patient",
    "Sexe de l'appelant",
    "Horaire", "Période de l'appel", "Délai de reconnaissance"
  )
  var_label(tt) <- bn
  faveur <- tt |>
    dplyr::select(en_faveur_acr, en_defaveur_acr) |>
    pivot_longer(
      cols = everything(),
      names_to = "type",
      values_to = "value"
    )

  tt <- tt |>
    dplyr::select(-c(en_faveur_acr, en_defaveur_acr))

  save(tt, faveur, bn, file = "datas/acr90.RData")
}

importph()
load(file = "datas/acr90.RData")
