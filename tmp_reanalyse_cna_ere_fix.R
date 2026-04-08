library(readxl)
library(dplyr)
library(tidyr)
library(devtools)

project_dir <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut"
outil_dir <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/OutilCntBfa"

devtools::load_all(project_dir, quiet = TRUE)

source(file.path(project_dir, "scripts", "diagnostic_pre_cholette_ere.R"))
source(file.path(project_dir, "scripts", "diagnostic_global_prix_volume_ere.R"))

cfg <- QnaSut::load_config(file.path(outil_dir, "config.yml"))
imports <- QnaSut::charger_donnees_cnt(cfg)
cna_ere <- imports$cna_ere_struct

types_map <- c(
  CnaErECrt = "crt",
  CnaErEVol = "vpap",
  CnaErECh = "ch"
)

detail_source <- purrr::imap_dfr(cna_ere, function(comp_data, composante) {
  purrr::imap_dfr(comp_data, function(tbl, type_prix) {
    if (!type_prix %in% names(types_map) || !is.data.frame(tbl)) {
      return(tibble())
    }
    type_simple <- unname(types_map[[type_prix]])
    tbl |>
      tibble::as_tibble() |>
      rename(annee = 1) |>
      pivot_longer(
        cols = -annee,
        names_to = "Code_Produit",
        values_to = "valeur"
      ) |>
      mutate(
        composante = composante,
        type_prix = type_simple
      )
  })
})

detail_wide <- detail_source |>
  select(annee, Code_Produit, composante, type_prix, valeur) |>
  tidyr::pivot_wider(names_from = type_prix, values_from = valeur) |>
  mutate(
    post_base = annee > 2015,
    non_zero = coalesce(crt, 0) != 0 | coalesce(vpap, 0) != 0 | coalesce(ch, 0) != 0,
    eq_crt_vpap = dplyr::near(crt, vpap, tol = 1e-9),
    eq_crt_ch = dplyr::near(crt, ch, tol = 1e-9),
    eq_vpap_ch = dplyr::near(vpap, ch, tol = 1e-9)
  )

post_base_non_zero <- detail_wide |>
  filter(post_base, non_zero)

cat("SOURCE_POST_BASE_NON_ZERO=", nrow(post_base_non_zero), "\n", sep = "")
cat("SOURCE_EQ_CRT_VPAP=", sum(post_base_non_zero$eq_crt_vpap, na.rm = TRUE), "\n", sep = "")
cat("SOURCE_EQ_CRT_CH=", sum(post_base_non_zero$eq_crt_ch, na.rm = TRUE), "\n", sep = "")
cat("SOURCE_EQ_VPAP_CH=", sum(post_base_non_zero$eq_vpap_ch, na.rm = TRUE), "\n", sep = "")

cat("SOURCE_SAMPLE_AA000=\n")
print(
  detail_wide |>
    filter(Code_Produit == "AA000", composante == "CF Marchande Menage Prix d'acquisition", annee >= 2015) |>
    arrange(annee) |>
    select(annee, crt, vpap, ch)
)

pre_path <- file.path(project_dir, "Diagnostic_PreCholette_ERE_20260408.xlsx")
global_path <- file.path(project_dir, "Diagnostic_Global_Prix_Volume_ERE_20260408.xlsx")

pre_res <- diagnostiquer_pre_cholette_ere(
  project_dir = project_dir,
  output_excel = pre_path
)

global_res <- diagnostiquer_global_prix_volume_ere(
  project_dir = project_dir,
  output_excel = global_path
)

cat("PRE_ALERTES_CALAGE=", nrow(pre_res$alertes_calage), "\n", sep = "")
cat("PRE_ALERTES_EGALITE=", nrow(pre_res$detail_crt_vpap |>
  filter(annee > 2015, coalesce(abs_diff_annuelle, 0) == 0, coalesce(abs_ecart_trim_max, 0) == 0)), "\n", sep = "")

cat("GLOBAL_EGALITES_POST_BASE=", nrow(global_res$egalites_post_base), "\n", sep = "")
cat("GLOBAL_TOP_STAGES=\n")
print(
  global_res$synthese_stages |>
    select(stage, n_lignes, n_post_base_non_zero, n_eq_crt_vpap, part_eq_crt_vpap_post_base) |>
    arrange(desc(part_eq_crt_vpap_post_base))
)

cat("GLOBAL_TOP_EGALITES=\n")
print(
  global_res$egalites_post_base |>
    select(stage, annee, Code_Produit, composante, crt, vpap, ch) |>
    head(20)
)
