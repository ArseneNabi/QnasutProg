suppressPackageStartupMessages({
  library(QnaSut)
  library(dplyr)
  library(readxl)
})

source("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut/scripts/diagnostic_pre_cholette_ere.R")

contexte <- .construire_contexte_pre_cholette_ere("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
path_cna <- contexte$donnees$paths$path_cna

headers_raw <- readxl::read_excel(path_cna, sheet = "CnaEre", n_max = 3, col_names = FALSE, .name_repair = "minimal")
headers_t <- t(headers_raw) |> as.data.frame()
colnames(headers_t) <- c("Type_Prix", "Agregat", "Produit")

cat("=== TYPES DE PRIX BRUTS DANS CnaEre ===\n")
print(sort(unique(trimws(headers_t$Type_Prix))))

cna_ere_long <- .construire_cibles_annuelles_pre_cholette_ere(contexte$donnees$cna_ere_struct)

cat("\n=== EXEMPLE AA000 / CF Marchande Menage Prix d'acquisition ===\n")
print(
  cna_ere_long |>
    dplyr::filter(
      Code_Produit == "AA000",
      composante == "CF Marchande Menage Prix d'acquisition"
    ) |>
    dplyr::select(type_prix, annee, valeur_annuelle_cible) |>
    tidyr::pivot_wider(names_from = type_prix, values_from = valeur_annuelle_cible)
)

cat("\n=== NOMBRE DE LIGNES NON NULLES POST-2015 OU CRT = VPAP DANS CnaEre ===\n")
print(
  cna_ere_long |>
    dplyr::filter(type_prix %in% c("crt", "vpap")) |>
    dplyr::select(type_prix, annee, Code_Produit, composante, valeur_annuelle_cible) |>
    tidyr::pivot_wider(names_from = type_prix, values_from = valeur_annuelle_cible) |>
    dplyr::filter(
      annee > 2015,
      (abs(crt) > 1e-9 | abs(vpap) > 1e-9)
    ) |>
    dplyr::summarise(
      n_lignes = dplyr::n(),
      n_egalites = sum(abs(crt - vpap) <= 1e-9, na.rm = TRUE)
    )
)

cat("\n=== COMPARAISON CRT / VPAP / CH DANS CnaEre APRES 2015 ===\n")
print(
  cna_ere_long |>
    dplyr::filter(type_prix %in% c("crt", "vpap", "ch")) |>
    dplyr::select(type_prix, annee, Code_Produit, composante, valeur_annuelle_cible) |>
    tidyr::pivot_wider(names_from = type_prix, values_from = valeur_annuelle_cible) |>
    dplyr::filter(
      annee > 2015,
      (abs(crt) > 1e-9 | abs(vpap) > 1e-9 | abs(ch) > 1e-9)
    ) |>
    dplyr::summarise(
      n_lignes = dplyr::n(),
      n_crt_eq_vpap = sum(abs(crt - vpap) <= 1e-9, na.rm = TRUE),
      n_crt_eq_ch = sum(abs(crt - ch) <= 1e-9, na.rm = TRUE),
      n_vpap_eq_ch = sum(abs(vpap - ch) <= 1e-9, na.rm = TRUE)
    )
)
