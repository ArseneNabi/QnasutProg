source("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut/scripts/diagnostic_pre_cholette_ere.R")

res <- diagnostiquer_pre_cholette_ere(
  project_dir = "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut",
  export_excel = FALSE
)

cat("\n=== NOMS CALAGE PRODUIT ===\n")
print(names(res$calage_produit))

cat("\n=== TOP PRODUITS AVEC ECARTS CRT/VPAP ===\n")
summary_prod <- res$calage_detail |>
  dplyr::filter(annee_complete, type_prix %in% c("crt", "vpap")) |>
  dplyr::group_by(bloc, Code_Produit, type_prix) |>
  dplyr::summarise(
    n_ecarts = sum(statut_calage == "ecart", na.rm = TRUE),
    ecart_abs_max = max(ecart_abs, na.rm = TRUE),
    ecart_abs_moyen = mean(dplyr::if_else(statut_calage == "ecart", ecart_abs, NA_real_), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ecart_abs_max = dplyr::if_else(is.infinite(ecart_abs_max), 0, ecart_abs_max),
    ecart_abs_moyen = dplyr::if_else(is.nan(ecart_abs_moyen), 0, ecart_abs_moyen)
  ) |>
  dplyr::arrange(dplyr::desc(n_ecarts), dplyr::desc(ecart_abs_max), bloc, Code_Produit, type_prix)
print(utils::head(summary_prod, 60), width = Inf)

cat("\n=== PRODUITS SUSPICION CRT=VPAP AVEC CIBLES DIFFERENTES ===\n")
print(
  utils::head(
    dplyr::arrange(
      res$alertes_egalite,
      dplyr::desc(abs_delta_cible_max),
      dplyr::desc(abs_delta_max)
    ),
    40
  ),
  width = Inf
)
