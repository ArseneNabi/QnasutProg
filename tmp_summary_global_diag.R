source("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut/scripts/diagnostic_global_prix_volume_ere.R")

res <- diagnostiquer_global_prix_volume_ere(
  project_dir = "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut",
  export_excel = FALSE
)

cat("n_egalites_post_base=", nrow(res$egalites_post_base), "\n", sep = "")

cat("\n=== SYNTHESE STAGES ===\n")
print(utils::head(res$synthese_stages, 20))

cat("\n=== CHAINAGE RESUME ===\n")
print(utils::head(res$chainage_resume, 20))

cat("\n=== TOP EGALITES POST BASE ===\n")
print(
  utils::head(
    dplyr::select(
      res$egalites_post_base,
      stage, code, composante, annee, valeur_crt, valeur_vpap, valeur_ch
    ),
    30
  )
)

cat("\n=== TOP ANOMALIES DE CHAINAGE VPAP ===\n")
print(
  utils::head(
    res$chainage_detail |>
      dplyr::filter(abs_ecart_vpap > 1e-6 | abs_ecart_ch > 1e-6) |>
      dplyr::select(
        stage, code, composante, annee, trimestre,
        valeur_crt, valeur_vpap, valeur_ch,
        vpap_calcule, ch_calcule,
        abs_ecart_vpap, abs_ecart_ch
      ) |>
      dplyr::arrange(dplyr::desc(abs_ecart_vpap), dplyr::desc(abs_ecart_ch)),
    40
  )
)
