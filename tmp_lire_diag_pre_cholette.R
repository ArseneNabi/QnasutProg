source("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut/scripts/diagnostic_pre_cholette_ere.R")

res <- diagnostiquer_pre_cholette_ere(
  project_dir = "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut",
  output_excel = "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut/Diagnostic_PreCholette_ERE_TEST.xlsx",
  export_excel = FALSE
)

cat("alertes_calage=", nrow(res$alertes_calage), "\n", sep = "")
cat("alertes_egalite=", nrow(res$alertes_egalite), "\n", sep = "")

cat("\n=== ALERTES EGALITE ===\n")
print(utils::head(res$alertes_egalite, 20))

cat("\n=== ALERTES CALAGE ===\n")
print(
  utils::head(
    dplyr::arrange(res$alertes_calage, dplyr::desc(ecart_abs)),
    20
  )
)

cat("\n=== SYNTHESE CALAGE PAR BLOC/TYPE ===\n")
print(
  res$calage_detail |>
    dplyr::group_by(bloc, type_prix, statut_calage) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(bloc, type_prix, statut_calage)
)

cat("\n=== TOP ECARTS CRT/VPAP SUR ANNEES COMPLETES ===\n")
print(
  utils::head(
    res$calage_detail |>
      dplyr::filter(
        annee_complete,
        type_prix %in% c("crt", "vpap"),
        statut_calage == "ecart"
      ) |>
      dplyr::select(
        bloc, type_prix, Code_Produit, composante, annee,
        somme_trimestrielle, valeur_annuelle_cible, ecart_annuel, ecart_abs
      ) |>
      dplyr::arrange(dplyr::desc(ecart_abs)),
    30
  )
)

cat("\n=== GRAVITE DES ECARTS CRT/VPAP ===\n")
print(
  res$calage_detail |>
    dplyr::filter(
      annee_complete,
      type_prix %in% c("crt", "vpap"),
      statut_calage == "ecart"
    ) |>
    dplyr::mutate(
      classe_ecart = dplyr::case_when(
        ecart_abs <= 1 ~ "<= 1",
        ecart_abs <= 100 ~ "1 - 100",
        ecart_abs <= 1000 ~ "100 - 1000",
        TRUE ~ "> 1000"
      )
    ) |>
    dplyr::count(bloc, type_prix, classe_ecart) |>
    dplyr::arrange(bloc, type_prix, classe_ecart)
)

cat("\n=== TOP EGALITES CRT/VPAP ===\n")
top_egalites <- dplyr::arrange(
  dplyr::filter(res$egalite_crt_vpap$synthese, part_trim_identiques >= 0.75) |>
    dplyr::select(
      bloc, Code_Produit, composante, n_trim, n_trim_identiques,
      part_trim_identiques, abs_delta_max, abs_delta_moyen,
      n_annees_cibles, n_annees_cibles_identiques, abs_delta_cible_max
    ),
  dplyr::desc(abs_delta_cible_max),
  dplyr::desc(abs_delta_max)
)
print(top_egalites, n = 20, width = Inf)
