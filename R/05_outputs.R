#' Exporter les résultats vers Excel avec taux de croissance
#'
#' @description
#' Exporte une liste de data frames vers un fichier Excel multi-feuilles.
#' Pour chaque feuille contenant \code{valeur_cal}, un taux de croissance
#' trimestriel (\code{evol_trim_pct}) est ajouté automatiquement par code.
#' Pour la synthèse (colonnes \code{B1_crt}, \code{P1_crt}), les évolutions
#' sont calculées sur ces colonnes.
#'
#' @param list_df Liste nommée de data frames à exporter.
#' @param file_name Chemin du fichier Excel de sortie.
#"   Par d\u00e9faut \code{"Comptes_Trimestriels_Resultats.xlsx"}.
#'
#' @return Invisible : chemin du fichier exporté.
#' @export
export_results_excel <- function(list_df,
                                 file_name = "Comptes_Trimestriels_Resultats.xlsx") {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Le package 'writexl' est requis pour export_results_excel().", call. = FALSE)
  }

  # Ajout automatique des taux de croissance selon le type de feuille
  list_df_enriched <- lapply(list_df, function(df) {
    if ("valeur_cal" %in% names(df)) {
      df |>
        dplyr::arrange(full_code, annee, trimestre) |>
        dplyr::group_by(full_code) |>
        dplyr::mutate(
          evol_trim_pct = round((valeur_cal / dplyr::lag(valeur_cal) - 1) * 100, 2)
        ) |>
        dplyr::ungroup()
    } else if ("B1_crt" %in% names(df)) {
      df |>
        dplyr::arrange(Code_Branche, annee, trimestre) |>
        dplyr::group_by(Code_Branche) |>
        dplyr::mutate(
          evol_B1_crt_pct = round((B1_crt / dplyr::lag(B1_crt) - 1) * 100, 2),
          evol_P1_crt_pct = round((P1_crt / dplyr::lag(P1_crt) - 1) * 100, 2)
        ) |>
        dplyr::ungroup()
    } else {
      df
    }
  })

  writexl::write_xlsx(list_df_enriched, path = file_name)
  message("\u2705 Export termin\u00e9 : ", file_name,
          " (", length(list_df_enriched), " feuilles)")
  invisible(file_name)
}

#' Graphique de comparaison Indicateur vs Résultat Calé
#' @param df_source les indicateurs sources utilisés
#' @param df_bench les agregats calé
#' @param code_branche est la brahnce dont on faire l'etalonnage graphique
#' @export
plot_benchmark_compare <- function(df_source, df_bench, code_branche) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est requis pour plot_benchmark_compare().")
  }

  # Pr\u00e9paration des donn\u00e9es
  s_data <- df_source |>
    dplyr::filter(full_code == code_branche) |>
    dplyr::select(periode, valeur) |>
    dplyr::mutate(Serie = "Indicateur Brut (Source)")

  b_data <- df_bench |>
    dplyr::filter(full_code == code_branche) |>
    dplyr::select(periode, valeur_cal) |>
    dplyr::rename(valeur = valeur_cal) |>
    dplyr::mutate(Serie = "S\u00e9rie Cal\u00e9e (Cholette)")

  df_plot <- dplyr::bind_rows(s_data, b_data)

  # Rendu graphique
  ggplot2::ggplot(df_plot, aes(x = periode, y = valeur, color = Serie, group = Serie)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    theme_minimal() +
    ggplot2::labs(title = paste("Analyse de calage :", code_branche),
         subtitle = "V\u00e9rification de la conservation du profil trimestriel",
         x = "P\u00e9riode", y = "Niveau") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}
