# R/12_pipeline_ci_branches.R

#' Calculer la CI trimestrielle par branche
#'
#' @param prod_complete Table de production trimestrielle complète.
#' @param res_trim Résultat du module TRE.
#' @param ci_crt CI annuelle à prix courants.
#' @param ci_vol CI annuelle en volume.
#'
#' @return Liste contenant les objets intermédiaires et finaux du calcul de CI.
#' @export
calculer_ci_branches <- function(prod_complete, res_trim, ci_crt, ci_vol) {

  p1_agreg <- prod_complete %>%
    dplyr::mutate(Code_Branche = trimws(sub(".*\\*", "", full_code))) %>%
    dplyr::group_by(annee, trimestre, Code_Branche) %>%
    dplyr::summarise(
      P1_crt_agg  = sum(valeur_crt, na.rm = TRUE),
      P1_vpap_agg = sum(valeur_vpap, na.rm = TRUE),
      .groups = "drop"
    )

  df_ratios_clean <- res_trim$coef_tech_trim %>%
    dplyr::mutate(
      Code_Branche = trimws(as.character(Code_Branche)),
      Annee = as.integer(Annee),
      Trimestre = as.integer(Trimestre)
    ) %>%
    dplyr::select(Annee, Trimestre, Code_Branche, Type_Prix, Coef_Technique) %>%
    dplyr::filter(Type_Prix %in% c("Courant", "Constant")) %>%
    tidyr::pivot_wider(
      names_from = Type_Prix,
      values_from = Coef_Technique,
      values_fill = 0
    ) %>%
    dplyr::rename(CT_Crt = Courant, CT_Vol = Constant)

  df_calcul_p2 <- p1_agreg %>%
    dplyr::inner_join(
      df_ratios_clean,
      by = c("annee" = "Annee", "trimestre" = "Trimestre", "Code_Branche")
    ) %>%
    dplyr::mutate(
      Ind_P2_vpap = P1_vpap_agg * CT_Vol,
      Ind_P2_crt  = P1_crt_agg * CT_Crt
    )

  est_p2_chained <- df_calcul_p2 %>%
    dplyr::arrange(Code_Branche, annee, trimestre) %>%
    dplyr::group_by(Code_Branche) %>%
    dplyr::mutate(
      P2_ch_est = calcul_valeur_chainee_trim(
        IndCrt = Ind_P2_crt,
        IndVol = Ind_P2_vpap
      )
    ) %>%
    dplyr::ungroup()

  ci_crt_agg <- ci_crt %>%
    dplyr::mutate(full_code = extract_branch_code(full_code)) %>%
    dplyr::group_by(annee, full_code) %>%
    dplyr::summarise(ci_crt_agg = sum(valeur, na.rm = TRUE), .groups = "drop")

  ci_vol_agg <- ci_vol %>%
    dplyr::mutate(full_code = extract_branch_code(full_code)) %>%
    dplyr::group_by(annee, full_code) %>%
    dplyr::summarise(ci_vol_agg = sum(valeur, na.rm = TRUE), .groups = "drop")

  ci_bran <- ci_crt_agg %>%
    dplyr::inner_join(ci_vol_agg, by = c("annee", "full_code")) %>%
    dplyr::arrange(full_code, annee) %>%
    dplyr::group_by(full_code) %>%
    dplyr::mutate(
      ci_ch_agg = calcul_valeur_chainee_annuel(
        IndCrt = ci_crt_agg,
        IndVol = ci_vol_agg
      )
    ) %>%
    dplyr::ungroup()

  bench_p2_crt <- benchmark_groupe(
    df_source = est_p2_chained %>%
      dplyr::rename(valeur = Ind_P2_crt, full_code = Code_Branche),
    df_target = ci_crt_agg %>%
      dplyr::rename(valeur = ci_crt_agg),
    type_filter = NULL,
    value_col = "valeur"
  ) %>%
    dplyr::rename(P2_crt_cal = valeur_cal)

  bench_p2_ch <- benchmark_groupe(
    df_source = est_p2_chained %>%
      dplyr::rename(valeur = P2_ch_est, full_code = Code_Branche),
    df_target = ci_bran %>%
      dplyr::select(annee, full_code, valeur = ci_ch_agg),
    type_filter = NULL,
    value_col = "valeur"
  ) %>%
    dplyr::rename(P2_ch_cal = valeur_cal)

  p2_final_cal <- dplyr::inner_join(
    bench_p2_crt %>%
      dplyr::select(annee, trimestre, Code_Branche = full_code, P2_crt_cal),
    bench_p2_ch %>%
      dplyr::select(annee, trimestre, Code_Branche = full_code, P2_ch_cal),
    by = c("annee", "trimestre", "Code_Branche")
  ) %>%
    dplyr::group_by(Code_Branche) %>%
    dplyr::mutate(
      P2_vpap_cal = dechainer_valeurs(P2_crt_cal, P2_ch_cal, trim = TRUE)
    ) %>%
    dplyr::ungroup()

  list(
    p1_agreg        = p1_agreg,
    df_ratios_clean = df_ratios_clean,
    df_calcul_p2    = df_calcul_p2,
    est_p2_chained  = est_p2_chained,
    ci_crt_agg      = ci_crt_agg,
    ci_vol_agg      = ci_vol_agg,
    ci_bran         = ci_bran,
    bench_p2_crt    = bench_p2_crt,
    bench_p2_ch     = bench_p2_ch,
    p2_final_cal    = p2_final_cal
  )
}
