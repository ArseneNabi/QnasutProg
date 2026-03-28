# R/14_pipeline_comptes_branches.R

#' Calculer les comptes trimestriels par branche
#'
#' @param p1_agreg Production agrégée par branche.
#' @param p2_final_cal CI trimestrielle finale par branche.
#'
#' @return Table synthétique des comptes par branche : P1, P2, VA.
#' @export
calculer_comptes_branches <- function(p1_agreg, p2_final_cal) {
  p1_agreg %>%
    dplyr::inner_join(
      p2_final_cal,
      by = c("annee", "trimestre", "Code_Branche")
    ) %>%
    dplyr::mutate(
      B1_crt  = P1_crt_agg - P2_crt_cal,
      B1_vpap = P1_vpap_agg - P2_vpap_cal
    ) %>%
    dplyr::select(
      Code_Branche,
      annee,
      trimestre,
      P1_vpap = P1_vpap_agg,
      P1_crt  = P1_crt_agg,
      P2_vpap = P2_vpap_cal,
      P2_crt  = P2_crt_cal,
      B1_vpap,
      B1_crt
    ) %>%
    dplyr::arrange(Code_Branche, annee, trimestre)
}
