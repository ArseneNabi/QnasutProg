# R/15_pipeline_p1p2_produits.R

#' Calculer P1 et P2 par produit
#'
#' @param p1_agreg Production agrégée par branche.
#' @param p2_final_cal CI finale par branche.
#' @param poids_p1p2 Poids branche-produit trimestriels.
#'
#' @return Liste contenant `p1_par_produit` et `p2_par_produit`.
#' @export
calculer_p1_p2_par_produit <- function(p1_agreg, p2_final_cal, poids_p1p2) {

  p1_par_produit <- transformer_branche_produit(
    df_branches   = p1_agreg,
    poids_tbl     = poids_p1p2,
    operation     = "P1",
    value_crt_col = "P1_crt_agg",
    value_vol_col = "P1_vpap_agg",
    annee_col     = "annee",
    trimestre_col = "trimestre",
    branche_col   = "Code_Branche",
    produit_col   = "Code_Produit",
    normalize     = TRUE
  )

  p2_par_produit <- transformer_branche_produit(
    df_branches   = p2_final_cal,
    poids_tbl     = poids_p1p2,
    operation     = "P2",
    value_crt_col = "P2_crt_cal",
    value_vol_col = "P2_vpap_cal",
    annee_col     = "annee",
    trimestre_col = "trimestre",
    branche_col   = "Code_Branche",
    produit_col   = "Code_Produit",
    normalize     = TRUE
  )

  list(
    p1_par_produit = p1_par_produit,
    p2_par_produit = p2_par_produit
  )
}
