# R/10_pipeline_tre.R

extraire_bornes_cfg <- function(cfg) {
  list(
    derniere_annee_cna = cfg$derniere_annee_definitif,
    annee_fin_proj     = cfg$annee_fin_projection
  )
}

preparer_resultat_tre <- function(res_trim) {
  res_trim$coef_tech_trim <- res_trim$ct_trim
  res_trim
}

#' Exécuter le module TRE
#'
#' @param donnees Liste retournée par [charger_donnees_cnt()].
#' @param cfg Liste de configuration.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{ratios_annuels}{Ratios annuels calculés sur le TRE agrégé}
#'   \item{res_trim}{Sortie complète de `trimestrialiser_cnt_complet()`}
#'   \item{coef_tech_trim}{Coefficients techniques trimestriels}
#'   \item{poids_trim}{Poids trimestriels}
#'   \item{ct_trim}{Coefficients techniques trimestriels bruts}
#' }
#'
#' @export
executer_module_tre <- function(donnees, cfg) {
  message("🚀 Démarrage du module TRE...")

  bornes <- extraire_bornes_cfg(cfg)

  ratios_annuels <- calculer_ratios_annuels_etal(donnees$db_tre_etal)

  res_trim <- trimestrialiser_cnt_complet(
    ratios_annuels      = ratios_annuels,
    df_prix_niv3        = donnees$prix_niv3,
    derniere_annee_cna  = bornes$derniere_annee_cna,
    annee_fin_proj      = bornes$annee_fin_proj
  )

  res_trim <- preparer_resultat_tre(res_trim)

  message("✅ Ratios trimestriels calculés et disponibles.")

  list(
    ratios_annuels = ratios_annuels,
    res_trim       = res_trim,
    coef_tech_trim = res_trim$coef_tech_trim,
    poids_trim     = res_trim$poids_trim,
    ct_trim        = res_trim$ct_trim
  )
}
