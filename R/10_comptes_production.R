#' @import dplyr
#' @import tidyr
NULL

# ==============================================================================
# COMPTES DE PRODUCTION PAR BRANCHE (P1, P2, VA)
# ==============================================================================

#' Calculer les comptes de production complets (P1, P2, VA) pour toutes les branches
#'
#' @description
#' Pour les branches présentes dans \code{p1_agreg_complet} mais absentes de
#' \code{p2_final_cal} (typiquement GZ001 et HZ001 ajoutées par Ind5), calcule
#' la consommation intermédiaire trimestrielle à partir des coefficients
#' techniques du TRE :
#' \deqn{P2\_crt = P1\_crt \times CT\_courant}
#' \deqn{P2\_vpap = P1\_vpap \times CT\_constant}
#'
#' Ensuite, pour l'ensemble des 60 branches :
#' \itemize{
#'   \item \code{P1_ch} et \code{P2_ch} sont calculés par chaînage
#'     (\code{calcul_valeur_chainee_trim()}).
#'   \item \code{VA = P1 - P2} en courant, VPAP et chaîné.
#' }
#'
#' @param p1_agreg_complet Tibble production par branche toutes sources confondues
#'   (y compris GZ001/HZ001). Colonnes : \code{annee}, \code{trimestre},
#'   \code{Code_Branche}, \code{P1_crt_agg}, \code{P1_vpap_agg}.
#' @param p2_final_cal Tibble CI par branche issu de \code{calculer_ci_branches()}.
#'   Colonnes : \code{annee}, \code{trimestre}, \code{Code_Branche},
#'   \code{P2_crt_cal}, \code{P2_ch_cal}, \code{P2_vpap_cal}.
#' @param ct_trim Tibble coefficients techniques trimestriels
#'   (\code{res_trim$ct_trim}). Colonnes : \code{Annee}, \code{Trimestre},
#'   \code{Code_Branche}, \code{Type_Prix}, \code{Coef_Technique}.
#'
#' @return Tibble \code{df_comptes_finaux} avec les colonnes :
#' \describe{
#'   \item{\code{annee}, \code{trimestre}, \code{Code_Branche}}{Identifiants.}
#'   \item{\code{P1_crt}, \code{P1_vpap}, \code{P1_ch}}{Production (courant, VPAP, chaîné).}
#'   \item{\code{P2_crt}, \code{P2_vpap}, \code{P2_ch}}{CI (courant, VPAP, chaîné).}
#'   \item{\code{VA_crt}, \code{VA_vpap}, \code{VA_ch}}{Valeur ajoutée (courant, VPAP, chaîné).}
#' }
#' @export
calculer_comptes_production <- function(p1_agreg_complet, p2_final_cal, ct_trim) {

  branches_manquantes <- setdiff(
    unique(p1_agreg_complet$Code_Branche),
    unique(p2_final_cal$Code_Branche)
  )

  if (length(branches_manquantes) > 0) {
    message("\u25b6 Calcul CI par CT pour branches manquantes : ",
            paste(branches_manquantes, collapse = ", "))

    ct_wide <- ct_trim |>
      dplyr::filter(Code_Branche %in% branches_manquantes) |>
      dplyr::select(
        annee     = Annee,
        trimestre = Trimestre,
        Code_Branche,
        Type_Prix,
        Coef_Technique
      ) |>
      dplyr::mutate(
        annee     = as.numeric(annee),
        trimestre = as.numeric(trimestre),
        Type_Prix = dplyr::case_when(
          Type_Prix == "Courant"  ~ "CT_crt",
          Type_Prix == "Constant" ~ "CT_vol",
          TRUE ~ Type_Prix
        )
      ) |>
      tidyr::pivot_wider(names_from = Type_Prix, values_from = Coef_Technique)

    p2_manquantes <- p1_agreg_complet |>
      dplyr::filter(Code_Branche %in% branches_manquantes) |>
      dplyr::inner_join(ct_wide, by = c("annee", "trimestre", "Code_Branche")) |>
      dplyr::mutate(
        P2_crt_cal  = P1_crt_agg  * CT_crt,
        P2_vpap_cal = P1_vpap_agg * CT_vol
      ) |>
      dplyr::group_by(Code_Branche) |>
      dplyr::mutate(
        P2_ch_cal = calcul_valeur_chainee_trim(P2_crt_cal, P2_vpap_cal)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(annee, trimestre, Code_Branche,
                    P2_crt_cal, P2_ch_cal, P2_vpap_cal)

    p2_complet <- dplyr::bind_rows(p2_final_cal, p2_manquantes)
  } else {
    p2_complet <- p2_final_cal
  }

  # Chainage P1 (P1_ch par branche)
  message("\u25b6 Cha\u00eenage P1 par branche...")
  p1_chaine <- p1_agreg_complet |>
    dplyr::group_by(Code_Branche) |>
    dplyr::mutate(
      P1_ch = calcul_valeur_chainee_trim(P1_crt_agg, P1_vpap_agg)
    ) |>
    dplyr::ungroup()

  # Assemblage et calcul VA
  message("\u25b6 Calcul VA = P1 - P2...")
  df <- p1_chaine |>
    dplyr::inner_join(p2_complet, by = c("annee", "trimestre", "Code_Branche")) |>
    dplyr::mutate(
      VA_crt  = P1_crt_agg  - P2_crt_cal,
      VA_vpap = P1_vpap_agg - P2_vpap_cal,
      VA_ch   = P1_ch       - P2_ch_cal
    ) |>
    dplyr::rename(
      P1_crt  = P1_crt_agg,
      P1_vpap = P1_vpap_agg,
      P2_crt  = P2_crt_cal,
      P2_vpap = P2_vpap_cal,
      P2_ch   = P2_ch_cal
    ) |>
    dplyr::select(
      annee, trimestre, Code_Branche,
      P1_crt, P1_vpap, P1_ch,
      P2_crt, P2_vpap, P2_ch,
      VA_crt, VA_vpap, VA_ch
    ) |>
    dplyr::arrange(annee, trimestre, Code_Branche)

  n_branches <- dplyr::n_distinct(df$Code_Branche)
  n_na       <- sum(is.na(df$VA_crt)) + sum(is.na(df$VA_vpap)) + sum(is.na(df$VA_ch))
  message("\u2705 df_comptes_finaux : ", n_branches, " branches | NA total : ", n_na)

  df
}
