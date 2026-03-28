#' @import dplyr
#' @import tidyr
NULL

# ==============================================================================
# IND5 : INDICATEURS INDIRECTS MARGES COMMERCE ET TRANSPORT
# ==============================================================================

#' Mettre à jour la production par branche avec les résultats Ind4
#'
#' Remplace les valeurs des branches Ind4 dans \code{p1_agreg} par les
#' estimations finales de \code{ind4_final} (méthode Leontief).
#'
#' @param p1_agreg Tibble production par branche issu de
#'   \code{calculer_ci_branches()}. Colonnes attendues : \code{annee},
#'   \code{trimestre}, \code{Code_Branche}, \code{P1_crt_agg},
#'   \code{P1_vpap_agg}.
#' @param ind4_final Tibble des résultats Ind4 issu de
#'   \code{calculer_ind4_depuis_ci()}. Colonnes attendues : \code{annee},
#'   \code{trimestre}, \code{full_code}, \code{valeur_crt_cal},
#'   \code{valeur_vpap_cal}.
#'
#' @return Tibble au format de \code{p1_agreg}, avec les branches Ind4
#'   remplacées par les valeurs calibrées.
#' @export
integrer_ind4_dans_p1 <- function(p1_agreg, ind4_final) {

  ind4_branches <- ind4_final |>
    dplyr::mutate(Code_Branche = extract_branch_code(full_code)) |>
    dplyr::distinct(Code_Branche) |>
    dplyr::pull(Code_Branche)

  ind4_p1 <- ind4_final |>
    dplyr::mutate(Code_Branche = extract_branch_code(full_code)) |>
    dplyr::transmute(
      annee        = as.numeric(annee),
      trimestre    = as.numeric(trimestre),
      Code_Branche,
      P1_crt_agg  = valeur_crt_cal,
      P1_vpap_agg = valeur_vpap_cal
    )

  p1_agreg |>
    dplyr::filter(!Code_Branche %in% ind4_branches) |>
    dplyr::bind_rows(ind4_p1) |>
    dplyr::arrange(annee, trimestre, Code_Branche)
}


#' Calculer les indicateurs Ind5 : marges commerce et transport
#'
#' Chaîne complète menant de la production par branche (incluant Ind4) aux
#' indicateurs trimestriels de marge pour les branches commerce et transport :
#'
#' \enumerate{
#'   \item Mise à jour de \code{p1_agreg} avec \code{ind4_final}.
#'   \item Désagrégation branche → produit N3 via \code{transformer_branche_produit()}.
#'   \item Agrégation N3 → nomenclature ERE (\code{Code_Prod_Ct}).
#'   \item Calcul et trimestrialisation des ratios ERE (courant et volume).
#'   \item Ind5 = \eqn{\sum_{\text{produits}} (P1 + M)_{\text{trim}} \times \text{taux\_marge\_trim}},
#'         où M désigne les importations trimestrielles, conformément à la base
#'         utilisée dans \code{calculer_ratios_ere()}.
#' }
#'
#' @param p1_agreg Tibble production par branche (sortie de
#'   \code{calculer_ci_branches()}).
#' @param ind4_final Tibble résultats Ind4 (sortie de
#'   \code{calculer_ind4_depuis_ci()}).
#' @param poids_trim Tibble poids TRE trimestrialisés (\code{res_trim$poids_trim}).
#' @param Map_Produits Table de correspondance produits (colonnes
#'   \code{Code_Prod_N3} et \code{Code_Prod_Ct}).
#' @param cna_ere_struct Liste ERE annuelle (sortie de
#'   \code{import_cna_ere_structured()}).
#' @param ind_ce_struct Liste des indicateurs import/export trimestriels
#'   (\code{donnees$ind_ce_struct}). Utilisée pour les importations par produit.
#' @param derniere_annee_cna Dernière année des CNA définitifs.
#' @param annee_fin_proj Année de fin de projection.
#' @param prod_crt Tibble des productions annuelles en courant (cibles CNA),
#'   tel que \code{donnees$prod_crt}. Doit contenir \code{full_code},
#'   \code{annee}, \code{valeur}.
#' @param prod_ch Tibble des productions annuelles en volume chaîné (cibles CNA),
#'   tel que \code{donnees$prod_ch}. Doit contenir \code{full_code},
#'   \code{annee}, \code{valeur}.
#' @param composantes Vecteur des composantes à calculer. Par défaut
#'   \code{c("MARGE de commerce", "MARGE de transport")}.
#' @param map_branches Vecteur nommé faisant correspondre chaque composante au
#'   code branche ERE (sans préfixe). Par défaut
#'   \code{c("MARGE de commerce" = "GZ001", "MARGE de transport" = "HZ001")}.
#'
#' @return Liste avec les éléments :
#' \describe{
#'   \item{\code{p1_agreg_complet}}{Production par branche mise à jour avec Ind4.}
#'   \item{\code{p1_par_produit}}{Production par produit N3 (valeur_crt, valeur_vol).}
#'   \item{\code{p1_ere_crt}}{Production par produit ERE en courant.}
#'   \item{\code{p1_ere_vol}}{Production par produit ERE en volume.}
#'   \item{\code{ratios_ere_trim_crt}}{Ratios ERE trimestriels en courant.}
#'   \item{\code{ratios_ere_trim_vol}}{Ratios ERE trimestriels en volume.}
#'   \item{\code{ind5_marges}}{Indicateurs Ind5 bruts : annee, trimestre,
#'     composante, full_code, valeur_crt, valeur_vol.}
#'   \item{\code{cnt_ind5_crt}}{Ind5 benchmarké en courant (Cholette).}
#'   \item{\code{cnt_ind5_ch}}{Ind5 benchmarké en volume chaîné (Cholette).}
#' }
#' @export
calculer_ind5_depuis_production <- function(p1_agreg,
                                             ind4_final,
                                             poids_trim,
                                             Map_Produits,
                                             cna_ere_struct,
                                             ind_ce_struct,
                                             prod_crt,
                                             prod_ch,
                                             derniere_annee_cna,
                                             annee_fin_proj,
                                             composantes = c("MARGE de commerce",
                                                             "MARGE de transport"),
                                             map_branches = c(
                                               "MARGE de commerce" = "GZ001",
                                               "MARGE de transport" = "HZ001"
                                             )) {

  message("\u25b6 Ind5 \u2014 \u00e9tape 1/5 : int\u00e9gration Ind4 dans p1_agreg...")
  p1_agreg_complet <- integrer_ind4_dans_p1(p1_agreg, ind4_final)

  # ------------------------------------------------------------------
  # 2. Production par produit N3 puis agrégation ERE
  # ------------------------------------------------------------------
  message("\u25b6 Ind5 \u2014 \u00e9tape 2/5 : d\u00e9sagr\u00e9gation branche \u2192 produit...")
  p1_par_produit <- transformer_branche_produit(
    df_branches   = p1_agreg_complet,
    poids_tbl     = poids_trim,
    operation     = "P1",
    value_crt_col = "P1_crt_agg",
    value_vol_col = "P1_vpap_agg",
    normalize     = TRUE
  )

  # ------------------------------------------------------------------
  # 3. Agrégation N3 -> nomenclature ERE (Code_Prod_Ct)
  # ------------------------------------------------------------------
  message("\u25b6 Ind5 \u2014 \u00e9tape 3/5 : agr\u00e9gation N3 \u2192 ERE + importations...")
  table_n3_ere <- Map_Produits |>
    dplyr::transmute(
      Code_Produit = trimws(as.character(Code_Prod_N3)),
      Code_Prod_Ct = trimws(as.character(Code_Prod_Ct))
    ) |>
    dplyr::distinct()

  p1_ere_crt <- p1_par_produit |>
    dplyr::inner_join(table_n3_ere, by = "Code_Produit") |>
    dplyr::group_by(annee, trimestre, Code_Produit = Code_Prod_Ct) |>
    dplyr::summarise(P1_crt = sum(valeur_crt, na.rm = TRUE), .groups = "drop")

  p1_ere_vol <- p1_par_produit |>
    dplyr::inner_join(table_n3_ere, by = "Code_Produit") |>
    dplyr::group_by(annee, trimestre, Code_Produit = Code_Prod_Ct) |>
    dplyr::summarise(P1_vol = sum(valeur_vol, na.rm = TRUE), .groups = "drop")

  # Importations trimestrielles brutes (indicateurs, format ERE)
  imp_ere_crt <- pivoter_ere_long(ind_ce_struct[["Import"]][["Crt"]], "Crt", "IMPORTATIONS") |>
    dplyr::select(annee, trimestre, Code_Produit, imp_crt = valeur)

  imp_ere_vol <- pivoter_ere_long(ind_ce_struct[["Import"]][["Ch"]], "Ch", "IMPORTATIONS") |>
    dplyr::select(annee, trimestre, Code_Produit, imp_vol = valeur)

  # Base courant : P1 + importations (par produit ERE)
  base_ere_crt <- p1_ere_crt |>
    dplyr::left_join(imp_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::mutate(base_crt = P1_crt + tidyr::replace_na(imp_crt, 0)) |>
    dplyr::select(annee, trimestre, Code_Produit, base_crt)

  # Base volume : P1_vol + importations volume (par produit ERE)
  base_ere_vol <- p1_ere_vol |>
    dplyr::left_join(imp_ere_vol, by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::mutate(base_vol = P1_vol + tidyr::replace_na(imp_vol, 0)) |>
    dplyr::select(annee, trimestre, Code_Produit, base_vol)

  # ------------------------------------------------------------------
  # 4. Ratios ERE (courant et volume) + trimestrialisation
  # ------------------------------------------------------------------
  message("\u25b6 Ind5 \u2014 \u00e9tape 4/5 : calcul des ratios ERE...")
  ratios_ere_ann_crt <- calculer_ratios_ere(cna_ere_struct, type_prix = "CnaErECrt")
  ratios_ere_ann_vol <- calculer_ratios_ere(cna_ere_struct, type_prix = "CnaErEVol")

  ratios_ere_trim_crt <- trimestrialiser_ratios_ere(
    ratios_ere_ann_crt,
    derniere_annee_cna = derniere_annee_cna,
    annee_fin_proj     = annee_fin_proj
  )
  ratios_ere_trim_vol <- trimestrialiser_ratios_ere(
    ratios_ere_ann_vol,
    derniere_annee_cna = derniere_annee_cna,
    annee_fin_proj     = annee_fin_proj
  )

  # ------------------------------------------------------------------
  # 5. Ind5 = sum_produits((P1 + M) * taux_marge)
  # ------------------------------------------------------------------
  message("\u25b6 Ind5 \u2014 \u00e9tape 5/5 : calcul des marges (commerce + transport)...")

  .calc_marge <- function(base_ere, base_col, ratios_trim) {
    ratios_trim |>
      dplyr::filter(composante %in% composantes) |>
      dplyr::inner_join(
        base_ere |> dplyr::rename(base = dplyr::all_of(base_col)),
        by = c("annee", "trimestre", "Code_Produit")
      ) |>
      dplyr::mutate(marge = base * ratio_trim) |>
      dplyr::group_by(annee, trimestre, composante) |>
      dplyr::summarise(valeur = sum(marge, na.rm = TRUE), .groups = "drop")
  }

  ind5_crt <- .calc_marge(base_ere_crt, "base_crt", ratios_ere_trim_crt) |>
    dplyr::rename(valeur_crt = valeur)

  ind5_vol <- .calc_marge(base_ere_vol, "base_vol", ratios_ere_trim_vol) |>
    dplyr::rename(valeur_vol = valeur)

  # Assemblage + full_code (format benchmarking)
  codes_branches <- map_branches[composantes]
  ind5_marges <- dplyr::left_join(ind5_crt, ind5_vol,
                                   by = c("annee", "trimestre", "composante")) |>
    dplyr::left_join(
      tibble::tibble(
        composante = names(codes_branches),
        full_code  = paste0("Ind5_TOTAL*", codes_branches)
      ),
      by = "composante"
    ) |>
    dplyr::select(annee, trimestre, composante, full_code, valeur_crt, valeur_vol)

  # ------------------------------------------------------------------
  # 6. Benchmarking Cholette sur les cibles CNA annuelles
  # ------------------------------------------------------------------
  message("\u25b6 Ind5 \u2014 \u00e9tape 6/6 : benchmarking Cholette (courant + cha\u00een\u00e9)...")

  codes_ind5 <- unique(ind5_marges$full_code)

  # Cibles annuelles (filtrées sur les codes Ind5)
  cible_crt <- prod_crt |>
    dplyr::filter(full_code %in% codes_ind5) |>
    dplyr::select(full_code, annee, valeur)

  cible_ch <- prod_ch |>
    dplyr::filter(full_code %in% codes_ind5) |>
    dplyr::select(full_code, annee, valeur)

  # Indicateurs au format attendu par benchmark_groupe()
  ind5_source_crt <- ind5_marges |>
    dplyr::mutate(type_ind = "Ind5",
                  periode  = paste0(annee, "T", trimestre)) |>
    dplyr::rename(valeur = valeur_crt)

  ind5_source_ch <- ind5_marges |>
    dplyr::mutate(type_ind = "Ind5",
                  periode  = paste0(annee, "T", trimestre)) |>
    dplyr::rename(valeur = valeur_vol)

  cnt_ind5_crt <- benchmark_groupe(ind5_source_crt, cible_crt,
                                    type_filter = "Ind5", value_col = "valeur")
  cnt_ind5_ch  <- benchmark_groupe(ind5_source_ch,  cible_ch,
                                    type_filter = "Ind5", value_col = "valeur")

  # ------------------------------------------------------------------
  # 7. VPAP (déchaînage) + intégration dans p1_agreg_complet
  # ------------------------------------------------------------------
  message("\u25b6 Ind5 \u2014 \u00e9tape 7/7 : VPAP (d\u00e9cha\u00eenage) + int\u00e9gration dans p1_agreg...")

  ind5_p1 <- cnt_ind5_crt |>
    dplyr::select(annee, trimestre, full_code, val_crt = valeur_cal) |>
    dplyr::inner_join(
      cnt_ind5_ch |> dplyr::select(annee, trimestre, full_code, val_ch = valeur_cal),
      by = c("annee", "trimestre", "full_code")
    ) |>
    dplyr::arrange(full_code, annee, trimestre) |>
    dplyr::group_by(full_code) |>
    dplyr::mutate(
      P1_vpap_agg = dechainer_valeurs(val_crt, val_ch, trim = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      annee        = as.numeric(annee),
      trimestre    = as.numeric(trimestre),
      Code_Branche = extract_branch_code(full_code)
    ) |>
    dplyr::select(annee, trimestre, Code_Branche,
                  P1_crt_agg = val_crt, P1_vpap_agg)

  p1_agreg_complet <- dplyr::bind_rows(p1_agreg_complet, ind5_p1) |>
    dplyr::arrange(annee, trimestre, Code_Branche)

  message("\u2705 Ind5 termin\u00e9.")

  list(
    p1_agreg_complet    = p1_agreg_complet,
    p1_par_produit      = p1_par_produit,
    p1_ere_crt          = p1_ere_crt,
    p1_ere_vol          = p1_ere_vol,
    ratios_ere_trim_crt = ratios_ere_trim_crt,
    ratios_ere_trim_vol = ratios_ere_trim_vol,
    ind5_marges         = ind5_marges,
    cnt_ind5_crt        = cnt_ind5_crt,
    cnt_ind5_ch         = cnt_ind5_ch
  )
}
