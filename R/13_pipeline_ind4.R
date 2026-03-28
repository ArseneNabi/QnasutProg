# R/13_pipeline_ind4.R

#' Calculer les séries Ind4 à partir de la CI par branche
#'
#' @param p2_final_cal CI trimestrielle finale par branche.
#' @param res_trim Résultat du module TRE.
#' @param Map_Produits Nomenclature produits.
#' @param ind_crt Indicateurs trimestriels courants.
#' @param prod_crt Production annuelle courante.
#' @param prod_ch Production annuelle chaînée.
#'
#' @return Liste contenant les objets intermédiaires et les résultats Ind4.
#' @export
calculer_ind4_depuis_ci <- function(p2_final_cal, res_trim, Map_Produits,
                                    ind_crt, prod_crt, prod_ch) {

  p2_par_produit_n3 <- transformer_branche_produit(
    df_branches   = p2_final_cal,
    poids_tbl     = res_trim$poids_trim,
    operation     = "P2",
    value_crt_col = "P2_crt_cal",
    value_vol_col = "P2_vpap_cal",
    annee_col     = "annee",
    trimestre_col = "trimestre",
    branche_col   = "Code_Branche",
    produit_col   = "Code_Produit",
    normalize     = TRUE
  )

  p2_par_produit_etal <- p2_par_produit_n3 %>%
    dplyr::inner_join(
      Map_Produits %>%
        dplyr::transmute(
          Code_Produit   = trimws(as.character(Code_Prod_N3)),
          Code_Prod_Etal = trimws(as.character(Code_Prod_Etal))
        ) %>%
        dplyr::distinct(),
      by = "Code_Produit"
    ) %>%
    dplyr::group_by(annee, trimestre, Code_Prod_Etal) %>%
    dplyr::summarise(
      valeur_crt = sum(valeur_crt, na.rm = TRUE),
      valeur_vol = sum(valeur_vol, na.rm = TRUE),
      .groups = "drop"
    )

  produits_ind4 <- ind_crt %>%
    dplyr::filter(type_ind == "Ind4") %>%
    dplyr::mutate(Code_Prod_Etal = sub("^Ind4_TOTAL\\*", "", full_code)) %>%
    dplyr::distinct(Code_Prod_Etal) %>%
    dplyr::pull(Code_Prod_Etal)

  p2_par_produit_etal_ind4 <- p2_par_produit_etal %>%
    dplyr::filter(Code_Prod_Etal %in% produits_ind4)

  ind4_source_complete <- p2_par_produit_etal_ind4 %>%
    dplyr::transmute(
      annee = annee,
      trimestre = trimestre,
      full_code = paste0("Ind4_TOTAL*", Code_Prod_Etal),
      valeur_crt = valeur_crt,
      valeur_vol = valeur_vol
    ) %>%
    dplyr::arrange(full_code, annee, trimestre) %>%
    dplyr::group_by(full_code) %>%
    dplyr::mutate(
      valeur_ch = calcul_valeur_chainee_trim(
        IndCrt = valeur_crt,
        IndVol = valeur_vol
      )
    ) %>%
    dplyr::ungroup()

  ind4_crt_source <- ind4_source_complete %>%
    dplyr::transmute(
      annee = annee,
      trimestre = trimestre,
      periode = paste0(annee, "T", trimestre),
      full_code = full_code,
      valeur = valeur_crt,
      branche_macro = sub("^Ind4_", "", full_code),
      type_ind = "Ind4"
    )

  ind4_ch_source <- ind4_source_complete %>%
    dplyr::transmute(
      annee = annee,
      trimestre = trimestre,
      periode = paste0(annee, "T", trimestre),
      full_code = full_code,
      valeur = valeur_ch,
      branche_macro = sub("^Ind4_", "", full_code),
      type_ind = "Ind4"
    )

  cnt_ind4_crt <- benchmark_groupe(
    df_source = ind4_crt_source,
    df_target = prod_crt,
    type_filter = "Ind4",
    value_col = "valeur"
  )

  cnt_ind4_ch <- benchmark_groupe(
    df_source = ind4_ch_source,
    df_target = prod_ch,
    type_filter = "Ind4",
    value_col = "valeur"
  )

  ind4_final <- dplyr::inner_join(
    cnt_ind4_crt %>%
      dplyr::select(annee, trimestre, full_code, valeur_crt_cal = valeur_cal),
    cnt_ind4_ch %>%
      dplyr::select(annee, trimestre, full_code, valeur_ch_cal = valeur_cal),
    by = c("annee", "trimestre", "full_code")
  ) %>%
    dplyr::group_by(full_code) %>%
    dplyr::mutate(
      valeur_vpap_cal = dechainer_valeurs(valeur_crt_cal, valeur_ch_cal, trim = TRUE)
    ) %>%
    dplyr::ungroup()

  list(
    p2_par_produit_n3        = p2_par_produit_n3,
    p2_par_produit_etal      = p2_par_produit_etal,
    produits_ind4            = produits_ind4,
    p2_par_produit_etal_ind4 = p2_par_produit_etal_ind4,
    ind4_source_complete     = ind4_source_complete,
    ind4_crt_source          = ind4_crt_source,
    ind4_ch_source           = ind4_ch_source,
    cnt_ind4_crt             = cnt_ind4_crt,
    cnt_ind4_ch              = cnt_ind4_ch,
    ind4_final               = ind4_final
  )
}
