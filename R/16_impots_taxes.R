# ==============================================================================
# 16_impots_taxes.R
# Calcul des Impots et Taxes Nets (Ind6) — Optique Production
#
# Methode :
#   - Courant  : benchmarking indicateur IndCrt[Impots_Nets] sur ImpCrt
#   - Chaine   : indicateur calcule depuis ERE (somme impots+subventions
#                en volume et courant, puis chainage) benchmarke sur ImpCh
#   - VPAP     : dechainage depuis courant et chaine
# ==============================================================================

#' Importer les donnees CNA des impots et taxes nets
#'
#' Lit les feuilles ImpCrt, ImpVol et ImpCh du fichier R_CNA.xlsx.
#' Chaque feuille a le meme format que les autres feuilles CNA :
#' 3 lignes d'en-tete (type / agregat / code), puis les donnees annuelles.
#' Les colonnes d'interet sont Impots_Nets, Taxes et Subvention.
#'
#' @param path_cna Chemin vers R_CNA.xlsx.
#'
#' @return Liste avec trois elements :
#' \describe{
#'   \item{imp_crt}{Tibble long issu de ImpCrt (courant).}
#'   \item{imp_vol}{Tibble long issu de ImpVol (volume VPAP).}
#'   \item{imp_ch}{Tibble long issu de ImpCh (chaine).}
#' }
#' @export
importer_cna_impots <- function(path_cna) {

  if (!file.exists(path_cna))
    stop("Fichier introuvable : ", path_cna, call. = FALSE)

  .lire <- function(sheet) {
    safe_import(
      import_matrix_cnt(path_cna, sheet),
      paste0("ImpCNA_", sheet)
    )
  }

  list(
    imp_crt = .lire("ImpCrt"),
    imp_vol = .lire("ImpVol"),
    imp_ch  = .lire("ImpCh")
  )
}


#' Extraire l'indicateur Impots_Nets depuis ind_crt (IndCrt)
#'
#' Filtre les lignes de \code{ind_crt} dont \code{type_ind} vaut
#' \code{"Impots_Nets"}, \code{"Taxes"} ou \code{"Subvention"}
#' (tels qu'ils apparaissent en ligne 2 des en-tetes Excel).
#' Retourne un tibble au format attendu par \code{benchmark_groupe()}.
#'
#' @param ind_crt Tibble issu de \code{import_matrix_cnt()} sur IndCrt.
#' @param ind_cst Tibble issu de \code{import_matrix_cnt()} sur IndCst.
#'
#' @return Liste avec :
#' \describe{
#'   \item{ind_imp_crt}{Indicateur courant Impots_Nets (IndCrt).}
#'   \item{ind_taxes_crt}{Indicateur courant Taxes (IndCrt).}
#'   \item{ind_subv_crt}{Indicateur courant Subvention (IndCrt).}
#' }
#' @export
extraire_ind_impots <- function(ind_crt, ind_cst) {

  # Les type_ind correspondent aux valeurs de la ligne 2 d'en-tete Excel
  types_imp  <- c("Impots_Nets", "Impots_nets", "impots_nets", "Impots nets")
  types_tax  <- c("Taxes", "taxes", "Taxe")
  types_subv <- c("Subvention", "subvention", "Subventions")

  .filtrer <- function(df, types) {
    res <- dplyr::filter(df, trimws(.data$type_ind) %in% types)
    if (nrow(res) == 0)
      warning("Aucune ligne trouvee pour type_ind %in% c(",
              paste(types, collapse = ", "), ") dans l'indicateur.",
              call. = FALSE)
    res
  }

  list(
    ind_imp_crt  = .filtrer(ind_crt, types_imp),
    ind_taxes_crt = .filtrer(ind_crt, types_tax),
    ind_subv_crt  = .filtrer(ind_crt, types_subv)
  )
}


#' Calculer l'indicateur constant des impots nets depuis les ERE
#'
#' L'indicateur constant n'est pas disponible directement dans IndCst
#' (colonne remplie de 1,0). Il est calcule indirectement :
#'
#' \enumerate{
#'   \item Pour chaque produit ERE, on somme les composantes
#'         "IMPOT sur Import", "IMPOT sur export", "IMPOT sur produit"
#'         et "Subventions sur produits" issues des ressources ERE
#'         en courant (\code{ressources_crt}) et en volume (\code{ressources_vpap}).
#'   \item On agrege par trimestre (tous produits).
#'   \item On chaine la serie agregee courant + volume pour obtenir
#'         l'indicateur chaine des impots nets.
#' }
#'
#' Cet indicateur sera ensuite benchmarke sur les cibles ImpCh.
#'
#' @param ressources_crt Tibble ressources ERE en courant
#'   (\code{ere_res$ressources_crt}). Colonnes : annee, trimestre,
#'   Code_Produit, composante, valeur_composante.
#' @param ressources_vpap Tibble ressources ERE en VPAP
#'   (\code{ere_res$ressources_vpap}). Meme structure.
#' @param composantes_impots Vecteur des noms de composantes ERE
#'   a additionner. Par defaut : impots sur import/export/produit
#'   et subventions (en valeur negative pour les subventions).
#' @param signe_subventions Numerique : -1 (defaut) si les subventions
#'   sont a deduire des impots pour obtenir les impots nets,
#'   +1 si elles sont deja en negatif dans les ressources ERE.
#'
#' @return Tibble avec colonnes : annee, trimestre, ind_imp_crt_ere,
#'   ind_imp_vol_ere, ind_imp_ch_ere.
#'   Format compatible avec \code{benchmark_groupe()} apres mise en forme.
#' @export
calculer_ind_impots_depuis_ere <- function(ressources_crt,
                                           ressources_vpap,
                                           composantes_impots = c(
                                             "IMPOT sur Import",
                                             "IMPOT sur export",
                                             "IMPOT sur produit",
                                             "Subventions sur produits"
                                           ),
                                           signe_subventions = -1) {

  message("\u25b6 Calcul indicateur impots/taxes depuis ERE...")

  # Patterns de recherche insensibles a la casse
  pattern_subv <- "subvention"
  pattern_imp  <- "impot|taxe"

  .agreger_composantes <- function(df_res) {
    df_res |>
      dplyr::filter(
        # Garder impots et subventions
        grepl(pattern_imp,  .data$composante, ignore.case = TRUE) |
          grepl(pattern_subv, .data$composante, ignore.case = TRUE)
      ) |>
      dplyr::mutate(
        # Signe : les subventions sont deduites pour obtenir les impots nets
        signe = dplyr::if_else(
          grepl(pattern_subv, .data$composante, ignore.case = TRUE),
          as.numeric(signe_subventions),
          1
        ),
        valeur_signee = .data$valeur_composante * .data$signe
      ) |>
      dplyr::group_by(.data$annee, .data$trimestre) |>
      dplyr::summarise(
        valeur = sum(.data$valeur_signee, na.rm = TRUE),
        .groups = "drop"
      )
  }

  agg_crt <- .agreger_composantes(ressources_crt) |>
    dplyr::rename(ind_imp_crt_ere = .data$valeur)

  agg_vol <- .agreger_composantes(ressources_vpap) |>
    dplyr::rename(ind_imp_vol_ere = .data$valeur)

  # Jointure courant + volume
  agg <- dplyr::inner_join(agg_crt, agg_vol,
                           by = c("annee", "trimestre")) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  # Chainage de la serie agregee
  agg <- agg |>
    dplyr::mutate(
      ind_imp_ch_ere = calcul_valeur_chainee_trim(
        IndCrt = .data$ind_imp_crt_ere,
        IndVol = .data$ind_imp_vol_ere
      )
    )

  n_neg <- sum(agg$ind_imp_crt_ere < 0, na.rm = TRUE)
  if (n_neg > 0)
    message("  \u26a0\ufe0f  ", n_neg,
            " valeurs negatives dans l'indicateur ERE courant ",
            "(normal si subventions > impots).")

  message("\u2705 Indicateur impots ERE | ",
          nrow(agg), " trimestres | NA ch : ",
          sum(is.na(agg$ind_imp_ch_ere)))

  agg
}


#' Benchmarker les impots et taxes nets
#'
#' Pipeline complet :
#' \enumerate{
#'   \item Benchmarking courant : \code{ind_imp_crt} (IndCrt) sur ImpCrt.
#'   \item Benchmarking chaine  : indicateur ERE calcule sur ImpCh.
#'   \item VPAP par dechainage.
#' }
#'
#' @param ind_imp_crt Tibble indicateur courant Impots_Nets issu de
#'   \code{extraire_ind_impots()$ind_imp_crt}.
#' @param ind_imp_ere Tibble indicateur ERE calcule par
#'   \code{calculer_ind_impots_depuis_ere()}.
#' @param imp_cna Liste retournee par \code{importer_cna_impots()} :
#'   elements \code{imp_crt} et \code{imp_ch}.
#' @param derniere_annee_cna Derniere annee des CNA definitifs.
#' @param annee_fin_proj Annee de fin de projection.
#' @param codes_negatifs_autorises Vecteur de codes pour lesquels les
#'   negatifs Cholette sont acceptes (typiquement les codes subventions).
#'   Defaut : \code{character(0)}.
#'
#' @return Liste :
#' \describe{
#'   \item{cnt_imp_net_crt}{Impots nets benchmarkes en courant.}
#'   \item{cnt_imp_net_ch}{Impots nets benchmarkes en volume chaine.}
#'   \item{cnt_imp_net_vpap}{VPAP des impots nets (courant + ch + vpap).}
#'   \item{ind_ere_source}{Indicateur ERE utilise comme source chaine.}
#' }
#' @export
benchmarker_impots_taxes <- function(ind_imp_crt,
                                     ind_imp_ere,
                                     imp_cna,
                                     derniere_annee_cna,
                                     annee_fin_proj,
                                     codes_negatifs_autorises = character(0)) {

  message("\u25b6 Benchmarking Impots & Taxes Nets...")

  # ---------------------------------------------------------------
  # 1. Cibles annuelles
  # ---------------------------------------------------------------
  # Filtrer uniquement la composante Impots_Nets dans les CNA
  types_imp <- c("Impots_Nets", "Impots_nets", "impots_nets")

  .extraire_cible <- function(df_cna) {
    df_cna |>
      dplyr::filter(trimws(.data$type_ind) %in% types_imp) |>
      dplyr::group_by(.data$annee, .data$full_code) |>
      dplyr::summarise(valeur = sum(.data$valeur, na.rm = TRUE),
                       .groups = "drop")
  }

  cible_crt <- .extraire_cible(imp_cna$imp_crt)
  cible_ch  <- .extraire_cible(imp_cna$imp_ch)

  # ---------------------------------------------------------------
  # 2. Mise en forme de l'indicateur courant
  #    (deja au format benchmark_groupe : full_code, annee, trimestre, valeur)
  # ---------------------------------------------------------------
  src_crt <- ind_imp_crt |>
    dplyr::mutate(periode = paste0(.data$annee, "T", .data$trimestre))

  # ---------------------------------------------------------------
  # 3. Benchmarking courant
  # ---------------------------------------------------------------
  cnt_imp_net_crt <- benchmark_groupe(
    df_source   = src_crt,
    df_target   = cible_crt,
    type_filter = NULL,
    value_col   = "valeur",
    codes_negatifs_autorises = codes_negatifs_autorises
  )

  message("  Courant : ", nrow(cnt_imp_net_crt), " lignes | ",
          dplyr::n_distinct(cnt_imp_net_crt$full_code), " codes | ",
          "methodes : ", paste(table(cnt_imp_net_crt$methode_cal), collapse = " / "))

  # ---------------------------------------------------------------
  # 4. Mise en forme de l'indicateur chaine (depuis ERE)
  #    On construit un full_code unique "Ind6_TOTAL*ImpNets_Agrege"
  #    pour le benchmarking agrege
  # ---------------------------------------------------------------

  # Identifier les codes presents dans les cibles chaine
  codes_ch <- unique(cible_ch$full_code)

  # Si plusieurs codes existent dans ImpCh, on distribue l'indicateur
  # ERE agrege proportionnellement a la structure des cibles
  if (length(codes_ch) == 1) {

    # Cas simple : un seul code (agregat)
    src_ch <- ind_imp_ere |>
      dplyr::mutate(
        full_code = codes_ch[1],
        type_ind  = "Ind6",
        periode   = paste0(.data$annee, "T", .data$trimestre),
        valeur    = .data$ind_imp_ch_ere
      ) |>
      dplyr::select(.data$annee, .data$trimestre, .data$periode,
                    .data$full_code, .data$type_ind, .data$valeur)

    cnt_imp_net_ch <- benchmark_groupe(
      df_source   = src_ch,
      df_target   = cible_ch,
      type_filter = NULL,
      value_col   = "valeur",
      codes_negatifs_autorises = codes_negatifs_autorises
    )

  } else {

    # Cas multi-codes : distribuer proportionnellement par annee
    # selon les poids de la cible chaine (structure annuelle)
    poids_ch <- cible_ch |>
      dplyr::group_by(.data$annee) |>
      dplyr::mutate(
        total_an = sum(abs(.data$valeur), na.rm = TRUE),
        poids    = dplyr::if_else(.data$total_an == 0, 0,
                                  .data$valeur / .data$total_an)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(.data$annee, .data$full_code, .data$poids)

    src_ch_multi <- ind_imp_ere |>
      dplyr::select(.data$annee, .data$trimestre,
                    valeur = .data$ind_imp_ch_ere) |>
      dplyr::left_join(poids_ch, by = "annee",
                       relationship = "many-to-many") |>
      dplyr::mutate(
        valeur    = .data$valeur * .data$poids,
        type_ind  = "Ind6",
        periode   = paste0(.data$annee, "T", .data$trimestre)
      )

    cnt_imp_net_ch <- benchmark_groupe(
      df_source   = src_ch_multi,
      df_target   = cible_ch,
      type_filter = NULL,
      value_col   = "valeur",
      codes_negatifs_autorises = codes_negatifs_autorises
    )
  }

  message("  Chaine : ", nrow(cnt_imp_net_ch), " lignes | ",
          dplyr::n_distinct(cnt_imp_net_ch$full_code), " codes | ",
          "methodes : ", paste(table(cnt_imp_net_ch$methode_cal), collapse = " / "))

  # ---------------------------------------------------------------
  # 5. Calcul VPAP par dechainage
  # ---------------------------------------------------------------
  cnt_imp_net_vpap <- dplyr::full_join(
    cnt_imp_net_crt |>
      dplyr::select(.data$annee, .data$trimestre, .data$full_code,
                    valeur_crt = .data$valeur_cal),
    cnt_imp_net_ch |>
      dplyr::select(.data$annee, .data$trimestre, .data$full_code,
                    valeur_ch  = .data$valeur_cal),
    by = c("annee", "trimestre", "full_code")
  ) |>
    dplyr::mutate(
      dplyr::across(c(.data$valeur_crt, .data$valeur_ch),
                    ~ tidyr::replace_na(.x, 0))
    ) |>
    dplyr::arrange(.data$full_code, .data$annee, .data$trimestre) |>
    dplyr::group_by(.data$full_code) |>
    dplyr::mutate(
      valeur_vpap = dechainer_valeurs(.data$valeur_crt, .data$valeur_ch,
                                      trim = TRUE)
    ) |>
    dplyr::ungroup()

  n_na_vpap <- sum(is.na(cnt_imp_net_vpap$valeur_vpap))
  message("\u2705 Impots nets | VPAP calcule | NA vpap : ", n_na_vpap)

  list(
    cnt_imp_net_crt  = cnt_imp_net_crt,
    cnt_imp_net_ch   = cnt_imp_net_ch,
    cnt_imp_net_vpap = cnt_imp_net_vpap,
    ind_ere_source   = ind_imp_ere
  )
}


#' Executer le pipeline complet d'equilibrage des impots/taxes nets
#'
#' Encapsule l'ancienne section 3 du Rmd :
#' import CNA impots, extraction des indicateurs, calcul de l'indicateur
#' ERE, benchmarking courant/chaine/VPAP, verification annuelle des
#' contraintes et export Excel optionnel.
#'
#' @param root_dir Repertoire racine contenant le fichier `R_CNA.xlsx`.
#' @param donnees Liste retournee par `charger_donnees_cnt()`
#'   (au minimum `ind_crt` et `ind_cst`).
#' @param ere_res Liste retournee par `executer_ressources_ere()`
#'   (au minimum `ressources_crt` et `ressources_vpap`).
#' @param derniere_annee_cna Derniere annee des CNA definitifs.
#' @param annee_fin_proj Annee de fin de projection.
#' @param codes_negatifs_autorises Vecteur de codes pour lesquels les
#'   negatifs Cholette sont acceptes.
#' @param export_excel Booleen, `TRUE` pour exporter un fichier Excel.
#'
#' @return Liste avec au moins :
#' \describe{
#'   \item{imp_cna}{Cibles CNA impots (`ImpCrt`, `ImpVol`, `ImpCh`).}
#'   \item{ind_imp_extraits}{Indicateurs extraits depuis `donnees`.}
#'   \item{ind_imp_ere}{Indicateur impots calcule depuis ERE.}
#'   \item{imp_bench}{Sortie complete de `benchmarker_impots_taxes()`.}
#'   \item{cnt_imp_net_crt}{Serie benchmarkee en courant.}
#'   \item{cnt_imp_net_ch}{Serie benchmarkee en chaine.}
#'   \item{cnt_imp_net_vpap}{Serie dechainee VPAP.}
#'   \item{verif_crt}{Verification annuelle courant (benchmark vs CNA).}
#'   \item{alertes_imp}{Sous-ensemble des ecarts annuels non nuls.}
#'   \item{path_export_imp}{Chemin du fichier Excel exporte, sinon `NULL`.}
#' }
#' @export
executer_pipeline_impots_taxes <- function(root_dir,
                                           donnees,
                                           ere_res,
                                           derniere_annee_cna,
                                           annee_fin_proj,
                                           codes_negatifs_autorises = character(0),
                                           export_excel = TRUE) {

  message("\u25b6 Pipeline Impots/Taxes nets : demarrage")

  path_cna <- file.path(root_dir, "R_CNA.xlsx")
  imp_cna <- importer_cna_impots(path_cna)

  ind_imp_extraits <- extraire_ind_impots(
    ind_crt = donnees$ind_crt,
    ind_cst = donnees$ind_cst
  )

  ind_imp_ere <- calculer_ind_impots_depuis_ere(
    ressources_crt  = ere_res$ressources_crt,
    ressources_vpap = ere_res$ressources_vpap
  )

  imp_bench <- benchmarker_impots_taxes(
    ind_imp_crt = ind_imp_extraits$ind_imp_crt,
    ind_imp_ere = ind_imp_ere,
    imp_cna = imp_cna,
    derniere_annee_cna = derniere_annee_cna,
    annee_fin_proj = annee_fin_proj,
    codes_negatifs_autorises = codes_negatifs_autorises
  )

  cnt_imp_net_crt  <- imp_bench$cnt_imp_net_crt
  cnt_imp_net_ch   <- imp_bench$cnt_imp_net_ch
  cnt_imp_net_vpap <- imp_bench$cnt_imp_net_vpap

  types_imp <- c("Impots_Nets", "Impots_nets", "impots_nets", "Impots nets")

  cible_crt <- imp_cna$imp_crt |>
    dplyr::filter(trimws(.data$type_ind) %in% types_imp) |>
    dplyr::group_by(.data$annee, .data$full_code) |>
    dplyr::summarise(cible_cna = sum(.data$valeur, na.rm = TRUE), .groups = "drop")

  somme_trim <- cnt_imp_net_crt |>
    dplyr::group_by(.data$annee, .data$full_code) |>
    dplyr::summarise(
      somme_trim_bench = sum(.data$valeur_cal, na.rm = TRUE),
      .groups = "drop"
    )

  verif_crt <- dplyr::full_join(
    cible_crt,
    somme_trim,
    by = c("annee", "full_code")
  ) |>
    dplyr::mutate(
      dplyr::across(c(.data$cible_cna, .data$somme_trim_bench),
                    ~ tidyr::replace_na(.x, 0)),
      ecart = .data$somme_trim_bench - .data$cible_cna,
      ecart_abs = abs(.data$ecart),
      ecart_rel_pct = dplyr::if_else(
        .data$cible_cna == 0,
        NA_real_,
        100 * .data$ecart / .data$cible_cna
      )
    ) |>
    dplyr::arrange(.data$full_code, .data$annee)

  alertes_imp <- verif_crt |>
    dplyr::filter(.data$ecart_abs > 1e-8)

  message("\u2705 Verification annuelle impots (courant) : ",
          nrow(verif_crt), " lignes | alertes : ", nrow(alertes_imp))

  path_export_imp <- NULL
  if (isTRUE(export_excel)) {
    path_export_imp <- file.path(
      root_dir,
      paste0("Impots_Taxes_Nets_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    )

    writexl::write_xlsx(
      list(
        imp_cna_crt       = imp_cna$imp_crt,
        ind_imp_extraits  = ind_imp_extraits$ind_imp_crt,
        ind_imp_ere       = ind_imp_ere,
        cnt_imp_net_crt   = cnt_imp_net_crt,
        cnt_imp_net_ch    = cnt_imp_net_ch,
        cnt_imp_net_vpap  = cnt_imp_net_vpap,
        verif_crt         = verif_crt,
        alertes_imp       = alertes_imp
      ),
      path = path_export_imp
    )

    message("\u2705 Export impots/taxes : ", path_export_imp)
  }

  list(
    imp_cna = imp_cna,
    ind_imp_extraits = ind_imp_extraits,
    ind_imp_ere = ind_imp_ere,
    imp_bench = imp_bench,
    cnt_imp_net_crt = cnt_imp_net_crt,
    cnt_imp_net_ch = cnt_imp_net_ch,
    cnt_imp_net_vpap = cnt_imp_net_vpap,
    verif_crt = verif_crt,
    alertes_imp = alertes_imp,
    path_export_imp = path_export_imp
  )
}
