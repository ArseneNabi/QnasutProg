#' @import dplyr
#' @import tidyr
NULL

# ==============================================================================
# ERE — IMPORTATIONS ET EXPORTATIONS (BENCHMARKING + VPAP)
# ==============================================================================

#' Distribuer une cible annuelle CNA à plat sur toutes les années-trimestres
#'
#' Pour les années couvertes par le CNA : valeur = cible / 4.
#' Pour les années de projection (au-delà du CNA) : valeur = dernière cible CNA / 4.
#'
#' @param cna Tibble cible CNA : \code{annee}, \code{Code_Produit}, \code{valeur}.
#' @param annees_toutes Vecteur entier de toutes les années à couvrir.
#'
#' @return Tibble \code{annee}, \code{trimestre}, \code{full_code}, \code{valeur_cal}.
#' @keywords internal
distribuer_plat <- function(cna, annees_toutes) {

  derniere_val <- cna |>
    dplyr::group_by(.data$Code_Produit) |>
    dplyr::slice_max(.data$annee, n = 1, with_ties = FALSE) |>
    dplyr::select(.data$Code_Produit,
                  derniere_valeur = .data$valeur,
                  derniere_annee  = .data$annee) |>
    dplyr::ungroup()

  tidyr::expand_grid(
    annee        = annees_toutes,
    trimestre    = 1:4,
    Code_Produit = unique(cna$Code_Produit)
  ) |>
    dplyr::left_join(
      cna |> dplyr::rename(valeur_cna = .data$valeur),
      by = c("annee", "Code_Produit")
    ) |>
    dplyr::left_join(derniere_val, by = "Code_Produit") |>
    dplyr::mutate(
      valeur_cal = tidyr::replace_na(
        dplyr::coalesce(.data$valeur_cna, .data$derniere_valeur) / 4, 0)
    ) |>
    dplyr::select(annee, trimestre, full_code = .data$Code_Produit, valeur_cal)
}


#' Benchmarker les importations et exportations trimestrielles
#'
#' Convertit les indicateurs import/export de \code{ind_ce_struct} en format
#' long, les cale sur les cibles CNA annuelles par la méthode Cholette, puis
#' calcule la VPAP par déchaînage.
#'
#' Pour deux situations, la méthode Cholette est remplacée par une
#' \strong{distribution plate} (cible CNA / 4 par trimestre, dernière valeur
#' CNA extrapolée sur les années de projection) :
#' \enumerate{
#'   \item Le produit est absent des indicateurs trimestriels.
#'   \item Le benchmarking Cholette produit des valeurs négatives pour ce
#'     produit (cibles annuelles résiduelles/faibles).
#' }
#' Les produits traités en distribution plate sont signalés dans le message
#' de fin.
#'
#' @param ind_ce_struct Liste des indicateurs trimestriels import/export.
#' @param cna_ere_struct Liste ERE annuelle.
#'
#' @return Liste à deux éléments : \code{cnt_imp_final}, \code{cnt_exp_final}.
#' @export
executer_benchmarking_imp_exp <- function(ind_ce_struct, cna_ere_struct) {

  message("\u25b6 Benchmarking imports/exports...")

  # --- Indicateurs trimestriels (format long) ---
  ind_imp_crt <- pivoter_ere_long(ind_ce_struct[["Import"]][["Crt"]], "Crt", "IMPORTATIONS")
  ind_imp_ch  <- pivoter_ere_long(ind_ce_struct[["Import"]][["Ch"]],  "Ch",  "IMPORTATIONS")
  ind_exp_crt <- pivoter_ere_long(ind_ce_struct[["Export"]][["Crt"]], "Crt", "EXPORTATIONS")
  ind_exp_ch  <- pivoter_ere_long(ind_ce_struct[["Export"]][["Ch"]],  "Ch",  "EXPORTATIONS")

  # Grille complète des années (pour la distribution plate en projection)
  annees_toutes <- sort(unique(ind_imp_crt$annee))

  # --- Cibles annuelles CNA (deux versions) ---
  # - "plein"  : toutes les années y compris les zéros → pour distribuer_plat
  # - "filtre" : seulement les années non nulles       → pour Cholette
  .lire_cna <- function(comp, type_prix, filtre_zero = FALSE) {
    df <- pivoter_ere_long(cna_ere_struct[[comp]][[type_prix]], type_prix, comp) |>
      dplyr::select(annee, Code_Produit, valeur)
    if (filtre_zero) dplyr::filter(df, .data$valeur != 0) else df
  }
  nm_exp <- names(cna_ere_struct)[grepl("Exportation", names(cna_ere_struct))]

  # CNA filtrés pour le benchmarking Cholette (évite les contraintes nulles)
  cna_imp_crt <- .lire_cna("IMPORTATIONS", "CnaErECrt", filtre_zero = TRUE)
  cna_imp_ch  <- .lire_cna("IMPORTATIONS", "CnaErECh",  filtre_zero = TRUE)
  cna_exp_crt <- .lire_cna(nm_exp, "CnaErECrt", filtre_zero = TRUE)
  cna_exp_ch  <- .lire_cna(nm_exp, "CnaErECh",  filtre_zero = TRUE)

  # CNA complets pour la distribution plate (les zéros indiquent absence réelle)
  cna_imp_crt_plein <- .lire_cna("IMPORTATIONS", "CnaErECrt")
  cna_imp_ch_plein  <- .lire_cna("IMPORTATIONS", "CnaErECh")
  cna_exp_crt_plein <- .lire_cna(nm_exp, "CnaErECrt")
  cna_exp_ch_plein  <- .lire_cna(nm_exp, "CnaErECh")

  # --- Benchmarking Cholette (interne) ---
  .bench <- function(ind, cna, type) {
    benchmark_groupe(
      ind |>
        dplyr::rename(full_code = Code_Produit) |>
        dplyr::mutate(type_ind = type,
                      periode  = paste0(annee, "T", trimestre)),
      cna |> dplyr::rename(full_code = Code_Produit),
      type_filter = type,
      value_col   = "valeur"
    )
  }

  # --- Repli distribution plate avec détection des négatifs ---
  # Retourne une table unifiée couvrant TOUS les produits CNA non nuls
  .bench_avec_repli <- function(ind, cna, cna_plein, type, label) {

    prods_ind <- unique(ind$Code_Produit)
    prods_cna <- unique(cna$Code_Produit)
    # Produits présents dans le CNA complet (y compris ceux tout à zéro sauf projection)
    prods_cna_plein <- unique(cna_plein$Code_Produit)

    # Produits sans indicateur → distribution plate directe
    prods_sans_ind <- setdiff(prods_cna, prods_ind)
    cna_avec_ind   <- cna |> dplyr::filter(.data$Code_Produit %in% prods_ind)

    # Benchmarking Cholette pour les produits couverts par l'indicateur
    bench_res <- if (nrow(cna_avec_ind) > 0) {
      .bench(ind, cna_avec_ind, type)
    } else {
      tibble::tibble(annee = integer(), trimestre = integer(),
                     full_code = character(), valeur_cal = numeric())
    }

    # Produits pour lesquels benchmark_groupe n'a produit aucun résultat
    prods_bench_vide <- setdiff(unique(cna_avec_ind$Code_Produit),
                                unique(bench_res$full_code))

    # Produits avec valeurs négatives après Cholette → distribution plate
    prods_negatifs <- bench_res |>
      dplyr::group_by(.data$full_code) |>
      dplyr::summarise(has_neg = any(.data$valeur_cal < 0, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::filter(.data$has_neg) |>
      dplyr::pull(.data$full_code)

    # Signalement
    if (length(prods_sans_ind) > 0)
      message("  \u21b3 [", label, "] distribution plate (sans indicateur) : ",
              paste(sort(prods_sans_ind), collapse = ", "))
    if (length(prods_bench_vide) > 0)
      message("  \u21b3 [", label, "] distribution plate (benchmark \u00e9chou\u00e9) : ",
              paste(sort(prods_bench_vide), collapse = ", "))
    if (length(prods_negatifs) > 0)
      message("  \u21b3 [", label, "] distribution plate (n\u00e9gatifs Cholette) : ",
              paste(sort(prods_negatifs), collapse = ", "))

    prods_negatifs <- union(prods_negatifs, prods_bench_vide)

    prods_plat <- union(prods_sans_ind, prods_negatifs)

    # Résultat Cholette sans les produits remplacés
    res_cholette <- bench_res |>
      dplyr::filter(!.data$full_code %in% prods_plat) |>
      dplyr::select(annee, trimestre, full_code, valeur_cal)

    # Distribution plate : utilise le CNA COMPLET (avec zéros) pour que les
    # années à cible nulle restent à 0 et que la projection utilise la vraie
    # dernière valeur CNA (pas la dernière valeur non nulle)
    prods_plat_ou_zero <- union(prods_plat,
                                 setdiff(prods_cna_plein, prods_cna))
    cna_plat  <- cna_plein |>
      dplyr::filter(.data$Code_Produit %in% prods_plat_ou_zero)
    res_plat  <- if (nrow(cna_plat) > 0) {
      distribuer_plat(cna_plat, annees_toutes)
    } else {
      tibble::tibble(annee = integer(), trimestre = integer(),
                     full_code = character(), valeur_cal = numeric())
    }

    dplyr::bind_rows(res_cholette, res_plat)
  }

  cnt_imp_crt_l <- .bench_avec_repli(ind_imp_crt, cna_imp_crt, cna_imp_crt_plein, "Import", "imp_crt")
  cnt_imp_ch_l  <- .bench_avec_repli(ind_imp_ch,  cna_imp_ch,  cna_imp_ch_plein,  "Import", "imp_ch")
  cnt_exp_crt_l <- .bench_avec_repli(ind_exp_crt, cna_exp_crt, cna_exp_crt_plein, "Export", "exp_crt")
  cnt_exp_ch_l  <- .bench_avec_repli(ind_exp_ch,  cna_exp_ch,  cna_exp_ch_plein,  "Export", "exp_ch")

  # --- Assemblage final avec couverture symétrique crt / ch ---
  # full_join + replace_na(0) pour les produits présents dans un seul des deux
  .avec_vpap <- function(cnt_crt, cnt_ch, col_crt, col_ch, col_vpap) {
    dplyr::full_join(
      cnt_crt |> dplyr::select(annee, trimestre,
                                Code_Produit = full_code,
                                !!col_crt := valeur_cal),
      cnt_ch  |> dplyr::select(annee, trimestre,
                                Code_Produit = full_code,
                                !!col_ch  := valeur_cal),
      by = c("annee", "trimestre", "Code_Produit")
    ) |>
      dplyr::mutate(
        dplyr::across(c(!!col_crt, !!col_ch), \(x) tidyr::replace_na(x, 0))
      ) |>
      dplyr::group_by(Code_Produit) |>
      dplyr::mutate(
        !!col_vpap := tidyr::replace_na(
          dechainer_valeurs(.data[[col_crt]], .data[[col_ch]], trim = TRUE), 0)
      ) |>
      dplyr::ungroup()
  }

  cnt_imp_final <- .avec_vpap(cnt_imp_crt_l, cnt_imp_ch_l, "imp_crt", "imp_ch", "imp_vpap")
  cnt_exp_final <- .avec_vpap(cnt_exp_crt_l, cnt_exp_ch_l, "exp_crt", "exp_ch", "exp_vpap")

  message("\u2705 Imports/Exports | produits imp : ",
          dplyr::n_distinct(cnt_imp_final$Code_Produit),
          " | produits exp : ",
          dplyr::n_distinct(cnt_exp_final$Code_Produit),
          " | NA imp_vpap : ", sum(is.na(cnt_imp_final$imp_vpap)),
          " | NA exp_vpap : ", sum(is.na(cnt_exp_final$exp_vpap)))

  list(cnt_imp_final = cnt_imp_final,
       cnt_exp_final = cnt_exp_final)
}
