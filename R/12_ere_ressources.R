#' @import dplyr
#' @import tidyr
NULL

# ==============================================================================
# ERE — PRODUCTION ET CI PAR PRODUIT ERE + RESSOURCES (COURANT, VPAP, CHAÎNÉ)
# ==============================================================================

#' Transformer la production branche → produit ERE (toutes branches)
#'
#' Désagrège \code{p1_agreg_complet} (niveau branche, 60 branches incluant
#' GZ001 et HZ001) en produits N3 via les poids du TRE, puis agrège vers
#' la nomenclature ERE (\code{Code_Prod_Ct}).
#'
#' Cette fonction doit être appelée \strong{après} que \code{p1_agreg_complet}
#' soit finalisé à 60 branches (c'est-à-dire après
#' \code{calculer_ind5_depuis_production()}), de sorte que la production des
#' branches commerce (GZ001) et transport (HZ001) soit correctement ventilée
#' vers leurs produits respectifs.
#'
#' @param p1_agreg_complet Tibble production par branche, toutes branches.
#'   Colonnes attendues : \code{annee}, \code{trimestre}, \code{Code_Branche},
#'   \code{P1_crt_agg}, \code{P1_vpap_agg}.
#' @param poids_trim Tibble des poids branche-produit, typiquement
#'   \code{res_trim$poids_trim}.
#' @param Map_Produits Table de correspondance produits. Doit contenir
#'   \code{Code_Prod_N3} et \code{Code_Prod_Ct}.
#'
#' @return Liste à trois éléments :
#' \describe{
#'   \item{\code{p1_par_produit}}{Production désagrégée par produit N3
#'     (\code{valeur_crt}, \code{valeur_vol}).}
#'   \item{\code{p1_ere_crt}}{Production agrégée par produit ERE en courant
#'     (\code{P1_crt}).}
#'   \item{\code{p1_ere_vol}}{Production agrégée par produit ERE en volume VPAP
#'     (\code{P1_vol}).}
#'   \item{\code{p1_ere_ch}}{Production agrégée par produit ERE en volume
#'     chaîné (\code{P1_ch}).}
#' }
#' @export
calculer_p1_ere <- function(p1_agreg_complet, poids_trim, Map_Produits) {

  # 1. Branche → produit N3
  p1_par_produit <- transformer_branche_produit(
    df_branches   = p1_agreg_complet,
    poids_tbl     = poids_trim,
    operation     = "P1",
    value_crt_col = "P1_crt_agg",
    value_vol_col = "P1_vpap_agg",
    normalize     = TRUE
  )

  # 2. Produit N3 → nomenclature ERE
  table_n3_ere <- Map_Produits |>
    dplyr::transmute(
      Code_Produit = trimws(as.character(Code_Prod_N3)),
      Code_Prod_Ct = trimws(as.character(Code_Prod_Ct))
    ) |>
    dplyr::distinct()

  .agreger_ere <- function(col_val, col_sortie) {
    p1_par_produit |>
      dplyr::inner_join(table_n3_ere, by = "Code_Produit") |>
      dplyr::group_by(annee, trimestre, Code_Produit = Code_Prod_Ct) |>
      dplyr::summarise(!!col_sortie := sum(.data[[col_val]], na.rm = TRUE),
                       .groups = "drop")
  }

  p1_ere_crt <- .agreger_ere("valeur_crt", "P1_crt")
  p1_ere_vol <- .agreger_ere("valeur_vol", "P1_vol")
  p1_ere_ch <- dplyr::inner_join(
    p1_ere_crt,
    p1_ere_vol,
    by = c("annee", "trimestre", "Code_Produit")
  ) |>
    dplyr::arrange(Code_Produit, annee, trimestre) |>
    dplyr::group_by(Code_Produit) |>
    dplyr::mutate(P1_ch = calcul_valeur_chainee_trim(P1_crt, P1_vol)) |>
    dplyr::ungroup() |>
    dplyr::select(annee, trimestre, Code_Produit, P1_ch)

  message("\u2705 p1_ere | ", nrow(p1_ere_crt), " lignes | ",
          dplyr::n_distinct(p1_ere_crt$Code_Produit), " produits ERE | ",
          dplyr::n_distinct(p1_par_produit$Code_Produit), " produits N3 | ",
          "NA crt : ", sum(is.na(p1_ere_crt$P1_crt)),
          " | NA vol : ", sum(is.na(p1_ere_vol$P1_vol)),
          " | NA ch : ", sum(is.na(p1_ere_ch$P1_ch)))

  list(p1_par_produit = p1_par_produit,
       p1_ere_crt     = p1_ere_crt,
       p1_ere_vol     = p1_ere_vol,
       p1_ere_ch      = p1_ere_ch)
}


#' Agréger la CI trimestrielle vers la nomenclature ERE
#'
#' Convertit \code{p2_par_produit_n3} (nomenclature N3) en produits ERE
#' (\code{Code_Prod_Ct}) en courant et en volume.
#'
#' @param p2_par_produit_n3 Tibble CI par produit N3 issu de
#'   \code{calculer_ind4_depuis_ci()}. Colonnes : \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{valeur_crt}, \code{valeur_vol}.
#' @param Map_Produits Table de correspondance produits (\code{donnees$Map_Produits}).
#'   Doit contenir \code{Code_Prod_N3} et \code{Code_Prod_Ct}.
#'
#' @return Liste à trois éléments :
#' \describe{
#'   \item{\code{p2_ere_crt}}{CI par produit ERE en courant (\code{P2_crt}).}
#'   \item{\code{p2_ere_vol}}{CI par produit ERE en volume (\code{P2_vol}).}
#'   \item{\code{p2_ere_ch}}{CI par produit ERE en volume chaîné (\code{P2_ch}).}
#' }
#' @export
calculer_p2_ere <- function(p2_par_produit_n3, Map_Produits) {

  table_n3_ere <- Map_Produits |>
    dplyr::transmute(
      Code_Produit = trimws(as.character(Code_Prod_N3)),
      Code_Prod_Ct = trimws(as.character(Code_Prod_Ct))
    ) |>
    dplyr::distinct()

  .agreger <- function(col_val, col_sortie) {
    p2_par_produit_n3 |>
      dplyr::inner_join(table_n3_ere, by = "Code_Produit") |>
      dplyr::group_by(annee, trimestre, Code_Produit = Code_Prod_Ct) |>
      dplyr::summarise(!!col_sortie := sum(.data[[col_val]], na.rm = TRUE),
                       .groups = "drop")
  }

  p2_ere_crt <- .agreger("valeur_crt", "P2_crt")
  p2_ere_vol <- .agreger("valeur_vol", "P2_vol")
  p2_ere_ch <- dplyr::inner_join(
    p2_ere_crt,
    p2_ere_vol,
    by = c("annee", "trimestre", "Code_Produit")
  ) |>
    dplyr::arrange(Code_Produit, annee, trimestre) |>
    dplyr::group_by(Code_Produit) |>
    dplyr::mutate(P2_ch = calcul_valeur_chainee_trim(P2_crt, P2_vol)) |>
    dplyr::ungroup() |>
    dplyr::select(annee, trimestre, Code_Produit, P2_ch)

  message("\u2705 p2_ere | ", nrow(p2_ere_crt), " lignes | ",
          dplyr::n_distinct(p2_ere_crt$Code_Produit), " produits ERE | ",
          "NA crt : ", sum(is.na(p2_ere_crt$P2_crt)),
          " | NA vol : ", sum(is.na(p2_ere_vol$P2_vol)),
          " | NA ch : ", sum(is.na(p2_ere_ch$P2_ch)))

  list(p2_ere_crt = p2_ere_crt,
       p2_ere_vol = p2_ere_vol,
       p2_ere_ch  = p2_ere_ch)
}


#' Benchmarker la production trimestrielle produit contre les cibles CNA annuelles
#'
#' La production produit ERE issue de \code{calculer_p1_ere()} est dérivée d'une
#' désagrégation TRE branche → produit. Cette étape la recale sur les totaux
#' annuels CNA par produit ERE (\code{cna_ere_struct[["PRODUCTION"]]}),
#' garantissant que la composante PRODUCTION des ressources ERE est cohérente
#' avec les comptes annuels. Les marges (commerce, transport, TVA…) étant
#' calculées en aval sur la base de cette production, elles bénéficient
#' également de ce calage.
#'
#' @param p1_ere_crt Tibble production par produit ERE en courant, sortie de
#'   \code{calculer_p1_ere()}. Colonnes : \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{P1_crt}.
#' @param p1_ere_vol Tibble production par produit ERE en volume (VPAP),
#'   utilisé uniquement pour rétro-compatibilité de sortie. Colonnes :
#'   \code{annee}, \code{trimestre}, \code{Code_Produit}, \code{P1_vol}.
#' @param cna_ere_struct Liste ERE annuelle. Doit contenir l'élément
#'   \code{"PRODUCTION"} avec sous-éléments \code{CnaErECrt} et \code{CnaErECh}.
#' @param p1_ere_ch Tibble production par produit ERE en volume chaîné.
#'   Colonnes : \code{annee}, \code{trimestre}, \code{Code_Produit}, \code{P1_ch}.
#'   Si \code{NULL}, la série chaînée est reconstruite à partir de
#'   \code{p1_ere_crt} et \code{p1_ere_vol}.
#'
#' @return Liste à trois éléments :
#' \describe{
#'   \item{\code{p1_ere_crt}}{Production benchmarkée en courant.}
#'   \item{\code{p1_ere_vol}}{Production benchmarkée en volume VPAP
#'     (recalculée depuis courant + chaîné).}
#'   \item{\code{p1_ere_ch}}{Production benchmarkée en volume chaîné
#'     (série utilisée pour le benchmarking).}
#' }
#' @export
benchmarker_p1_ere <- function(p1_ere_crt, p1_ere_vol, cna_ere_struct,
                               p1_ere_ch = NULL) {

  .lire_cna_p1 <- function(type_prix) {
    pivoter_ere_long(
      cna_ere_struct[["PRODUCTION"]][[type_prix]],
      type_prix, "PRODUCTION"
    ) |>
      dplyr::select(annee, Code_Produit, valeur) |>
      dplyr::filter(.data$valeur != 0)
  }
  cna_p1_crt <- .lire_cna_p1("CnaErECrt")
  cna_p1_ch  <- .lire_cna_p1("CnaErECh")

  .bench_p1 <- function(p1_ere, col_val, cna_p1, type_label) {
    ind <- p1_ere |>
      dplyr::rename(full_code = Code_Produit, valeur = !!col_val) |>
      dplyr::mutate(type_ind = "PRODUCTION",
                    periode  = paste0(annee, "T", trimestre))

    res <- benchmark_groupe(
      ind,
      cna_p1 |> dplyr::rename(full_code = Code_Produit),
      type_filter = "PRODUCTION",
      value_col   = "valeur"
    )

    prods_bench       <- unique(res$full_code)
    prods_hors_bench  <- setdiff(unique(p1_ere$Code_Produit), prods_bench)

    if (length(prods_hors_bench) > 0)
      message("  \u21b3 [P1 ", type_label, "] hors benchmarking (pas de cible CNA) : ",
              paste(sort(prods_hors_bench), collapse = ", "))

    dplyr::bind_rows(
      res |>
        dplyr::select(annee, trimestre, Code_Produit = full_code,
                      !!col_val := valeur_cal),
      p1_ere |>
        dplyr::filter(.data$Code_Produit %in% prods_hors_bench)
    ) |>
      dplyr::arrange(Code_Produit, annee, trimestre)
  }

  message("\u25b6 Benchmarking P1 produit ERE contre cibles CNA annuelles...")
  if (is.null(p1_ere_ch)) {
    p1_ere_ch <- dplyr::inner_join(
      p1_ere_crt,
      p1_ere_vol,
      by = c("annee", "trimestre", "Code_Produit")
    ) |>
      dplyr::arrange(Code_Produit, annee, trimestre) |>
      dplyr::group_by(Code_Produit) |>
      dplyr::mutate(P1_ch = calcul_valeur_chainee_trim(P1_crt, P1_vol)) |>
      dplyr::ungroup() |>
      dplyr::select(annee, trimestre, Code_Produit, P1_ch)
  }

  p1_crt_bench <- .bench_p1(p1_ere_crt, "P1_crt", cna_p1_crt, "crt")
  p1_ch_bench  <- .bench_p1(p1_ere_ch,  "P1_ch",  cna_p1_ch,  "ch")
  p1_vol_bench <- dplyr::inner_join(
    p1_crt_bench,
    p1_ch_bench,
    by = c("annee", "trimestre", "Code_Produit")
  ) |>
    dplyr::arrange(Code_Produit, annee, trimestre) |>
    dplyr::group_by(Code_Produit) |>
    dplyr::mutate(P1_vol = dechainer_valeurs(P1_crt, P1_ch, trim = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(annee, trimestre, Code_Produit, P1_vol)

  message("\u2705 P1 bench\u00e9e | produits : ",
          dplyr::n_distinct(p1_crt_bench$Code_Produit),
          " | NA crt : ", sum(is.na(p1_crt_bench$P1_crt)),
          " | NA vol : ", sum(is.na(p1_vol_bench$P1_vol)),
          " | NA ch : ", sum(is.na(p1_ch_bench$P1_ch)))

  list(p1_ere_crt = p1_crt_bench,
       p1_ere_vol = p1_vol_bench,
       p1_ere_ch  = p1_ch_bench)
}


#' Benchmarker la CI trimestrielle produit contre les cibles CNA annuelles
#'
#' La CI produit ERE issue de \code{calculer_p2_ere()} est dérivée d'une
#' désagrégation TRE branche → produit. Cette étape la recale sur les totaux
#' annuels CNA par produit ERE (\code{cna_ere_struct[["CI Prix
#' d'acquisition"]]}), garantissant l'égalité annuelle Ressources = Emplois
#' sur les années des comptes annuels.
#'
#' Pour les années de projection (au-delà de la dernière année CNA),
#' la CI conserve la forme trimestrielle issue de la désagrégation TRE.
#'
#' @param p2_ere_crt Tibble CI par produit ERE en courant, sortie de
#'   \code{calculer_p2_ere()}. Colonnes : \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{P2_crt}.
#' @param p2_ere_vol Tibble CI par produit ERE en volume (VPAP),
#'   utilisé uniquement pour rétro-compatibilité de sortie. Colonnes :
#'   \code{annee}, \code{trimestre}, \code{Code_Produit}, \code{P2_vol}.
#' @param cna_ere_struct Liste ERE annuelle. Doit contenir l'élément
#'   \code{"CI Prix d'acquisition"} avec sous-éléments \code{CnaErECrt}
#'   et \code{CnaErECh}.
#' @param p2_ere_ch Tibble CI par produit ERE en volume chaîné.
#'   Colonnes : \code{annee}, \code{trimestre}, \code{Code_Produit}, \code{P2_ch}.
#'   Si \code{NULL}, la série chaînée est reconstruite à partir de
#'   \code{p2_ere_crt} et \code{p2_ere_vol}.
#'
#' @return Liste à trois éléments :
#' \describe{
#'   \item{\code{p2_ere_crt}}{CI benchmarkée en courant.}
#'   \item{\code{p2_ere_vol}}{CI benchmarkée en volume VPAP
#'     (recalculée depuis courant + chaîné).}
#'   \item{\code{p2_ere_ch}}{CI benchmarkée en volume chaîné
#'     (série utilisée pour le benchmarking).}
#' }
#' @export
benchmarker_p2_ere <- function(p2_ere_crt, p2_ere_vol, cna_ere_struct,
                               p2_ere_ch = NULL) {

  .lire_cna_ci <- function(type_prix) {
    pivoter_ere_long(
      cna_ere_struct[["CI Prix d'acquisition"]][[type_prix]],
      type_prix, "CI"
    ) |>
      dplyr::select(annee, Code_Produit, valeur) |>
      dplyr::filter(.data$valeur != 0)
  }
  cna_ci_crt <- .lire_cna_ci("CnaErECrt")
  cna_ci_ch  <- .lire_cna_ci("CnaErECh")

  .bench_ci <- function(p2_ere, col_val, cna_ci, type_label) {
    ind <- p2_ere |>
      dplyr::rename(full_code = Code_Produit, valeur = !!col_val) |>
      dplyr::mutate(type_ind = "CI",
                    periode  = paste0(annee, "T", trimestre))

    res <- benchmark_groupe(
      ind,
      cna_ci |> dplyr::rename(full_code = Code_Produit),
      type_filter = "CI",
      value_col   = "valeur"
    )

    # Produits absents du résultat (pas de cible CNA → garder valeur TRE)
    prods_bench <- unique(res$full_code)
    prods_all   <- unique(p2_ere$Code_Produit)
    prods_hors_bench <- setdiff(prods_all, prods_bench)

    if (length(prods_hors_bench) > 0)
      message("  \u21b3 [CI ", type_label, "] hors benchmarking (pas de cible CNA) : ",
              paste(sort(prods_hors_bench), collapse = ", "))

    dplyr::bind_rows(
      res |>
        dplyr::select(annee, trimestre, Code_Produit = full_code,
                      !!col_val := valeur_cal),
      p2_ere |>
        dplyr::filter(.data$Code_Produit %in% prods_hors_bench)
    ) |>
      dplyr::arrange(Code_Produit, annee, trimestre)
  }

  message("\u25b6 Benchmarking CI produit ERE contre cibles CNA annuelles...")
  if (is.null(p2_ere_ch)) {
    p2_ere_ch <- dplyr::inner_join(
      p2_ere_crt,
      p2_ere_vol,
      by = c("annee", "trimestre", "Code_Produit")
    ) |>
      dplyr::arrange(Code_Produit, annee, trimestre) |>
      dplyr::group_by(Code_Produit) |>
      dplyr::mutate(P2_ch = calcul_valeur_chainee_trim(P2_crt, P2_vol)) |>
      dplyr::ungroup() |>
      dplyr::select(annee, trimestre, Code_Produit, P2_ch)
  }

  p2_crt_bench <- .bench_ci(p2_ere_crt, "P2_crt", cna_ci_crt, "crt")
  p2_ch_bench  <- .bench_ci(p2_ere_ch,  "P2_ch",  cna_ci_ch,  "ch")
  p2_vol_bench <- dplyr::inner_join(
    p2_crt_bench,
    p2_ch_bench,
    by = c("annee", "trimestre", "Code_Produit")
  ) |>
    dplyr::arrange(Code_Produit, annee, trimestre) |>
    dplyr::group_by(Code_Produit) |>
    dplyr::mutate(P2_vol = dechainer_valeurs(P2_crt, P2_ch, trim = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(annee, trimestre, Code_Produit, P2_vol)

  message("\u2705 CI bench\u00e9e | produits crt : ",
          dplyr::n_distinct(p2_crt_bench$Code_Produit),
          " | NA crt : ", sum(is.na(p2_crt_bench$P2_crt)),
          " | NA vol : ", sum(is.na(p2_vol_bench$P2_vol)),
          " | NA ch : ", sum(is.na(p2_ch_bench$P2_ch)))

  list(p2_ere_crt = p2_crt_bench,
       p2_ere_vol = p2_vol_bench,
       p2_ere_ch  = p2_ch_bench)
}


#' Assembler les ressources ERE en courant, VPAP et chaîné
#'
#' Calcule et trimestrialise les ratios ERE pour le courant et le volume,
#' assemble les ressources complètes (9 composantes), puis dérive les valeurs
#' chaînées.
#'
#' Les ressources sont : PRODUCTION, IMPORTATIONS, IMPOT sur Import,
#' IMPOT sur export, MARGE de commerce, MARGE de transport, TVA,
#' IMPOT sur produit, Subventions.
#'
#' @param p1_ere_crt Tibble production par produit ERE en courant
#'   (colonnes : \code{annee}, \code{trimestre}, \code{Code_Produit}, \code{P1_crt}).
#' @param p1_ere_vol Tibble production par produit ERE en volume
#'   (colonnes : \code{annee}, \code{trimestre}, \code{Code_Produit}, \code{P1_vol}).
#' @param cnt_imp_final Tibble importations benchmarkées (sortie de
#'   \code{executer_benchmarking_imp_exp()}).
#' @param cna_ere_struct Liste ERE annuelle (\code{donnees$cna_ere_struct}).
#' @param derniere_annee_cna Dernière année des CNA définitifs.
#' @param annee_fin_proj Année de fin de projection.
#'
#' @return Liste à quatre éléments :
#' \describe{
#'   \item{\code{ressources_crt}}{Ressources ERE en courant (9 composantes,
#'     39 produits).}
#'   \item{\code{ressources_vpap}}{Ressources ERE en volume VPAP.}
#'   \item{\code{ressources_chaine}}{Ressources ERE totales en chaîné
#'     (\code{valeur_crt}, \code{valeur_vpap}, \code{valeur_ch}) par produit.}
#'   \item{\code{noms_ere}}{Vecteur des noms des composantes ERE.}
#' }
#' @export
executer_ressources_ere <- function(p1_ere_crt, p1_ere_vol, cnt_imp_final,
                                     cna_ere_struct,
                                     derniere_annee_cna, annee_fin_proj) {

  noms_ere <- names(cna_ere_struct)

  message("\u25b6 Ressources ERE courant...")
  ratios_ere_crt  <- calculer_ratios_ere(cna_ere_struct, type_prix = "CnaErECrt")
  ratios_ere_trim <- trimestrialiser_ratios_ere(ratios_ere_crt,
                                                 derniere_annee_cna, annee_fin_proj)
  ressources_crt <- assembler_ressources_ere(
    p1_ere         = p1_ere_crt |> dplyr::select(annee, trimestre, Code_Produit, P1_crt),
    imp_ere        = cnt_imp_final |> dplyr::select(annee, trimestre, Code_Produit, imp_crt),
    ratios_trim    = ratios_ere_trim,
    type_prix      = "CnaErECrt",
    noms_ere       = noms_ere,
    composante_p1  = "P1_crt",
    composante_imp = "imp_crt"
  )

  message("\u25b6 Ressources ERE volume (VPAP)...")
  ratios_ere_vol      <- calculer_ratios_ere(cna_ere_struct, type_prix = "CnaErEVol")
  ratios_ere_vol_trim <- trimestrialiser_ratios_ere(ratios_ere_vol,
                                                     derniere_annee_cna, annee_fin_proj)
  ressources_vpap <- assembler_ressources_ere(
    p1_ere         = p1_ere_vol |> dplyr::select(annee, trimestre, Code_Produit, P1_vol),
    imp_ere        = cnt_imp_final |> dplyr::select(annee, trimestre, Code_Produit, imp_vpap),
    ratios_trim    = ratios_ere_vol_trim,
    type_prix      = "CnaErEVol",
    noms_ere       = noms_ere,
    composante_p1  = "P1_vol",
    composante_imp = "imp_vpap"
  )

  message("\u25b6 Ressources ERE cha\u00een\u00e9es...")
  ressources_chaine <- dplyr::inner_join(
    ressources_crt |>
      dplyr::group_by(annee, trimestre, Code_Produit) |>
      dplyr::summarise(valeur_crt  = sum(valeur_composante, na.rm = TRUE),
                       .groups = "drop"),
    ressources_vpap |>
      dplyr::group_by(annee, trimestre, Code_Produit) |>
      dplyr::summarise(valeur_vpap = sum(valeur_composante, na.rm = TRUE),
                       .groups = "drop"),
    by = c("annee", "trimestre", "Code_Produit")
  ) |>
    dplyr::group_by(Code_Produit) |>
    dplyr::mutate(valeur_ch = tidyr::replace_na(
      calcul_valeur_chainee_trim(valeur_crt, valeur_vpap), 0)) |>
    dplyr::ungroup()

  message("\u2705 Ressources ERE | ",
          dplyr::n_distinct(ressources_crt$composante), " composantes | ",
          dplyr::n_distinct(ressources_crt$Code_Produit), " produits | ",
          "NA crt : ", sum(is.na(ressources_crt$valeur_composante)),
          " | NA ch : ", sum(is.na(ressources_chaine$valeur_ch)))

  list(ressources_crt   = ressources_crt,
       ressources_vpap  = ressources_vpap,
       ressources_chaine = ressources_chaine,
       noms_ere          = noms_ere)
}
