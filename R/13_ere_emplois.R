#' @import dplyr
#' @import tidyr
NULL

# ==============================================================================
# ERE — EMPLOIS (COURANT + VOLUME + VPAP)
# ==============================================================================

#' Assembler les emplois ERE en courant, volume chaîné et VPAP
#'
#' Encapsule les sections 2.5 et 2.7 du programme : estimation des emplois
#' ERE par méthode de benchmarking ou de solde, assemblage courant et chaîné,
#' et calcul de la VPAP par déchaînage.
#'
#' La logique de chaque composante emploi (CFmarch, CFnmarch, CFapu, CFisblsm,
#' FBCF, VS, AOV) est pilotée par le fichier \code{Methode_ERE.xlsx} et par
#' \code{map_feuille_cna}.
#'
#' @param ere_res Liste retournée par \code{executer_ressources_ere()} ; doit
#'   contenir \code{ressources_crt}, \code{ressources_vpap} et \code{noms_ere}.
#' @param p2_ere_crt Tibble CI par produit ERE en courant (colonne \code{P2_crt}).
#' @param p2_ere_vol Tibble CI par produit ERE en volume (colonne \code{P2_vol}).
#' @param cnt_exp_final Tibble exportations benchmarkées (sortie de
#'   \code{executer_benchmarking_imp_exp()}).
#' @param p1_ere_crt Tibble production par produit ERE en courant.
#' @param p1_ere_vol Tibble production par produit ERE en volume.
#' @param cna_ere_struct Liste ERE annuelle.
#' @param path_methode_ere Chemin vers \code{Methode_ERE.xlsx}.
#' @param map_feuille_cna Tibble de correspondance feuille → composante CNA.
#'   Colonnes : \code{feuille}, \code{composante_cna}.
#' @param map_apu_cols Tibble de correspondance code ERE → colonne APU dans
#'   \code{ind_crt/ind_cst}. Colonnes : \code{Code_Produit}, \code{col_apu}.
#' @param ind_crt Tibble indicateurs trimestriels courants (\code{donnees$ind_crt}).
#' @param ind_cst Tibble indicateurs trimestriels constants (\code{donnees$ind_cst}).
#'
#' @return Liste à trois éléments :
#' \describe{
#'   \item{\code{emplois_crt}}{Emplois ERE en courant (9 composantes, 0 NA).}
#'   \item{\code{emplois_vol}}{Emplois ERE en volume chaîné.}
#'   \item{\code{emplois_vpap}}{Emplois ERE en VPAP (courant + chaîné + vpap).}
#' }
#' @export
executer_emplois_ere <- function(ere_res,
                                  p2_ere_crt, p2_ere_vol,
                                  cnt_exp_final,
                                  p1_ere_crt, p1_ere_vol,
                                  cna_ere_struct,
                                  path_methode_ere,
                                  map_feuille_cna,
                                  map_apu_cols,
                                  ind_crt, ind_cst) {

  ressources_crt  <- ere_res$ressources_crt
  ressources_vpap <- ere_res$ressources_vpap
  noms_ere        <- ere_res$noms_ere

  # --- Lecture des méthodes ---
  message("\u25b6 Emplois ERE \u2014 lecture des m\u00e9thodes...")
  methodes_ere <- lire_methodes_ere(path_methode_ere, map_feuille_cna)

  prods_solde_vs      <- dplyr::pull(
    dplyr::filter(methodes_ere, feuille == "VS",      Methode == "solde"),
    Code_Produit)
  prods_solde_cfmarch <- dplyr::pull(
    dplyr::filter(methodes_ere, feuille == "CFmarch", Methode == "solde"),
    Code_Produit)

  # --- Grille trimestres x produits ---
  grille_trim <- dplyr::distinct(ressources_crt, annee, trimestre, Code_Produit)

  # --- Indicateurs APU (courant + constant) ---
  .ind_apu <- function(ind, map_apu) {
    ind |>
      dplyr::filter(full_code %in% map_apu$col_apu) |>
      dplyr::select(annee, trimestre, full_code, valeur) |>
      dplyr::left_join(map_apu, by = c("full_code" = "col_apu")) |>
      dplyr::select(annee, trimestre, Code_Produit, valeur)
  }
  ind_apu_trim      <- .ind_apu(ind_crt, map_apu_cols)
  ind_apu_vpap_trim <- .ind_apu(ind_cst, map_apu_cols)

  # ------------------------------------------------------------------
  # A. EMPLOIS COURANTS
  # ------------------------------------------------------------------
  message("\u25b6 Emplois ERE courants...")

  ind_ressources_trim <- ressources_crt |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(valeur = sum(valeur_composante, na.rm = TRUE), .groups = "drop")
  ind_prod_trim <- dplyr::rename(p1_ere_crt, valeur = P1_crt)

  # Composantes benchmarkees directement par indicateur trimestriel
  emplois_bench <- estimer_emplois_ere(
    map_feuille_cna, methodes_ere, cna_ere_struct,
    grille_trim, ind_ressources_trim, ind_prod_trim, ind_apu_trim
  )

  emplois_non_solde <- dplyr::bind_rows(
    dplyr::mutate(dplyr::rename(p2_ere_crt, valeur_cal = P2_crt),  composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_crt), composante = "EXPORTATIONS"),
    emplois_bench
  )
  # Composantes residuelles : solde trimestriel puis benchmarking annuel CNA
  solde_vs <- calculer_solde_ere(
    "VS", methodes_ere, ind_ressources_trim, emplois_non_solde,
    c("CI", "EXPORTATIONS", "CFmarch", "CFnmarch", "CFapu", "CFisblsm", "FBCF", "AOV"),
    map_feuille_cna, cna_ere_struct, type_prix = "CnaErECrt"
  )
  solde_cfmarch <- calculer_solde_ere(
    "CFmarch", methodes_ere, ind_ressources_trim,
    dplyr::bind_rows(emplois_non_solde, solde_vs),
    c("CI", "EXPORTATIONS", "CFnmarch", "CFapu", "CFisblsm", "FBCF", "VS", "AOV"),
    map_feuille_cna, cna_ere_struct, type_prix = "CnaErECrt"
  )

  emplois_crt <- dplyr::bind_rows(
    dplyr::mutate(dplyr::rename(p2_ere_crt, valeur_cal = P2_crt),  composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_crt), composante = "EXPORTATIONS"),
    emplois_bench |>
      dplyr::filter(
        !(composante == "VS"      & Code_Produit %in% prods_solde_vs),
        !(composante == "CFmarch" & Code_Produit %in% prods_solde_cfmarch)
      ),
    solde_vs,
    solde_cfmarch
  ) |>
    dplyr::select(annee, trimestre, Code_Produit, composante, valeur_cal) |>
    dplyr::arrange(composante, Code_Produit, annee, trimestre)

  # ------------------------------------------------------------------
  # B. EMPLOIS EN VOLUME CHAÎNÉ
  # ------------------------------------------------------------------
  message("\u25b6 Emplois ERE volume cha\u00een\u00e9...")

  ind_ressources_vpap_trim <- ressources_vpap |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(valeur = sum(valeur_composante, na.rm = TRUE), .groups = "drop")
  ind_prod_vpap_trim <- dplyr::rename(p1_ere_vol, valeur = P1_vol)

  # Composantes benchmarkees directement par indicateur trimestriel
  emplois_bench_ch <- estimer_emplois_ere(
    map_feuille_cna, methodes_ere, cna_ere_struct,
    grille_trim, ind_ressources_vpap_trim,
    ind_prod_vpap_trim, ind_apu_vpap_trim,
    type_prix = "CnaErECh"
  )

  emplois_non_solde_vol <- dplyr::bind_rows(
    dplyr::mutate(dplyr::rename(p2_ere_vol, valeur_cal = P2_vol),  composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_vpap), composante = "EXPORTATIONS"),
    emplois_bench_ch
  )
  # Composantes residuelles : solde trimestriel puis benchmarking annuel CNA
  solde_vs_vol <- calculer_solde_ere(
    "VS", methodes_ere, ind_ressources_vpap_trim, emplois_non_solde_vol,
    c("CI", "EXPORTATIONS", "CFmarch", "CFnmarch", "CFapu", "CFisblsm", "FBCF", "AOV"),
    map_feuille_cna, cna_ere_struct, type_prix = "CnaErECh"
  )
  solde_cfmarch_vol <- calculer_solde_ere(
    "CFmarch", methodes_ere, ind_ressources_vpap_trim,
    dplyr::bind_rows(emplois_non_solde_vol, solde_vs_vol),
    c("CI", "EXPORTATIONS", "CFnmarch", "CFapu", "CFisblsm", "FBCF", "VS", "AOV"),
    map_feuille_cna, cna_ere_struct, type_prix = "CnaErECh"
  )

  emplois_vol <- dplyr::bind_rows(
    dplyr::mutate(dplyr::rename(p2_ere_vol, valeur_cal = P2_vol),  composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_vpap), composante = "EXPORTATIONS"),
    emplois_bench_ch |>
      dplyr::filter(
        !(composante == "VS"      & Code_Produit %in% prods_solde_vs),
        !(composante == "CFmarch" & Code_Produit %in% prods_solde_cfmarch)
      ),
    solde_vs_vol,
    solde_cfmarch_vol
  ) |>
    dplyr::select(annee, trimestre, Code_Produit, composante, valeur_ch = valeur_cal) |>
    dplyr::arrange(composante, Code_Produit, annee, trimestre)

  # ------------------------------------------------------------------
  # C. VPAP PAR DÉCHAÎNAGE
  # ------------------------------------------------------------------
  emplois_vpap <- emplois_vol |>
    dplyr::inner_join(
      dplyr::rename(emplois_crt, valeur_crt = valeur_cal),
      by = c("annee", "trimestre", "Code_Produit", "composante"),
      relationship = "one-to-one"
    ) |>
    dplyr::group_by(Code_Produit, composante) |>
    dplyr::mutate(valeur_vpap = tidyr::replace_na(
      dechainer_valeurs(valeur_crt, valeur_ch, trim = TRUE), 0)) |>
    dplyr::ungroup()

  message("\u2705 Emplois ERE | ", nrow(emplois_crt), " lignes | ",
          dplyr::n_distinct(emplois_crt$composante), " composantes | ",
          "NA crt : ", sum(is.na(emplois_crt$valeur_cal)),
          " | NA ch : ",   sum(is.na(emplois_vol$valeur_ch)),
          " | NA vpap : ", sum(is.na(emplois_vpap$valeur_vpap)))

  list(emplois_crt  = emplois_crt,
       emplois_vol  = emplois_vol,
       emplois_vpap = emplois_vpap)
}
