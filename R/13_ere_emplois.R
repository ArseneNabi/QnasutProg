#' @import dplyr
#' @import tidyr
NULL

#' Lire et normaliser les methodes d estimation ERE depuis Methode_ERE.xlsx
#'
#' @param path_methode_ere Chemin vers le fichier Methode_ERE.xlsx.
#' @param map_feuille_cna Tibble de correspondance feuille <-> composante_cna
#'   (colonnes : feuille, composante_cna).
#' @return Tibble avec colonnes feuille, Code_Produit, Methode, composante_cna.
#' @export
lire_methodes_ere <- function(path_methode_ere, map_feuille_cna) {
  map_feuille_cna$feuille |>
    purrr::set_names() |>
    purrr::map_dfr(function(f) {
      readxl::read_excel(path_methode_ere, sheet = f) |>
        dplyr::select(Code_Produit = Code, Methode) |>
        dplyr::mutate(feuille = f)
    }) |>
    dplyr::left_join(map_feuille_cna, by = "feuille") |>
    dplyr::mutate(Methode = tolower(trimws(Methode))) |>
    dplyr::mutate(Methode = dplyr::case_when(
      grepl("lissage", Methode) ~ "lissage",
      grepl("indicateur.*ressource", Methode) ~ "ind_ressources",
      grepl("ind apu", Methode) ~ "ind_apu",
      grepl("prod", Methode) ~ "prod",
      grepl("solde", Methode) ~ "solde",
      TRUE ~ "zero"
    ))
}

#' Estimer une composante emploi ERE par benchmarking
#'
#' Construit l indicateur trimestriel selon la methode definie dans les
#' metadonnees (lissage, ind_ressources, prod, ind_apu, zero), puis benchmarke
#' sur la cible CNA annuelle. Les produits a methode solde sont exclus.
#'
#' @param feuille_nm Nom de la feuille dans Methode_ERE.xlsx.
#' @param composante_cna_nm Nom exact de la composante dans cna_ere_struct.
#' @param methodes_ere Table longue des methodes (feuille, Code_Produit, Methode).
#' @param cna_ere_struct Liste des composantes ERE annuelles.
#' @param grille_trim Tibble annee x trimestre x Code_Produit.
#' @param ind_ressources_trim Indicateur ressources totales trimestriel.
#' @param ind_prod_trim Indicateur production trimestriel.
#' @param ind_apu_trim Indicateur APU trimestriel.
#' @param type_prix Type de prix pour la cible CNA : \code{"CnaErECrt"} (defaut,
#'   prix courants) ou \code{"CnaErECh"} (volume chaine).
#' @return Tibble : annee, trimestre, Code_Produit, valeur_cal, composante.
#' @export
estimer_composante_emploi <- function(feuille_nm, composante_cna_nm,
                                      methodes_ere, cna_ere_struct,
                                      grille_trim, ind_ressources_trim,
                                      ind_prod_trim, ind_apu_trim,
                                      type_prix = "CnaErECrt") {
  meth_comp <- dplyr::filter(methodes_ere, feuille == feuille_nm, Methode != "solde")
  produits_methodes <- dplyr::select(meth_comp, Code_Produit, Methode)

  sous_element <- if (type_prix == "CnaErECh") "CnaErECh" else "CnaErECrt"
  libelle_prix <- if (type_prix == "CnaErECh") "Ch" else "Crt"

  cna_target <- pivoter_ere_long(
    cna_ere_struct[[composante_cna_nm]][[sous_element]], libelle_prix, feuille_nm
  ) |>
    dplyr::select(annee, Code_Produit, valeur)

  get_prods <- function(m) {
    dplyr::pull(dplyr::filter(produits_methodes, Methode == m), Code_Produit)
  }

  source_trim <- dplyr::bind_rows(
    {
      p <- get_prods("lissage")
      if (length(p) > 0) {
        dplyr::mutate(dplyr::filter(grille_trim, Code_Produit %in% p), valeur = 1)
      } else {
        dplyr::tibble()
      }
    },
    {
      p <- get_prods("ind_ressources")
      if (length(p) > 0) {
        dplyr::filter(ind_ressources_trim, Code_Produit %in% p)
      } else {
        dplyr::tibble()
      }
    },
    {
      p <- get_prods("prod")
      if (length(p) > 0) {
        dplyr::filter(ind_prod_trim, Code_Produit %in% p)
      } else {
        dplyr::tibble()
      }
    },
    {
      p <- get_prods("ind_apu")
      if (length(p) > 0) {
        dplyr::filter(ind_apu_trim, Code_Produit %in% p)
      } else {
        dplyr::tibble()
      }
    }
  )

  if (nrow(source_trim) == 0) {
    return(dplyr::tibble())
  }

  source_bench <- dplyr::mutate(
    source_trim,
    full_code = Code_Produit,
    type_ind = feuille_nm,
    periode = paste0(annee, "T", trimestre)
  )
  target_bench <- dplyr::rename(
    dplyr::filter(cna_target, Code_Produit %in% unique(source_trim$Code_Produit)),
    full_code = Code_Produit
  )

  res_bench <- benchmark_groupe(
    source_bench, target_bench,
    type_filter = feuille_nm, value_col = "valeur"
  )

  prods_zero <- get_prods("zero")
  zero_trim <- dplyr::mutate(
    dplyr::filter(grille_trim, Code_Produit %in% prods_zero),
    valeur_cal = 0, full_code = Code_Produit
  )

  dplyr::bind_rows(
    dplyr::select(res_bench, annee, trimestre, Code_Produit = full_code, valeur_cal),
    dplyr::select(zero_trim, annee, trimestre, Code_Produit, valeur_cal)
  ) |>
    dplyr::mutate(composante = feuille_nm)
}

#' Estimer toutes les composantes emplois ERE par benchmarking
#'
#' Applique estimer_composante_emploi() sur toutes les composantes de
#' map_feuille_cna et retourne un tibble consolide.
#' Les produits marques \code{Methode == "solde"} sont exclus ici et doivent
#' etre traites ensuite via \code{calculer_solde_ere()}.
#'
#' @param map_feuille_cna Tibble feuille <-> composante_cna.
#' @param methodes_ere Table des methodes.
#' @param cna_ere_struct Liste des composantes ERE annuelles.
#' @param grille_trim Grille trimestres x produits.
#' @param ind_ressources_trim Indicateur ressources totales trimestriel.
#' @param ind_prod_trim Indicateur production trimestriel.
#' @param ind_apu_trim Indicateur APU trimestriel.
#' @param type_prix \code{"CnaErECrt"} (defaut) ou \code{"CnaErECh"}.
#' @return Tibble consolide de toutes les composantes benchmarkees.
#' @export
estimer_emplois_ere <- function(map_feuille_cna, methodes_ere, cna_ere_struct,
                                grille_trim, ind_ressources_trim,
                                ind_prod_trim, ind_apu_trim,
                                type_prix = "CnaErECrt") {
  map_feuille_cna$feuille |>
    purrr::set_names() |>
    purrr::map_dfr(function(f) {
      cna_nm <- map_feuille_cna$composante_cna[map_feuille_cna$feuille == f]
      estimer_composante_emploi(
        f, cna_nm, methodes_ere, cna_ere_struct,
        grille_trim, ind_ressources_trim,
        ind_prod_trim, ind_apu_trim,
        type_prix = type_prix
      )
    })
}

#' Calculer une composante emploi ERE par la methode du solde
#'
#' Construit d abord un indicateur trimestriel residuel :
#' \deqn{solde_{trim} = ressources_{trim} - \sum autres\_emplois_{trim}}
#' puis benchmarke ce solde trimestriel sur la cible annuelle CNA de la
#' composante correspondante. Les annees sans cible CNA conservent le profil de
#' l indicateur trimestriel (comportement standard de \code{benchmark_groupe()}).
#'
#' @param feuille_nm Nom de la composante solde (ex. "VS", "CFmarch").
#' @param methodes_ere Table longue des methodes (feuille, Code_Produit, Methode).
#' @param ind_ressources_trim Ressources totales trimestrielles par produit.
#' @param emplois_non_solde Emplois deja estimes (annee, trimestre, Code_Produit, valeur_cal, composante).
#' @param composantes_autres Vecteur des composantes a soustraire pour obtenir le solde.
#' @param map_feuille_cna Tibble feuille <-> composante_cna.
#' @param cna_ere_struct Liste des composantes ERE annuelles.
#' @param type_prix Type de prix pour la cible CNA : \code{"CnaErECrt"} (defaut)
#'   ou \code{"CnaErECh"}.
#' @return Tibble : annee, trimestre, Code_Produit, valeur_cal, composante.
#' @export
calculer_solde_ere <- function(feuille_nm, methodes_ere, ind_ressources_trim,
                               emplois_non_solde, composantes_autres,
                               map_feuille_cna, cna_ere_struct,
                               type_prix = "CnaErECrt") {
  prods_solde <- dplyr::pull(
    dplyr::filter(methodes_ere, feuille == feuille_nm, Methode == "solde"),
    Code_Produit
  )

  if (length(prods_solde) == 0) {
    return(dplyr::tibble())
  }

  autres_agg <- emplois_non_solde |>
    dplyr::filter(Code_Produit %in% prods_solde, composante %in% composantes_autres) |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(autres_emplois = sum(valeur_cal, na.rm = TRUE), .groups = "drop")

  solde_indicateur <- ind_ressources_trim |>
    dplyr::filter(Code_Produit %in% prods_solde) |>
    dplyr::rename(total_ressources = valeur) |>
    dplyr::left_join(autres_agg, by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::mutate(
      valeur_ind = total_ressources - tidyr::replace_na(autres_emplois, 0),
      composante = feuille_nm
    ) |>
    dplyr::select(annee, trimestre, Code_Produit, valeur_ind, composante)

  composante_cna_nm <- map_feuille_cna$composante_cna[map_feuille_cna$feuille == feuille_nm][1]
  if (is.na(composante_cna_nm) || is.null(cna_ere_struct[[composante_cna_nm]])) {
    return(dplyr::transmute(
      solde_indicateur, annee, trimestre, Code_Produit,
      valeur_cal = valeur_ind, composante
    ))
  }

  sous_element <- if (type_prix == "CnaErECh") "CnaErECh" else "CnaErECrt"
  libelle_prix <- if (type_prix == "CnaErECh") "Ch" else "Crt"

  cna_target <- pivoter_ere_long(
    cna_ere_struct[[composante_cna_nm]][[sous_element]], libelle_prix, feuille_nm
  ) |>
    dplyr::filter(Code_Produit %in% prods_solde) |>
    dplyr::select(annee, Code_Produit, valeur)

  if (nrow(cna_target) == 0) {
    return(dplyr::transmute(
      solde_indicateur, annee, trimestre, Code_Produit,
      valeur_cal = valeur_ind, composante
    ))
  }

  source_bench <- solde_indicateur |>
    dplyr::mutate(
      full_code = Code_Produit,
      type_ind = feuille_nm,
      periode = paste0(annee, "T", trimestre)
    )
  target_bench <- dplyr::rename(cna_target, full_code = Code_Produit)

  res_bench <- benchmark_groupe(
    source_bench, target_bench,
    type_filter = feuille_nm, value_col = "valeur_ind"
  )

  if (nrow(res_bench) == 0) {
    return(dplyr::transmute(
      solde_indicateur, annee, trimestre, Code_Produit,
      valeur_cal = valeur_ind, composante
    ))
  }

  dplyr::select(
    res_bench, annee, trimestre, Code_Produit = full_code,
    valeur_cal, composante
  )
}

# ==============================================================================
# ERE — EMPLOIS (COURANT + VOLUME + VPAP)
# ==============================================================================

#' Assembler les emplois ERE en courant, volume chaîné et VPAP
#'
#' Estimation des emplois
#' ERE par méthode de benchmarking, assemblage courant et chaîné,
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
# ==============================================================================
# ERE — EMPLOIS (COURANT + VOLUME + VPAP)
# ==============================================================================
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

  message("▶ Emplois ERE — lecture des méthodes...")
  methodes_ere <- lire_methodes_ere(path_methode_ere, map_feuille_cna)

  prods_solde_vs      <- dplyr::pull(
    dplyr::filter(methodes_ere, feuille == "VS", Methode == "solde"),
    Code_Produit)

  prods_solde_cfmarch <- dplyr::pull(
    dplyr::filter(methodes_ere, feuille == "CFmarch", Methode == "solde"),
    Code_Produit)

  grille_trim <- dplyr::distinct(ressources_crt, annee, trimestre, Code_Produit)

  # ------------------------------------------------------------------
  # APU
  # ------------------------------------------------------------------
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
  message("▶ Emplois ERE courants...")

  ind_ressources_trim <- ressources_crt |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(valeur = sum(valeur_composante, na.rm = TRUE), .groups = "drop")

  ind_prod_trim <- dplyr::rename(p1_ere_crt, valeur = P1_crt)

  emplois_bench <- estimer_emplois_ere(
    map_feuille_cna, methodes_ere, cna_ere_struct,
    grille_trim, ind_ressources_trim, ind_prod_trim, ind_apu_trim
  )

  emplois_non_solde <- dplyr::bind_rows(
    dplyr::mutate(dplyr::rename(p2_ere_crt, valeur_cal = P2_crt), composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_crt), composante = "EXPORTATIONS"),
    emplois_bench
  )

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
    dplyr::mutate(dplyr::rename(p2_ere_crt, valeur_cal = P2_crt), composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_crt), composante = "EXPORTATIONS"),
    emplois_bench |>
      dplyr::filter(
        !(composante == "VS" & Code_Produit %in% prods_solde_vs),
        !(composante == "CFmarch" & Code_Produit %in% prods_solde_cfmarch)
      ),
    solde_vs,
    solde_cfmarch
  ) |>
    dplyr::select(annee, trimestre, Code_Produit, composante, valeur_cal)

  # ------------------------------------------------------------------
  # B. EMPLOIS VOLUME
  # ------------------------------------------------------------------
  message("▶ Emplois ERE volume chaîné...")

  ind_ressources_vpap_trim <- ressources_vpap |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(valeur = sum(valeur_composante, na.rm = TRUE), .groups = "drop")

  ind_prod_vpap_trim <- dplyr::rename(p1_ere_vol, valeur = P1_vol)

  emplois_bench_ch <- estimer_emplois_ere(
    map_feuille_cna, methodes_ere, cna_ere_struct,
    grille_trim, ind_ressources_vpap_trim,
    ind_prod_vpap_trim, ind_apu_vpap_trim,
    type_prix = "CnaErECh"
  )

  emplois_non_solde_vol <- dplyr::bind_rows(
    dplyr::mutate(dplyr::rename(p2_ere_vol, valeur_cal = P2_vol), composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_vpap), composante = "EXPORTATIONS"),
    emplois_bench_ch
  )

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
    dplyr::mutate(dplyr::rename(p2_ere_vol, valeur_cal = P2_vol), composante = "CI"),
    dplyr::mutate(dplyr::select(cnt_exp_final, annee, trimestre,
                                Code_Produit, valeur_cal = exp_vpap), composante = "EXPORTATIONS"),
    emplois_bench_ch |>
      dplyr::filter(
        !(composante == "VS" & Code_Produit %in% prods_solde_vs),
        !(composante == "CFmarch" & Code_Produit %in% prods_solde_cfmarch)
      ),
    solde_vs_vol,
    solde_cfmarch_vol
  ) |>
    dplyr::select(annee, trimestre, Code_Produit, composante, valeur_ch = valeur_cal)

  # ============================================================================
  # 🔧 CORRECTION JOIN (DUPLICATS)
  # ============================================================================

  .normaliser_emploi_unique <- function(df, nom_df) {

    df <- df |> dplyr::distinct()

    dup <- df |>
      dplyr::count(annee, trimestre, Code_Produit, composante) |>
      dplyr::filter(n > 1)

    if (nrow(dup) > 0) {

      message("⚠️ Doublons détectés dans ", nom_df,
              " → agrégation par (annee, trimestre, Code_Produit, composante)")

      col_valeur <- dplyr::case_when(
        "valeur_cal" %in% names(df) ~ "valeur_cal",
        "valeur_ch"  %in% names(df) ~ "valeur_ch",
        TRUE ~ NA_character_
      )

      if (is.na(col_valeur)) {
        stop(
          "Aucune colonne de valeur reconnue dans ", nom_df,
          ". Colonnes disponibles : ", paste(names(df), collapse = ", "),
          call. = FALSE
        )
      }

      df <- df |>
        dplyr::group_by(annee, trimestre, Code_Produit, composante) |>
        dplyr::summarise(
          valeur_tmp = sum(.data[[col_valeur]], na.rm = TRUE),
          .groups = "drop"
        )

      names(df)[names(df) == "valeur_tmp"] <- col_valeur
    }

    df
  }

  emplois_crt <- .normaliser_emploi_unique(emplois_crt, "emplois_crt")
  emplois_vol <- .normaliser_emploi_unique(emplois_vol, "emplois_vol")

  # ------------------------------------------------------------------
  # C. VPAP
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

  list(
    emplois_crt  = emplois_crt,
    emplois_vol  = emplois_vol,
    emplois_vpap = emplois_vpap
  )
}
