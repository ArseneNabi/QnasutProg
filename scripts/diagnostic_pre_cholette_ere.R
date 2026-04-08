#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(QnaSut)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(writexl)
})

.norm_text_pre_cholette_ere <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  gsub("[[:space:]]+", " ", x)
}

.ref_composantes_pre_cholette_ere <- function() {
  tibble::tribble(
    ~composante_canonique, ~alias,
    "PRODUCTION", "PRODUCTION",
    "IMPORTATIONS", "IMPORTATIONS",
    "IMPOT sur Import", "IMPOT sur Import",
    "IMPOT sur export", "IMPOT sur export",
    "MARGE de commerce", "MARGE de commerce",
    "MARGE de transport", "MARGE de transport",
    "TVA", "TVA",
    "IMPOT sur produit", "IMPOT sur produit",
    "Subventions", "Subventions",
    "CI Prix d'acquisition", "CI",
    "CI Prix d'acquisition", "CI Prix d'acquisition",
    "CF Marchande Menage Prix d'acquisition", "CFmarch",
    "CF Marchande Menage Prix d'acquisition", "CF Marchande Menage Prix d'acquisition",
    "CF Non Marchande Menage Prix d'acquisition", "CFnmarch",
    "CF Non Marchande Menage Prix d'acquisition", "CF Non Marchande Menage Prix d'acquisition",
    "CF Non Marchande APU Prix d'acquisition", "CFapu",
    "CF Non Marchande APU Prix d'acquisition", "CF Non Marchande APU Prix d'acquisition",
    "CF Non Marchande ISBL Prix d'acquisition", "CFisblsm",
    "CF Non Marchande ISBL Prix d'acquisition", "CF Non Marchande ISBL Prix d'acquisition",
    "FBCF Prix d'acquisition", "FBCF",
    "FBCF Prix d'acquisition", "FBCF Prix d'acquisition",
    "VS Prix d'acquisition", "VS",
    "VS Prix d'acquisition", "VS Prix d'acquisition",
    "Exportation Prix d'acquisition", "EXPORTATIONS",
    "Exportation Prix d'acquisition", "Exportation Prix d'acquisition",
    "Exportation Prix d'acquisition", "Exportations de biens et services",
    "Aquisition moyen cession de origen de valeur Prix d'acquisition", "AOV",
    "Aquisition moyen cession de origen de valeur Prix d'acquisition", "Aquisition moyen cession de origen de valeur Prix d'acquisition"
  ) |>
    dplyr::mutate(alias_norm = .norm_text_pre_cholette_ere(alias))
}

.standardiser_composante_pre_cholette_ere <- function(x) {
  ref <- .ref_composantes_pre_cholette_ere()
  x_chr <- as.character(x)
  x_norm <- .norm_text_pre_cholette_ere(x_chr)
  idx <- match(x_norm, ref$alias_norm)

  x_std <- trimws(gsub("[[:space:]]+", " ", x_chr))
  x_std[!is.na(idx)] <- ref$composante_canonique[idx[!is.na(idx)]]
  x_std[is.na(x)] <- NA_character_
  x_std
}

.mapper_type_prix_pre_cholette_ere <- function(type_prix) {
  dplyr::case_when(
    type_prix == "CnaErECrt" ~ "crt",
    type_prix == "CnaErEVol" ~ "vpap",
    type_prix == "CnaErECh" ~ "ch",
    TRUE ~ type_prix
  )
}

.resoudre_repertoire_qnasut <- function(project_dir = NULL) {
  candidats <- unique(c(
    project_dir,
    getwd(),
    dirname(getwd()),
    file.path(getwd(), "..")
  ))

  for (cand in candidats) {
    if (is.null(cand) || is.na(cand)) {
      next
    }

    cand_norm <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (!dir.exists(cand_norm)) {
      next
    }

    if (file.exists(file.path(cand_norm, "DESCRIPTION")) &&
        dir.exists(file.path(cand_norm, "OutilCntBfa"))) {
      return(cand_norm)
    }
  }

  stop(
    "Impossible de resoudre le repertoire QnaSut. ",
    "Passez `project_dir` explicitement.",
    call. = FALSE
  )
}

.resoudre_repertoire_outil_cnt_bfa <- function(project_dir) {
  candidats <- unique(c(
    file.path(project_dir, "OutilCntBfa"),
    file.path(dirname(project_dir), "OutilCntBfa"),
    file.path(getwd(), "OutilCntBfa"),
    normalizePath(file.path(getwd(), "..", "OutilCntBfa"),
                  winslash = "/", mustWork = FALSE)
  ))

  for (cand in candidats) {
    cand_norm <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (!dir.exists(cand_norm)) {
      next
    }

    if (file.exists(file.path(cand_norm, "config.yml"))) {
      return(cand_norm)
    }
  }

  stop(
    "Impossible de resoudre le repertoire OutilCntBfa avec `config.yml`.",
    call. = FALSE
  )
}

.map_feuille_cna_pre_cholette_ere <- function() {
  tibble::tribble(
    ~feuille,    ~composante_cna,
    "CFmarch",   "CF Marchande Menage   Prix d'acquisition",
    "CFnmarch",  "CF Non Marchande Menage Prix d'acquisition",
    "CFapu",     "CF Non Marchande APU Prix d'acquisition",
    "CFisblsm",  "CF Non Marchande  ISBL Prix d'acquisition",
    "FBCF",      "FBCF Prix d'acquisition",
    "VS",        "VS Prix d'acquisition",
    "AOV",       "Aquisition moyen cession de origen de valeur Prix d'acquisition"
  )
}

.map_apu_cols_pre_cholette_ere <- function() {
  tibble::tribble(
    ~Code_Produit, ~col_apu,
    "JZ000", "Ind1_APU*JZ000",
    "KZ000", "Ind1_APU*K21004",
    "OZ000", "Ind1_APU*L22000",
    "PZ000", "Ind1_APU*P26000",
    "QZ000", "Ind1_APU*QZ000",
    "RS001", "Ind1_APU*RS001"
  )
}

.construire_contexte_pre_cholette_ere <- function(project_dir = NULL) {
  projet_qnasut <- .resoudre_repertoire_qnasut(project_dir)
  repertoire_outil <- .resoudre_repertoire_outil_cnt_bfa(projet_qnasut)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(repertoire_outil)

  cfg <- QnaSut::load_config()
  donnees <- QnaSut::charger_donnees_cnt(cfg)
  tre <- QnaSut::executer_module_tre(donnees, cfg)
  prod_direct <- QnaSut::executer_optique_production_directe(donnees, tre)

  ci_branches <- QnaSut::calculer_ci_branches(
    prod_complete = prod_direct$prod_complete,
    res_trim = tre$res_trim,
    ci_crt = donnees$ci_crt,
    ci_vol = donnees$ci_vol
  )

  ind4_ci <- QnaSut::calculer_ind4_depuis_ci(
    p2_final_cal = ci_branches$p2_final_cal,
    res_trim = tre$res_trim,
    Map_Produits = donnees$Map_Produits,
    ind_crt = donnees$ind_crt,
    prod_crt = donnees$prod_crt,
    prod_ch = donnees$prod_ch
  )

  ind5 <- QnaSut::calculer_ind5_depuis_production(
    p1_agreg = ci_branches$p1_agreg,
    ind4_final = ind4_ci$ind4_final,
    poids_trim = tre$res_trim$poids_trim,
    Map_Produits = donnees$Map_Produits,
    cna_ere_struct = donnees$cna_ere_struct,
    ind_ce_struct = donnees$ind_ce_struct,
    prod_crt = donnees$prod_crt,
    prod_ch = donnees$prod_ch,
    derniere_annee_cna = cfg$derniere_annee_definitif,
    annee_fin_proj = cfg$annee_fin_projection
  )

  p1_final <- QnaSut::calculer_p1_ere(
    ind5$p1_agreg_complet,
    tre$res_trim$poids_trim,
    donnees$Map_Produits
  )
  p1_ere_bench <- QnaSut::benchmarker_p1_ere(
    p1_final$p1_ere_crt,
    p1_final$p1_ere_vol,
    donnees$cna_ere_struct
  )

  p2_ere <- QnaSut::calculer_p2_ere(ind4_ci$p2_par_produit_n3, donnees$Map_Produits)
  p2_ere_bench <- QnaSut::benchmarker_p2_ere(
    p2_ere$p2_ere_crt,
    p2_ere$p2_ere_vol,
    donnees$cna_ere_struct
  )

  imp_exp <- QnaSut::executer_benchmarking_imp_exp(
    donnees$ind_ce_struct,
    donnees$cna_ere_struct
  )

  ere_res <- QnaSut::executer_ressources_ere(
    p1_ere_crt = p1_ere_bench$p1_ere_crt,
    p1_ere_vol = p1_ere_bench$p1_ere_vol,
    cnt_imp_final = imp_exp$cnt_imp_final,
    cna_ere_struct = donnees$cna_ere_struct,
    derniere_annee_cna = cfg$derniere_annee_definitif,
    annee_fin_proj = cfg$annee_fin_projection
  )

  ere_emp <- QnaSut::executer_emplois_ere(
    ere_res = ere_res,
    p2_ere_crt = p2_ere_bench$p2_ere_crt,
    p2_ere_vol = p2_ere_bench$p2_ere_vol,
    cnt_exp_final = imp_exp$cnt_exp_final,
    p1_ere_crt = p1_ere_bench$p1_ere_crt,
    p1_ere_vol = p1_ere_bench$p1_ere_vol,
    cna_ere_struct = donnees$cna_ere_struct,
    path_methode_ere = file.path(cfg$root_dir, "Methode_ERE.xlsx"),
    map_feuille_cna = .map_feuille_cna_pre_cholette_ere(),
    map_apu_cols = .map_apu_cols_pre_cholette_ere(),
    ind_crt = donnees$ind_crt,
    ind_cst = donnees$ind_cst
  )

  list(
    project_dir = projet_qnasut,
    outil_cnt_bfa_dir = repertoire_outil,
    cfg = cfg,
    donnees = donnees,
    tre = tre,
    prod_direct = prod_direct,
    ci_branches = ci_branches,
    ind4_ci = ind4_ci,
    ind5 = ind5,
    p1_final = p1_final,
    p1_ere_bench = p1_ere_bench,
    p2_ere = p2_ere,
    p2_ere_bench = p2_ere_bench,
    imp_exp = imp_exp,
    ere_res = ere_res,
    ere_emp = ere_emp
  )
}

.construire_cibles_annuelles_pre_cholette_ere <- function(cna_ere_struct) {
  purrr::imap_dfr(cna_ere_struct, function(comp_list, composante_source) {
    if (is.null(comp_list) || !is.list(comp_list)) {
      return(tibble::tibble())
    }

    purrr::imap_dfr(comp_list, function(tbl, type_prix_source) {
      if (!type_prix_source %in% c("CnaErECrt", "CnaErEVol", "CnaErECh") ||
          is.null(tbl)) {
        return(tibble::tibble())
      }

      QnaSut::pivoter_ere_long(tbl, type_prix_source, composante_source) |>
        dplyr::transmute(
          type_prix = .mapper_type_prix_pre_cholette_ere(type_prix_source),
          annee = as.integer(annee),
          Code_Produit = as.character(Code_Produit),
          composante_source = as.character(composante),
          composante = .standardiser_composante_pre_cholette_ere(composante),
          valeur_annuelle_cible = as.numeric(valeur)
        )
    })
  }) |>
    dplyr::filter(!is.na(Code_Produit), Code_Produit != "") |>
    dplyr::arrange(type_prix, Code_Produit, composante, annee)
}

.normaliser_series_trimestrielles_pre_cholette_ere <- function(df, bloc, type_prix,
                                                               valeur_col) {
  df |>
    dplyr::transmute(
      bloc = bloc,
      type_prix = type_prix,
      annee = as.integer(annee),
      trimestre = as.integer(trimestre),
      Code_Produit = as.character(Code_Produit),
      composante_source = as.character(composante),
      composante = .standardiser_composante_pre_cholette_ere(composante),
      valeur = as.numeric(.data[[valeur_col]])
    ) |>
    dplyr::arrange(bloc, type_prix, Code_Produit, composante, annee, trimestre)
}

.construire_ressources_chaine_pre_cholette_ere <- function(ressources_crt,
                                                           ressources_vpap) {
  crt <- .normaliser_series_trimestrielles_pre_cholette_ere(
    ressources_crt,
    bloc = "ressource",
    type_prix = "crt",
    valeur_col = "valeur_composante"
  ) |>
    dplyr::rename(valeur_crt = valeur)

  vpap <- .normaliser_series_trimestrielles_pre_cholette_ere(
    ressources_vpap,
    bloc = "ressource",
    type_prix = "vpap",
    valeur_col = "valeur_composante"
  ) |>
    dplyr::rename(valeur_vpap = valeur)

  dplyr::full_join(
    crt |>
      dplyr::select(-type_prix),
    vpap |>
      dplyr::select(-type_prix),
    by = c("bloc", "annee", "trimestre", "Code_Produit",
           "composante_source", "composante"),
    relationship = "one-to-one"
  ) |>
    dplyr::arrange(Code_Produit, composante, annee, trimestre) |>
    dplyr::group_by(Code_Produit, composante) |>
    dplyr::mutate(
      valeur = tidyr::replace_na(
        QnaSut::calcul_valeur_chainee_trim(valeur_crt, valeur_vpap), 0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      bloc = "ressource",
      type_prix = "ch",
      annee,
      trimestre,
      Code_Produit,
      composante_source,
      composante,
      valeur
    )
}

.construire_series_pre_cholette_ere <- function(ere_res, ere_emp) {
  dplyr::bind_rows(
    .normaliser_series_trimestrielles_pre_cholette_ere(
      ere_res$ressources_crt,
      bloc = "ressource",
      type_prix = "crt",
      valeur_col = "valeur_composante"
    ),
    .normaliser_series_trimestrielles_pre_cholette_ere(
      ere_res$ressources_vpap,
      bloc = "ressource",
      type_prix = "vpap",
      valeur_col = "valeur_composante"
    ),
    .construire_ressources_chaine_pre_cholette_ere(
      ere_res$ressources_crt,
      ere_res$ressources_vpap
    ),
    .normaliser_series_trimestrielles_pre_cholette_ere(
      ere_emp$emplois_crt,
      bloc = "emploi",
      type_prix = "crt",
      valeur_col = "valeur_cal"
    ),
    .normaliser_series_trimestrielles_pre_cholette_ere(
      ere_emp$emplois_vol,
      bloc = "emploi",
      type_prix = "ch",
      valeur_col = "valeur_ch"
    ),
    .normaliser_series_trimestrielles_pre_cholette_ere(
      ere_emp$emplois_vpap,
      bloc = "emploi",
      type_prix = "vpap",
      valeur_col = "valeur_vpap"
    )
  ) |>
    dplyr::arrange(bloc, type_prix, Code_Produit, composante, annee, trimestre)
}

.analyser_calage_annuel_pre_cholette_ere <- function(series_trimestrielles,
                                                     cibles_annuelles,
                                                     tol_calage = 1e-6) {
  series_trimestrielles |>
    dplyr::group_by(
      bloc, type_prix, Code_Produit, composante, composante_source, annee
    ) |>
    dplyr::summarise(
      somme_trimestrielle = sum(valeur, na.rm = TRUE),
      n_trim = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      cibles_annuelles |>
        dplyr::select(type_prix, Code_Produit, composante,
                      valeur_annuelle_cible, annee),
      by = c("type_prix", "Code_Produit", "composante", "annee")
    ) |>
    dplyr::mutate(
      annee_complete = n_trim == 4L,
      cible_presente = !is.na(valeur_annuelle_cible),
      ecart_annuel = somme_trimestrielle - valeur_annuelle_cible,
      ecart_abs = abs(ecart_annuel),
      ecart_relatif = dplyr::if_else(
        cible_presente & abs(valeur_annuelle_cible) > 0,
        ecart_annuel / valeur_annuelle_cible,
        NA_real_
      ),
      statut_calage = dplyr::case_when(
        !cible_presente ~ "sans_cible",
        !annee_complete ~ "annee_incomplete",
        ecart_abs <= tol_calage ~ "ok",
        TRUE ~ "ecart"
      ),
      cale_annuel = dplyr::case_when(
        statut_calage == "ok" ~ TRUE,
        statut_calage %in% c("sans_cible", "annee_incomplete") ~ NA,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::arrange(bloc, type_prix, Code_Produit, composante, annee)
}

.synthese_calage_par_produit_pre_cholette_ere <- function(calage_detail) {
  calage_detail |>
    dplyr::group_by(bloc, type_prix, Code_Produit) |>
    dplyr::summarise(
      n_lignes = dplyr::n(),
      n_lignes_annee_complete = sum(annee_complete, na.rm = TRUE),
      n_ok = sum(statut_calage == "ok", na.rm = TRUE),
      n_ecart = sum(statut_calage == "ecart", na.rm = TRUE),
      n_sans_cible = sum(statut_calage == "sans_cible", na.rm = TRUE),
      n_annee_incomplete = sum(statut_calage == "annee_incomplete", na.rm = TRUE),
      ecart_abs_max = suppressWarnings(max(ecart_abs, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ecart_abs_max = dplyr::if_else(
        is.finite(ecart_abs_max), ecart_abs_max, NA_real_
      )
    ) |>
    dplyr::arrange(bloc, type_prix, Code_Produit)
}

.synthese_calage_par_composante_pre_cholette_ere <- function(calage_detail) {
  calage_detail |>
    dplyr::group_by(bloc, type_prix, Code_Produit, composante) |>
    dplyr::summarise(
      n_lignes = dplyr::n(),
      n_ok = sum(statut_calage == "ok", na.rm = TRUE),
      n_ecart = sum(statut_calage == "ecart", na.rm = TRUE),
      n_sans_cible = sum(statut_calage == "sans_cible", na.rm = TRUE),
      n_annee_incomplete = sum(statut_calage == "annee_incomplete", na.rm = TRUE),
      ecart_abs_max = suppressWarnings(max(ecart_abs, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ecart_abs_max = dplyr::if_else(
        is.finite(ecart_abs_max), ecart_abs_max, NA_real_
      )
    ) |>
    dplyr::arrange(bloc, type_prix, Code_Produit, composante)
}

.analyser_egalite_crt_vpap_pre_cholette_ere <- function(series_trimestrielles,
                                                        cibles_annuelles,
                                                        tol_egalite = 1e-9,
                                                        rel_tol_egalite = 1e-12,
                                                        seuil_part_identiques = 0.75) {
  crt <- series_trimestrielles |>
    dplyr::filter(type_prix == "crt") |>
    dplyr::rename(valeur_crt = valeur)

  vpap <- series_trimestrielles |>
    dplyr::filter(type_prix == "vpap") |>
    dplyr::rename(valeur_vpap = valeur)

  detail_trim <- dplyr::inner_join(
    crt |>
      dplyr::select(-type_prix),
    vpap |>
      dplyr::select(-type_prix),
    by = c("bloc", "annee", "trimestre", "Code_Produit",
           "composante_source", "composante"),
    relationship = "one-to-one"
  ) |>
    dplyr::mutate(
      delta_crt_vpap = valeur_crt - valeur_vpap,
      abs_delta_crt_vpap = abs(delta_crt_vpap),
      seuil_identite = pmax(
        tol_egalite,
        rel_tol_egalite * pmax(abs(valeur_crt), abs(valeur_vpap), 1)
      ),
      crt_identique_vpap = abs_delta_crt_vpap <= seuil_identite
    )

  cibles_crt <- cibles_annuelles |>
    dplyr::filter(type_prix == "crt") |>
    dplyr::rename(cible_crt = valeur_annuelle_cible) |>
    dplyr::select(Code_Produit, composante, annee, cible_crt)

  cibles_vpap <- cibles_annuelles |>
    dplyr::filter(type_prix == "vpap") |>
    dplyr::rename(cible_vpap = valeur_annuelle_cible) |>
    dplyr::select(Code_Produit, composante, annee, cible_vpap)

  cibles_comparees <- dplyr::full_join(
    cibles_crt, cibles_vpap,
    by = c("Code_Produit", "composante", "annee")
  ) |>
    dplyr::mutate(
      delta_cible_crt_vpap = cible_crt - cible_vpap,
      abs_delta_cible_crt_vpap = abs(delta_cible_crt_vpap),
      cible_crt_identique_vpap = abs_delta_cible_crt_vpap <= tol_egalite
    )

  synthese <- detail_trim |>
    dplyr::group_by(bloc, Code_Produit, composante) |>
    dplyr::summarise(
      n_trim = dplyr::n(),
      n_trim_identiques = sum(crt_identique_vpap, na.rm = TRUE),
      part_trim_identiques = n_trim_identiques / n_trim,
      abs_delta_max = max(abs_delta_crt_vpap, na.rm = TRUE),
      abs_delta_moyen = mean(abs_delta_crt_vpap, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      cibles_comparees |>
        dplyr::group_by(Code_Produit, composante) |>
        dplyr::summarise(
          n_annees_cibles = sum(!is.na(cible_crt) | !is.na(cible_vpap)),
          n_annees_cibles_identiques = sum(cible_crt_identique_vpap, na.rm = TRUE),
          abs_delta_cible_max = suppressWarnings(max(abs_delta_cible_crt_vpap, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          abs_delta_cible_max = dplyr::if_else(
            is.finite(abs_delta_cible_max), abs_delta_cible_max, NA_real_
          )
        ),
      by = c("Code_Produit", "composante")
    ) |>
    dplyr::mutate(
      suspect_egalite = part_trim_identiques >= seuil_part_identiques &
        dplyr::coalesce(abs_delta_cible_max, 0) > tol_egalite
    ) |>
    dplyr::arrange(dplyr::desc(part_trim_identiques),
                   dplyr::desc(abs_delta_cible_max), bloc, Code_Produit, composante)

  list(
    detail_trim = detail_trim,
    cibles_comparees = cibles_comparees,
    synthese = synthese
  )
}

#' Diagnostiquer les ERE avant l'equilibrage multivarie
#'
#' @description
#' Reconstruit les tables ERE juste avant l'appel a
#' \code{executer_equilibrage_ere()}, puis controle :
#' \enumerate{
#'   \item le calage annuel de chaque agregat trimestriel par produit,
#'     composante et type de prix (\code{crt}, \code{ch}, \code{vpap}) ;
#'   \item les cas ou \code{crt} et \code{vpap} sont identiques trop souvent.
#' }
#'
#' Le pipeline principal n'est pas modifie : ce script relance simplement les
#' etapes existantes jusqu'a \code{ere_res} et \code{ere_emp}.
#'
#' @param project_dir Repertoire racine QnaSut. Si \code{NULL}, detection
#'   automatique a partir du repertoire courant.
#' @param output_excel Chemin du fichier Excel de sortie. Si \code{NULL},
#'   un fichier \code{Diagnostic_PreCholette_ERE_<date>.xlsx} est cree dans
#'   \code{cfg$root_dir}.
#' @param export_excel Booleen ; si \code{TRUE}, exporte un classeur Excel.
#' @param tol_calage Tolerance absolue pour juger le calage annuel.
#' @param tol_egalite Tolerance absolue pour juger \code{crt == vpap}.
#' @param rel_tol_egalite Tolerance relative additionnelle pour
#'   \code{crt == vpap}.
#' @param seuil_part_identiques Seuil a partir duquel une egalite frequente
#'   \code{crt == vpap} est signalee comme potentiellement suspecte.
#'
#' @return Une liste de tables de diagnostic.
diagnostiquer_pre_cholette_ere <- function(project_dir = NULL,
                                           output_excel = NULL,
                                           export_excel = TRUE,
                                           tol_calage = 1e-6,
                                           tol_egalite = 1e-9,
                                           rel_tol_egalite = 1e-12,
                                           seuil_part_identiques = 0.75) {
  contexte <- .construire_contexte_pre_cholette_ere(project_dir)

  series_trimestrielles <- .construire_series_pre_cholette_ere(
    contexte$ere_res,
    contexte$ere_emp
  )
  cibles_annuelles <- .construire_cibles_annuelles_pre_cholette_ere(
    contexte$donnees$cna_ere_struct
  )

  calage_detail <- .analyser_calage_annuel_pre_cholette_ere(
    series_trimestrielles,
    cibles_annuelles,
    tol_calage = tol_calage
  )

  calage_produit <- .synthese_calage_par_produit_pre_cholette_ere(calage_detail)
  calage_composante <- .synthese_calage_par_composante_pre_cholette_ere(calage_detail)

  egalite_crt_vpap <- .analyser_egalite_crt_vpap_pre_cholette_ere(
    series_trimestrielles,
    cibles_annuelles,
    tol_egalite = tol_egalite,
    rel_tol_egalite = rel_tol_egalite,
    seuil_part_identiques = seuil_part_identiques
  )

  alertes_calage <- calage_detail |>
    dplyr::filter(statut_calage == "ecart") |>
    dplyr::arrange(dplyr::desc(ecart_abs), bloc, type_prix, Code_Produit, composante, annee)

  alertes_egalite <- egalite_crt_vpap$synthese |>
    dplyr::filter(suspect_egalite) |>
    dplyr::arrange(dplyr::desc(part_trim_identiques),
                   dplyr::desc(abs_delta_cible_max), bloc, Code_Produit, composante)

  if (is.null(output_excel)) {
    output_excel <- file.path(
      contexte$cfg$root_dir,
      paste0("Diagnostic_PreCholette_ERE_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    )
  }

  if (isTRUE(export_excel)) {
    writexl::write_xlsx(
      list(
        Synthese_Calage_Produit = calage_produit,
        Synthese_Calage_Composante = calage_composante,
        Detail_Calage = calage_detail,
        Alertes_Calage = alertes_calage,
        Synthese_CRT_VPAP = egalite_crt_vpap$synthese,
        Detail_CRT_VPAP = egalite_crt_vpap$detail_trim,
        Cibles_Annuelles = cibles_annuelles,
        Cibles_CRT_VPAP = egalite_crt_vpap$cibles_comparees
      ),
      path = output_excel
    )

    message("Export diagnostic pre-Cholette : ", output_excel)
  }

  list(
    output_excel = output_excel,
    contexte = contexte,
    series_trimestrielles = series_trimestrielles,
    cibles_annuelles = cibles_annuelles,
    calage_detail = calage_detail,
    calage_produit = calage_produit,
    calage_composante = calage_composante,
    alertes_calage = alertes_calage,
    egalite_crt_vpap = egalite_crt_vpap,
    alertes_egalite = alertes_egalite
  )
}

if (sys.nframe() == 0L) {
  diagnostiquer_pre_cholette_ere()
}
