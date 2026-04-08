#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(QnaSut)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(writexl)
})

.resoudre_repertoire_qnasut_diag_global_ere <- function(project_dir = NULL) {
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
    if (file.exists(file.path(cand_norm, "DESCRIPTION")) &&
        dir.exists(file.path(cand_norm, "R"))) {
      return(cand_norm)
    }
  }

  stop("Impossible de resoudre le repertoire QnaSut.", call. = FALSE)
}

.source_script_diag_global_ere <- function(project_dir = NULL) {
  projet_qnasut <- .resoudre_repertoire_qnasut_diag_global_ere(project_dir)
  path_script <- file.path(projet_qnasut, "scripts", "diagnostic_pre_cholette_ere.R")

  if (!file.exists(path_script)) {
    stop("Script introuvable : ", path_script, call. = FALSE)
  }

  source(path_script, local = FALSE)
  invisible(path_script)
}

.is_equal_diag_global_ere <- function(x, y, tol_abs = 1e-6, tol_rel = 1e-10) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ref <- pmax(abs(x), abs(y), 1)
  !is.na(x) & !is.na(y) & abs(x - y) <= (tol_abs + tol_rel * ref)
}

.annualiser_stage_diag_global_ere <- function(df,
                                              stage,
                                              niveau,
                                              code_col,
                                              composante = NULL,
                                              crt_col = NULL,
                                              vpap_col = NULL,
                                              ch_col = NULL,
                                              trimestre_col = "trimestre") {
  cols_existantes <- names(df)
  code_col <- rlang::as_name(rlang::ensym(code_col))
  trimestre_present <- !is.null(trimestre_col) && trimestre_col %in% cols_existantes

  composante_tbl <- if (is.null(composante)) {
    tibble::tibble(composante = NA_character_)
  } else if (length(composante) == 1L && composante %in% cols_existantes) {
    df |>
      dplyr::transmute(composante = as.character(.data[[composante]]))
  } else {
    tibble::tibble(composante = as.character(composante))
  }

  base <- df |>
    dplyr::transmute(
      annee = as.integer(.data$annee),
      trimestre = if (trimestre_present) as.integer(.data[[trimestre_col]]) else NA_integer_,
      code = as.character(.data[[code_col]]),
      composante = composante_tbl$composante,
      valeur_crt = if (!is.null(crt_col) && crt_col %in% cols_existantes) as.numeric(.data[[crt_col]]) else NA_real_,
      valeur_vpap = if (!is.null(vpap_col) && vpap_col %in% cols_existantes) as.numeric(.data[[vpap_col]]) else NA_real_,
      valeur_ch = if (!is.null(ch_col) && ch_col %in% cols_existantes) as.numeric(.data[[ch_col]]) else NA_real_
    )

  annualise <- base |>
    dplyr::group_by(annee, code, composante) |>
    dplyr::summarise(
      n_obs = sum(!is.na(trimestre)),
      valeur_crt = if (all(is.na(valeur_crt))) NA_real_ else sum(valeur_crt, na.rm = TRUE),
      valeur_vpap = if (all(is.na(valeur_vpap))) NA_real_ else sum(valeur_vpap, na.rm = TRUE),
      valeur_ch = if (all(is.na(valeur_ch))) NA_real_ else sum(valeur_ch, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      stage = stage,
      niveau = niveau,
      frequence_source = dplyr::if_else(trimestre_present, "trimestriel", "annuel"),
      .before = 1
    )

  annualise |>
    dplyr::mutate(
      post_base = annee > 2015L,
      nonzero_crt_vpap = dplyr::coalesce(abs(valeur_crt) > 1e-9, FALSE) |
        dplyr::coalesce(abs(valeur_vpap) > 1e-9, FALSE),
      delta_crt_vpap = valeur_crt - valeur_vpap,
      abs_delta_crt_vpap = abs(delta_crt_vpap),
      ratio_crt_vpap = dplyr::if_else(
        !is.na(valeur_vpap) & abs(valeur_vpap) > 1e-12,
        valeur_crt / valeur_vpap,
        NA_real_
      ),
      egalite_crt_vpap = .is_equal_diag_global_ere(valeur_crt, valeur_vpap),
      egalite_crt_vpap_post_base = post_base & nonzero_crt_vpap & egalite_crt_vpap,
      delta_crt_ch = valeur_crt - valeur_ch,
      abs_delta_crt_ch = abs(delta_crt_ch),
      delta_vpap_ch = valeur_vpap - valeur_ch,
      abs_delta_vpap_ch = abs(delta_vpap_ch)
    ) |>
    dplyr::select(
      stage, niveau, code, composante, annee, frequence_source, n_obs,
      valeur_crt, valeur_vpap, valeur_ch,
      delta_crt_vpap, abs_delta_crt_vpap, ratio_crt_vpap,
      egalite_crt_vpap, egalite_crt_vpap_post_base, nonzero_crt_vpap,
      delta_crt_ch, abs_delta_crt_ch, delta_vpap_ch, abs_delta_vpap_ch
    )
}

.annualiser_cna_ere_diag_global_ere <- function(cna_ere_struct) {
  .construire_cibles_annuelles_pre_cholette_ere(cna_ere_struct) |>
    dplyr::select(type_prix, annee, Code_Produit, composante, valeur_annuelle_cible) |>
    tidyr::pivot_wider(
      names_from = type_prix,
      values_from = valeur_annuelle_cible,
      names_prefix = "valeur_"
    ) |>
    dplyr::rename(
      code = Code_Produit,
      valeur_crt = valeur_crt,
      valeur_vpap = valeur_vpap,
      valeur_ch = valeur_ch
    ) |>
    dplyr::mutate(
      stage = "cna_ere_source",
      niveau = "produit_composante",
      frequence_source = "annuel",
      n_obs = 1L,
      post_base = annee > 2015L,
      nonzero_crt_vpap = dplyr::coalesce(abs(valeur_crt) > 1e-9, FALSE) |
        dplyr::coalesce(abs(valeur_vpap) > 1e-9, FALSE),
      delta_crt_vpap = valeur_crt - valeur_vpap,
      abs_delta_crt_vpap = abs(delta_crt_vpap),
      ratio_crt_vpap = dplyr::if_else(
        !is.na(valeur_vpap) & abs(valeur_vpap) > 1e-12,
        valeur_crt / valeur_vpap,
        NA_real_
      ),
      egalite_crt_vpap = .is_equal_diag_global_ere(valeur_crt, valeur_vpap),
      egalite_crt_vpap_post_base = post_base & nonzero_crt_vpap & egalite_crt_vpap,
      delta_crt_ch = valeur_crt - valeur_ch,
      abs_delta_crt_ch = abs(delta_crt_ch),
      delta_vpap_ch = valeur_vpap - valeur_ch,
      abs_delta_vpap_ch = abs(delta_vpap_ch)
    ) |>
    dplyr::select(
      stage, niveau, code, composante, annee, frequence_source, n_obs,
      valeur_crt, valeur_vpap, valeur_ch,
      delta_crt_vpap, abs_delta_crt_vpap, ratio_crt_vpap,
      egalite_crt_vpap, egalite_crt_vpap_post_base, nonzero_crt_vpap,
      delta_crt_ch, abs_delta_crt_ch, delta_vpap_ch, abs_delta_vpap_ch
    )
}

.annualiser_ci_source_diag_global_ere <- function(ci_crt, ci_vol, ci_ch) {
  crt <- ci_crt |>
    dplyr::mutate(code = QnaSut::extract_branch_code(full_code)) |>
    dplyr::group_by(annee, code) |>
    dplyr::summarise(valeur_crt = sum(valeur, na.rm = TRUE), .groups = "drop")

  vol <- ci_vol |>
    dplyr::mutate(code = QnaSut::extract_branch_code(full_code)) |>
    dplyr::group_by(annee, code) |>
    dplyr::summarise(valeur_vpap = sum(valeur, na.rm = TRUE), .groups = "drop")

  ch <- ci_ch |>
    dplyr::mutate(code = QnaSut::extract_branch_code(full_code)) |>
    dplyr::group_by(annee, code) |>
    dplyr::summarise(valeur_ch = sum(valeur, na.rm = TRUE), .groups = "drop")

  dplyr::full_join(crt, vol, by = c("annee", "code")) |>
    dplyr::full_join(ch, by = c("annee", "code")) |>
    dplyr::mutate(
      stage = "ci_branche_source",
      niveau = "branche",
      composante = "CI",
      frequence_source = "annuel",
      n_obs = 1L,
      post_base = annee > 2015L,
      nonzero_crt_vpap = dplyr::coalesce(abs(valeur_crt) > 1e-9, FALSE) |
        dplyr::coalesce(abs(valeur_vpap) > 1e-9, FALSE),
      delta_crt_vpap = valeur_crt - valeur_vpap,
      abs_delta_crt_vpap = abs(delta_crt_vpap),
      ratio_crt_vpap = dplyr::if_else(
        !is.na(valeur_vpap) & abs(valeur_vpap) > 1e-12,
        valeur_crt / valeur_vpap,
        NA_real_
      ),
      egalite_crt_vpap = .is_equal_diag_global_ere(valeur_crt, valeur_vpap),
      egalite_crt_vpap_post_base = post_base & nonzero_crt_vpap & egalite_crt_vpap,
      delta_crt_ch = valeur_crt - valeur_ch,
      abs_delta_crt_ch = abs(delta_crt_ch),
      delta_vpap_ch = valeur_vpap - valeur_ch,
      abs_delta_vpap_ch = abs(delta_vpap_ch)
    ) |>
    dplyr::select(
      stage, niveau, code, composante, annee, frequence_source, n_obs,
      valeur_crt, valeur_vpap, valeur_ch,
      delta_crt_vpap, abs_delta_crt_vpap, ratio_crt_vpap,
      egalite_crt_vpap, egalite_crt_vpap_post_base, nonzero_crt_vpap,
      delta_crt_ch, abs_delta_crt_ch, delta_vpap_ch, abs_delta_vpap_ch
    )
}

.annualiser_pivot_comptes_diag_global_ere <- function(df_comptes_finaux) {
  df_comptes_finaux |>
    tidyr::pivot_longer(
      cols = c(P1_crt, P1_vpap, P1_ch, P2_crt, P2_vpap, P2_ch, VA_crt, VA_vpap, VA_ch),
      names_to = c("composante", ".value"),
      names_pattern = "^(P1|P2|VA)_(crt|vpap|ch)$"
    ) |>
    .annualiser_stage_diag_global_ere(
      stage = "comptes_branches",
      niveau = "branche_composante",
      code_col = "Code_Branche",
      composante = "composante",
      crt_col = "crt",
      vpap_col = "vpap",
      ch_col = "ch"
    )
}

.annualiser_long_pre_cholette_diag_global_ere <- function(series_pre_cholette) {
  series_pre_cholette |>
    dplyr::group_by(bloc, Code_Produit, composante, annee, type_prix) |>
    dplyr::summarise(
      valeur = sum(valeur, na.rm = TRUE),
      n_obs = dplyr::n(),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = type_prix,
      values_from = c(valeur, n_obs),
      names_sep = "_"
    ) |>
    dplyr::mutate(
      stage = dplyr::if_else(bloc == "ressource", "ere_ressource_pre_cholette", "ere_emploi_pre_cholette"),
      niveau = "produit_composante",
      frequence_source = "trimestriel",
      n_obs = dplyr::coalesce(n_obs_crt, n_obs_vpap, n_obs_ch, 0L),
      post_base = annee > 2015L,
      nonzero_crt_vpap = dplyr::coalesce(abs(valeur_crt) > 1e-9, FALSE) |
        dplyr::coalesce(abs(valeur_vpap) > 1e-9, FALSE),
      delta_crt_vpap = valeur_crt - valeur_vpap,
      abs_delta_crt_vpap = abs(delta_crt_vpap),
      ratio_crt_vpap = dplyr::if_else(
        !is.na(valeur_vpap) & abs(valeur_vpap) > 1e-12,
        valeur_crt / valeur_vpap,
        NA_real_
      ),
      egalite_crt_vpap = .is_equal_diag_global_ere(valeur_crt, valeur_vpap),
      egalite_crt_vpap_post_base = post_base & nonzero_crt_vpap & egalite_crt_vpap,
      delta_crt_ch = valeur_crt - valeur_ch,
      abs_delta_crt_ch = abs(delta_crt_ch),
      delta_vpap_ch = valeur_vpap - valeur_ch,
      abs_delta_vpap_ch = abs(delta_vpap_ch),
      code = Code_Produit
    ) |>
    dplyr::select(
      stage, niveau, code, composante, annee, frequence_source, n_obs,
      valeur_crt, valeur_vpap, valeur_ch,
      delta_crt_vpap, abs_delta_crt_vpap, ratio_crt_vpap,
      egalite_crt_vpap, egalite_crt_vpap_post_base, nonzero_crt_vpap,
      delta_crt_ch, abs_delta_crt_ch, delta_vpap_ch, abs_delta_vpap_ch
    )
}

.resumer_stages_diag_global_ere <- function(annual_detail) {
  annual_detail |>
    dplyr::group_by(stage, niveau) |>
    dplyr::summarise(
      n_lignes = dplyr::n(),
      n_post_base = sum(post_base, na.rm = TRUE),
      n_nonzero_crt_vpap = sum(post_base & nonzero_crt_vpap, na.rm = TRUE),
      n_egalites_crt_vpap = sum(egalite_crt_vpap_post_base, na.rm = TRUE),
      part_egalites_crt_vpap = dplyr::if_else(
        n_nonzero_crt_vpap > 0,
        n_egalites_crt_vpap / n_nonzero_crt_vpap,
        NA_real_
      ),
      abs_delta_crt_vpap_max = max(abs_delta_crt_vpap, na.rm = TRUE),
      abs_delta_crt_vpap_mediane = stats::median(abs_delta_crt_vpap[post_base & nonzero_crt_vpap], na.rm = TRUE),
      abs_delta_crt_ch_max = max(abs_delta_crt_ch, na.rm = TRUE),
      abs_delta_vpap_ch_max = max(abs_delta_vpap_ch, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      abs_delta_crt_vpap_max = dplyr::if_else(is.infinite(abs_delta_crt_vpap_max), NA_real_, abs_delta_crt_vpap_max),
      abs_delta_crt_ch_max = dplyr::if_else(is.infinite(abs_delta_crt_ch_max), NA_real_, abs_delta_crt_ch_max),
      abs_delta_vpap_ch_max = dplyr::if_else(is.infinite(abs_delta_vpap_ch_max), NA_real_, abs_delta_vpap_ch_max),
      abs_delta_crt_vpap_mediane = dplyr::if_else(is.nan(abs_delta_crt_vpap_mediane), NA_real_, abs_delta_crt_vpap_mediane)
    ) |>
    dplyr::arrange(dplyr::desc(part_egalites_crt_vpap), dplyr::desc(n_egalites_crt_vpap), stage)
}

.controler_chainage_quarterly_diag_global_ere <- function(df,
                                                          stage,
                                                          code_col,
                                                          composante = NULL,
                                                          crt_col,
                                                          vpap_col,
                                                          ch_col) {
  code_col <- rlang::as_name(rlang::ensym(code_col))
  composante_tbl <- if (is.null(composante)) {
    tibble::tibble(composante = NA_character_)
  } else if (length(composante) == 1L && composante %in% names(df)) {
    df |>
      dplyr::transmute(composante = as.character(.data[[composante]]))
  } else {
    tibble::tibble(composante = as.character(composante))
  }

  base <- df |>
    dplyr::transmute(
      annee = as.integer(annee),
      trimestre = as.integer(trimestre),
      code = as.character(.data[[code_col]]),
      composante = composante_tbl$composante,
      valeur_crt = as.numeric(.data[[crt_col]]),
      valeur_vpap = as.numeric(.data[[vpap_col]]),
      valeur_ch = as.numeric(.data[[ch_col]])
    ) |>
    dplyr::arrange(code, composante, annee, trimestre) |>
    dplyr::group_by(code, composante) |>
    dplyr::mutate(
      ch_calcule = QnaSut::calcul_valeur_chainee_trim(valeur_crt, valeur_vpap),
      vpap_calcule = QnaSut::dechainer_valeurs(valeur_crt, valeur_ch, trim = TRUE),
      ecart_ch = valeur_ch - ch_calcule,
      abs_ecart_ch = abs(ecart_ch),
      ecart_vpap = valeur_vpap - vpap_calcule,
      abs_ecart_vpap = abs(ecart_vpap)
    ) |>
    dplyr::ungroup()

  detail <- base |>
    dplyr::mutate(stage = stage, .before = 1)

  resume <- detail |>
    dplyr::group_by(stage, code, composante) |>
    dplyr::summarise(
      n_trim = dplyr::n(),
      abs_ecart_ch_max = max(abs_ecart_ch, na.rm = TRUE),
      abs_ecart_vpap_max = max(abs_ecart_vpap, na.rm = TRUE),
      abs_ecart_ch_moyen = mean(abs_ecart_ch, na.rm = TRUE),
      abs_ecart_vpap_moyen = mean(abs_ecart_vpap, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      abs_ecart_ch_max = dplyr::if_else(is.infinite(abs_ecart_ch_max), NA_real_, abs_ecart_ch_max),
      abs_ecart_vpap_max = dplyr::if_else(is.infinite(abs_ecart_vpap_max), NA_real_, abs_ecart_vpap_max)
    ) |>
    dplyr::arrange(dplyr::desc(abs_ecart_vpap_max), dplyr::desc(abs_ecart_ch_max), stage, code)

  list(detail = detail, resume = resume)
}

diagnostiquer_global_prix_volume_ere <- function(project_dir = NULL,
                                                 output_excel = NULL,
                                                 export_excel = TRUE,
                                                 tol_abs = 1e-6,
                                                 tol_rel = 1e-10) {
  .source_script_diag_global_ere(project_dir)

  contexte <- .construire_contexte_pre_cholette_ere(project_dir)
  series_pre_cholette <- .construire_series_pre_cholette_ere(contexte$ere_res, contexte$ere_emp)
  calage_pre_cholette <- .analyser_calage_annuel_pre_cholette_ere(
    series_pre_cholette,
    .construire_cibles_annuelles_pre_cholette_ere(contexte$donnees$cna_ere_struct)
  )

  df_comptes_finaux <- QnaSut::calculer_comptes_production(
    p1_agreg_complet = contexte$ind5$p1_agreg_complet,
    p2_final_cal = contexte$ci_branches$p2_final_cal,
    ct_trim = contexte$tre$res_trim$ct_trim
  )

  annual_detail <- dplyr::bind_rows(
    .annualiser_cna_ere_diag_global_ere(contexte$donnees$cna_ere_struct),
    .annualiser_ci_source_diag_global_ere(
      contexte$donnees$ci_crt,
      contexte$donnees$ci_vol,
      contexte$donnees$ci_ch
    ),
    .annualiser_stage_diag_global_ere(
      contexte$prod_direct$prod_complete,
      stage = "prod_directe",
      niveau = "full_code",
      code_col = "full_code",
      crt_col = "valeur_crt",
      vpap_col = "valeur_vpap",
      ch_col = "valeur_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$ci_branches$p2_final_cal,
      stage = "ci_branches",
      niveau = "branche",
      code_col = "Code_Branche",
      composante = "CI",
      crt_col = "P2_crt_cal",
      vpap_col = "P2_vpap_cal",
      ch_col = "P2_ch_cal"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$ind4_ci$ind4_final,
      stage = "ind4",
      niveau = "full_code",
      code_col = "full_code",
      crt_col = "valeur_crt_cal",
      vpap_col = "valeur_vpap_cal",
      ch_col = "valeur_ch_cal"
    ),
    .annualiser_pivot_comptes_diag_global_ere(df_comptes_finaux),
    .annualiser_stage_diag_global_ere(
      contexte$p1_final$p1_ere_ch |>
        dplyr::left_join(contexte$p1_final$p1_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
        dplyr::left_join(contexte$p1_final$p1_ere_vol, by = c("annee", "trimestre", "Code_Produit")),
      stage = "p1_ere_pre_bench",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "PRODUCTION",
      crt_col = "P1_crt",
      vpap_col = "P1_vol",
      ch_col = "P1_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$p1_ere_bench$p1_ere_ch |>
        dplyr::left_join(contexte$p1_ere_bench$p1_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
        dplyr::left_join(contexte$p1_ere_bench$p1_ere_vol, by = c("annee", "trimestre", "Code_Produit")),
      stage = "p1_ere_post_bench",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "PRODUCTION",
      crt_col = "P1_crt",
      vpap_col = "P1_vol",
      ch_col = "P1_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$p2_ere$p2_ere_ch |>
        dplyr::left_join(contexte$p2_ere$p2_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
        dplyr::left_join(contexte$p2_ere$p2_ere_vol, by = c("annee", "trimestre", "Code_Produit")),
      stage = "p2_ere_pre_bench",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "CI",
      crt_col = "P2_crt",
      vpap_col = "P2_vol",
      ch_col = "P2_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$p2_ere_bench$p2_ere_ch |>
        dplyr::left_join(contexte$p2_ere_bench$p2_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
        dplyr::left_join(contexte$p2_ere_bench$p2_ere_vol, by = c("annee", "trimestre", "Code_Produit")),
      stage = "p2_ere_post_bench",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "CI",
      crt_col = "P2_crt",
      vpap_col = "P2_vol",
      ch_col = "P2_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$imp_exp$cnt_imp_final,
      stage = "imports_ere",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "IMPORTATIONS",
      crt_col = "imp_crt",
      vpap_col = "imp_vpap",
      ch_col = "imp_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$imp_exp$cnt_exp_final,
      stage = "exports_ere",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "EXPORTATIONS",
      crt_col = "exp_crt",
      vpap_col = "exp_vpap",
      ch_col = "exp_ch"
    ),
    .annualiser_stage_diag_global_ere(
      contexte$ere_res$ressources_chaine,
      stage = "ressources_ere_total",
      niveau = "produit",
      code_col = "Code_Produit",
      composante = "TOTAL_RESSOURCES",
      crt_col = "valeur_crt",
      vpap_col = "valeur_vpap",
      ch_col = "valeur_ch"
    ),
    .annualiser_long_pre_cholette_diag_global_ere(series_pre_cholette)
  ) |>
    dplyr::mutate(
      post_base = annee > 2015L,
      nonzero_crt_vpap = dplyr::coalesce(abs(valeur_crt) > 1e-9, FALSE) |
        dplyr::coalesce(abs(valeur_vpap) > 1e-9, FALSE),
      egalite_crt_vpap = .is_equal_diag_global_ere(valeur_crt, valeur_vpap, tol_abs = tol_abs, tol_rel = tol_rel),
      egalite_crt_vpap_post_base = post_base & nonzero_crt_vpap & egalite_crt_vpap
    )

  synthese_stages <- .resumer_stages_diag_global_ere(annual_detail)

  egalites_post_base <- annual_detail |>
    dplyr::filter(egalite_crt_vpap_post_base) |>
    dplyr::arrange(stage, code, composante, annee)

  chainage_prod <- .controler_chainage_quarterly_diag_global_ere(
    contexte$prod_direct$prod_complete,
    stage = "prod_directe",
    code_col = "full_code",
    crt_col = "valeur_crt",
    vpap_col = "valeur_vpap",
    ch_col = "valeur_ch"
  )

  chainage_ci <- .controler_chainage_quarterly_diag_global_ere(
    contexte$ci_branches$p2_final_cal,
    stage = "ci_branches",
    code_col = "Code_Branche",
    composante = "CI",
    crt_col = "P2_crt_cal",
    vpap_col = "P2_vpap_cal",
    ch_col = "P2_ch_cal"
  )

  chainage_ind4 <- .controler_chainage_quarterly_diag_global_ere(
    contexte$ind4_ci$ind4_final,
    stage = "ind4",
    code_col = "full_code",
    crt_col = "valeur_crt_cal",
    vpap_col = "valeur_vpap_cal",
    ch_col = "valeur_ch_cal"
  )

  chainage_p1_ere <- .controler_chainage_quarterly_diag_global_ere(
    contexte$p1_ere_bench$p1_ere_ch |>
      dplyr::left_join(contexte$p1_ere_bench$p1_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
      dplyr::left_join(contexte$p1_ere_bench$p1_ere_vol, by = c("annee", "trimestre", "Code_Produit")),
    stage = "p1_ere_post_bench",
    code_col = "Code_Produit",
    composante = "PRODUCTION",
    crt_col = "P1_crt",
    vpap_col = "P1_vol",
    ch_col = "P1_ch"
  )

  chainage_p2_ere <- .controler_chainage_quarterly_diag_global_ere(
    contexte$p2_ere_bench$p2_ere_ch |>
      dplyr::left_join(contexte$p2_ere_bench$p2_ere_crt, by = c("annee", "trimestre", "Code_Produit")) |>
      dplyr::left_join(contexte$p2_ere_bench$p2_ere_vol, by = c("annee", "trimestre", "Code_Produit")),
    stage = "p2_ere_post_bench",
    code_col = "Code_Produit",
    composante = "CI",
    crt_col = "P2_crt",
    vpap_col = "P2_vol",
    ch_col = "P2_ch"
  )

  chainage_imp <- .controler_chainage_quarterly_diag_global_ere(
    contexte$imp_exp$cnt_imp_final,
    stage = "imports_ere",
    code_col = "Code_Produit",
    composante = "IMPORTATIONS",
    crt_col = "imp_crt",
    vpap_col = "imp_vpap",
    ch_col = "imp_ch"
  )

  chainage_exp <- .controler_chainage_quarterly_diag_global_ere(
    contexte$imp_exp$cnt_exp_final,
    stage = "exports_ere",
    code_col = "Code_Produit",
    composante = "EXPORTATIONS",
    crt_col = "exp_crt",
    vpap_col = "exp_vpap",
    ch_col = "exp_ch"
  )

  chainage_ressources <- .controler_chainage_quarterly_diag_global_ere(
    contexte$ere_res$ressources_chaine,
    stage = "ressources_ere_total",
    code_col = "Code_Produit",
    composante = "TOTAL_RESSOURCES",
    crt_col = "valeur_crt",
    vpap_col = "valeur_vpap",
    ch_col = "valeur_ch"
  )

  chainage_emplois <- .controler_chainage_quarterly_diag_global_ere(
    contexte$ere_emp$emplois_vpap,
    stage = "emplois_ere_pre_cholette",
    code_col = "Code_Produit",
    composante = "composante",
    crt_col = "valeur_crt",
    vpap_col = "valeur_vpap",
    ch_col = "valeur_ch"
  )

  chainage_detail <- dplyr::bind_rows(
    chainage_prod$detail,
    chainage_ci$detail,
    chainage_ind4$detail,
    chainage_p1_ere$detail,
    chainage_p2_ere$detail,
    chainage_imp$detail,
    chainage_exp$detail,
    chainage_ressources$detail,
    chainage_emplois$detail
  )

  chainage_resume <- dplyr::bind_rows(
    chainage_prod$resume,
    chainage_ci$resume,
    chainage_ind4$resume,
    chainage_p1_ere$resume,
    chainage_p2_ere$resume,
    chainage_imp$resume,
    chainage_exp$resume,
    chainage_ressources$resume,
    chainage_emplois$resume
  ) |>
    dplyr::group_by(stage) |>
    dplyr::summarise(
      n_series = dplyr::n(),
      abs_ecart_ch_max = max(abs_ecart_ch_max, na.rm = TRUE),
      abs_ecart_vpap_max = max(abs_ecart_vpap_max, na.rm = TRUE),
      abs_ecart_ch_moyen = mean(abs_ecart_ch_moyen, na.rm = TRUE),
      abs_ecart_vpap_moyen = mean(abs_ecart_vpap_moyen, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      abs_ecart_ch_max = dplyr::if_else(is.infinite(abs_ecart_ch_max), NA_real_, abs_ecart_ch_max),
      abs_ecart_vpap_max = dplyr::if_else(is.infinite(abs_ecart_vpap_max), NA_real_, abs_ecart_vpap_max)
    ) |>
    dplyr::arrange(dplyr::desc(abs_ecart_vpap_max), dplyr::desc(abs_ecart_ch_max), stage)

  top_anomalies <- annual_detail |>
    dplyr::filter(post_base, nonzero_crt_vpap) |>
    dplyr::arrange(
      dplyr::desc(egalite_crt_vpap_post_base),
      dplyr::desc(abs_delta_crt_vpap),
      stage, code, composante, annee
    ) |>
    dplyr::mutate(
      type_anomalie = dplyr::case_when(
        egalite_crt_vpap_post_base ~ "crt_egal_vpap_post_base",
        TRUE ~ "ecart_crt_vpap"
      )
    )

  if (is.null(output_excel)) {
    output_excel <- file.path(
      contexte$project_dir,
      paste0("Diagnostic_Global_Prix_Volume_ERE_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    )
  }

  if (isTRUE(export_excel)) {
    writexl::write_xlsx(
      list(
        Synthese_Stages = synthese_stages,
        Detail_Annuel_Stages = annual_detail,
        Egalites_Post2015 = egalites_post_base,
        Chainage_Resume = chainage_resume,
        Chainage_Detail = chainage_detail,
        Calage_PreCholette = calage_pre_cholette,
        Alertes_Calage_CRT_VPAP = calage_pre_cholette |>
          dplyr::filter(type_prix %in% c("crt", "vpap"), statut_calage == "ecart"),
        Top_Anomalies = top_anomalies,
        Synthese_CRT_VPAP_PreCholette = .analyser_egalite_crt_vpap_pre_cholette_ere(
          series_pre_cholette,
          .construire_cibles_annuelles_pre_cholette_ere(contexte$donnees$cna_ere_struct)
        )$synthese
      ),
      path = output_excel
    )

    message("Export diagnostic global prix-volume ERE : ", output_excel)
  }

  list(
    output_excel = output_excel,
    contexte = contexte,
    df_comptes_finaux = df_comptes_finaux,
    annual_detail = annual_detail,
    synthese_stages = synthese_stages,
    egalites_post_base = egalites_post_base,
    chainage_resume = chainage_resume,
    chainage_detail = chainage_detail,
    calage_pre_cholette = calage_pre_cholette,
    top_anomalies = top_anomalies
  )
}

if (sys.nframe() == 0L) {
  diagnostiquer_global_prix_volume_ere()
}
