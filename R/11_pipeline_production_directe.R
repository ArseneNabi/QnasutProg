# R/11_pipeline_production_directe.R

benchmarks_production_directe <- function(ind_crt, ind_cst, prod_crt, prod_ch) {
  message("--- Benchmarking groupe 1 (Direct) ---")

  list(
    cnt_ind1_crt = benchmark_groupe(ind_crt, prod_crt, "Ind1", "valeur"),
    cnt_ind1_ch  = benchmark_groupe(ind_cst, prod_ch, "Ind1", "valeur"),
    cnt_ind2_crt = benchmark_groupe(ind_crt, prod_crt, "Ind2", "valeur"),
    cnt_ind3_ch  = benchmark_groupe(ind_cst, prod_ch, "Ind3", "valeur")
  )
}

appliquer_prix_production <- function(cnt_ind2_crt, cnt_ind3_ch, prix_etal) {
  message("--- Application des Prix ---")

  list(
    est_ind2_vol = apply_price_to_bench(
      df_bench  = cnt_ind2_crt,
      df_prix   = prix_etal,
      operation = "deflate"
    ),
    est_ind3_val = apply_price_to_bench(
      df_bench  = cnt_ind3_ch,
      df_prix   = prix_etal,
      operation = "inflate"
    )
  )
}

benchmarks_production_apres_prix <- function(est_ind2_vol, est_ind3_val, prod_crt, prod_ch) {
  message("--- Benchmarking groupe 2 ---")

  list(
    cnt_ind2_ch = benchmark_groupe(
      df_source   = est_ind2_vol,
      df_target   = prod_ch,
      type_filter = "Ind2",
      value_col   = "valeur_estimee"
    ),
    cnt_ind3_crt = benchmark_groupe(
      df_source   = est_ind3_val,
      df_target   = prod_crt,
      type_filter = "Ind3",
      value_col   = "valeur_estimee"
    )
  )
}

consolider_production_complete <- function(cnt_ind1_crt, cnt_ind1_ch,
                                           cnt_ind2_crt, cnt_ind2_ch,
                                           cnt_ind3_crt, cnt_ind3_ch) {
  message("📦 Consolidation des résultats P1...")

  df_all_crt <- dplyr::bind_rows(
    dplyr::mutate(cnt_ind1_crt, methode = "Ind1"),
    dplyr::mutate(cnt_ind2_crt, methode = "Ind2"),
    dplyr::mutate(cnt_ind3_crt, methode = "Ind3")
  ) |>
    dplyr::select(full_code, annee, trimestre, valeur_crt = valeur_cal)

  df_all_ch <- dplyr::bind_rows(
    dplyr::mutate(cnt_ind1_ch, methode = "Ind1"),
    dplyr::mutate(cnt_ind2_ch, methode = "Ind2"),
    dplyr::mutate(cnt_ind3_ch, methode = "Ind3")
  ) |>
    dplyr::select(full_code, annee, trimestre, valeur_ch = valeur_cal)

  prod_complete <- dplyr::inner_join(
    df_all_crt,
    df_all_ch,
    by = c("full_code", "annee", "trimestre")
  ) |>
    dplyr::group_by(full_code) |>
    dplyr::mutate(
      valeur_vpap = dechainer_valeurs(valeur_crt, valeur_ch, trim = TRUE)
    ) |>
    dplyr::ungroup()

  list(
    df_all_crt    = df_all_crt,
    df_all_ch     = df_all_ch,
    prod_complete = prod_complete
  )
}

#' Exécuter l'optique production directe
#'
#' @param donnees Liste chargée par [charger_donnees_cnt()].
#' @param tre Résultat de [executer_module_tre()].
#'
#' @return Une liste contenant les sorties intermédiaires et finales :
#' \describe{
#'   \item{cnt_ind1_crt, cnt_ind1_ch}{Benchmarking Ind1}
#'   \item{cnt_ind2_crt, cnt_ind2_ch}{Benchmarking Ind2}
#'   \item{cnt_ind3_crt, cnt_ind3_ch}{Benchmarking Ind3}
#'   \item{est_ind2_vol, est_ind3_val}{Étapes prix}
#'   \item{df_all_crt, df_all_ch}{Tables consolidées}
#'   \item{prod_complete}{Production trimestrielle complète}
#' }
#'
#' @export
executer_optique_production_directe <- function(donnees, tre) {
  step1 <- benchmarks_production_directe(
    ind_crt  = donnees$ind_crt,
    ind_cst  = donnees$ind_cst,
    prod_crt = donnees$prod_crt,
    prod_ch  = donnees$prod_ch
  )

  step2 <- appliquer_prix_production(
    cnt_ind2_crt = step1$cnt_ind2_crt,
    cnt_ind3_ch  = step1$cnt_ind3_ch,
    prix_etal    = donnees$prix_etal
  )

  step3 <- benchmarks_production_apres_prix(
    est_ind2_vol = step2$est_ind2_vol,
    est_ind3_val = step2$est_ind3_val,
    prod_crt     = donnees$prod_crt,
    prod_ch      = donnees$prod_ch
  )

  step4 <- consolider_production_complete(
    cnt_ind1_crt = step1$cnt_ind1_crt,
    cnt_ind1_ch  = step1$cnt_ind1_ch,
    cnt_ind2_crt = step1$cnt_ind2_crt,
    cnt_ind2_ch  = step3$cnt_ind2_ch,
    cnt_ind3_crt = step3$cnt_ind3_crt,
    cnt_ind3_ch  = step1$cnt_ind3_ch
  )

  c(step1, step2, step3, step4)
}
