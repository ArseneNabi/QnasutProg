library(devtools)
library(dplyr)
library(tidyr)

project_dir <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut"

devtools::load_all(project_dir, quiet = TRUE)
source(file.path(project_dir, "scripts", "diagnostic_pre_cholette_ere.R"), local = FALSE)

.annualiser <- function(df, code_col, value_col) {
  code_col <- rlang::as_name(rlang::ensym(code_col))
  value_col <- rlang::as_name(rlang::ensym(value_col))

  df |>
    transmute(
      annee = as.integer(.data$annee),
      trimestre = as.integer(.data$trimestre),
      code = as.character(.data[[code_col]]),
      valeur = as.numeric(.data[[value_col]])
    ) |>
    group_by(annee, code) |>
    summarise(
      somme_trimestrielle = sum(valeur, na.rm = TRUE),
      n_trim = n(),
      .groups = "drop"
    )
}

.preparer_cible <- function(df, code_col, value_col, type_filter = NULL) {
  code_col <- rlang::as_name(rlang::ensym(code_col))
  value_col <- rlang::as_name(rlang::ensym(value_col))

  out <- df
  if (!is.null(type_filter) && "type_ind" %in% names(out)) {
    out <- out |>
      filter(.data$type_ind == type_filter)
  }

  out |>
    transmute(
      annee = as.integer(.data$annee),
      code = as.character(.data[[code_col]]),
      valeur_annuelle_cible = as.numeric(.data[[value_col]])
    ) |>
    distinct()
}

.comparer <- function(stage, type_prix, qdf, code_col, value_col, cible_df,
                      cible_code_col, cible_value_col, type_filter = NULL,
                      tol = 1e-6) {
  annuel <- .annualiser(qdf, {{ code_col }}, {{ value_col }})
  cible <- .preparer_cible(cible_df, {{ cible_code_col }}, {{ cible_value_col }}, type_filter)

  annuel |>
    left_join(cible, by = c("annee", "code")) |>
    mutate(
      stage = stage,
      type_prix = type_prix,
      annee_complete = n_trim == 4L,
      cible_presente = !is.na(valeur_annuelle_cible),
      ecart_annuel = somme_trimestrielle - valeur_annuelle_cible,
      ecart_abs = abs(ecart_annuel),
      ecart_relatif = if_else(
        cible_presente & abs(valeur_annuelle_cible) > 0,
        ecart_annuel / valeur_annuelle_cible,
        NA_real_
      ),
      statut = case_when(
        !cible_presente ~ "sans_cible",
        !annee_complete ~ "annee_incomplete",
        ecart_abs <= tol ~ "ok",
        TRUE ~ "ecart"
      )
    ) |>
    select(stage, type_prix, code, annee, n_trim, annee_complete,
           cible_presente, somme_trimestrielle, valeur_annuelle_cible,
           ecart_annuel, ecart_abs, ecart_relatif, statut)
}

.cibles_ere <- function(cna_ere_struct, composante_source, type_prix_source) {
  QnaSut::pivoter_ere_long(cna_ere_struct[[composante_source]][[type_prix_source]],
                           type_prix_source, composante_source) |>
    transmute(
      annee = as.integer(annee),
      code = as.character(Code_Produit),
      valeur_annuelle_cible = as.numeric(valeur)
    )
}

ctx <- .construire_contexte_pre_cholette_ere(project_dir)

prod_crt <- ctx$donnees$prod_crt
prod_ch  <- ctx$donnees$prod_ch

prod_direct_crt <- .comparer(
  "prod_directe", "crt",
  ctx$prod_direct$prod_complete, full_code, valeur_crt,
  prod_crt, full_code, valeur, NULL
)
prod_direct_ch <- .comparer(
  "prod_directe", "ch",
  ctx$prod_direct$prod_complete, full_code, valeur_ch,
  prod_ch, full_code, valeur, NULL
)

ci_branches_crt <- .comparer(
  "ci_branches", "crt",
  ctx$ci_branches$p2_final_cal, Code_Branche, P2_crt_cal,
  ctx$ci_branches$ci_crt_agg, full_code, ci_crt_agg, NULL
)
ci_branches_ch <- .comparer(
  "ci_branches", "ch",
  ctx$ci_branches$p2_final_cal, Code_Branche, P2_ch_cal,
  ctx$ci_branches$ci_bran, full_code, ci_ch_agg, NULL
)
ci_branches_vpap <- .comparer(
  "ci_branches", "vpap",
  ctx$ci_branches$p2_final_cal, Code_Branche, P2_vpap_cal,
  ctx$ci_branches$ci_vol_agg, full_code, ci_vol_agg, NULL
)

ind4_crt <- .comparer(
  "ind4", "crt",
  ctx$ind4_ci$ind4_final, full_code, valeur_crt_cal,
  prod_crt, full_code, valeur, "Ind4"
)
ind4_ch <- .comparer(
  "ind4", "ch",
  ctx$ind4_ci$ind4_final, full_code, valeur_ch_cal,
  prod_ch, full_code, valeur, "Ind4"
)
ind4_vpap <- .comparer(
  "ind4", "vpap",
  ctx$ind4_ci$ind4_final, full_code, valeur_vpap_cal,
  prod_ch, full_code, valeur, "Ind4"
)

ind5_crt <- .comparer(
  "ind5", "crt",
  ctx$ind5$cnt_ind5_crt, full_code, valeur_cal,
  prod_crt, full_code, valeur, "Ind5"
)
ind5_ch <- .comparer(
  "ind5", "ch",
  ctx$ind5$cnt_ind5_ch, full_code, valeur_cal,
  prod_ch, full_code, valeur, "Ind5"
)

p1_pre_crt <- .comparer(
  "p1_ere_pre_bench", "crt",
  ctx$p1_final$p1_ere_crt, Code_Produit, P1_crt,
  .cibles_ere(ctx$donnees$cna_ere_struct, "PRODUCTION", "CnaErECrt"),
  code, valeur_annuelle_cible, NULL
)
p1_pre_vpap <- .comparer(
  "p1_ere_pre_bench", "vpap",
  ctx$p1_final$p1_ere_vol, Code_Produit, P1_vol,
  .cibles_ere(ctx$donnees$cna_ere_struct, "PRODUCTION", "CnaErEVol"),
  code, valeur_annuelle_cible, NULL
)
p1_pre_ch <- .comparer(
  "p1_ere_pre_bench", "ch",
  ctx$p1_final$p1_ere_ch, Code_Produit, P1_ch,
  .cibles_ere(ctx$donnees$cna_ere_struct, "PRODUCTION", "CnaErECh"),
  code, valeur_annuelle_cible, NULL
)

p1_post_crt <- .comparer(
  "p1_ere_post_bench", "crt",
  ctx$p1_ere_bench$p1_ere_crt, Code_Produit, P1_crt,
  .cibles_ere(ctx$donnees$cna_ere_struct, "PRODUCTION", "CnaErECrt"),
  code, valeur_annuelle_cible, NULL
)
p1_post_vpap <- .comparer(
  "p1_ere_post_bench", "vpap",
  ctx$p1_ere_bench$p1_ere_vol, Code_Produit, P1_vol,
  .cibles_ere(ctx$donnees$cna_ere_struct, "PRODUCTION", "CnaErEVol"),
  code, valeur_annuelle_cible, NULL
)
p1_post_ch <- .comparer(
  "p1_ere_post_bench", "ch",
  ctx$p1_ere_bench$p1_ere_ch, Code_Produit, P1_ch,
  .cibles_ere(ctx$donnees$cna_ere_struct, "PRODUCTION", "CnaErECh"),
  code, valeur_annuelle_cible, NULL
)

p2_pre_crt <- .comparer(
  "p2_ere_pre_bench", "crt",
  ctx$p2_ere$p2_ere_crt, Code_Produit, P2_crt,
  .cibles_ere(ctx$donnees$cna_ere_struct, "CI Prix d'acquisition", "CnaErECrt"),
  code, valeur_annuelle_cible, NULL
)
p2_pre_vpap <- .comparer(
  "p2_ere_pre_bench", "vpap",
  ctx$p2_ere$p2_ere_vol, Code_Produit, P2_vol,
  .cibles_ere(ctx$donnees$cna_ere_struct, "CI Prix d'acquisition", "CnaErEVol"),
  code, valeur_annuelle_cible, NULL
)
p2_pre_ch <- .comparer(
  "p2_ere_pre_bench", "ch",
  ctx$p2_ere$p2_ere_ch, Code_Produit, P2_ch,
  .cibles_ere(ctx$donnees$cna_ere_struct, "CI Prix d'acquisition", "CnaErECh"),
  code, valeur_annuelle_cible, NULL
)

p2_post_crt <- .comparer(
  "p2_ere_post_bench", "crt",
  ctx$p2_ere_bench$p2_ere_crt, Code_Produit, P2_crt,
  .cibles_ere(ctx$donnees$cna_ere_struct, "CI Prix d'acquisition", "CnaErECrt"),
  code, valeur_annuelle_cible, NULL
)
p2_post_vpap <- .comparer(
  "p2_ere_post_bench", "vpap",
  ctx$p2_ere_bench$p2_ere_vol, Code_Produit, P2_vol,
  .cibles_ere(ctx$donnees$cna_ere_struct, "CI Prix d'acquisition", "CnaErEVol"),
  code, valeur_annuelle_cible, NULL
)
p2_post_ch <- .comparer(
  "p2_ere_post_bench", "ch",
  ctx$p2_ere_bench$p2_ere_ch, Code_Produit, P2_ch,
  .cibles_ere(ctx$donnees$cna_ere_struct, "CI Prix d'acquisition", "CnaErECh"),
  code, valeur_annuelle_cible, NULL
)

exp_name <- names(ctx$donnees$cna_ere_struct)[grepl("Exportation", names(ctx$donnees$cna_ere_struct))][1]

imp_crt <- .comparer(
  "imports_ere", "crt",
  ctx$imp_exp$cnt_imp_final, Code_Produit, imp_crt,
  .cibles_ere(ctx$donnees$cna_ere_struct, "IMPORTATIONS", "CnaErECrt"),
  code, valeur_annuelle_cible, NULL
)
imp_vpap <- .comparer(
  "imports_ere", "vpap",
  ctx$imp_exp$cnt_imp_final, Code_Produit, imp_vpap,
  .cibles_ere(ctx$donnees$cna_ere_struct, "IMPORTATIONS", "CnaErEVol"),
  code, valeur_annuelle_cible, NULL
)
imp_ch <- .comparer(
  "imports_ere", "ch",
  ctx$imp_exp$cnt_imp_final, Code_Produit, imp_ch,
  .cibles_ere(ctx$donnees$cna_ere_struct, "IMPORTATIONS", "CnaErECh"),
  code, valeur_annuelle_cible, NULL
)

exp_crt <- .comparer(
  "exports_ere", "crt",
  ctx$imp_exp$cnt_exp_final, Code_Produit, exp_crt,
  .cibles_ere(ctx$donnees$cna_ere_struct, exp_name, "CnaErECrt"),
  code, valeur_annuelle_cible, NULL
)
exp_vpap <- .comparer(
  "exports_ere", "vpap",
  ctx$imp_exp$cnt_exp_final, Code_Produit, exp_vpap,
  .cibles_ere(ctx$donnees$cna_ere_struct, exp_name, "CnaErEVol"),
  code, valeur_annuelle_cible, NULL
)
exp_ch <- .comparer(
  "exports_ere", "ch",
  ctx$imp_exp$cnt_exp_final, Code_Produit, exp_ch,
  .cibles_ere(ctx$donnees$cna_ere_struct, exp_name, "CnaErECh"),
  code, valeur_annuelle_cible, NULL
)

all_res <- bind_rows(
  prod_direct_crt, prod_direct_ch,
  ci_branches_crt, ci_branches_vpap, ci_branches_ch,
  ind4_crt, ind4_vpap, ind4_ch,
  ind5_crt, ind5_ch,
  p1_pre_crt, p1_pre_vpap, p1_pre_ch,
  p1_post_crt, p1_post_vpap, p1_post_ch,
  p2_pre_crt, p2_pre_vpap, p2_pre_ch,
  p2_post_crt, p2_post_vpap, p2_post_ch,
  imp_crt, imp_vpap, imp_ch,
  exp_crt, exp_vpap, exp_ch
)

summary_tbl <- all_res |>
  filter(annee_complete) |>
  count(stage, type_prix, statut) |>
  tidyr::pivot_wider(names_from = statut, values_from = n, values_fill = 0) |>
  mutate(total = ok + ecart + sans_cible) |>
  arrange(stage, type_prix)

top_ecarts <- all_res |>
  filter(annee_complete, statut == "ecart") |>
  arrange(desc(ecart_abs)) |>
  select(stage, type_prix, code, annee, somme_trimestrielle,
         valeur_annuelle_cible, ecart_annuel, ecart_abs, ecart_relatif)

cat("SUMMARY_PIPELINE_CALAGE\n")
print(summary_tbl, n = Inf)

cat("TOP50_ECARTS_PIPELINE\n")
print(head(top_ecarts, 50), n = 50)

cat("TOP50_ECARTS_POST_BENCH\n")
print(
  top_ecarts |>
    filter(!grepl("pre_bench", stage)) |>
    head(50),
  n = 50
)
