# =============================================================================
# Tests unitaires : 03_benchmark.R
# =============================================================================

# Données minimales reproductibles pour les tests
make_source <- function(codes = "A*B01", annees = 2015:2018, type = "Ind1") {
  tidyr::expand_grid(
    full_code = codes,
    annee     = annees,
    trimestre = 1:4
  ) |>
    dplyr::mutate(
      valeur   = 100 + annee - 2015 + trimestre,
      type_ind = type,
      periode  = paste0(annee, "T", trimestre)
    )
}

make_target <- function(codes = "A*B01", annees = 2015:2017) {
  tidyr::expand_grid(full_code = codes, annee = annees) |>
    dplyr::mutate(valeur = (annee - 2014) * 400 + 10)
}

# -----------------------------------------------------------------------------
# Structure de la sortie
# -----------------------------------------------------------------------------
test_that("benchmark_groupe retourne un tibble avec valeur_cal", {
  src <- make_source()
  tgt <- make_target()
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur")
  expect_s3_class(res, "tbl_df")
  expect_true("valeur_cal" %in% names(res))
})

test_that("benchmark_groupe conserve le même nombre de lignes que la source filtrée", {
  src <- make_source()
  tgt <- make_target()
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur")
  expect_equal(nrow(res), nrow(src))
})

test_that("benchmark_groupe fonctionne sans type_filter (NULL)", {
  src <- make_source() |> dplyr::select(-type_ind)
  tgt <- make_target()
  res <- benchmark_groupe(src, tgt, type_filter = NULL, value_col = "valeur")
  expect_true("valeur_cal" %in% names(res))
  expect_equal(nrow(res), nrow(src))
})

# -----------------------------------------------------------------------------
# Cohérence des résultats
# -----------------------------------------------------------------------------
test_that("benchmark_groupe : les valeurs benchmarkées sont numériques et positives", {
  src <- make_source()
  tgt <- make_target()
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur")
  expect_type(res$valeur_cal, "double")
  expect_true(all(res$valeur_cal > 0))
})

test_that("benchmark_groupe : la somme annuelle benchmarkée est proche de la cible", {
  src <- make_source(annees = 2015:2016)
  tgt <- make_target(annees = 2015:2016)
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur")

  sommes <- res |>
    dplyr::group_by(full_code, annee) |>
    dplyr::summarise(sum_cal = sum(valeur_cal), .groups = "drop") |>
    dplyr::left_join(tgt, by = c("full_code", "annee"))

  # La somme trimestrielle benchmarkée doit être très proche de la cible annuelle
  expect_true(all(abs(sommes$sum_cal - sommes$valeur) < 1e-3))
})

# -----------------------------------------------------------------------------
# Cas limites
# -----------------------------------------------------------------------------
test_that("benchmark_groupe : code sans données cibles est ignoré sans erreur", {
  src <- make_source(codes = c("A*B01", "A*B02"))
  tgt <- make_target(codes = "A*B01")   # B02 n'a pas de cible
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur")
  # Seul B01 doit être dans le résultat
  expect_equal(unique(res$full_code), "A*B01")
})

test_that("benchmark_groupe : plusieurs codes sont traités indépendamment", {
  src <- make_source(codes = c("A*B01", "A*B02"))
  tgt <- make_target(codes = c("A*B01", "A*B02"))
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur")
  expect_equal(sort(unique(res$full_code)), c("A*B01", "A*B02"))
  expect_equal(nrow(res), nrow(src))
})

test_that("benchmark_groupe avec lambda=0 (additif) fonctionne", {
  src <- make_source()
  tgt <- make_target()
  res <- benchmark_groupe(src, tgt, type_filter = "Ind1", value_col = "valeur", lambda = 0)
  expect_true("valeur_cal" %in% names(res))
})
