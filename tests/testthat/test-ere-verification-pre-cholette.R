# =============================================================================
# Tests unitaires : 19_ere_verification_pre_cholette.R
# =============================================================================

.build_data_precholette <- function(neutral = TRUE) {
  grille <- tidyr::expand_grid(
    annee = c(2022L, 2023L),
    trimestre = 1:4
  )

  emploi_ci <- if (neutral) c(100, 100, 100, 100, 100, 100, 100, 100) else c(90, 95, 110, 105, 95, 100, 105, 95)

  dplyr::bind_rows(
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "PRODUCTION", valeur_trimestrielle = 120, valeur_annuelle = 480, type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "IMPORTATIONS", valeur_trimestrielle = 20, valeur_annuelle = 80, type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "FBCF", valeur_trimestrielle = 40, valeur_annuelle = 160, type_bloc = "emploi"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "CI Prix d'acquisition", valeur_trimestrielle = emploi_ci, valeur_annuelle = 400, type_bloc = "emploi")
  )
}

test_that("verifier_calage_annuel_composantes_ere retourne un resume coherent", {
  data_test <- .build_data_precholette(neutral = TRUE)

  res <- verifier_calage_annuel_composantes_ere(data_test, tol = 1e-8)

  expect_true(is.list(res))
  expect_true(all(c("detail", "resume", "coherent_global") %in% names(res)))
  expect_true(isTRUE(res$coherent_global))
  expect_equal(res$resume$nb_non_coherents, 0)
})

test_that("verifier_preconditions_cholette_ere_produit detecte une non-neutralite annuelle", {
  data_test <- .build_data_precholette(neutral = FALSE)

  res <- verifier_preconditions_cholette_ere_produit(
    data_produit = data_test,
    composantes_ajustables = "CI Prix d'acquisition",
    tol = 1e-8
  )

  expect_false(isTRUE(res$faisable_pre_cholette))
  expect_false(isTRUE(res$controle_neutralite_ecarts$coherent_global))
  expect_true(any(!res$controle_neutralite_ecarts$detail$coherent))
})

test_that("verifier_preconditions_cholette_ere_produit valide un produit coherent", {
  data_test <- .build_data_precholette(neutral = TRUE)

  res <- verifier_preconditions_cholette_ere_produit(
    data_produit = data_test,
    composantes_ajustables = "CI Prix d'acquisition",
    tol = 1e-8
  )

  expect_true(isTRUE(res$faisable_pre_cholette))
  expect_true(isTRUE(res$controle_calage_annuel$coherent_global))
  expect_true(isTRUE(res$controle_neutralite_ecarts$coherent_global))
})
