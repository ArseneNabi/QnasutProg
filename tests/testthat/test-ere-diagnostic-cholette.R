# =============================================================================
# Tests unitaires : 18_ere_diagnostic_cholette.R
# =============================================================================

.build_data_diag <- function() {
  grille <- tidyr::expand_grid(
    annee = c(2022L, 2023L),
    trimestre = 1:4
  )

  dplyr::bind_rows(
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "PRODUCTION", valeur_trimestrielle = 100, valeur_annuelle = 400, type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "IMPORTATIONS", valeur_trimestrielle = 30, valeur_annuelle = 120, type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "FBCF", valeur_trimestrielle = 40, valeur_annuelle = 160, type_bloc = "emploi"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "CI Prix d'acquisition", valeur_trimestrielle = 90, valeur_annuelle = 360, type_bloc = "emploi")
  )
}

test_that("diagnostiquer_faisabilite_cholette_ere_produit detecte la contrainte triviale", {
  data_test <- .build_data_diag()

  res <- diagnostiquer_faisabilite_cholette_ere_produit(
    data_produit = data_test,
    composantes_ajustables = "CI Prix d'acquisition",
    tester_cholette = FALSE
  )

  expect_true(isTRUE(res$diagnostic_contraintes$coherence_annuelle))
  expect_true(isTRUE(res$diagnostic_contraintes$coherence_trimestrielle))
  expect_true(isTRUE(res$diagnostic_contraintes$contrainte_contemporaine_triviale))
  expect_true("contrainte_contemporaine_triviale" %in% res$verdict$motifs)
})

test_that("diagnostiquer_faisabilite_cholette_ere_tous_produits signale modele absent", {
  data_test <- .build_data_diag()
  data_aa <- data_test |> dplyr::mutate(Code_Produit = "AA000")

  data_ere <- dplyr::bind_rows(data_test, data_aa)

  model_equil <- tibble::tribble(
    ~Code_Produit, ~composante_ajustable,
    "MN002", "CI Prix d'acquisition"
  )

  res <- diagnostiquer_faisabilite_cholette_ere_tous_produits(
    data_ere = data_ere,
    model_equil = model_equil,
    tester_cholette = FALSE
  )

  expect_equal(nrow(res$resume_produits), 2)
  expect_true(any(res$resume_produits$Code_Produit == "AA000" &
                    res$resume_produits$verdict == "non_faisable_probable"))
})
