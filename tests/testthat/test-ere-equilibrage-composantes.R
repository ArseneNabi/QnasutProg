# =============================================================================
# Tests unitaires : harmonisation des composantes ERE pour l'equilibrage
# =============================================================================

test_that("construire_data_ere_depuis_pipeline harmonise les alias courts d'emplois", {
  grille <- tidyr::expand_grid(
    annee = 2022L,
    trimestre = 1:4,
    Code_Produit = "AA000"
  )

  ressources_bench <- grille |>
    dplyr::mutate(
      composante = "PRODUCTION",
      valeur_composante = 100
    )

  emplois_bench <- dplyr::bind_rows(
    dplyr::mutate(grille, composante = "CI", valeur_cal = 40),
    dplyr::mutate(grille, composante = "CFmarch", valeur_cal = 20),
    dplyr::mutate(grille, composante = "VS", valeur_cal = 10),
    dplyr::mutate(grille, composante = "EXPORTATIONS", valeur_cal = 15)
  )

  cna_ere_struct <- list(
    "PRODUCTION" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 400)
    ),
    "CI Prix d'acquisition" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 160)
    ),
    "CF Marchande Menage Prix d'acquisition" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 80)
    ),
    "VS Prix d'acquisition" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 40)
    ),
    "Exportations de biens et services" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 60)
    )
  )

  res <- construire_data_ere_depuis_pipeline(
    ressources_bench = ressources_bench,
    emplois_bench = emplois_bench,
    cna_ere_struct = cna_ere_struct,
    type_cna = "CnaErECrt"
  )

  expect_true("CI Prix d'acquisition" %in% res$composante)
  expect_true("CF Marchande Menage Prix d'acquisition" %in% res$composante)
  expect_true("VS Prix d'acquisition" %in% res$composante)
  expect_true("Exportation Prix d'acquisition" %in% res$composante)
  expect_false(any(res$composante %in% c("CI", "CFmarch", "VS", "EXPORTATIONS")))

  export_ann <- res |>
    dplyr::filter(.data$composante == "Exportation Prix d'acquisition") |>
    dplyr::pull(.data$valeur_annuelle) |>
    unique()

  expect_equal(export_ann, 60)
})

test_that("executer_equilibrage_ere accepte un modele long et des emplois courts", {
  grille <- tidyr::expand_grid(
    annee = 2022L,
    trimestre = 1:4,
    Code_Produit = "AA000"
  )

  ere_res <- list(
    ressources_crt = grille |>
      dplyr::mutate(composante = "PRODUCTION", valeur_composante = 25),
    ressources_vpap = grille |>
      dplyr::mutate(composante = "PRODUCTION", valeur_composante = 25)
  )

  ere_emp <- list(
    emplois_crt = grille |>
      dplyr::mutate(composante = "CI", valeur_cal = 20),
    emplois_vpap = grille |>
      dplyr::mutate(composante = "CI", valeur_vpap = 20)
  )

  modele_equilibrage <- list(
    produits_composantes_autorisees = tibble::tibble(
      Code_Produit = "AA000",
      Composante = "CI Prix d'acquisition",
      autorise = TRUE
    )
  )

  cna_ere_struct <- list(
    "PRODUCTION" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 100),
      CnaErEVol = tibble::tibble(annee = 2022L, AA000 = 100)
    ),
    "CI Prix d'acquisition" = list(
      CnaErECrt = tibble::tibble(annee = 2022L, AA000 = 80),
      CnaErEVol = tibble::tibble(annee = 2022L, AA000 = 80)
    )
  )

  res <- executer_equilibrage_ere(
    ere_res = ere_res,
    ere_emp = ere_emp,
    modele_equilibrage = modele_equilibrage,
    cna_ere_struct = cna_ere_struct
  )

  expect_false(res$synthese_crt$echec[[1]])
  expect_false(res$synthese_vpap$echec[[1]])
  expect_true(all(res$emplois_ere_crt_equilibres$composante == "CI Prix d'acquisition"))
  expect_true(all(res$emplois_ere_vpap_equilibres$composante == "CI Prix d'acquisition"))
})

test_that("preparer_donnees_equilibrage_ere_produit harmonise les periodes communes", {
  grille <- tidyr::expand_grid(
    annee = c(2022L, 2023L),
    trimestre = 1:4
  )

  data_test <- dplyr::bind_rows(
    dplyr::mutate(grille, Code_Produit = "AA000", composante = "PRODUCTION", valeur_trimestrielle = 100, valeur_annuelle = 400, type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "AA000", composante = "IMPORTATIONS", valeur_trimestrielle = 20, valeur_annuelle = 80, type_bloc = "ressource"),
    dplyr::mutate(grille[-8, ], Code_Produit = "AA000", composante = "CI Prix d'acquisition", valeur_trimestrielle = 40, valeur_annuelle = 160, type_bloc = "emploi"),
    dplyr::mutate(grille, Code_Produit = "AA000", composante = "CF Marchande Menage Prix d'acquisition", valeur_trimestrielle = 20, valeur_annuelle = 80, type_bloc = "emploi")
  )

  res <- preparer_donnees_equilibrage_ere_produit(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    )
  )

  expect_equal(nrow(dplyr::distinct(res, annee, trimestre)), 7)
  expect_false(any(res$annee == 2023L & res$trimestre == 4L))
})

test_that("preparer_donnees_equilibrage_ere_produit recalcule la cible annuelle sur annee incomplete", {
  grille <- tidyr::expand_grid(
    annee = c(2022L, 2023L),
    trimestre = 1:4
  )

  data_test <- dplyr::bind_rows(
    dplyr::mutate(grille, Code_Produit = "AA000", composante = "PRODUCTION", valeur_trimestrielle = 100, valeur_annuelle = 400, type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "AA000", composante = "IMPORTATIONS", valeur_trimestrielle = 20, valeur_annuelle = 80, type_bloc = "ressource"),
    dplyr::mutate(grille[-8, ], Code_Produit = "AA000", composante = "CI Prix d'acquisition", valeur_trimestrielle = 40, valeur_annuelle = c(rep(160, 4), rep(160, 3)), type_bloc = "emploi"),
    dplyr::mutate(grille, Code_Produit = "AA000", composante = "CF Marchande Menage Prix d'acquisition", valeur_trimestrielle = 20, valeur_annuelle = c(rep(80, 4), rep(80, 4)), type_bloc = "emploi")
  )

  res <- preparer_donnees_equilibrage_ere_produit(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    )
  )

  cible_2023_ci <- res |>
    dplyr::filter(.data$annee == 2023L,
                  .data$composante == "CI Prix d'acquisition") |>
    dplyr::pull(.data$valeur_annuelle) |>
    unique()

  cible_2023_cf <- res |>
    dplyr::filter(.data$annee == 2023L,
                  .data$composante == "CF Marchande Menage Prix d'acquisition") |>
    dplyr::pull(.data$valeur_annuelle) |>
    unique()

  expect_equal(cible_2023_ci, 120)
  expect_equal(cible_2023_cf, 60)
})

test_that("equilibrer_produit_ere_multivariatecholette accepte une sortie directe du solveur", {
  grille <- tidyr::expand_grid(
    annee = c(2022L, 2023L),
    trimestre = 1:4
  )

  data_test <- dplyr::bind_rows(
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "PRODUCTION", valeur_trimestrielle = c(140, 150, 145, 155, 160, 150, 165, 170), valeur_annuelle = c(rep(590, 4), rep(645, 4)), type_bloc = "ressource"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "CI Prix d'acquisition", valeur_trimestrielle = c(70, 75, 72, 78, 80, 76, 82, 84), valeur_annuelle = c(rep(295, 4), rep(322, 4)), type_bloc = "emploi"),
    dplyr::mutate(grille, Code_Produit = "MN002", composante = "CF Marchande Menage Prix d'acquisition", valeur_trimestrielle = c(70, 75, 73, 77, 80, 74, 83, 86), valeur_annuelle = c(rep(295, 4), rep(323, 4)), type_bloc = "emploi")
  )

  fake_solver <- function(xlist, tcvector, ccvector) {
    list(
      "CI Prix d'acquisition" = xlist[["CI Prix d'acquisition"]],
      "CF Marchande Menage Prix d'acquisition" = xlist[["CF Marchande Menage Prix d'acquisition"]]
    )
  }

  res <- equilibrer_produit_ere_multivariatecholette(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    ),
    call_cholette = fake_solver
  )

  expect_equal(sort(unique(res$series_ajustees$composante)),
               sort(c("CI Prix d'acquisition", "CF Marchande Menage Prix d'acquisition")))
  expect_equal(nrow(res$series_ajustees), 16)
})
