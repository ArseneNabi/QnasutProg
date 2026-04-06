# =============================================================================
# Tests unitaires : 17_ere_equilibrage_multivarie.R
# =============================================================================

.build_data_test <- function() {
  grille <- tidyr::expand_grid(
    annee = c(2022L, 2023L),
    trimestre = 1:4
  )

  ressources <- dplyr::bind_rows(
    dplyr::mutate(grille, composante = "PRODUCTION", valeur_trimestrielle = 100, valeur_annuelle = 400, type_bloc = "ressource"),
    dplyr::mutate(grille, composante = "IMPORTATIONS", valeur_trimestrielle = 20, valeur_annuelle = 80, type_bloc = "ressource")
  )

  emplois_fixes <- dplyr::bind_rows(
    dplyr::mutate(grille, composante = "FBCF", valeur_trimestrielle = 30, valeur_annuelle = 120, type_bloc = "emploi"),
    dplyr::mutate(grille, composante = "EXPORTATIONS", valeur_trimestrielle = 10, valeur_annuelle = 40, type_bloc = "emploi")
  )

  emplois_aj <- dplyr::bind_rows(
    dplyr::mutate(grille, composante = "CI Prix d'acquisition", valeur_trimestrielle = 40, valeur_annuelle = 240, type_bloc = "emploi"),
    dplyr::mutate(grille, composante = "CF Marchande Menage Prix d'acquisition", valeur_trimestrielle = 20, valeur_annuelle = 160, type_bloc = "emploi")
  )

  dplyr::bind_rows(ressources, emplois_fixes, emplois_aj) |>
    dplyr::mutate(Code_Produit = "MN002") |>
    dplyr::select(Code_Produit, annee, trimestre, composante,
                  valeur_trimestrielle, valeur_annuelle, type_bloc)
}

.fake_multivariatecholette <- function(xlist, tcvector, ccvector) {
  trimes <- length(xlist[["CI Prix d'acquisition"]])

  # contrainte_t = 120 - 40 = 80
  # repartition arbitraire: 50 + 30 = 80
  out_ci <- stats::ts(rep(50, trimes), start = c(2022, 1), frequency = 4)
  out_cfm <- stats::ts(rep(30, trimes), start = c(2022, 1), frequency = 4)

  list(
    result = list(
      "CI Prix d'acquisition" = out_ci,
      "CF Marchande Menage Prix d'acquisition" = out_cfm
    ),
    xlist = xlist,
    tcvector = tcvector,
    ccvector = ccvector
  )
}

.fake_multivariatecholette_error <- function(xlist, tcvector, ccvector) {
  stop("jdplus.toolkit.base.core.ssf.SsfException: Inconsistent constraints in the model")
}

test_that("preparer_contraintes_equilibrage_ere_produit construit bien ccvector", {
  data_test <- .build_data_test()

  prep_data <- preparer_donnees_equilibrage_ere_produit(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    )
  )

  prep <- preparer_contraintes_equilibrage_ere_produit(prep_data)

  expect_equal(
    prep$ccvector,
    "CONTRAINTE_CONTEMP = CI Prix d'acquisition + CF Marchande Menage Prix d'acquisition"
  )
  expect_true(all(c("CONTRAINTE_CONTEMP", "CI Prix d'acquisition") %in% names(prep$xlist)))
  expect_equal(length(prep$tcvector), 2)
})

test_that("equilibrer_produit_ere_multivariatecholette retourne des series equilibrees", {
  data_test <- .build_data_test()

  res <- equilibrer_produit_ere_multivariatecholette(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    ),
    call_cholette = .fake_multivariatecholette
  )

  expect_true("series_ajustees" %in% names(res))
  expect_equal(length(unique(res$series_ajustees$composante)), 2)

  max_ecart <- max(abs(res$diagnostic$controle_contemporain$ecart_contemporain))
  expect_lt(max_ecart, 1e-9)
})

test_that("equilibrer_ere_multivarie signale un produit sans modele", {
  data_test <- .build_data_test()

  model_equil <- tibble::tribble(
    ~Code_Produit, ~composante_ajustable,
    "AA000", "CI Prix d'acquisition"
  )

  expect_error(
    equilibrer_ere_multivarie(
      data_ere = data_test,
      model_equil = model_equil,
      call_cholette = .fake_multivariatecholette
    ),
    "Aucun modele de bouclage"
  )
})

test_that("equilibrer_produit_ere_multivariatecholette expose le debug pre-cholette", {
  data_test <- .build_data_test()

  res <- equilibrer_produit_ere_multivariatecholette(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    ),
    mode_debug = TRUE
  )

  expect_equal(res$status, "debug_pre_cholette")
  expect_true(all(c("xlist", "tcvector", "ccvector", "start", "frequency", "noms_series") %in%
                    names(res$debug_pre_cholette)))
})

test_that("equilibrer_produit_ere_multivariatecholette detecte explicitement le cas univarie", {
  data_test <- .build_data_test()

  res <- equilibrer_produit_ere_multivariatecholette(
    data_produit = data_test,
    composantes_ajustables = "CI Prix d'acquisition"
  )

  expect_equal(res$status, "cas_univarie_non_supporte_par_multivariatecholette")
})

test_that("equilibrer_produit_ere_multivariatecholette retourne un echec explicite sans fallback silencieux", {
  data_test <- .build_data_test()

  res <- equilibrer_produit_ere_multivariatecholette(
    data_produit = data_test,
    composantes_ajustables = c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition"
    ),
    call_cholette = .fake_multivariatecholette_error
  )

  expect_equal(res$status, "echec_multivariatecholette")
  expect_match(res$message, "Inconsistent constraints")
  expect_equal(res$fallback_utilise, "aucun")
})
