testthat::test_that("diagnostiquer_ere renvoie les tables attendues", {
  base <- tibble::tibble(
    annee = rep(c(2022, 2023), each = 4),
    trimestre = rep(1:4, 2),
    Code_Produit = "AA000"
  )

  ressources <- dplyr::bind_rows(
    dplyr::mutate(base, composante = "PRODUCTION", valeur_composante = 80),
    dplyr::mutate(base, composante = "IMPORTATIONS", valeur_composante = 20)
  )

  emplois <- dplyr::bind_rows(
    dplyr::mutate(base, composante = "CI", valeur_cal = 50),
    dplyr::mutate(base, composante = "CFmarch", valeur_cal = 50)
  )

  p1_av <- list(
    p1_ere_crt = dplyr::mutate(base, P1_crt = 100),
    p1_ere_ch  = dplyr::mutate(base, P1_ch = 100),
    p1_ere_vol = dplyr::mutate(base, P1_vol = 100)
  )
  p2_av <- list(
    p2_ere_crt = dplyr::mutate(base, P2_crt = 40),
    p2_ere_ch  = dplyr::mutate(base, P2_ch = 40),
    p2_ere_vol = dplyr::mutate(base, P2_vol = 40)
  )

  p1_ap <- p1_av
  p2_ap <- p2_av
  p1_ap$p1_ere_crt$P1_crt[1] <- 110

  comptes_branches <- tibble::tibble(
    annee = rep(c(2022, 2023), each = 4),
    trimestre = rep(1:4, 2),
    Code_Branche = "A01001",
    P1_crt = 200,
    P2_crt = 80,
    B1_crt = 120
  )

  diag <- diagnostiquer_ere(
    ere_ressources = list(ressources_crt = ressources),
    emplois_ere = list(emplois_crt = emplois),
    p1_ere_avant = p1_av,
    p1_ere_apres = p1_ap,
    p2_ere_avant = p2_av,
    p2_ere_apres = p2_ap,
    cna_ere_struct = NULL,
    comptes_branches = comptes_branches,
    seuil_rel_alerte = 0.01,
    seuil_rupture = 0.20
  )

  testthat::expect_true(is.list(diag))
  testthat::expect_true("equilibre_trim" %in% names(diag))
  testthat::expect_true("equilibre_annuel" %in% names(diag))
  testthat::expect_true("ecarts_benchmarking" %in% names(diag))
  testthat::expect_true("ratios_p2_sur_p1" %in% names(diag))
  testthat::expect_true("coherence_b1_branche" %in% names(diag))
  testthat::expect_equal(unique(diag$equilibre_trim$ecart_abs), 0)
  testthat::expect_true(any(grepl("hors cible CNA", diag$alertes)))
})

testthat::test_that("diagnostiquer_ere fonctionne en mode ressources-emplois seul", {
  base <- tibble::tibble(
    annee = rep(2023, 4),
    trimestre = 1:4,
    Code_Produit = "AB000"
  )

  ressources <- dplyr::mutate(base, composante = "TOTAL", valeur_composante = c(100, 110, 120, 130))
  emplois <- dplyr::mutate(base, composante = "TOTAL", valeur_cal = c(98, 108, 121, 132))

  diag <- diagnostiquer_ere(
    ere_ressources = ressources,
    emplois_ere = emplois,
    seuil_abs_alerte = 1
  )

  testthat::expect_true(nrow(diag$equilibre_trim) == 4)
  testthat::expect_true(nrow(diag$ecarts_benchmarking) == 0)
  testthat::expect_true(any(grepl("Ressources-Emplois", diag$alertes)))
})
