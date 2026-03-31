testthat::test_that("diagnostiquer_ere renvoie les tables attendues", {
  base <- tibble::tibble(
    annee = rep(c(2022, 2023), each = 4),
    trimestre = rep(1:4, 2),
    Code_Produit = "AA000"
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
  testthat::expect_true("ecarts_benchmarking" %in% names(diag))
  testthat::expect_true("ratios_p2_sur_p1" %in% names(diag))
  testthat::expect_true("coherence_b1_branche" %in% names(diag))
  testthat::expect_true(any(grepl("hors cible CNA", diag$alertes)))
})
