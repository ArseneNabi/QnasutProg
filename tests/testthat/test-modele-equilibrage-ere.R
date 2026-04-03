# =============================================================================
# Tests unitaires : 17_modele_equilibrage_ere.R
# =============================================================================

test_that("importer_modele_equilibrage_ere_excel reconstruit la structure attendue", {
  path_xlsx <- tempfile(fileext = ".xlsx")

  model_equil <- tibble::tribble(
    ~...1, ~...2, ~...3, ~...4, ~...5, ~...6, ~...7, ~...8, ~...9, ~...10, ~...11,
    "Code_Produit", "Designation", "", "Modele", "", "", "", "", "", "", "",
    "BD000", "Produit BD", "", "Solde CF FBCF", "", "", "", "", "", "", "",
    "AA000", "Produit AA", "", "Solde VS", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "Modele", "CI Prix d'acquisition", "CF Marchande Menage Prix d'acquisition",
    "CF Non Marchande Menage Prix d'acquisition", "CF Non Marchande APU Prix d'acquisition",
    "CF Non Marchande ISBL Prix d'acquisition", "FBCF Prix d'acquisition",
    "VS Prix d'acquisition",
    "", "", "", "Solde CF FBCF", "No", "Ok", "Ok", "Ok", "Ok", "Ok", "No",
    "", "", "", "Solde VS", "No", "No", "No", "No", "No", "No", "Ok"
  )

  writexl::write_xlsx(list(ModelEquil = model_equil), path = path_xlsx)

  res <- importer_modele_equilibrage_ere_excel(path_xlsx)

  expect_true(is.list(res))
  expect_true(all(c(
    "produits_modeles",
    "modeles_composantes",
    "produits_composantes_autorisees"
  ) %in% names(res)))

  expect_equal(nrow(res$produits_modeles), 2)
  expect_true(all(c("Code_Produit", "Designation", "Modele") %in% names(res$produits_modeles)))
  expect_true(all(c("Modele", "Composante", "autorise") %in% names(res$modeles_composantes)))
  expect_true(all(c("Code_Produit", "Modele", "Composante", "autorise") %in%
                    names(res$produits_composantes_autorisees)))

  bd <- res$produits_composantes_autorisees |>
    dplyr::filter(Code_Produit == "BD000", Composante == "FBCF Prix d'acquisition")
  expect_true(isTRUE(bd$autorise[[1]]))
})

test_that("importer_modele_equilibrage_ere_excel échoue si feuille absente", {
  path_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(AutreFeuille = tibble::tibble(x = 1)), path = path_xlsx)

  expect_error(
    importer_modele_equilibrage_ere_excel(path_xlsx, sheet = "ModelEquil"),
    "Feuille 'ModelEquil' absente"
  )
})

test_that("sauvegarder/charger_modele_equilibrage_ere gèrent overwrite", {
  path_xlsx <- tempfile(fileext = ".xlsx")
  out_dir <- tempdir(check = TRUE)

  model_equil <- tibble::tribble(
    ~...1, ~...2, ~...3, ~...4, ~...5, ~...6, ~...7, ~...8, ~...9, ~...10, ~...11,
    "Code_Produit", "Designation", "", "Modele", "", "", "", "", "", "", "",
    "BD000", "Produit BD", "", "Solde CF FBCF", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "Modele", "CI Prix d'acquisition", "CF Marchande Menage Prix d'acquisition",
    "CF Non Marchande Menage Prix d'acquisition", "CF Non Marchande APU Prix d'acquisition",
    "CF Non Marchande ISBL Prix d'acquisition", "FBCF Prix d'acquisition",
    "VS Prix d'acquisition",
    "", "", "", "Solde CF FBCF", "No", "Ok", "Ok", "Ok", "Ok", "Ok", "No"
  )

  writexl::write_xlsx(list(ModelEquil = model_equil), path = path_xlsx)

  path_rds <- sauvegarder_modele_equilibrage_ere(
    path_excel = path_xlsx,
    output_dir = out_dir,
    output_file = "Modele_Equilibrage_ERE_test.rds",
    overwrite = TRUE
  )

  expect_true(file.exists(path_rds))

  expect_error(
    sauvegarder_modele_equilibrage_ere(
      path_excel = path_xlsx,
      output_dir = out_dir,
      output_file = "Modele_Equilibrage_ERE_test.rds",
      overwrite = FALSE
    ),
    "Le fichier existe déjà"
  )

  obj <- charger_modele_equilibrage_ere(path_rds)
  expect_true(is.list(obj))
  expect_equal(nrow(obj$produits_modeles), 1)
})
