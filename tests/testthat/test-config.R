# =============================================================================
# Tests unitaires : 00_config.R
# =============================================================================

# -----------------------------------------------------------------------------
# Cas nominal : config.yml valide
# -----------------------------------------------------------------------------
test_that("load_config() charge correctement un config.yml valide", {
  # Créer un config.yml temporaire
  tmp_dir <- withr::local_tempdir()
  config_content <- paste(
    "default:",
    '  root_dir: "/tmp/test"',
    '  data_hist_dir: "/tmp/test/hist"',
    '  nomen_dir: "/tmp/test/nomen"',
    "  derniere_annee_definitif: 2022",
    "  annee_fin_projection: 2024",
    sep = "\n"
  )
  writeLines(config_content, file.path(tmp_dir, "config.yml"))

  cfg <- suppressWarnings(load_config(file.path(tmp_dir, "config.yml")))

  expect_type(cfg, "list")
  expect_equal(cfg$derniere_annee_definitif, 2022)
  expect_equal(cfg$annee_fin_projection,     2024)
  expect_equal(cfg$root_dir,                 "/tmp/test")
})

test_that("load_config() retourne les 5 clés attendues", {
  tmp_dir <- withr::local_tempdir()
  config_content <- paste(
    "default:",
    '  root_dir: "/tmp/test"',
    '  data_hist_dir: "/tmp/test/hist"',
    '  nomen_dir: "/tmp/test/nomen"',
    "  derniere_annee_definitif: 2022",
    "  annee_fin_projection: 2024",
    sep = "\n"
  )
  writeLines(config_content, file.path(tmp_dir, "config.yml"))
  cfg <- suppressWarnings(load_config(file.path(tmp_dir, "config.yml")))

  expected_keys <- c("root_dir", "data_hist_dir", "nomen_dir",
                     "derniere_annee_definitif", "annee_fin_projection")
  expect_true(all(expected_keys %in% names(cfg)))
})

# -----------------------------------------------------------------------------
# Cas d'erreur
# -----------------------------------------------------------------------------
test_that("load_config() lève une erreur si le fichier n'existe pas", {
  expect_error(
    load_config("/chemin/inexistant/config.yml"),
    regexp = "introuvable"
  )
})

test_that("load_config() lève une erreur si la section 'default' est absente", {
  tmp_dir <- withr::local_tempdir()
  writeLines("autre_section:\n  cle: valeur", file.path(tmp_dir, "config.yml"))
  expect_error(
    load_config(file.path(tmp_dir, "config.yml")),
    regexp = "default"
  )
})

test_that("load_config() lève une erreur si une clé obligatoire est manquante", {
  tmp_dir <- withr::local_tempdir()
  # Manque nomen_dir et les bornes temporelles
  config_content <- paste(
    "default:",
    '  root_dir: "/tmp/test"',
    '  data_hist_dir: "/tmp/test/hist"',
    sep = "\n"
  )
  writeLines(config_content, file.path(tmp_dir, "config.yml"))
  expect_error(
    load_config(file.path(tmp_dir, "config.yml")),
    regexp = "manquantes"
  )
})
