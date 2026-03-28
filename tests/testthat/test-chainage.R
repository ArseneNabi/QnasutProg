# =============================================================================
# Tests unitaires : 06_chainage.R
# =============================================================================

# -----------------------------------------------------------------------------
# agregat_annuel_repete
# NB: prend un vecteur TRIMESTRIEL et répète le total annuel sur 4 trimestres
# -----------------------------------------------------------------------------
test_that("agregat_annuel_repete répète correctement le total annuel", {
  # 2 années complètes (8 trimestres) : total an1 = 100+110+90+100 = 400
  x <- c(100, 110, 90, 100,   # année 1
         200, 210, 190, 200)  # année 2
  res <- agregat_annuel_repete(x)
  expect_equal(length(res), 8)
  expect_equal(res[1:4], rep(400, 4))
  expect_equal(res[5:8], rep(800, 4))
})

test_that("agregat_annuel_repete : année incomplète donne NA en fin", {
  x <- c(100, 110, 90, 100,  # année complète
         50, 60)             # 2 trimestres seulement
  res <- agregat_annuel_repete(x)
  expect_equal(length(res), 6)
  expect_equal(res[1:4], rep(400, 4))
  expect_true(all(is.na(res[5:6])))
})

test_that("agregat_annuel_repete : vecteur trop court (< 4) retourne NA", {
  res <- agregat_annuel_repete(c(50, 60, 70))
  expect_equal(length(res), 3)
  expect_true(all(is.na(res)))
})

# -----------------------------------------------------------------------------
# calcul_indice_volume_annuel
# -----------------------------------------------------------------------------
test_that("calcul_indice_volume_annuel retourne 1 pour la première année", {
  crt <- c(100, 110, 121)
  vol <- c(100, 105, 110)
  res <- calcul_indice_volume_annuel(crt, vol)
  expect_equal(res[1], 1)
  expect_equal(length(res), 3)
})

test_that("calcul_indice_volume_annuel calcule correctement le rapport vol/crt_lag", {
  crt <- c(100, 110)
  vol <- c(100, 110)
  res <- calcul_indice_volume_annuel(crt, vol)
  # I_2 = vol[2] / crt[1] = 110 / 100 = 1.1
  expect_equal(res[2], 1.1)
})

# -----------------------------------------------------------------------------
# chainer_indices
# -----------------------------------------------------------------------------
test_that("chainer_indices produit le bon cumul", {
  indices <- c(1, 1.1, 0.9)
  res <- chainer_indices(indices)
  expect_equal(res[1], 1)
  expect_equal(res[2], 1.1)
  expect_equal(res[3], 1.1 * 0.9, tolerance = 1e-9)
})

# -----------------------------------------------------------------------------
# calcul_valeur_chainee_annuel
# -----------------------------------------------------------------------------
test_that("calcul_valeur_chainee_annuel retourne des valeurs positives", {
  crt <- c(100, 110, 121)
  vol <- c(100, 105, 110)
  res <- calcul_valeur_chainee_annuel(crt, vol)
  expect_true(all(res > 0))
  expect_equal(length(res), 3)
})

test_that("calcul_valeur_chainee_annuel : première valeur = première valeur courante", {
  crt <- c(100, 110, 121)
  vol <- c(100, 105, 110)
  res <- calcul_valeur_chainee_annuel(crt, vol)
  expect_equal(res[1], crt[1])
})

# -----------------------------------------------------------------------------
# calcul_valeur_chainee_trim
# -----------------------------------------------------------------------------
test_that("calcul_valeur_chainee_trim retourne le bon nombre d'éléments", {
  # 2 années complètes = 8 trimestres
  n <- 8
  crt <- rep(c(25, 27, 25, 23), 2)
  vol <- rep(c(24, 26, 24, 22), 2)
  res <- calcul_valeur_chainee_trim(crt, vol)
  expect_equal(length(res), n)
})

test_that("calcul_valeur_chainee_trim gère une année incomplète sans erreur", {
  # 2 ans complets + 2 trimestres = 10 valeurs
  crt <- c(rep(c(25, 27, 25, 23), 2), 26, 28)
  vol <- c(rep(c(24, 26, 24, 22), 2), 25, 27)
  expect_no_error(calcul_valeur_chainee_trim(crt, vol))
})

test_that("calcul_valeur_chainee_trim retourne des valeurs numériques", {
  crt <- rep(c(100, 110, 105, 95), 3)
  vol <- rep(c(98,  108, 103, 91), 3)
  res <- calcul_valeur_chainee_trim(crt, vol)
  expect_type(res, "double")
})

# -----------------------------------------------------------------------------
# dechainer_valeurs (VPAP)
# -----------------------------------------------------------------------------
test_that("dechainer_valeurs retourne le bon nombre d'éléments", {
  n <- 8
  crt     <- rep(c(100, 110, 105, 95), 2)
  chaine  <- rep(c(98,  107, 103, 92), 2)
  res <- dechainer_valeurs(crt, chaine, trim = TRUE)
  expect_equal(length(res), n)
})

test_that("dechainer_valeurs retourne des valeurs numériques non nulles", {
  crt    <- rep(c(100, 110, 105, 95), 2)
  chaine <- rep(c(98,  107, 103, 92), 2)
  res <- dechainer_valeurs(crt, chaine, trim = TRUE)
  expect_type(res, "double")
  expect_true(all(!is.nan(res)))
})

test_that("dechainer_valeurs mode annuel fonctionne", {
  crt    <- c(100, 110, 121)
  chaine <- c(100, 105, 112)
  res <- dechainer_valeurs(crt, chaine, trim = FALSE)
  expect_equal(length(res), 3)
})
