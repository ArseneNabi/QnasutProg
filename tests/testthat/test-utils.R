# =============================================================================
# Tests unitaires : 02_utils.R
# =============================================================================

# -----------------------------------------------------------------------------
# extract_target_code
# -----------------------------------------------------------------------------
test_that("extract_target_code supprime le préfixe avant le premier _", {
  expect_equal(extract_target_code("Ind1_PRIVE*A01007"), "PRIVE*A01007")
  expect_equal(extract_target_code("Ind3_TOTAL*B02001"), "TOTAL*B02001")
})

test_that("extract_target_code gère un code sans underscore", {
  expect_equal(extract_target_code("A01007"), "A01007")
})

test_that("extract_target_code fonctionne sur un vecteur", {
  codes <- c("Ind1_PRIVE*A01007", "Ind2_TOTAL*B01001")
  res   <- extract_target_code(codes)
  expect_equal(length(res), 2)
  expect_equal(res[1], "PRIVE*A01007")
  expect_equal(res[2], "TOTAL*B01001")
})

# -----------------------------------------------------------------------------
# extract_price_branch
# -----------------------------------------------------------------------------
test_that("extract_price_branch extrait correctement le code après *", {
  expect_equal(extract_price_branch("Ind1_PRIVE*A01007"), "A01007")
  expect_equal(extract_price_branch("CI_TOTAL*B02001"),   "B02001")
})

test_that("extract_price_branch gère un code déjà propre", {
  expect_equal(extract_price_branch("A01007"), "A01007")
})

test_that("extract_price_branch supprime les espaces", {
  expect_equal(extract_price_branch("Ind1* A01007 "), "A01007")
})

# -----------------------------------------------------------------------------
# extract_branch_code
# -----------------------------------------------------------------------------
test_that("extract_branch_code extrait la partie après le dernier *", {
  expect_equal(extract_branch_code("Ind1_PRIVE*A01007"), "A01007")
  expect_equal(extract_branch_code("CI_TOTAL*B01001"),   "B01001")
})

test_that("extract_branch_code retourne le code intact si pas de *", {
  expect_equal(extract_branch_code("A01007"), "A01007")
})

test_that("extract_branch_code fonctionne sur un vecteur", {
  codes <- c("Ind1_PRIVE*A01007", "B01001", "CI_TOTAL*B02001")
  res   <- extract_branch_code(codes)
  expect_equal(res, c("A01007", "B01001", "B02001"))
})

test_that("extract_branch_code supprime les espaces", {
  expect_equal(extract_branch_code("TOTAL* A01007 "), "A01007")
})
