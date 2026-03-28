#' Construire les tables de passage (nomenclatures) et les enregistrer en RDS
#'
#' @description
#' Génère :
#' - `Map_Branches.rds` à partir de l'onglet `Nomen_Bran`
#' - `Map_Produits.rds` à partir de l'onglet `Nomen_Prod`
#'
#' Cette fonction est destinée à être utilisée uniquement lors d'une mise à jour
#' exceptionnelle des nomenclatures Excel. Les fichiers RDS produits sont ensuite
#' réutilisés dans les compilations courantes.
#'
#' Structure attendue de l'onglet `Nomen_Prod` :
#' A = Code_Prod_N3
#' B = Libelle_N3
#' C = Code_Prod_N2
#' D = Libelle_N2
#' E = Code_Prod_Etal
#' F = Code_Prod_Ct
#' G = Code_Prod_Tpub
#' H = Libelle_Tpub
#' I = Type_Prod
#'
#' Structure attendue de l'onglet `Nomen_Bran` :
#' A = Code_Bran_N2
#' B = Libelle_N2
#' C = Code_Bran_Etal
#' D = Code_Bran_Ct
#' E = Code_Bran_Tpub
#' F = Libelle_Tpub
#'
#' @param path_excel Chemin complet vers le fichier Excel des nomenclatures.
#' @param output_folder Dossier de sortie des fichiers RDS.
#' @param overwrite Logique. Si FALSE et que les fichiers existent déjà, stop.
#'
#' @return Liste invisible contenant `Map_Branches` et `Map_Produits`.
#' @export
build_nomenclatures <- function(path_excel, output_folder, overwrite = FALSE) {

  if (!file.exists(path_excel)) {
    stop("Fichier Excel introuvable : ", path_excel)
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  out_bran <- file.path(output_folder, "Map_Branches.rds")
  out_prod <- file.path(output_folder, "Map_Produits.rds")

  if (!overwrite && (file.exists(out_bran) || file.exists(out_prod))) {
    stop(
      "Les fichiers RDS existent d\u00e9j\u00e0. Mets overwrite = TRUE pour les r\u00e9g\u00e9n\u00e9rer.\n",
      " - ", out_bran, "\n",
      " - ", out_prod
    )
  }

  # ============================================================
  # 1. PRODUITS
  # ============================================================
  message("\u1f504 Lecture Excel : onglet 'Nomen_Prod' ...")
  prod_raw <- readxl::read_excel(
    path_excel, sheet = "Nomen_Prod",
    col_types = "text", .name_repair = "minimal"
  )

  if (ncol(prod_raw) < 9) {
    stop("L'onglet Nomen_Prod doit contenir au moins 9 colonnes (A..I).")
  }

  names(prod_raw)[1:9] <- c(
    "Code_Prod_N3",
    "Libelle_N3",
    "Code_Prod_N2",
    "Libelle_N2",
    "Code_Prod_Etal",
    "Code_Prod_Ct",
    "Code_Prod_Tpub",
    "Libelle_Tpub",
    "Type_Prod"
  )

  map_prod <- prod_raw |>
    dplyr::select(
      Code_Prod_N3, Libelle_N3, Code_Prod_N2, Libelle_N2,
      Code_Prod_Etal, Code_Prod_Ct, Code_Prod_Tpub,
      Libelle_Tpub, Type_Prod
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ trimws(as.character(.x)))) |>
    dplyr::filter(!is.na(Code_Prod_N3), Code_Prod_N3 != "") |>
    dplyr::distinct()

  saveRDS(map_prod, out_prod)
  message("\u2705 Map_Produits.rds g\u00e9n\u00e9r\u00e9 : ", nrow(map_prod), " lignes. -> ", out_prod)

  # ============================================================
  # 2. BRANCHES
  # ============================================================
  message("\u1f504 Lecture Excel : onglet 'Nomen_Bran' ...")
  bran_raw <- readxl::read_excel(
    path_excel, sheet = "Nomen_Bran",
    col_types = "text", .name_repair = "minimal"
  )

  if (ncol(bran_raw) < 6) {
    stop("L'onglet Nomen_Bran doit contenir au moins 6 colonnes (A..F).")
  }

  names(bran_raw)[1:6] <- c(
    "Code_Bran_N2",
    "Libelle_N2",
    "Code_Bran_Etal",
    "Code_Bran_Ct",
    "Code_Bran_Tpub",
    "Libelle_Tpub"
  )

  map_bran <- bran_raw |>
    dplyr::select(
      Code_Bran_N2, Libelle_N2, Code_Bran_Etal,
      Code_Bran_Ct, Code_Bran_Tpub, Libelle_Tpub
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ trimws(as.character(.x)))) |>
    dplyr::filter(!is.na(Code_Bran_N2), Code_Bran_N2 != "") |>
    dplyr::distinct()

  saveRDS(map_bran, out_bran)
  message("\u2705 Map_Branches.rds g\u00e9n\u00e9r\u00e9 : ", nrow(map_bran), " lignes. -> ", out_bran)

  invisible(list(Map_Branches = map_bran, Map_Produits = map_prod))
}
