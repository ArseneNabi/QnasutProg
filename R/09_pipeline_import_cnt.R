# R/09_pipeline_import_cnt.R

# -------------------------------------------------------------------------
# Helpers internes
# -------------------------------------------------------------------------

construire_paths_cnt <- function(cfg) {
  list(
    path_ind       = file.path(cfg$root_dir, "R_Indicateur.xlsx"),
    path_cna       = file.path(cfg$root_dir, "R_CNA.xlsx"),
    path_prix      = file.path(cfg$root_dir, "R_Prix.xlsx"),
    path_tre       = file.path(cfg$root_dir, "R_Serie_TRE.xlsx"),
    path_db_tre    = file.path(cfg$data_hist_dir, "Base_TRE_Historique.rds"),
    path_passage   = file.path(cfg$data_hist_dir, "Table_Passage_Niv2_Etal.rds"),
    path_map_bran  = file.path(cfg$nomen_dir, "Map_Branches.rds"),
    path_map_prod  = file.path(cfg$nomen_dir, "Map_Produits.rds")
  )
}

verifier_fichiers_cnt <- function(paths) {
  fichiers <- unlist(paths)
  absents <- fichiers[!file.exists(fichiers)]
  if (length(absents) > 0) {
    stop(
      "Fichier(s) introuvable(s) :\n- ",
      paste(absents, collapse = "\n- "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

charger_bloc_cna_indicateurs <- function(paths) {
  list(
    ind_crt        = safe_import(import_matrix_cnt(paths$path_ind,  "IndCrt"),  "IndCrt"),
    ind_cst        = safe_import(import_matrix_cnt(paths$path_ind,  "IndCst"),  "IndCst"),
    prod_crt       = safe_import(import_matrix_cnt(paths$path_cna,  "ProdCrt"), "ProdCrt"),
    prod_ch        = safe_import(import_matrix_cnt(paths$path_cna,  "ProdCh"),  "ProdCh"),
    ci_crt         = safe_import(import_matrix_cnt(paths$path_cna,  "CiCrt"),   "CiCrt"),
    ci_vol         = safe_import(import_matrix_cnt(paths$path_cna,  "CiVol"),   "CiVol"),
    ci_ch          = safe_import(import_matrix_cnt(paths$path_cna,  "CiCh"),    "CiCh"),
    ind_ce_struct  = safe_import(import_ind_ce_structured(paths$path_ind, "IndCE"),   "IndCE"),
    cna_ere_struct = safe_import(import_cna_ere_structured(paths$path_cna, "CnaEre"), "CnaEre")
  )
}

charger_bloc_prix <- function(paths) {
  list(
    prix_etal = safe_import(
      import_matrix_prix(paths$path_prix, "Prix_Etal") |> prepare_price_data(),
      "Prix_Etal"
    ),
    prix_niv3 = safe_import(import_prix_niv3(paths$path_prix), "Prix_Niv3"),
    prix_ct   = safe_import(
      import_matrix_prix(paths$path_prix, "Prix_Ct") |> prepare_price_data(),
      "Prix_Ct"
    )
  )
}

charger_bloc_tre <- function(paths) {
  db_tre <- safe_import(readRDS(paths$path_db_tre), "Base_TRE_Historique.rds")
  table_passage <- safe_import(readRDS(paths$path_passage), "Table_Passage_Niv2_Etal.rds")

  db_tre_etal <- agreger_tre_etalonnage(db_tre, table_passage)

  list(
    db_tre_etal   = db_tre_etal,
    table_passage = table_passage
  )
}

charger_nomenclatures_cnt <- function(paths) {
  list(
    Map_Branches = safe_import(readRDS(paths$path_map_bran), "Map_Branches.rds"),
    Map_Produits = safe_import(readRDS(paths$path_map_prod), "Map_Produits.rds")
  )
}

# -------------------------------------------------------------------------
# Fonction exportée
# -------------------------------------------------------------------------

#' Charger toutes les données nécessaires au pipeline CNT
#'
#' @param cfg Liste de configuration retournée par [load_config()].
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{paths}{Chemins des fichiers sources}
#'   \item{ind_crt, ind_cst}{Indicateurs trimestriels}
#'   \item{prod_crt, prod_ch}{Production annuelle}
#'   \item{ci_crt, ci_vol, ci_ch}{Consommation intermédiaire annuelle}
#'   \item{ind_ce_struct, cna_ere_struct}{Structures ERE}
#'   \item{prix_etal, prix_niv3, prix_ct}{Matrices de prix}
#'   \item{db_tre_etal}{TRE historique agrégé vers étalonnage}
#'   \item{table_passage}{Passage niv2 -> étalonnage}
#'   \item{Map_Branches, Map_Produits}{Nomenclatures}
#' }
#'
#' @export
charger_donnees_cnt <- function(cfg) {
  message("📥 Chargement des données CNT...")

  paths <- construire_paths_cnt(cfg)
  verifier_fichiers_cnt(paths)

  bloc_cna  <- charger_bloc_cna_indicateurs(paths)
  bloc_prix <- charger_bloc_prix(paths)
  bloc_tre  <- charger_bloc_tre(paths)
  bloc_map  <- charger_nomenclatures_cnt(paths)

  out <- c(
    list(paths = paths),
    bloc_cna,
    bloc_prix,
    bloc_tre,
    bloc_map
  )

  message("✅ Toutes les données CNT ont été importées avec succès.")
  out
}
