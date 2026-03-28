
# ==============================================================================
# 1. FONCTIONS D'EXTRACTION (MOTEUR ROBUSTE)
# ==============================================================================

#' Extraction Matrice Stricte
#' @noRd
extract_matrix_strict <- function(df_global, rows_data, cols_data, row_header_idx,
                                  type_bloc = "branche", fixed_label = NULL) {

  # 1. Extraction des Ent\u00eates
  vec_headers <- as.character(unlist(df_global[row_header_idx, cols_data]))
  vec_headers[is.na(vec_headers)] <- paste0("Vide_", seq_along(vec_headers[is.na(vec_headers)]))

  # 2. Extraction des Donn\u00e9es
  df_data <- as.data.frame(df_global[rows_data, c(1, cols_data)])
  colnames(df_data) <- c("Code_Produit", vec_headers)

  # 3. Pivot
  df_long <- df_data |>
    tidyr::pivot_longer(
      cols = -Code_Produit,
      names_to = "Header_Col",
      values_to = "Valeur"
    ) |>
    dplyr::mutate(
      Valeur = as.character(Valeur),
      # Nettoyage ROBUSTE
      Valeur = ifelse(grepl("^-+$", Valeur), "0", Valeur),
      Valeur = gsub("[[:space:]\u00A0]", "", Valeur), # Espaces standards + ins\u00e9cables
      Valeur = suppressWarnings(as.numeric(Valeur))
    ) |>
    dplyr::filter(!is.na(Valeur))

  # 4. Finalisation
  if (type_bloc == "branche") {
    df_long <- df_long |>
      dplyr::mutate(Code_Branche = Header_Col, Operation = fixed_label)
  } else {
    df_long <- df_long |>
      dplyr::mutate(Code_Branche = "Non_Ventil\u00e9", Operation = Header_Col)
  }

  return(df_long |> dplyr::select(Code_Produit, Code_Branche, Operation, Valeur))
}

#' Extraction Revenus (Compte Exploitation)
#' @noRd
extract_revenus_strict <- function(df_global, rows_data, cols_data, row_header_idx) {

  vec_branches <- as.character(unlist(df_global[row_header_idx, cols_data]))
  df_data <- as.data.frame(df_global[rows_data, c(1, cols_data)])
  colnames(df_data) <- c("Nom_Operation", vec_branches)

  df_long <- df_data |>
    tidyr::pivot_longer(
      cols = -Nom_Operation,
      names_to = "Code_Branche",
      values_to = "Valeur"
    ) |>
    dplyr::mutate(
      Valeur = as.character(Valeur),
      Valeur = ifelse(grepl("^-+$", Valeur), "0", Valeur),
      Valeur = gsub("[[:space:]\u00A0]", "", Valeur),
      Valeur = suppressWarnings(as.numeric(Valeur)),
      Code_Produit = "Op_Repartition",
      Operation = Nom_Operation
    ) |>
    dplyr::filter(!is.na(Valeur))

  return(df_long |> dplyr::select(Code_Produit, Code_Branche, Operation, Valeur))
}

# ==============================================================================
# 2. FONCTION D'ENRICHISSEMENT (CALCUL DES TOTAUX)
# ==============================================================================

#' Calculer et Ajouter les Lignes de Totaux
#' @noRd
enrichir_avec_totaux <- function(df_tre) {

  # 1. Totaux par BRANCHE (Pour Production P1 et CI P2)
  # On somme les produits pour obtenir le total branche.
  totaux_branches <- df_tre |>
    dplyr::filter(Operation %in% c("P1", "P2")) |>
    dplyr::group_by(Annee, Type_Prix, Operation, Code_Branche) |>
    dplyr::summarise(Valeur = sum(Valeur, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Code_Produit = "Total") # Code sp\u00e9cial pour identifier la ligne

  # 2. Totaux par PRODUIT (Pour les op\u00e9rations "Non_Ventil\u00e9" comme Taxes, Import...)
  totaux_produits <- df_tre |>
    dplyr::filter(Code_Branche == "Non_Ventil\u00e9") |>
    dplyr::group_by(Annee, Type_Prix, Operation, Code_Produit) |>
    dplyr::summarise(Valeur = sum(Valeur, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Code_Branche = "Non_Ventil\u00e9")

  # 3. Fusion avec les donn\u00e9es originales
  df_final <- dplyr::bind_rows(df_tre, totaux_branches)

  return(df_final)
}

# ==============================================================================
# 3. LECTURE ERETES (INTEGRATION COMPLETE)
# ==============================================================================

parse_eretes_sheet <- function(path, sheet_name, annee, type_prix) {

  message("   ... Lecture de ", sheet_name)

  # LECTURE ROBUSTE (Range A1)
  raw <- readxl::read_excel(path, sheet = sheet_name, col_names = FALSE,
                            range = readxl::cell_limits(c(1, 1), c(NA, NA)),
                            .name_repair = "minimal")

  # Coordonn\u00e9es
  cols_branches <- 11:131; cols_taxes <- 2:10; col_import <- 135; cols_finaux <- 136:144
  rows_res <- 8:212; rows_emp <- 218:422; rows_rev <- 424:432

  # Extraction
  df_p1 <- extract_matrix_strict(raw, rows_res, cols_branches, 6, "branche", "P1")
  df_p7 <- extract_matrix_strict(raw, rows_res, col_import, 6, "operation") |> dplyr::mutate(Operation="Importations")
  df_tax <- extract_matrix_strict(raw, rows_res, cols_taxes, 6, "operation")

  df_p2 <- extract_matrix_strict(raw, rows_emp, cols_branches, 216, "branche", "P2")
  df_p6 <- extract_matrix_strict(raw, rows_emp, col_import, 216, "operation") |> dplyr::mutate(Operation="Exportations")
  df_fin <- extract_matrix_strict(raw, rows_emp, cols_finaux, 217, "operation")

  df_rev <- extract_revenus_strict(raw, rows_rev, cols_branches, 216)

  # Fusion initiale
  df_brut <- dplyr::bind_rows(df_p1, df_p7, df_tax, df_p2, df_p6, df_fin, df_rev) |>
    dplyr::mutate(Annee = as.integer(annee), Type_Prix = type_prix) |>
    dplyr::select(Annee, Type_Prix, Code_Branche, Code_Produit, Operation, Valeur)

  # --- ETAPE CRUCIALE : AJOUT DES TOTAUX ---
  df_enrichi <- enrichir_avec_totaux(df_brut)

  return(df_enrichi)
}

# ==============================================================================
# 4. FONCTION EXPORT\u00c9E
# ==============================================================================
#' Mise à jour des TRE
#' @param  path_excel chemin du fichier
#' @param annee est annee du dernier TRE disponible
#' @param output_folder est le dossier de sortie
#' @export
admin_update_tre <- function(path_excel, annee, output_folder) {

  sheet_courant <- paste0("R_", annee)
  sheet_constant <- paste0("S_", annee)

  message("\u1f680 Importation TRE ", annee, " avec calcul des totaux...")

  tre_val <- tryCatch({
    parse_eretes_sheet(path_excel, sheet_courant, annee, "Courant")
  }, error = function(e) { message("\u274c ERREUR R_ : ", e$message); return(NULL) })

  tre_vol <- tryCatch({
    parse_eretes_sheet(path_excel, sheet_constant, annee, "Constant")
  }, error = function(e) { message("\u2139\ufe0f Pas de S_."); return(NULL) })

  final_db <- dplyr::bind_rows(tre_val, tre_vol)

  if(is.null(final_db) || nrow(final_db) == 0) stop("\u26d4 Arr\u00eat : Aucune donn\u00e9e.")

  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  path_file <- file.path(output_folder, "Base_TRE_Historique.rds")

  if (file.exists(path_file)) {
    old <- readRDS(path_file)
    final_db <- dplyr::bind_rows(old |> dplyr::filter(Annee != annee), final_db)
  }

  saveRDS(final_db, path_file)
  message("\u2705 Termin\u00e9. Base sauvegard\u00e9e (", nrow(final_db), " lignes).")
}
