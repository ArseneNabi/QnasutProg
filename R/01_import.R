#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import tibble
NULL


# ==============================================================================
# 0. HELPER : IMPORT SECURISE
# ==============================================================================

#' Import securise avec message d'erreur explicite
#'
#' Encapsule n'importe quelle expression d'importation dans un tryCatch et
#' retourne un message d'erreur clair si l'import echoue.
#'
#' @param expr Expression d'importation a evaluer.
#' @param label Chaine de caracteres identifiant la source (pour le message d'erreur).
#' @return Le resultat de \code{expr} si l'import reussit.
#' @export
safe_import <- function(expr, label) {
  tryCatch(expr, error = function(e) {
    stop(sprintf("[%s] Erreur import : %s", label, e$message))
  })
}

# ==============================================================================
# 1. IMPORTATION DES INDICATEURS ET COMPTES (CNA)
# ==============================================================================


#' Importation robuste des matrices CNT (Format 3 lignes d'en-tête)
#' @param path chemin
#' @param sheet la feuille concernée
#' @export
import_matrix_cnt <- function(path, sheet) {

  if (!file.exists(path)) stop("Fichier introuvable : ", path)

  # 1. Lecture des m\u00e9tadonn\u00e9es (Lignes 1 \u00e0 3)
  meta_raw <- readxl::read_excel(path, sheet = sheet, n_max = 3, col_names = FALSE, .name_repair = "minimal")

  # Extraction des vecteurs de m\u00e9tadonn\u00e9es
  branches <- as.character(meta_raw[1, -c(1:3)]) # Ligne 1
  types    <- as.character(meta_raw[2, -c(1:3)]) # Ligne 2
  codes    <- as.character(meta_raw[3, -c(1:3)]) # Ligne 3

  # Dictionnaire de correspondance
  dict_cols <- tibble::tibble(
    col_index = seq_along(codes),
    full_code = trimws(codes),
    branche_macro = trimws(branches),
    type_ind = trimws(types)
  ) |> dplyr::filter(!is.na(full_code), full_code != "")

  # 2. Lecture des donn\u00e9es (Skip 3 lignes)
  data_raw <- readxl::read_excel(path, sheet = sheet, skip = 3, col_names = FALSE, .name_repair = "minimal")

  # S\u00e9curit\u00e9 dimension
  max_idx <- 3 + max(dict_cols$col_index)
  if (ncol(data_raw) < max_idx) {
    missing_cols <- max_idx - ncol(data_raw)
    mat_na <- matrix(NA, nrow = nrow(data_raw), ncol = missing_cols)
    data_raw <- cbind(data_raw, as.data.frame(mat_na))
  }

  # S\u00e9lection et renommage
  cols_to_keep <- c(1, 2, 3, 3 + dict_cols$col_index)
  data_subset <- data_raw[, cols_to_keep]
  names(data_subset) <- c("annee", "trimestre", "periode", dict_cols$full_code)

  # 3. Pivot et Nettoyage
  data_subset |>
    dplyr::filter(!is.na(annee)) |>
    dplyr::mutate(
      annee = as.integer(annee),
      trimestre = suppressWarnings(as.integer(gsub("T", "", trimestre)))
    ) |>
    tidyr::pivot_longer(
      cols = -c(annee, trimestre, periode),
      names_to = "full_code",
      values_to = "valeur"
    ) |>
    dplyr::mutate(valeur = as.numeric(valeur)) |>
    dplyr::left_join(dict_cols, by = "full_code")
}


#' Importation structurée du Commerce Extérieur (IndCE)
#'
#' Transforme l'onglet IndCE (3 niveaux d'en-tête) en une liste de dataframes.
#' Structure de sortie : Liste$Flux$Type_Prix -> Dataframe (Format Large)
#'
#' @param path Chemin du fichier Excel
#' @param sheet Nom de la feuille (Défaut "IndCE")
#' @export
import_ind_ce_structured <- function(path, sheet = "IndCE") {

  if (!file.exists(path)) stop("Fichier introuvable : ", path)

  message("... Lecture structur\u00e9e de ", sheet)

  # 1. Lecture des En-t\u00eates (Lignes 1 \u00e0 3)
  headers_raw <- readxl::read_excel(path, sheet = sheet, n_max = 3, col_names = FALSE, .name_repair = "minimal")

  # Transposition
  headers_t <- t(headers_raw) |> as.data.frame()
  colnames(headers_t) <- c("Flux", "Type_Prix", "Produit")

  # 2. Nettoyage et Construction des noms uniques
  headers_clean <- headers_t |>
    dplyr::mutate(col_id = dplyr::row_number()) |>
    tidyr::fill(Flux, .direction = "down") |>
    dplyr::mutate(
      Flux = ifelse(is.na(Flux), "Inconnu", Flux),
      Type_Prix = ifelse(is.na(Type_Prix), "Standard", Type_Prix),
      Produit = ifelse(is.na(Produit), "Total", Produit),

      unique_name = dplyr::case_when(
        col_id == 1 ~ "annee",
        col_id == 2 ~ "trimestre",
        col_id == 3 ~ "periode",
        TRUE ~ paste(Flux, Type_Prix, Produit, sep = "||")
      )
    )

  # 3. Lecture des Donn\u00e9es
  data_raw <- readxl::read_excel(path, sheet = sheet, skip = 3, col_names = FALSE, .name_repair = "minimal")

  if(ncol(data_raw) < nrow(headers_clean)) {
    missing <- nrow(headers_clean) - ncol(data_raw)
    data_raw <- cbind(data_raw, matrix(NA, nrow=nrow(data_raw), ncol=missing))
  }

  data_subset <- data_raw[, 1:nrow(headers_clean)]

  # CORRECTIF : make.unique pour \u00e9viter l'erreur "duplicate names"
  names(data_subset) <- make.unique(headers_clean$unique_name)

  # 4. Transformation
  df_long <- data_subset |>
    dplyr::filter(!is.na(annee)) |>
    dplyr::mutate(
      annee = as.integer(annee),
      trimestre = suppressWarnings(as.integer(gsub("T", "", trimestre)))
    ) |>
    tidyr::pivot_longer(
      cols = -c(annee, trimestre, periode),
      names_to = "key",
      values_to = "valeur"
    ) |>
    tidyr::separate(key, c("Flux", "Type_Prix", "Produit"), sep = "\\|\\|", extra = "drop") |>
    dplyr::mutate(valeur = as.numeric(valeur))

  # 5. Cr\u00e9ation de la Liste Hi\u00e9rarchique
  liste_finale <- split(df_long, df_long$Flux) |>
    lapply(function(df_flux) {
      split(df_flux, df_flux$Type_Prix) |>
        lapply(function(df_type) {
          df_type |>
            dplyr::select(annee, trimestre, periode, Produit, valeur) |>
            tidyr::pivot_wider(
              names_from = Produit,
              values_from = valeur,
              values_fill = 0
            ) |>
            dplyr::arrange(annee, trimestre)
        })
    })

  return(liste_finale)
}

# ==============================================================================
# NETTOYAGE DES PRIX
# ==============================================================================

#' Préparation et nettoyage des données de prix
#'
#' @param df Un dataframe issu de import_matrix_prix
#' @return Un dataframe nettoyé avec colonnes standardisées (Code_Produit, IP)
#' @export
prepare_price_data <- function(df) {

  col_names <- names(df)

  if ("Code_Produit" %in% col_names) {
    df_clean <- df |>
      dplyr::mutate(
        Code_Produit = trimws(as.character(Code_Produit)),
        IP = suppressWarnings(as.numeric(as.character(IP)))
      )
  } else if ("branche_prix" %in% col_names) {
    df_clean <- df |>
      dplyr::rename(Code_Produit = branche_prix) |>
      dplyr::mutate(
        Code_Produit = trimws(as.character(Code_Produit)),
        IP = suppressWarnings(as.numeric(if ("prix" %in% names(df)) prix else valeur))
      )
  } else {
    stop("Erreur : Colonne de prix introuvable.")
  }

  return(df_clean |> dplyr::filter(!is.na(IP)))
}


# ==============================================================================
# 2. IMPORTATION DES PRIX
# ==============================================================================

#' Import spécifique pour les Prix
#' @param path est le chemin du fichier
#' @param sheet est la feuille concernée
#' @export
import_matrix_prix <- function(path, sheet) {

  if (!file.exists(path)) stop("Fichier introuvable : ", path)

  meta_raw <- readxl::read_excel(path, sheet = sheet, range = readxl::cell_rows(3), col_names = FALSE, .name_repair = "minimal")
  codes_raw <- as.character(meta_raw[1, -c(1:3)])

  dict_cols <- tibble::tibble(
    rel_index = seq_along(codes_raw),
    Code_Produit = trimws(codes_raw)
  ) |> dplyr::filter(!is.na(Code_Produit), Code_Produit != "")

  data_raw <- readxl::read_excel(path, sheet = sheet, skip = 3, col_names = FALSE, .name_repair = "minimal")

  required_cols_idx <- 3 + dict_cols$rel_index
  max_col_needed <- max(required_cols_idx)
  if (ncol(data_raw) < max_col_needed) {
    missing <- max_col_needed - ncol(data_raw)
    placerholder <- as.data.frame(matrix(NA, nrow=nrow(data_raw), ncol=missing))
    data_raw <- cbind(data_raw, placerholder)
  }

  data_subset <- data_raw[, c(1, 2, 3, required_cols_idx)]
  final_names <- c("annee", "trimestre", "periode", dict_cols$Code_Produit)
  names(data_subset) <- make.unique(final_names)

  data_subset |>
    dplyr::filter(!is.na(annee)) |>
    tidyr::pivot_longer(
      cols = -c(annee, trimestre, periode),
      names_to = "Code_Produit",
      values_to = "IP"
    ) |>
    dplyr::mutate(
      annee = as.integer(annee),
      trimestre = suppressWarnings(as.integer(gsub("T", "", trimestre))),
      IP = as.numeric(IP)
    )
}


# ==============================================================================
# 3. IMPORTATION DES RATIOS TRE
# ==============================================================================

#' Importation des matrices de ratios techniques (TRE)
#' @param path est le chemin du fichier
#' @param sheet est la feuille concernée
#' @export
import_matrix_tre <- function(path, sheet) {

  if (!file.exists(path)) stop("Fichier TRE introuvable : ", path)

  meta_raw <- readxl::read_excel(path, sheet = sheet, n_max = 2, col_names = FALSE, .name_repair = "minimal")
  branches <- as.character(meta_raw[1, -c(1:3)])
  produits <- as.character(meta_raw[2, -c(1:3)])

  dict_cols <- tibble::tibble(
    col_index = seq_along(branches),
    branche = trimws(branches),
    produit = trimws(produits),
    key_temp = paste(branche, produit, sep = "||")
  ) |> dplyr::filter(!is.na(branche), branche != "")

  data_raw <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = FALSE, .name_repair = "minimal")

  max_idx <- 3 + max(dict_cols$col_index)
  if (ncol(data_raw) < max_idx) {
    missing <- max_idx - ncol(data_raw)
    data_raw <- cbind(data_raw, as.data.frame(matrix(NA, nrow=nrow(data_raw), ncol=missing)))
  }

  cols_to_keep <- c(1, 2, 3, 3 + dict_cols$col_index)
  data_subset <- data_raw[, cols_to_keep]
  names(data_subset) <- c("annee", "trimestre", "periode", dict_cols$key_temp)

  data_subset |>
    dplyr::filter(!is.na(annee)) |>
    dplyr::mutate(
      annee = as.integer(annee),
      trimestre = suppressWarnings(as.integer(gsub("T", "", trimestre)))
    ) |>
    tidyr::pivot_longer(
      cols = -c(annee, trimestre, periode),
      names_to = "key_temp",
      values_to = "valeur"
    ) |>
    tidyr::separate(key_temp, c("branche", "produit"), sep = "\\|\\|") |>
    dplyr::mutate(valeur = as.numeric(valeur))
}


#' Importation des Prix détaillés (Niveau 3)
#' @param path est le chemin du fichier
#' @export
import_prix_niv3 <- function(path) {

  meta <- readxl::read_excel(path, sheet = "Prix_Niv3", range = readxl::cell_rows(3), col_names = FALSE)
  codes_produits <- as.character(meta[1, -c(1:3)])
  codes_produits <- codes_produits[!is.na(codes_produits)]

  df_raw <- readxl::read_excel(path, sheet = "Prix_Niv3", skip = 3, col_names = FALSE)
  df_select <- df_raw[, 1:(3 + length(codes_produits))]
  names(df_select) <- c("annee", "trimestre", "periode", codes_produits)

  df_long <- df_select |>
    dplyr::filter(!is.na(annee)) |>
    dplyr::mutate(
      annee = as.integer(annee),
      trimestre = as.integer(gsub("T", "", trimestre))
    ) |>
    tidyr::pivot_longer(
      cols = -c(annee, trimestre, periode),
      names_to = "Code_Produit",
      values_to = "IP"
    ) |>
    dplyr::mutate(
      Code_Produit = trimws(Code_Produit),
      IP = as.numeric(IP)
    )

  return(df_long)
}


#' Importation structurée de l'Équilibre Ressources-Emplois (CnaEre)
#'
#' @param path Chemin du fichier Excel
#' @param sheet Nom de la feuille (Défaut "CnaEre")
#' @export
import_cna_ere_structured <- function(path, sheet = "CnaEre") {

  if (!file.exists(path)) stop("Fichier introuvable : ", path)

  message("... Lecture structur\u00e9e de ", sheet)

  # 1. Lecture En-t\u00eates
  headers_raw <- readxl::read_excel(path, sheet = sheet, n_max = 3, col_names = FALSE, .name_repair = "minimal")

  # Transposition
  headers_t <- t(headers_raw) |> as.data.frame()
  colnames(headers_t) <- c("Type_Prix", "Agregat", "Produit")

  # 2. Nettoyage
  headers_clean <- headers_t |>
    dplyr::mutate(col_id = dplyr::row_number()) |>
    tidyr::fill(Type_Prix, .direction = "down") |>
    tidyr::fill(Agregat, .direction = "down") |>
    dplyr::mutate(
      is_time = col_id == 1 | grepl("Ann", Type_Prix, ignore.case = TRUE),

      Agregat   = trimws(ifelse(is.na(Agregat), "Inconnu", Agregat)),
      Type_Prix = trimws(ifelse(is.na(Type_Prix), "Standard", Type_Prix)),
      Produit   = trimws(ifelse(is.na(Produit), "Total", Produit)),

      unique_name = dplyr::case_when(
        is_time ~ "annee",
        TRUE ~ paste(Agregat, Type_Prix, Produit, sep = "||")
      )
    )

  # 3. Donn\u00e9es
  data_raw <- readxl::read_excel(path, sheet = sheet, skip = 3, col_names = FALSE, .name_repair = "minimal")

  if(ncol(data_raw) < nrow(headers_clean)) {
    missing <- nrow(headers_clean) - ncol(data_raw)
    data_raw <- cbind(data_raw, matrix(NA, nrow=nrow(data_raw), ncol=missing))
  }

  data_subset <- data_raw[, 1:nrow(headers_clean)]

  # CORRECTIF : make.unique pour g\u00e9rer les colonnes dupliqu\u00e9es dans Excel
  names(data_subset) <- make.unique(headers_clean$unique_name)

  # 4. Transformation
  df_long <- data_subset |>
    dplyr::filter(!is.na(annee)) |>
    dplyr::mutate(annee = as.integer(annee)) |>
    tidyr::pivot_longer(
      cols = -annee,
      names_to = "key",
      values_to = "valeur"
    ) |>
    # extra="drop" permet d'ignorer le suffixe ".1" ajout\u00e9 par make.unique
    tidyr::separate(key, c("Agregat", "Type_Prix", "Produit"), sep = "\\|\\|", extra = "drop") |>
    dplyr::mutate(valeur = as.numeric(valeur))

  # 5. Cr\u00e9ation Liste
  liste_finale <- split(df_long, df_long$Agregat) |>
    lapply(function(df_agreg) {
      split(df_agreg, df_agreg$Type_Prix) |>
        lapply(function(df_type) {
          df_type |>
            dplyr::select(annee, Produit, valeur) |>
            tidyr::pivot_wider(
              names_from = Produit,
              values_from = valeur,
              values_fill = 0
            ) |>
            dplyr::arrange(annee)
        })
    })

  return(liste_finale)
}


# ==============================================================================
# OPTIQUE DEPENSES : Pivotage ERE (format large -> long)
# ==============================================================================

#' Pivoter un sous-tibble ERE du format large vers le format long
#'
#' Transforme un tibble en format large (colonnes = codes produits) issu de
#' \code{ind_ce_struct} (trimestriel) ou \code{cna_ere_struct} (annuel) en
#' format long avec les colonnes \code{Code_Produit} et \code{valeur}.
#'
#' @param df Tibble en format large. Doit contenir \code{annee} et
#'   optionnellement \code{trimestre} et \code{periode}.
#' @param type_prix Chaine de caracteres : \code{"Crt"}, \code{"Ch"} ou
#'   \code{"Vol"}.
#' @param composante Nom de la composante ERE (ex : \code{"IMPORTATIONS"}).
#'
#' @return Tibble long avec colonnes \code{annee}, \code{trimestre} (si
#'   present), \code{Code_Produit}, \code{valeur}, \code{type_prix},
#'   \code{composante}.
#' @export
pivoter_ere_long <- function(df, type_prix, composante) {
  cols_id <- intersect(c("annee", "trimestre", "periode"), names(df))
  df |>
    tidyr::pivot_longer(
      cols      = -dplyr::all_of(cols_id),
      names_to  = "Code_Produit",
      values_to = "valeur"
    ) |>
    dplyr::mutate(
      type_prix  = type_prix,
      composante = composante
    )
}
