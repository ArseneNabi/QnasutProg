# ============================================================
# 17_modele_equilibrage_ere.R
# Paramétrage persistant du modèle de bouclage ERE
# ============================================================

# Helpers internes ---------------------------------------------------------

.norm_text_ere <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[[:space:]]+", " ", x)
  x
}

.parse_ok_no_ere <- function(x) {
  nx <- .norm_text_ere(x)

  dplyr::case_when(
    nx %in% c("ok", "oui", "true", "vrai", "1", "x") ~ TRUE,
    nx %in% c("no", "non", "false", "faux", "0", "", "na") ~ FALSE,
    TRUE ~ NA
  )
}

.default_output_dir_ere <- function(output_dir = NULL) {
  if (!is.null(output_dir) && nzchar(output_dir)) {
    return(output_dir)
  }

  cfg <- tryCatch(load_config(), error = function(e) NULL)

  if (!is.null(cfg) && !is.null(cfg$root_dir) && nzchar(cfg$root_dir)) {
    return(cfg$root_dir)
  }

  stop(
    "Impossible de déterminer `output_dir` automatiquement. ",
    "Fournis `output_dir` explicitement ou configure `root_dir` via `load_config()`.",
    call. = FALSE
  )
}

#' Importer le modèle de bouclage ERE depuis Excel
#'
#' @description
#' Lit la feuille `ModelEquil` du classeur `Methode_ERE.xlsx` et construit
#' un objet de paramétrage du bouclage ERE.
#'
#' La structure attendue est celle visible dans votre classeur :
#' \itemize{
#'   \item une table \strong{produit -> modèle} à gauche, avec au minimum
#'   les colonnes `Code`, `Désign.` (optionnelle) et `Modèle` ;
#'   \item une table \strong{modèle -> composantes autorisées} à droite,
#'   sur la même ligne d'en-tête, avec une colonne `Modèle` puis les
#'   composantes (`CI Prix d'acquisition`, `FBCF Prix d'acquisition`, etc.)
#'   remplies par `Ok` / `No`.
#' }
#'
#' La fonction est robuste aux accents, espaces multiples et variantes
#' mineures de casse.
#'
#' @param path_excel Chemin du fichier Excel.
#' @param sheet Nom de la feuille Excel à lire.
#' @param composantes_attendues Vecteur optionnel des composantes à rechercher.
#'   Si `NULL`, le vecteur standard ERE est utilisé.
#' @param strict Si `TRUE`, les incohérences d'import bloquent avec `stop()`.
#'   Si `FALSE`, certaines incohérences produisent des `warning()`.
#' @param header_row Numéro de la ligne d'en-tête si vous voulez forcer la
#'   détection. Si `NULL`, la fonction tente de la détecter automatiquement.
#' @param product_code_col Colonne du code produit si vous voulez la forcer
#'   (index numérique, en comptant depuis 1). Sinon détection automatique.
#' @param product_designation_col Colonne de la désignation produit si vous
#'   voulez la forcer. Sinon détection automatique.
#' @param product_modele_col Colonne du modèle côté table produits si vous
#'   voulez la forcer. Sinon détection automatique.
#' @param dictionary_modele_col Colonne du modèle côté dictionnaire des modèles
#'   si vous voulez la forcer. Sinon détection automatique.
#' @param composante_cols Vecteur nommé optionnel donnant explicitement les
#'   colonnes des composantes du dictionnaire. Les noms doivent être les noms
#'   standards des composantes.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{produits_modeles}{Table des produits et du modèle retenu.}
#'   \item{modeles_composantes}{Table longue des modèles et des composantes autorisées.}
#'   \item{produits_composantes_autorisees}{Table longue finale produit x composante.}
#'   \item{composantes_standardisees}{Vecteur standard des composantes reconnues.}
#'   \item{meta_import}{Métadonnées de lecture (feuille, ligne d'en-tête, colonnes détectées).}
#' }
#'
#' @export
importer_modele_equilibrage_ere_excel <- function(
    path_excel,
    sheet = "ModelEquil",
    composantes_attendues = NULL,
    strict = TRUE,
    header_row = NULL,
    product_code_col = NULL,
    product_designation_col = NULL,
    product_modele_col = NULL,
    dictionary_modele_col = NULL,
    composante_cols = NULL
) {
  if (!file.exists(path_excel)) {
    stop("Fichier Excel introuvable : ", path_excel, call. = FALSE)
  }

  feuilles <- readxl::excel_sheets(path_excel)
  if (!(sheet %in% feuilles)) {
    stop(
      "Feuille '", sheet, "' absente de ", basename(path_excel),
      ". Feuilles disponibles : ", paste(feuilles, collapse = ", "),
      call. = FALSE
    )
  }

  composantes_std <- if (is.null(composantes_attendues)) {
    c(
      "CI Prix d'acquisition",
      "CF Marchande Menage Prix d'acquisition",
      "CF Non Marchande Menage Prix d'acquisition",
      "CF Non Marchande APU Prix d'acquisition",
      "CF Non Marchande ISBL Prix d'acquisition",
      "FBCF Prix d'acquisition",
      "VS Prix d'acquisition",
      "Exportation Prix d'acquisition"
    )
  } else {
    composantes_attendues
  }

  raw <- readxl::read_excel(path_excel, sheet = sheet, col_names = FALSE)

  if (nrow(raw) == 0 || ncol(raw) == 0) {
    stop("La feuille '", sheet, "' est vide.", call. = FALSE)
  }

  raw_chr <- raw |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), trimws))

  mat <- as.matrix(raw_chr)
  mat_norm <- apply(mat, c(1, 2), .norm_text_ere)

  code_regex <- "^[A-Z]{2}[0-9]{3}$"
  norm_comp <- .norm_text_ere(composantes_std)

  # ------------------------------------------------------------------
  # 1. Détection de la ligne d'en-tête commune
  # ------------------------------------------------------------------
  if (is.null(header_row)) {
    header_score <- apply(mat_norm, 1, function(r) {
      score <- 0L

      if (any(grepl("code", r))) {
        score <- score + 1L
      }
      if (any(grepl("modele|model", r))) {
        score <- score + 1L
      }
      if (any(grepl("designation|design|libelle|produit", r))) {
        score <- score + 1L
      }

      comp_hits <- sum(vapply(norm_comp, function(cn) {
        any(grepl(cn, r, fixed = TRUE))
      }, logical(1)))

      score <- score + comp_hits
      score
    })

    header_row <- which.max(header_score)

    if (length(header_row) == 0 || header_score[header_row] < 5) {
      stop(
        "Impossible d'identifier proprement la ligne d'en-tête de la feuille '",
        sheet, "'. Vérifie la structure de `ModelEquil`.",
        call. = FALSE
      )
    }
  }

  header_norm <- mat_norm[header_row, ]
  header_raw <- mat[header_row, ]

  # ------------------------------------------------------------------
  # 2. Détection des colonnes du dictionnaire des modèles
  # ------------------------------------------------------------------
  if (is.null(composante_cols)) {
    comp_col_idx <- vapply(norm_comp, function(cn) {
      idx <- which(grepl(cn, header_norm, fixed = TRUE))[1]
      if (length(idx) == 0 || is.na(idx)) NA_integer_ else idx
    }, integer(1))
    names(comp_col_idx) <- composantes_std
  } else {
    comp_col_idx <- as.integer(composante_cols[composantes_std])
    names(comp_col_idx) <- composantes_std
  }

  missing_comp <- composantes_std[is.na(comp_col_idx)]
  if (length(missing_comp) > 0) {
    stop(
      "Colonnes composantes introuvables dans la feuille '", sheet, "' : ",
      paste(missing_comp, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(dictionary_modele_col)) {
    first_comp_col <- min(comp_col_idx)
    candidate_modele_cols <- which(grepl("modele|model", header_norm))
    candidate_modele_cols <- candidate_modele_cols[candidate_modele_cols < first_comp_col]

    if (length(candidate_modele_cols) == 0) {
      stop(
        "Impossible d'identifier la colonne `Modele` du dictionnaire des modèles.",
        call. = FALSE
      )
    }

    dictionary_modele_col <- max(candidate_modele_cols)
  }

  # ------------------------------------------------------------------
  # 3. Détection des colonnes de la table produits
  # ------------------------------------------------------------------
  if (is.null(product_code_col)) {
    code_hits <- vapply(seq_len(ncol(mat)), function(j) {
      vals <- toupper(trimws(mat[(header_row + 1):nrow(mat), j]))
      sum(grepl(code_regex, vals), na.rm = TRUE)
    }, integer(1))

    if (max(code_hits, na.rm = TRUE) == 0) {
      stop(
        "Aucune colonne ne correspond au format des codes produits (AA000, AB000, ...). ",
        "Vérifie la feuille Excel.",
        call. = FALSE
      )
    }

    product_code_col <- which.max(code_hits)
  }

  if (is.null(product_modele_col)) {
    modele_cols <- which(grepl("modele|model", header_norm))
    modele_cols <- setdiff(modele_cols, dictionary_modele_col)

    if (length(modele_cols) == 0) {
      stop(
        "Impossible d'identifier la colonne `Modele` de la table produits.",
        call. = FALSE
      )
    }

    product_modele_col <- modele_cols[1]
  }

  if (is.null(product_designation_col)) {
    desig_cols <- which(grepl("designation|design|libelle|produit", header_norm))
    desig_cols <- setdiff(
      desig_cols,
      c(product_code_col, product_modele_col, dictionary_modele_col, comp_col_idx)
    )

    product_designation_col <- if (length(desig_cols) > 0) desig_cols[1] else NA_integer_
  }

  # ------------------------------------------------------------------
  # 4. Lecture table produits -> modèle
  # ------------------------------------------------------------------
  data_rows <- seq.int(header_row + 1L, nrow(mat))
  code_vals <- toupper(trimws(mat[data_rows, product_code_col]))
  valid_prod_rows <- data_rows[grepl(code_regex, code_vals)]

  if (length(valid_prod_rows) == 0) {
    stop(
      "Aucun produit ERE valide détecté (format attendu : AA000, AB000, ...).",
      call. = FALSE
    )
  }

  produits_modeles <- tibble::tibble(
    Code_Produit = toupper(trimws(mat[valid_prod_rows, product_code_col])),
    Designation = if (!is.na(product_designation_col)) trimws(mat[valid_prod_rows, product_designation_col]) else NA_character_,
    Modele = trimws(mat[valid_prod_rows, product_modele_col])
  ) |>
    dplyr::filter(!is.na(.data$Code_Produit), .data$Code_Produit != "")

  produits_sans_modele <- produits_modeles |>
    dplyr::filter(is.na(.data$Modele) | .data$Modele == "") |>
    dplyr::pull(.data$Code_Produit)

  if (length(produits_sans_modele) > 0) {
    stop(
      "Produit(s) sans modèle renseigné : ",
      paste(produits_sans_modele, collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 5. Lecture dictionnaire des modèles -> composantes
  # ------------------------------------------------------------------
  dict_modele_vals <- trimws(mat[data_rows, dictionary_modele_col])
  valid_dict_rows <- data_rows[!is.na(dict_modele_vals) & dict_modele_vals != ""]

  if (length(valid_dict_rows) == 0) {
    stop(
      "Aucune ligne de dictionnaire des modèles détectée sous la colonne dictionnaire `Modele`.",
      call. = FALSE
    )
  }

  dict_raw <- tibble::tibble(
    Modele = trimws(mat[valid_dict_rows, dictionary_modele_col])
  )

  for (i in seq_along(composantes_std)) {
    dict_raw[[composantes_std[i]]] <- mat[valid_dict_rows, comp_col_idx[[i]]]
  }

  modeles_composantes <- dict_raw |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(composantes_std),
      names_to = "Composante",
      values_to = "valeur_source"
    ) |>
    dplyr::mutate(
      Modele = trimws(.data$Modele),
      autorise = .parse_ok_no_ere(.data$valeur_source)
    )

  valeurs_invalides <- modeles_composantes |>
    dplyr::filter(is.na(.data$autorise),
                  !is.na(.data$valeur_source),
                  trimws(.data$valeur_source) != "") |>
    dplyr::distinct(.data$valeur_source) |>
    dplyr::pull(.data$valeur_source)

  if (length(valeurs_invalides) > 0) {
    msg <- paste0(
      "Valeurs Ok/No non reconnues dans le dictionnaire des modèles : ",
      paste(valeurs_invalides, collapse = ", "),
      ". Valeurs attendues : Ok/No (ou variantes Oui/Non, True/False)."
    )
    if (isTRUE(strict)) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  modeles_composantes <- modeles_composantes |>
    dplyr::mutate(autorise = tidyr::replace_na(.data$autorise, FALSE))

  modeles_disponibles <- sort(unique(modeles_composantes$Modele))
  modeles_disponibles_norm <- .norm_text_ere(modeles_disponibles)

  # ------------------------------------------------------------------
  # 6. Validation croisée produits <-> dictionnaire
  # ------------------------------------------------------------------
  produits_modeles <- produits_modeles |>
    dplyr::mutate(.modele_norm = .norm_text_ere(.data$Modele)) |>
    dplyr::left_join(
      tibble::tibble(
        .modele_norm = modeles_disponibles_norm,
        Modele_std = modeles_disponibles
      ),
      by = ".modele_norm"
    ) |>
    dplyr::mutate(Modele = dplyr::coalesce(.data$Modele_std, .data$Modele)) |>
    dplyr::select(-.data$.modele_norm, -.data$Modele_std)

  modeles_absents <- setdiff(
    .norm_text_ere(unique(produits_modeles$Modele)),
    modeles_disponibles_norm
  )

  if (length(modeles_absents) > 0) {
    stop(
      "Modèle(s) référencé(s) côté produits mais absent(s) du dictionnaire : ",
      paste(modeles_absents, collapse = ", "),
      call. = FALSE
    )
  }

  produits_composantes_autorisees <- produits_modeles |>
    dplyr::left_join(modeles_composantes, by = "Modele") |>
    dplyr::select(
      .data$Code_Produit,
      .data$Designation,
      .data$Modele,
      .data$Composante,
      .data$autorise
    )

  list(
    produits_modeles = produits_modeles,
    modeles_composantes = modeles_composantes,
    produits_composantes_autorisees = produits_composantes_autorisees,
    composantes_standardisees = composantes_std,
    meta_import = list(
      path_excel = path_excel,
      sheet = sheet,
      header_row = header_row,
      product_code_col = product_code_col,
      product_designation_col = product_designation_col,
      product_modele_col = product_modele_col,
      dictionary_modele_col = dictionary_modele_col,
      composante_cols = unname(comp_col_idx)
    )
  )
}

#' Sauvegarder le modèle de bouclage ERE au format RDS
#'
#' @description
#' Importe la feuille `ModelEquil` depuis Excel puis sauvegarde l'objet de
#' paramétrage dans un fichier `.rds`.
#'
#' @param path_excel Chemin du fichier Excel.
#' @param output_dir Répertoire de sortie. Si `NULL`, la fonction tente
#'   d'utiliser `load_config()$root_dir`.
#' @param output_file Nom du fichier `.rds`.
#' @param sheet Nom de la feuille Excel.
#' @param overwrite Si `TRUE`, écrase un fichier existant.
#' @param strict Niveau de sévérité de l'import.
#' @param ... Arguments supplémentaires passés à
#'   `importer_modele_equilibrage_ere_excel()`.
#'
#' @return Invisiblement, le chemin complet du fichier `.rds` créé.
#'
#' @export
sauvegarder_modele_equilibrage_ere <- function(
    path_excel,
    output_dir = NULL,
    output_file = "Modele_Equilibrage_ERE.rds",
    sheet = "ModelEquil",
    overwrite = FALSE,
    strict = TRUE,
    ...
) {
  output_dir <- .default_output_dir_ere(output_dir)

  if (!dir.exists(output_dir)) {
    stop("Répertoire de sortie introuvable : ", output_dir, call. = FALSE)
  }

  path_rds <- file.path(output_dir, output_file)

  if (file.exists(path_rds) && !isTRUE(overwrite)) {
    stop(
      "Le fichier existe déjà : ", path_rds,
      ". Utilisez `overwrite = TRUE` pour le régénérer.",
      call. = FALSE
    )
  }

  modele <- importer_modele_equilibrage_ere_excel(
    path_excel = path_excel,
    sheet = sheet,
    strict = strict,
    ...
  )

  saveRDS(modele, path_rds)
  message("Modèle d'équilibrage ERE sauvegardé : ", path_rds)

  invisible(path_rds)
}

#' Charger le modèle de bouclage ERE depuis un fichier RDS
#'
#' @description
#' Charge un fichier `.rds` de paramétrage du bouclage ERE.
#'
#' @param path_rds Chemin du fichier `.rds`. Si `NULL`, la fonction tente
#'   d'utiliser `load_config()$root_dir/Modele_Equilibrage_ERE.rds`.
#' @param output_file Nom du fichier si `path_rds = NULL`.
#' @param validate Si `TRUE`, vérifie la structure minimale de l'objet.
#'
#' @return Une liste de paramétrage du modèle de bouclage ERE.
#'
#' @export
charger_modele_equilibrage_ere <- function(
    path_rds = NULL,
    output_file = "Modele_Equilibrage_ERE.rds",
    validate = TRUE
) {
  if (is.null(path_rds)) {
    output_dir <- .default_output_dir_ere(NULL)
    path_rds <- file.path(output_dir, output_file)
  }

  if (!file.exists(path_rds)) {
    stop("Fichier de paramétrage introuvable : ", path_rds, call. = FALSE)
  }

  obj <- readRDS(path_rds)

  if (isTRUE(validate)) {
    attendus <- c(
      "produits_modeles",
      "modeles_composantes",
      "produits_composantes_autorisees"
    )
    manquants <- setdiff(attendus, names(obj))

    if (length(manquants) > 0) {
      stop(
        "Structure invalide du fichier de paramétrage. Élément(s) manquant(s) : ",
        paste(manquants, collapse = ", "),
        call. = FALSE
      )
    }
  }

  obj
}

#' Créer ou mettre à jour le fichier de paramétrage du bouclage ERE
#'
#' @description
#' Wrapper pratique pour régénérer le fichier `.rds` de paramétrage du
#' bouclage ERE à partir du classeur Excel.
#'
#' @param path_excel Chemin du fichier Excel.
#' @param output_dir Répertoire de sortie. Si `NULL`, utilise `load_config()$root_dir`.
#' @param output_file Nom du fichier `.rds`.
#' @param sheet Nom de la feuille Excel.
#' @param strict Niveau de sévérité de l'import.
#' @param ... Arguments supplémentaires passés à l'import.
#'
#' @return Invisiblement, le chemin complet du fichier `.rds` mis à jour.
#'
#' @export
creer_ou_maj_modele_equilibrage_ere <- function(
    path_excel,
    output_dir = NULL,
    output_file = "Modele_Equilibrage_ERE.rds",
    sheet = "ModelEquil",
    strict = TRUE,
    ...
) {
  sauvegarder_modele_equilibrage_ere(
    path_excel = path_excel,
    output_dir = output_dir,
    output_file = output_file,
    sheet = sheet,
    overwrite = TRUE,
    strict = strict,
    ...
  )
}

#' Extraire les composantes autorisées pour un produit ERE
#'
#' @description
#' Retourne les composantes autorisées par le modèle de bouclage d'un produit.
#'
#' @param modele_obj Objet retourné par `importer_modele_equilibrage_ere_excel()`
#'   ou `charger_modele_equilibrage_ere()`.
#' @param code_produit Code produit ERE.
#' @param autorise_seulement Si `TRUE`, ne renvoie que les composantes autorisées.
#'
#' @return Un tibble.
#'
#' @export
composantes_autorisees <- function(
    modele_obj,
    code_produit,
    autorise_seulement = TRUE
) {
  if (!"produits_composantes_autorisees" %in% names(modele_obj)) {
    stop(
      "`modele_obj` ne contient pas `produits_composantes_autorisees`.",
      call. = FALSE
    )
  }

  out <- modele_obj$produits_composantes_autorisees |>
    dplyr::filter(.data$Code_Produit == code_produit)

  if (isTRUE(autorise_seulement)) {
    out <- out |>
      dplyr::filter(.data$autorise)
  }

  out
}
