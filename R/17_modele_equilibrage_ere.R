# ==============================================================================
# OPTIQUE DEPENSES : Parametrage du modele de bouclage ERE
# ============================================================================== 

#' Importer le paramétrage du modèle d'équilibrage ERE depuis Excel
#'
#' Lit la feuille `ModelEquil` du fichier `Methode_ERE.xlsx` et reconstruit
#' les trois objets de paramétrage :
#' - `produits_modeles` : table produit -> modèle retenu,
#' - `modeles_composantes` : dictionnaire modèle -> composantes autorisées,
#' - `produits_composantes_autorisees` : table dérivée produit -> composantes.
#'
#' La lecture est défensive : vérifie la présence de la feuille,
#' standardise les intitulés (accents/casse/espaces), valide les statuts `Ok/No`
#' et contrôle la cohérence produits <-> dictionnaire des modèles.
#'
#' @param path_excel Chemin vers le classeur `Methode_ERE.xlsx`.
#' @param sheet Nom de la feuille contenant le modèle (défaut : `"ModelEquil"`).
#' @param composantes_attendues Vecteur optionnel des composantes emplois
#'   attendues. Si `NULL`, le jeu standard ERE est utilisé.
#' @param strict Si `TRUE` (défaut), les incohérences bloquantes provoquent une
#'   erreur. Si `FALSE`, des warnings sont émis lorsque possible.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{produits_modeles}{Tibble avec `Code_Produit`, `Designation`, `Modele`.}
#'   \item{modeles_composantes}{Tibble long avec `Modele`, `Composante`,
#'     `autorise`, `valeur_source`.}
#'   \item{produits_composantes_autorisees}{Tibble long avec `Code_Produit`,
#'     `Designation`, `Modele`, `Composante`, `autorise`.}
#'   \item{composantes_standardisees}{Vecteur des composantes standardisées.}
#' }
#' @export
importer_modele_equilibrage_ere_excel <- function(
    path_excel,
    sheet = "ModelEquil",
    composantes_attendues = NULL,
    strict = TRUE
) {
  if (!file.exists(path_excel)) {
    stop("Fichier Excel introuvable: ", path_excel, call. = FALSE)
  }

  feuilles <- readxl::excel_sheets(path_excel)
  if (!(sheet %in% feuilles)) {
    stop(
      "Feuille '", sheet, "' absente de ", basename(path_excel),
      ". Feuilles disponibles: ", paste(feuilles, collapse = ", "),
      call. = FALSE
    )
  }

  composantes_std <- composantes_attendues %||% c(
    "CI Prix d'acquisition",
    "CF Marchande Menage Prix d'acquisition",
    "CF Non Marchande Menage Prix d'acquisition",
    "CF Non Marchande APU Prix d'acquisition",
    "CF Non Marchande ISBL Prix d'acquisition",
    "FBCF Prix d'acquisition",
    "VS Prix d'acquisition",
    "Exportation Prix d'acquisition"
  )

  raw <- readxl::read_excel(path_excel, sheet = sheet, col_names = FALSE)
  if (nrow(raw) == 0 || ncol(raw) == 0) {
    stop("La feuille '", sheet, "' est vide.", call. = FALSE)
  }

  raw_chr <- raw |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), trimws))

  norm_text <- function(x) {
    x <- ifelse(is.na(x), "", x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(trimws(x))
    x <- gsub("[[:space:]]+", " ", x)
    x
  }

  code_regex <- "^[A-Z]{2}[0-9]{3}$"
  norm_comp <- norm_text(composantes_std)

  mat <- as.matrix(raw_chr)
  mat_norm <- apply(mat, c(1, 2), norm_text)

  # --- Detection de la ligne d'en-tete du dictionnaire des modeles ---
  match_count <- apply(mat_norm, 1, function(r) {
    sum(vapply(norm_comp, function(cn) any(grepl(cn, r, fixed = TRUE)), logical(1)))
  })
  header_model_row <- which.max(match_count)

  if (length(header_model_row) == 0 || match_count[header_model_row] < 4) {
    stop(
      "Impossible d'identifier la zone dictionnaire des modèles dans la feuille '",
      sheet,
      "'. Vérifiez les intitulés des composantes.",
      call. = FALSE
    )
  }

  header_norm <- mat_norm[header_model_row, ]

  comp_col_idx <- vapply(norm_comp, function(cn) {
    idx <- which(grepl(cn, header_norm, fixed = TRUE))[1]
    if (is.na(idx)) NA_integer_ else idx
  }, integer(1))

  missing_comp <- composantes_std[is.na(comp_col_idx)]
  if (length(missing_comp) > 0) {
    stop(
      "Colonnes composantes introuvables dans la zone dictionnaire: ",
      paste(missing_comp, collapse = ", "),
      call. = FALSE
    )
  }

  first_comp_col <- min(comp_col_idx)
  model_col_idx <- max(1L, first_comp_col - 1L)

  # --- Extraction du dictionnaire modeles -> composantes ---
  dict_rows <- seq.int(header_model_row + 1L, nrow(mat))
  if (length(dict_rows) == 0) {
    stop("Aucune ligne de dictionnaire des modèles détectée.", call. = FALSE)
  }

  dict_raw <- tibble::tibble(
    Modele = mat[dict_rows, model_col_idx]
  )

  for (i in seq_along(composantes_std)) {
    dict_raw[[composantes_std[i]]] <- mat[dict_rows, comp_col_idx[[i]]]
  }

  dict_raw <- dict_raw |>
    dplyr::mutate(Modele = trimws(Modele)) |>
    dplyr::filter(!is.na(Modele), Modele != "")

  if (nrow(dict_raw) == 0) {
    stop("Le dictionnaire des modèles est vide après lecture.", call. = FALSE)
  }

  parse_ok_no <- function(x) {
    nx <- norm_text(x)
    out <- dplyr::case_when(
      nx %in% c("ok", "oui", "true", "vrai", "1", "x") ~ TRUE,
      nx %in% c("no", "non", "false", "faux", "0", "", "na") ~ FALSE,
      TRUE ~ NA
    )
    out
  }

  modeles_composantes <- dict_raw |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(composantes_std),
      names_to = "Composante",
      values_to = "valeur_source"
    ) |>
    dplyr::mutate(autorise = parse_ok_no(valeur_source))

  valeurs_invalides <- modeles_composantes |>
    dplyr::filter(is.na(autorise), !is.na(valeur_source), trimws(valeur_source) != "") |>
    dplyr::distinct(valeur_source) |>
    dplyr::pull(valeur_source)

  if (length(valeurs_invalides) > 0) {
    msg <- paste0(
      "Valeurs Ok/No non reconnues dans le dictionnaire des modèles: ",
      paste(valeurs_invalides, collapse = ", "),
      ". Valeurs attendues: Ok/No (ou variantes Oui/Non, True/False)."
    )
    if (isTRUE(strict)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  modeles_composantes <- modeles_composantes |>
    dplyr::mutate(autorise = tidyr::replace_na(autorise, FALSE)) |>
    dplyr::mutate(Modele = trimws(Modele))

  modeles_disponibles <- sort(unique(modeles_composantes$Modele))
  modeles_disponibles_norm <- norm_text(modeles_disponibles)

  # --- Extraction de la zone produit -> modele (au-dessus du dictionnaire) ---
  upper_idx <- if (header_model_row > 1) seq.int(1L, header_model_row - 1L) else integer(0)
  if (length(upper_idx) == 0) {
    stop("Zone produit -> modèle introuvable au-dessus du dictionnaire.", call. = FALSE)
  }

  upper <- mat[upper_idx, , drop = FALSE]
  upper_norm <- mat_norm[upper_idx, , drop = FALSE]

  header_score <- apply(upper_norm, 1, function(r) {
    score <- 0
    if (any(grepl("code", r))) score <- score + 1
    if (any(grepl("modele|model", r))) score <- score + 1
    if (any(grepl("designation|libelle|produit", r))) score <- score + 1
    score
  })

  header_prod_local <- if (length(header_score) > 0 && max(header_score) >= 2) {
    which.max(header_score)
  } else {
    NA_integer_
  }

  if (!is.na(header_prod_local)) {
    prod_header <- upper_norm[header_prod_local, ]
    if (header_prod_local >= nrow(upper)) {
      stop("Aucune ligne de données détectée dans la zone produit -> modèle.", call. = FALSE)
    }
    prod_data <- upper[(header_prod_local + 1):nrow(upper), , drop = FALSE]

    code_col <- which(grepl("code", prod_header))[1]
    modele_col <- which(grepl("modele|model", prod_header))[1]
    desig_col <- which(grepl("designation|libelle|produit", prod_header))[1]
  } else {
    prod_data <- upper

    code_counts <- apply(prod_data, 2, function(col) {
      sum(grepl(code_regex, trimws(col)), na.rm = TRUE)
    })
    code_col <- which.max(code_counts)

    modele_counts <- apply(prod_data, 2, function(col) {
      vals <- norm_text(trimws(col))
      sum(vals %in% modeles_disponibles_norm, na.rm = TRUE)
    })
    modele_col <- which.max(modele_counts)

    desig_col <- setdiff(seq_len(ncol(prod_data)), c(code_col, modele_col))[1]
  }

  if (is.na(code_col) || is.na(modele_col)) {
    stop(
      "Colonnes 'Code_Produit' et/ou 'Modele' introuvables dans la zone produit.",
      call. = FALSE
    )
  }

  produits_modeles <- tibble::tibble(
    Code_Produit = toupper(trimws(prod_data[, code_col])),
    Designation = if (!is.na(desig_col)) trimws(prod_data[, desig_col]) else NA_character_,
    Modele = trimws(prod_data[, modele_col])
  ) |>
    dplyr::filter(grepl(code_regex, Code_Produit))

  if (nrow(produits_modeles) == 0) {
    stop(
      "Aucun produit ERE valide détecté (format attendu: AA000, AB000, ...).",
      call. = FALSE
    )
  }

  produits_sans_modele <- produits_modeles |>
    dplyr::filter(is.na(Modele) | Modele == "") |>
    dplyr::pull(Code_Produit)

  if (length(produits_sans_modele) > 0) {
    stop(
      "Produit(s) sans modèle renseigné: ",
      paste(produits_sans_modele, collapse = ", "),
      call. = FALSE
    )
  }

  produits_modeles <- produits_modeles |>
    dplyr::mutate(
      .modele_norm = norm_text(Modele)
    ) |>
    dplyr::left_join(
      tibble::tibble(
        .modele_norm = modeles_disponibles_norm,
        Modele = modeles_disponibles
      ),
      by = ".modele_norm",
      suffix = c("_source", "")
    ) |>
    dplyr::mutate(Modele = dplyr::coalesce(Modele, Modele_source)) |>
    dplyr::select(-.modele_norm, -Modele_source)

  modeles_absents <- setdiff(
    norm_text(unique(produits_modeles$Modele)),
    modeles_disponibles_norm
  )
  if (length(modeles_absents) > 0) {
    stop(
      "Modèle(s) référencé(s) côté produits mais absent(s) du dictionnaire: ",
      paste(modeles_absents, collapse = ", "),
      call. = FALSE
    )
  }

  produits_composantes_autorisees <- produits_modeles |>
    dplyr::left_join(modeles_composantes, by = "Modele") |>
    dplyr::select(Code_Produit, Designation, Modele, Composante, autorise)

  list(
    produits_modeles = produits_modeles,
    modeles_composantes = modeles_composantes,
    produits_composantes_autorisees = produits_composantes_autorisees,
    composantes_standardisees = composantes_std
  )
}

#' Sauvegarder le modèle d'équilibrage ERE en fichier RDS persistant
#'
#' Importe la feuille `ModelEquil` depuis `Methode_ERE.xlsx`, construit l'objet
#' de paramétrage via [importer_modele_equilibrage_ere_excel()] puis le sauvegarde
#' sur disque (par défaut `Modele_Equilibrage_ERE.rds`).
#'
#' @param path_excel Chemin vers `Methode_ERE.xlsx`.
#' @param output_dir Répertoire de sortie du fichier `.rds`.
#' @param output_file Nom du fichier de sortie `.rds`.
#' @param sheet Nom de la feuille (défaut : `"ModelEquil"`).
#' @param overwrite Si `FALSE` (défaut), empêche l'écrasement d'un fichier existant.
#' @param strict Propagé à [importer_modele_equilibrage_ere_excel()].
#'
#' @return Invisiblement le chemin complet du fichier créé.
#' @export
sauvegarder_modele_equilibrage_ere <- function(
    path_excel,
    output_dir = "C:/CnaBfaScn08/CntBfaV4/07P_Outils/OutilCntBfa",
    output_file = "Modele_Equilibrage_ERE.rds",
    sheet = "ModelEquil",
    overwrite = FALSE,
    strict = TRUE
) {
  if (!dir.exists(output_dir)) {
    stop("Répertoire de sortie introuvable: ", output_dir, call. = FALSE)
  }

  path_rds <- file.path(output_dir, output_file)
  if (file.exists(path_rds) && !isTRUE(overwrite)) {
    stop(
      "Le fichier existe déjà: ", path_rds,
      ". Utilisez overwrite = TRUE pour le régénérer.",
      call. = FALSE
    )
  }

  modele <- importer_modele_equilibrage_ere_excel(
    path_excel = path_excel,
    sheet = sheet,
    strict = strict
  )

  saveRDS(modele, path_rds)
  message("Modèle d'équilibrage ERE sauvegardé: ", path_rds)
  invisible(path_rds)
}

#' Charger le modèle d'équilibrage ERE sauvegardé
#'
#' Lit le fichier `.rds` de paramétrage préparé en amont et valide sa structure
#' minimale (`produits_modeles`, `modeles_composantes`,
#' `produits_composantes_autorisees`).
#'
#' @param path_rds Chemin vers le fichier `.rds`.
#' @param validate Si `TRUE` (défaut), vérifie la structure minimale.
#'
#' @return Liste de paramétrage du modèle d'équilibrage ERE.
#' @export
charger_modele_equilibrage_ere <- function(
    path_rds = "C:/CnaBfaScn08/CntBfaV4/07P_Outils/OutilCntBfa/Modele_Equilibrage_ERE.rds",
    validate = TRUE
) {
  if (!file.exists(path_rds)) {
    stop("Fichier de paramétrage introuvable: ", path_rds, call. = FALSE)
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
        "Structure invalide du fichier de paramétrage. Élément(s) manquant(s): ",
        paste(manquants, collapse = ", "),
        call. = FALSE
      )
    }
  }

  obj
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
