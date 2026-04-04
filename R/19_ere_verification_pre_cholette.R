#' Vérifier le calage annuel des composantes ERE (pré-Cholette)
#'
#' @description
#' Contrôle, composante par composante, si les séries trimestrielles candidates
#' à l'équilibrage respectent déjà leur cible annuelle.
#'
#' Pour chaque couple `(composante, annee)`, la fonction calcule :
#' `somme_trimestrielle - cible_annuelle`, puis marque l'observation comme
#' cohérente si `abs(ecart) <= tol`.
#'
#' @param data_produit Tibble long d'un seul produit contenant au minimum
#'   `Code_Produit`, `annee`, `trimestre`, `composante`,
#'   `valeur_trimestrielle`, `valeur_annuelle`, `type_bloc`.
#' @param tol Tolérance numérique utilisée pour les comparaisons d'égalité.
#'
#' @return Liste contenant :
#' * `detail` : tableau par `composante x annee` avec `somme_trimestrielle`,
#'   `cible_annuelle`, `ecart`, `ecart_absolu`, `coherent`.
#' * `resume` : synthèse globale (`nb_groupes`, `nb_coherents`,
#'   `nb_non_coherents`, `max_ecart_absolu`, `tol`).
#' * `coherent_global` : booléen global.
#' @export
verifier_calage_annuel_composantes_ere <- function(data_produit, tol = 1e-8) {
  colonnes_requises <- c(
    "Code_Produit", "annee", "trimestre", "composante",
    "valeur_trimestrielle", "valeur_annuelle", "type_bloc"
  )

  manquantes <- setdiff(colonnes_requises, names(data_produit))
  if (length(manquantes) > 0) {
    stop(
      "data_produit incomplet. Colonnes manquantes : ",
      paste(manquantes, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(unique(data_produit$Code_Produit)) != 1) {
    stop("data_produit doit contenir un seul Code_Produit.", call. = FALSE)
  }

  detail <- data_produit |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(
      somme_trimestrielle = sum(.data$valeur_trimestrielle, na.rm = TRUE),
      cible_annuelle = dplyr::first(.data$valeur_annuelle),
      n_cibles_annuelles = dplyr::n_distinct(.data$valeur_annuelle),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ecart = .data$somme_trimestrielle - .data$cible_annuelle,
      ecart_absolu = abs(.data$ecart),
      coherent = !is.na(.data$cible_annuelle) &
        .data$n_cibles_annuelles == 1 &
        .data$ecart_absolu <= tol
    ) |>
    dplyr::arrange(.data$composante, .data$annee)

  resume <- detail |>
    dplyr::summarise(
      nb_groupes = dplyr::n(),
      nb_coherents = sum(.data$coherent, na.rm = TRUE),
      nb_non_coherents = sum(!.data$coherent, na.rm = TRUE),
      max_ecart_absolu = max(.data$ecart_absolu, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      max_ecart_absolu = ifelse(is.finite(.data$max_ecart_absolu), .data$max_ecart_absolu, NA_real_),
      tol = tol
    )

  coherent_global <- if (nrow(detail) == 0) FALSE else all(detail$coherent)

  message(
    "[pre-cholette] calage annuel : ",
    resume$nb_coherents,
    "/",
    resume$nb_groupes,
    " groupes coherents (tol=",
    tol,
    ")."
  )

  list(
    detail = detail,
    resume = resume,
    coherent_global = coherent_global
  )
}

#' Vérifier la neutralité annuelle des écarts trimestriels ERE
#'
#' @description
#' Contrôle, année par année, que la somme des écarts trimestriels
#' (`contrainte_contemp` ou `ecart_trim`) est proche de zéro.
#'
#' @param table_contrainte Table des contraintes trimestrielles avec au minimum
#'   `annee`, `trimestre`, et une colonne d'écart nommée
#'   `contrainte_contemp` ou `ecart_trim`.
#' @param tol Tolérance numérique utilisée pour tester la neutralité annuelle.
#'
#' @return Liste contenant :
#' * `detail` : tableau par année avec `somme_annuelle_ecarts`,
#'   `max_abs_trim`, `coherent`.
#' * `resume` : synthèse globale (`nb_annees`, `nb_coherentes`,
#'   `nb_non_coherentes`, `max_abs_somme_annuelle`, `tol`).
#' * `coherent_global` : booléen global.
#' @export
verifier_neutralite_annuelle_ecarts_ere <- function(table_contrainte, tol = 1e-8) {
  colonnes_requises <- c("annee", "trimestre")
  manquantes <- setdiff(colonnes_requises, names(table_contrainte))

  if (length(manquantes) > 0) {
    stop(
      "table_contrainte incomplete. Colonnes manquantes : ",
      paste(manquantes, collapse = ", "),
      call. = FALSE
    )
  }

  col_ecart <- NULL
  if ("contrainte_contemp" %in% names(table_contrainte)) {
    col_ecart <- "contrainte_contemp"
  } else if ("ecart_trim" %in% names(table_contrainte)) {
    col_ecart <- "ecart_trim"
  }

  if (is.null(col_ecart)) {
    stop(
      "table_contrainte doit contenir 'contrainte_contemp' ou 'ecart_trim'.",
      call. = FALSE
    )
  }

  detail <- table_contrainte |>
    dplyr::group_by(.data$annee) |>
    dplyr::summarise(
      somme_annuelle_ecarts = sum(.data[[col_ecart]], na.rm = TRUE),
      max_abs_trim = max(abs(.data[[col_ecart]]), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      max_abs_trim = ifelse(is.finite(.data$max_abs_trim), .data$max_abs_trim, NA_real_),
      coherent = abs(.data$somme_annuelle_ecarts) <= tol
    ) |>
    dplyr::arrange(.data$annee)

  resume <- detail |>
    dplyr::summarise(
      nb_annees = dplyr::n(),
      nb_coherentes = sum(.data$coherent, na.rm = TRUE),
      nb_non_coherentes = sum(!.data$coherent, na.rm = TRUE),
      max_abs_somme_annuelle = max(abs(.data$somme_annuelle_ecarts), na.rm = TRUE)
    ) |>
    dplyr::mutate(
      max_abs_somme_annuelle = ifelse(
        is.finite(.data$max_abs_somme_annuelle),
        .data$max_abs_somme_annuelle,
        NA_real_
      ),
      tol = tol,
      colonne_ecart = col_ecart
    )

  coherent_global <- if (nrow(detail) == 0) FALSE else all(detail$coherent)

  message(
    "[pre-cholette] neutralite ecarts : ",
    resume$nb_coherentes,
    "/",
    resume$nb_annees,
    " annees coherentes (tol=",
    tol,
    ")."
  )

  list(
    detail = detail,
    resume = resume,
    coherent_global = coherent_global
  )
}

#' Vérifier les préconditions Cholette multivarié pour un produit ERE
#'
#' @description
#' Exécute un audit préliminaire avant appel à
#' `rjd3bench::multivariatecholette()` pour un produit ERE :
#' 1. calage annuel de chaque composante trimestrielle ;
#' 2. neutralité annuelle des écarts trimestriels de contrainte.
#'
#' Cette fonction n'exécute **pas** le moteur Cholette : elle vérifie
#' exclusivement la faisabilité comptable en amont.
#'
#' @param data_produit Tibble long d'un seul produit ERE.
#' @param composantes_ajustables Vecteur des composantes autorisées à bouger.
#' @param tol Tolérance numérique utilisée par les deux contrôles.
#'
#' @return Liste contenant :
#' * `controle_calage_annuel`
#' * `controle_neutralite_ecarts`
#' * `resume`
#' * `faisable_pre_cholette` (booléen)
#' * `table_contrainte`
#' * `table_ecarts`
#' @export
verifier_preconditions_cholette_ere_produit <- function(
    data_produit,
    composantes_ajustables,
    tol = 1e-8) {

  data_prepared <- preparer_donnees_equilibrage_ere_produit(
    data_produit = data_produit,
    composantes_ajustables = composantes_ajustables
  )

  prep <- preparer_contraintes_equilibrage_ere_produit(data_prepared)

  controle_calage_annuel <- verifier_calage_annuel_composantes_ere(
    data_produit = dplyr::filter(data_prepared, .data$ajustable),
    tol = tol
  )

  table_ecarts <- data_prepared |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(
      ressources = sum(.data$valeur_trimestrielle[.data$type_bloc == "ressource"], na.rm = TRUE),
      emplois = sum(.data$valeur_trimestrielle[.data$type_bloc == "emploi"], na.rm = TRUE),
      ecart_trim = .data$ressources - .data$emplois,
      .groups = "drop"
    )

  controle_neutralite_ecarts <- verifier_neutralite_annuelle_ecarts_ere(
    table_contrainte = table_ecarts,
    tol = tol
  )

  faisable_pre_cholette <-
    isTRUE(controle_calage_annuel$coherent_global) &&
    isTRUE(controle_neutralite_ecarts$coherent_global)

  code_produit <- unique(data_prepared$Code_Produit)

  resume <- tibble::tibble(
    Code_Produit = code_produit,
    nb_composantes_ajustables = length(prep$composantes_ajustables),
    nb_composantes_figees = length(prep$composantes_figees),
    calage_annuel_ok = controle_calage_annuel$coherent_global,
    neutralite_annuelle_ecarts_ok = controle_neutralite_ecarts$coherent_global,
    faisable_pre_cholette = faisable_pre_cholette,
    tol = tol
  )

  if (faisable_pre_cholette) {
    message(
      "[pre-cholette] produit ", code_produit,
      " : preconditions satisfaites."
    )
  } else {
    message(
      "[pre-cholette] produit ", code_produit,
      " : preconditions NON satisfaites. Voir les tables detaillees."
    )
  }

  list(
    controle_calage_annuel = controle_calage_annuel,
    controle_neutralite_ecarts = controle_neutralite_ecarts,
    resume = resume,
    faisable_pre_cholette = faisable_pre_cholette,
    table_contrainte = prep$table_contrainte,
    table_ecarts = table_ecarts
  )
}

#' Exporter le diagnostic pré-Cholette ERE vers Excel
#'
#' @description
#' Exporte le résultat de `verifier_preconditions_cholette_ere_produit()` dans
#' un fichier Excel multi-feuilles.
#'
#' @param diagnostic_precholette Objet liste retourné par
#'   `verifier_preconditions_cholette_ere_produit()`.
#' @param fichier_sortie Chemin du fichier `.xlsx` à écrire.
#'
#' @return Invisiblement, le chemin du fichier exporté.
#' @export
exporter_diagnostic_pre_cholette_ere_excel <- function(
    diagnostic_precholette,
    fichier_sortie) {

  onglets <- list(
    resume = diagnostic_precholette$resume,
    calage_annuel_resume = diagnostic_precholette$controle_calage_annuel$resume,
    calage_annuel_detail = diagnostic_precholette$controle_calage_annuel$detail,
    neutralite_resume = diagnostic_precholette$controle_neutralite_ecarts$resume,
    neutralite_detail = diagnostic_precholette$controle_neutralite_ecarts$detail,
    contrainte_trim = diagnostic_precholette$table_contrainte,
    ecarts_trim = diagnostic_precholette$table_ecarts
  )

  writexl::write_xlsx(onglets, path = fichier_sortie)

  message("Diagnostic pre-cholette exporte : ", fichier_sortie)
  invisible(fichier_sortie)
}
