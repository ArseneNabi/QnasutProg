#' Diagnostiquer la faisabilite Cholette d'un produit ERE
#'
#' @description
#' Audit structurel d'un produit pour evaluer la faisabilite numerique de
#' `rjd3bench::multivariatecholette()` au-dela des simples controles comptables.
#' Le diagnostic couvre la qualite des series ajustables, la coherence des
#' contraintes, la robustesse matricielle (rang/colinearite) et, en option,
#' un test direct du solveur.
#'
#' @param data_produit Tibble long d'un seul produit ERE.
#' @param composantes_ajustables Vecteur des composantes theoriquement ajustables.
#' @param code_produit Code produit force (sinon derive de `data_produit`).
#' @param tol Tolerance numerique utilisee pour les tests d'egalite et de variance.
#' @param tester_cholette Si `TRUE`, tente `multivariatecholette()` via
#'   `equilibrer_produit_ere_multivariatecholette()` et capture le message.
#' @param call_cholette Fonction solveur injectable (tests unitaires).
#'
#' @return Liste riche contenant : `infos_generales`, `diagnostic_series`,
#'   `diagnostic_contraintes`, `diagnostic_matriciel`, `verdict`,
#'   `test_cholette` et `objets_cholette`.
#' @export
#'
#' @examples
#' \dontrun{
#' diagnostic <- diagnostiquer_faisabilite_cholette_ere_produit(
#'   data_produit = mon_produit,
#'   composantes_ajustables = c("CI Prix d'acquisition"),
#'   tester_cholette = TRUE
#' )
#' }
diagnostiquer_faisabilite_cholette_ere_produit <- function(
    data_produit,
    composantes_ajustables,
    code_produit = NULL,
    tol = 1e-8,
    tester_cholette = FALSE,
    call_cholette = rjd3bench::multivariatecholette) {

  .eq_tol <- function(x, y = 0, tolerance = tol) {
    abs(x - y) <= tolerance * pmax(1, abs(x), abs(y))
  }

  .safe_sd <- function(x) {
    x_ok <- x[is.finite(x)]
    if (length(x_ok) <= 1) {
      return(NA_real_)
    }
    stats::sd(x_ok)
  }

  .interprete_erreur_cholette <- function(msg) {
    if (is.null(msg) || is.na(msg) || msg == "") {
      return("erreur_non_documentee")
    }
    if (grepl("Inconsistent constraints", msg, ignore.case = TRUE)) {
      return("contraintes_incoherentes_ou_systeme_degenere")
    }
    if (grepl("singular|rank|colin", msg, ignore.case = TRUE)) {
      return("matrice_singuliere_ou_colinearite")
    }
    if (grepl("missing|na|nan|inf", msg, ignore.case = TRUE)) {
      return("donnees_non_finies")
    }
    "erreur_cholette_non_classee"
  }

  code <- if (is.null(code_produit)) unique(data_produit$Code_Produit) else code_produit
  if (length(code) != 1) {
    code <- NA_character_
  }

  erreur_preparation <- NULL
  data_prepared <- tryCatch(
    preparer_donnees_equilibrage_ere_produit(data_produit, composantes_ajustables),
    error = function(e) {
      erreur_preparation <<- conditionMessage(e)
      NULL
    }
  )

  prep <- NULL
  erreur_contraintes <- NULL
  if (!is.null(data_prepared)) {
    prep <- tryCatch(
      preparer_contraintes_equilibrage_ere_produit(data_prepared),
      error = function(e) {
        erreur_contraintes <<- conditionMessage(e)
        NULL
      }
    )
  }

  if (!is.null(prep)) {
    code <- prep$code_produit
  }

  composantes_effectives <- if (!is.null(data_prepared)) {
    data_prepared |>
      dplyr::filter(.data$ajustable) |>
      dplyr::distinct(.data$composante) |>
      dplyr::pull(.data$composante)
  } else {
    character(0)
  }

  composantes_figees <- if (!is.null(data_prepared)) {
    data_prepared |>
      dplyr::filter(.data$type_bloc == "emploi", !.data$ajustable) |>
      dplyr::distinct(.data$composante) |>
      dplyr::pull(.data$composante)
  } else {
    character(0)
  }

  index_trim <- if (!is.null(data_prepared)) {
    data_prepared |>
      dplyr::distinct(.data$annee, .data$trimestre) |>
      dplyr::arrange(.data$annee, .data$trimestre)
  } else {
    tibble::tibble(annee = integer(), trimestre = integer())
  }

  periode <- if (nrow(index_trim) > 0) {
    paste0(
      min(index_trim$annee), "T", min(index_trim$trimestre[index_trim$annee == min(index_trim$annee)]),
      " -> ",
      max(index_trim$annee), "T", max(index_trim$trimestre[index_trim$annee == max(index_trim$annee)])
    )
  } else {
    NA_character_
  }

  infos_generales <- list(
    code_produit = code,
    composantes_ajustables_theoriques = unique(composantes_ajustables),
    composantes_ajustables_effectives = composantes_effectives,
    composantes_figees = composantes_figees,
    nb_series_ajustables = length(composantes_effectives),
    nb_observations_trimestrielles = nrow(index_trim),
    periode = periode,
    erreur_preparation = erreur_preparation,
    erreur_contraintes = erreur_contraintes
  )

  diagnostic_series <- if (!is.null(data_prepared) && length(composantes_effectives) > 0) {
    purrr::map_dfr(composantes_effectives, function(comp) {
      dfc <- data_prepared |>
        dplyr::filter(.data$composante == comp) |>
        dplyr::arrange(.data$annee, .data$trimestre)

      valeurs <- dfc$valeur_trimestrielle
      sd_val <- .safe_sd(valeurs)
      moyenne_abs <- mean(abs(valeurs[is.finite(valeurs)]), na.rm = TRUE)
      seuil_quasi <- pmax(tol, 1e-6 * (ifelse(is.nan(moyenne_abs), 1, moyenne_abs)))

      total_annuel <- dfc |>
        dplyr::group_by(.data$annee) |>
        dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle), .groups = "drop") |>
        dplyr::summarise(total_annuel = sum(.data$valeur_annuelle, na.rm = TRUE)) |>
        dplyr::pull(.data$total_annuel)

      total_trim <- sum(valeurs, na.rm = TRUE)
      non_fini <- valeurs[is.finite(valeurs)]

      tibble::tibble(
        composante = comp,
        total_annuel = total_annuel,
        total_trimestriel = total_trim,
        nb_na = sum(is.na(valeurs)),
        nb_nan = sum(is.nan(valeurs)),
        nb_inf = sum(is.infinite(valeurs)),
        min_valeur = if (length(non_fini) == 0) NA_real_ else min(non_fini),
        max_valeur = if (length(non_fini) == 0) NA_real_ else max(non_fini),
        ecart_type = sd_val,
        serie_constante = isTRUE(!is.na(sd_val) && sd_val <= tol),
        serie_quasi_constante = isTRUE(!is.na(sd_val) && sd_val <= seuil_quasi),
        serie_entierement_nulle = isTRUE(.eq_tol(total_trim, 0) && all(.eq_tol(valeurs[is.finite(valeurs)], 0)))
      )
    })
  } else {
    tibble::tibble(
      composante = character(), total_annuel = numeric(), total_trimestriel = numeric(),
      nb_na = integer(), nb_nan = integer(), nb_inf = integer(),
      min_valeur = numeric(), max_valeur = numeric(), ecart_type = numeric(),
      serie_constante = logical(), serie_quasi_constante = logical(), serie_entierement_nulle = logical()
    )
  }

  theorique_non_mobilisable <- if (nrow(diagnostic_series) > 0) {
    diagnostic_series |>
      dplyr::filter(.data$serie_entierement_nulle | .eq_tol(.data$total_annuel, 0)) |>
      dplyr::pull(.data$composante)
  } else {
    character(0)
  }

  diagnostic_contraintes <- if (!is.null(prep)) {
    cible_annuelle <- data_prepared |>
      dplyr::filter(.data$ajustable) |>
      dplyr::group_by(.data$composante, .data$annee) |>
      dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle), .groups = "drop") |>
      dplyr::group_by(.data$annee) |>
      dplyr::summarise(cible_annuelle_ajustables = sum(.data$valeur_annuelle, na.rm = TRUE), .groups = "drop")

    controle_annuel <- prep$table_contrainte |>
      dplyr::group_by(.data$annee) |>
      dplyr::summarise(
        somme_annuelle_contrainte_contemp = sum(.data$contrainte_contemp, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::left_join(cible_annuelle, by = "annee") |>
      dplyr::mutate(
        ecart = .data$somme_annuelle_contrainte_contemp - .data$cible_annuelle_ajustables,
        coherent = .eq_tol(.data$ecart, 0)
      )

    somme_ajustables_trim <- data_prepared |>
      dplyr::filter(.data$ajustable) |>
      dplyr::group_by(.data$annee, .data$trimestre) |>
      dplyr::summarise(somme_ajustables = sum(.data$valeur_trimestrielle, na.rm = TRUE), .groups = "drop")

    controle_trimestriel <- prep$table_contrainte |>
      dplyr::select(.data$annee, .data$trimestre, contrainte_contemp = .data$contrainte_contemp) |>
      dplyr::left_join(somme_ajustables_trim, by = c("annee", "trimestre")) |>
      dplyr::mutate(
        ecart = .data$contrainte_contemp - .data$somme_ajustables,
        coherent = .eq_tol(.data$ecart, 0)
      )

    list(
      tcvector = prep$tcvector,
      ccvector = prep$ccvector,
      controle_annuel = controle_annuel,
      controle_trimestriel = controle_trimestriel,
      coherence_annuelle = all(controle_annuel$coherent),
      coherence_trimestrielle = all(controle_trimestriel$coherent),
      contrainte_contemporaine_triviale =
        length(composantes_effectives) == 1 && all(controle_trimestriel$coherent)
    )
  } else {
    list(
      tcvector = character(0),
      ccvector = character(0),
      controle_annuel = tibble::tibble(),
      controle_trimestriel = tibble::tibble(),
      coherence_annuelle = FALSE,
      coherence_trimestrielle = FALSE,
      contrainte_contemporaine_triviale = FALSE
    )
  }

  diagnostic_matriciel <- if (!is.null(data_prepared) && length(composantes_effectives) > 0) {
    mat <- data_prepared |>
      dplyr::filter(.data$ajustable) |>
      dplyr::select(.data$annee, .data$trimestre, .data$composante, .data$valeur_trimestrielle) |>
      tidyr::pivot_wider(names_from = .data$composante, values_from = .data$valeur_trimestrielle) |>
      dplyr::arrange(.data$annee, .data$trimestre)

    x <- mat |>
      dplyr::select(-.data$annee, -.data$trimestre) |>
      as.matrix()

    complete_rows <- stats::complete.cases(x)
    x_complete <- if (any(complete_rows)) x[complete_rows, , drop = FALSE] else matrix(numeric(0), ncol = ncol(x))

    corr <- if (ncol(x_complete) >= 2 && nrow(x_complete) >= 2) {
      stats::cor(x_complete)
    } else {
      matrix(NA_real_, nrow = ncol(x), ncol = ncol(x), dimnames = list(colnames(x), colnames(x)))
    }

    max_corr <- if (all(is.na(corr))) NA_real_ else max(abs(corr[upper.tri(corr)]), na.rm = TRUE)
    rang <- if (nrow(x_complete) > 0) qr(x_complete)$rank else NA_integer_
    var_cols <- apply(x_complete, 2, stats::var)

    list(
      matrice_correlations = corr,
      colinearite_forte = isTRUE(!is.na(max_corr) && max_corr >= 0.999),
      correlation_max_absolue = max_corr,
      rang_matrice = rang,
      nb_series = ncol(x),
      redondance_structurelle = isTRUE(!is.na(rang) && rang < ncol(x)),
      series_faible_variance = names(var_cols)[which(is.na(var_cols) | var_cols <= tol^2)],
      systeme_surcontraint_ou_degenere =
        isTRUE(!is.na(rang) && rang == 0) ||
        (length(composantes_effectives) == 1 &&
           isTRUE(diagnostic_contraintes$contrainte_contemporaine_triviale))
    )
  } else {
    list(
      matrice_correlations = matrix(NA_real_, nrow = 0, ncol = 0),
      colinearite_forte = FALSE,
      correlation_max_absolue = NA_real_,
      rang_matrice = NA_integer_,
      nb_series = 0L,
      redondance_structurelle = FALSE,
      series_faible_variance = character(0),
      systeme_surcontraint_ou_degenere = TRUE
    )
  }

  motifs <- character(0)

  if (!is.null(erreur_preparation) || !is.null(erreur_contraintes)) {
    motifs <- c(motifs, "preparation_invalide")
  }
  if (nrow(diagnostic_series) > 0 && any(diagnostic_series$nb_na + diagnostic_series$nb_nan + diagnostic_series$nb_inf > 0)) {
    motifs <- c(motifs, "NA_dans_les_series")
  }
  if (length(theorique_non_mobilisable) > 0) {
    motifs <- c(motifs, "serie_nulle")
  }
  if (nrow(diagnostic_series) > 0 && any(diagnostic_series$serie_quasi_constante)) {
    motifs <- c(motifs, "serie_quasi_constante")
  }
  if (!diagnostic_contraintes$coherence_annuelle || !diagnostic_contraintes$coherence_trimestrielle) {
    motifs <- c(motifs, "contraintes_incoherentes")
  }
  if (isTRUE(diagnostic_contraintes$contrainte_contemporaine_triviale)) {
    motifs <- c(motifs, "contrainte_contemporaine_triviale")
  }
  if (isTRUE(diagnostic_matriciel$colinearite_forte)) {
    motifs <- c(motifs, "colinearite_forte")
  }
  if (isTRUE(diagnostic_matriciel$systeme_surcontraint_ou_degenere) ||
      isTRUE(diagnostic_matriciel$redondance_structurelle)) {
    motifs <- c(motifs, "systeme_degenere")
  }

  composantes_theoriques_non_effectives <- setdiff(unique(composantes_ajustables), composantes_effectives)
  if (length(composantes_theoriques_non_effectives) > 0) {
    motifs <- c(motifs, "composante_theorique_absente")
  }

  test_cholette <- list(
    teste = tester_cholette,
    succes = NA,
    message_erreur_brut = NA_character_,
    message_erreur_interprete = NA_character_
  )

  if (tester_cholette && is.null(erreur_preparation) && is.null(erreur_contraintes)) {
    essai <- tryCatch(
      equilibrer_produit_ere_multivariatecholette(
        data_produit = data_produit,
        composantes_ajustables = composantes_ajustables,
        call_cholette = call_cholette
      ),
      error = function(e) e
    )

    if (inherits(essai, "error")) {
      msg <- conditionMessage(essai)
      test_cholette$succes <- FALSE
      test_cholette$message_erreur_brut <- msg
      test_cholette$message_erreur_interprete <- .interprete_erreur_cholette(msg)
      motifs <- c(motifs, "echec_cholette")
    } else {
      test_cholette$succes <- TRUE
      test_cholette$message_erreur_interprete <- "succes"
    }
  }

  motifs <- unique(motifs)

  verdict <- if (any(motifs %in% c(
    "preparation_invalide", "contraintes_incoherentes", "NA_dans_les_series",
    "systeme_degenere", "echec_cholette"
  ))) {
    "non_faisable_probable"
  } else if (length(motifs) > 0) {
    "a_verifier"
  } else {
    "faisable_probable"
  }

  list(
    infos_generales = infos_generales,
    diagnostic_series = diagnostic_series,
    composantes_theoriques_non_effectives = composantes_theoriques_non_effectives,
    composantes_theoriques_non_mobilisables = theorique_non_mobilisable,
    diagnostic_contraintes = diagnostic_contraintes,
    diagnostic_matriciel = diagnostic_matriciel,
    verdict = list(
      statut = verdict,
      motifs = motifs,
      message = paste(motifs, collapse = "; ")
    ),
    test_cholette = test_cholette,
    objets_cholette = if (!is.null(prep)) list(tcvector = prep$tcvector, ccvector = prep$ccvector) else NULL
  )
}

#' Diagnostiquer la faisabilite Cholette sur tous les produits ERE
#'
#' @description
#' Applique `diagnostiquer_faisabilite_cholette_ere_produit()` a l'ensemble des
#' produits presents dans `data_ere`, puis consolide une synthese exploitable
#' (resume par produit, statistiques globales, motifs d'echec frequents).
#'
#' @param data_ere Table longue multi-produits.
#' @param model_equil Table de modele avec au minimum
#'   `Code_Produit` et `composante_ajustable`.
#' @param tol Tolerance numerique.
#' @param tester_cholette Si `TRUE`, teste le solveur produit par produit.
#' @param call_cholette Fonction solveur injectable.
#' @param export_excel_path Chemin optionnel d'export Excel de la synthese.
#'
#' @return Liste avec `resume_produits`, `details_par_produit` et
#'   `statistiques_globales`.
#' @export
diagnostiquer_faisabilite_cholette_ere_tous_produits <- function(
    data_ere,
    model_equil,
    tol = 1e-8,
    tester_cholette = FALSE,
    call_cholette = rjd3bench::multivariatecholette,
    export_excel_path = NULL) {

  if (!all(c("Code_Produit", "composante_ajustable") %in% names(model_equil))) {
    stop(
      "model_equil doit contenir au minimum : Code_Produit, composante_ajustable.",
      call. = FALSE
    )
  }

  if (!"Code_Produit" %in% names(data_ere)) {
    stop("data_ere doit contenir la colonne Code_Produit.", call. = FALSE)
  }

  codes <- unique(data_ere$Code_Produit)

  details <- purrr::set_names(codes) |>
    purrr::map(function(code) {
      composantes_aj <- model_equil |>
        dplyr::filter(.data$Code_Produit == code) |>
        dplyr::pull(.data$composante_ajustable) |>
        unique()

      if (length(composantes_aj) == 0) {
        return(list(
          infos_generales = list(
            code_produit = code,
            composantes_ajustables_theoriques = character(0),
            composantes_ajustables_effectives = character(0),
            composantes_figees = character(0),
            nb_series_ajustables = 0L,
            nb_observations_trimestrielles = 0L,
            periode = NA_character_,
            erreur_preparation = "Aucun modele de bouclage pour ce produit.",
            erreur_contraintes = NA_character_
          ),
          diagnostic_series = tibble::tibble(),
          composantes_theoriques_non_effectives = character(0),
          composantes_theoriques_non_mobilisables = character(0),
          diagnostic_contraintes = list(
            tcvector = character(0),
            ccvector = character(0),
            coherence_annuelle = FALSE,
            coherence_trimestrielle = FALSE,
            contrainte_contemporaine_triviale = FALSE
          ),
          diagnostic_matriciel = list(
            colinearite_forte = FALSE,
            redondance_structurelle = FALSE,
            systeme_surcontraint_ou_degenere = TRUE
          ),
          verdict = list(
            statut = "non_faisable_probable",
            motifs = "modele_absent",
            message = "modele_absent"
          ),
          test_cholette = list(
            teste = tester_cholette,
            succes = NA,
            message_erreur_brut = NA_character_,
            message_erreur_interprete = "non_teste"
          ),
          objets_cholette = NULL
        ))
      }

      diagnostiquer_faisabilite_cholette_ere_produit(
        data_produit = dplyr::filter(data_ere, .data$Code_Produit == code),
        composantes_ajustables = composantes_aj,
        code_produit = code,
        tol = tol,
        tester_cholette = tester_cholette,
        call_cholette = call_cholette
      )
    })

  resume_produits <- purrr::imap_dfr(details, function(x, code) {
    tibble::tibble(
      Code_Produit = code,
      verdict = x$verdict$statut,
      motifs = paste(x$verdict$motifs, collapse = "; "),
      nb_series_ajustables = x$infos_generales$nb_series_ajustables,
      nb_obs_trimestrielles = x$infos_generales$nb_observations_trimestrielles,
      coherence_annuelle = isTRUE(x$diagnostic_contraintes$coherence_annuelle),
      coherence_trimestrielle = isTRUE(x$diagnostic_contraintes$coherence_trimestrielle),
      colinearite_forte = isTRUE(x$diagnostic_matriciel$colinearite_forte),
      systeme_degenere = isTRUE(x$diagnostic_matriciel$systeme_surcontraint_ou_degenere),
      cholette_teste = isTRUE(x$test_cholette$teste),
      cholette_succes = isTRUE(x$test_cholette$succes),
      message_cholette = x$test_cholette$message_erreur_interprete
    )
  })

  stats_verdict <- resume_produits |>
    dplyr::count(.data$verdict, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n))

  motifs_frequents <- resume_produits |>
    dplyr::mutate(motif = strsplit(.data$motifs, ";\\s*")) |>
    tidyr::unnest_longer(.data$motif, values_to = "motif") |>
    dplyr::filter(!is.na(.data$motif), .data$motif != "") |>
    dplyr::count(.data$motif, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n))

  statistiques_globales <- list(
    nb_produits = nrow(resume_produits),
    nb_faisable_probable = sum(resume_produits$verdict == "faisable_probable", na.rm = TRUE),
    nb_non_faisable_probable = sum(resume_produits$verdict == "non_faisable_probable", na.rm = TRUE),
    nb_a_verifier = sum(resume_produits$verdict == "a_verifier", na.rm = TRUE),
    stats_verdict = stats_verdict,
    motifs_frequents = motifs_frequents
  )

  if (!is.null(export_excel_path)) {
    writexl::write_xlsx(
      list(
        resume_produits = resume_produits,
        stats_verdict = stats_verdict,
        motifs_frequents = motifs_frequents
      ),
      path = export_excel_path
    )
  }

  list(
    resume_produits = resume_produits,
    details_par_produit = details,
    statistiques_globales = statistiques_globales
  )
}
