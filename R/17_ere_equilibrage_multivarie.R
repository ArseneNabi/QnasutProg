#' @import dplyr
#' @import tidyr
NULL

#' Recentrer les contraintes trimestrielles par année
#'
#' @description
#' Corrige une contrainte trimestrielle contemporaine lorsque, pour une année
#' donnée, la somme des contraintes n'est pas nulle à une tolérance près.
#' La correction est un recentrage annuel uniforme :
#' \deqn{offset = \frac{\sum_t contrainte_t}{n}}
#' \deqn{contrainte\_corrigee_t = contrainte\_initiale_t - offset}
#' où \eqn{n} est le nombre de trimestres observés de l'année.
#'
#' @param table_contrainte Tibble contenant au minimum
#'   \code{annee}, \code{trimestre}, \code{contrainte_contemp}.
#' @param tol_recentering Tolérance absolue sur la somme annuelle. Si
#'   \code{|somme_annuelle| <= tol_recentering}, aucun recentrage n'est appliqué.
#' @return Tibble enrichi avec :
#' \describe{
#'   \item{contrainte_initiale}{Contrainte trimestrielle avant correction.}
#'   \item{contrainte_contemp}{Contrainte trimestrielle corrigée (ou inchangée).}
#'   \item{offset_applique}{Offset annuel soustrait à chaque trimestre.}
#'   \item{somme_annuelle_avant}{Somme annuelle avant correction.}
#'   \item{somme_annuelle_apres}{Somme annuelle après correction.}
#' }
#' @export
recentrer_contraintes_trimestrielles_par_annee <- function(table_contrainte,
                                                            tol_recentering = 1e-8) {
  colonnes_requises <- c("annee", "trimestre", "contrainte_contemp")
  manquantes <- setdiff(colonnes_requises, names(table_contrainte))

  if (length(manquantes) > 0) {
    stop(
      "table_contrainte incomplet. Colonnes manquantes : ",
      paste(manquantes, collapse = ", "),
      call. = FALSE
    )
  }

  table_contrainte |>
    dplyr::arrange(.data$annee, .data$trimestre) |>
    dplyr::mutate(contrainte_initiale = .data$contrainte_contemp) |>
    dplyr::group_by(.data$annee) |>
    dplyr::mutate(
      nb_trimestres = dplyr::n(),
      somme_annuelle_avant = sum(.data$contrainte_initiale, na.rm = TRUE),
      offset_applique = dplyr::if_else(
        abs(.data$somme_annuelle_avant) > tol_recentering,
        .data$somme_annuelle_avant / .data$nb_trimestres,
        0
      ),
      contrainte_contemp = .data$contrainte_initiale - .data$offset_applique,
      somme_annuelle_apres = sum(.data$contrainte_contemp, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$nb_trimestres)
}

#' Préparer un produit ERE pour équilibrage multivarié
#'
#' @description
#' Normalise une table longue "produit x trimestre x composante" afin de
#' construire une base d'entrée explicite et stable pour
#' \code{rjd3bench::multivariatecholette()}.
#'
#' La table d'entrée est enrichie avec :
#' \itemize{
#'   \item un indicateur \code{ajustable} selon le modèle de bouclage ;
#'   \item une validation stricte des colonnes minimales ;
#'   \item un contrôle d'unicité sur les clés
#'   \code{Code_Produit, annee, trimestre, composante}.
#' }
#'
#' @param data_produit Tibble long d'un seul produit avec au minimum les
#'   colonnes suivantes : \code{Code_Produit}, \code{annee}, \code{trimestre},
#'   \code{composante}, \code{valeur_trimestrielle}, \code{valeur_annuelle},
#'   \code{type_bloc} (\code{"ressource"} ou \code{"emploi"}).
#' @param composantes_ajustables Vecteur de composantes emplois autorisées à
#'   bouger selon le modèle de bouclage du produit.
#' @return Tibble préparé avec la colonne logique \code{ajustable}.
#' @export
preparer_donnees_equilibrage_ere_produit <- function(data_produit,
                                                      composantes_ajustables) {

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

  if (length(composantes_ajustables) == 0) {
    stop("Le modele ne reference aucune composante ajustable.", call. = FALSE)
  }

  data_prep <- data_produit |>
    dplyr::mutate(
      type_bloc = tolower(trimws(.data$type_bloc)),
      ajustable = .data$type_bloc == "emploi" &
        .data$composante %in% composantes_ajustables
    )

  valeurs_type_bloc <- unique(data_prep$type_bloc)
  if (!all(valeurs_type_bloc %in% c("ressource", "emploi"))) {
    stop(
      "type_bloc doit valoir uniquement 'ressource' ou 'emploi'.",
      call. = FALSE
    )
  }

  duplicats <- data_prep |>
    dplyr::count(.data$Code_Produit, .data$annee, .data$trimestre, .data$composante,
                 name = "n") |>
    dplyr::filter(.data$n > 1)

  if (nrow(duplicats) > 0) {
    stop(
      "Doublons detectes sur (Code_Produit, annee, trimestre, composante).",
      call. = FALSE
    )
  }

  if (!all(composantes_ajustables %in% data_prep$composante[data_prep$type_bloc == "emploi"])) {
    composantes_absentes <- setdiff(
      composantes_ajustables,
      unique(data_prep$composante[data_prep$type_bloc == "emploi"])
    )
    stop(
      "Composantes ajustables absentes des emplois du produit : ",
      paste(composantes_absentes, collapse = ", "),
      call. = FALSE
    )
  }

  data_prep
}

#' Préparer les contraintes multivariées d'équilibrage ERE (un produit)
#'
#' @description
#' Construit les objets requis par \code{rjd3bench::multivariatecholette()} :
#' \itemize{
#'   \item \code{xlist} : séries trimestrielles \code{ts} des composantes
#'   ajustables + la cible contemporaine \code{CONTRAINTE_CONTEMP};
#'   \item \code{tcvector} : contraintes annuelles
#'   \code{Y_<composante> = sum(<composante>)} ;
#'   \item \code{ccvector} : contrainte contemporaine
#'   \code{CONTRAINTE_CONTEMP = comp1 + comp2 + ... + compN}.
#' }
#'
#' La cible contemporaine est calculée trimestre par trimestre :
#' \deqn{CONTRAINTE_CONTEMP_t = Ressources_t - Emplois\_figes_t}
#'
#' @param data_prepared Sortie de
#'   \code{preparer_donnees_equilibrage_ere_produit()}.
#' @param tol_recentering Tolérance absolue utilisée pour le recentrage annuel
#'   de la contrainte contemporaine. Si la somme annuelle de
#'   \code{contrainte_contemp} est inférieure à cette tolérance en valeur
#'   absolue, aucune correction n'est appliquée.
#' @return Liste contenant \code{xlist}, \code{tcvector}, \code{ccvector},
#'   \code{table_contrainte}, \code{composantes_ajustables},
#'   \code{composantes_figees} et \code{code_produit}.
#' @export
preparer_contraintes_equilibrage_ere_produit <- function(data_prepared,
                                                          tol_recentering = 1e-8) {
  if (!"ajustable" %in% names(data_prepared)) {
    stop("data_prepared doit contenir la colonne 'ajustable'.", call. = FALSE)
  }

  code_produit <- unique(data_prepared$Code_Produit)
  if (length(code_produit) != 1) {
    stop("data_prepared doit contenir un seul Code_Produit.", call. = FALSE)
  }

  composantes_ajustables <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::distinct(.data$composante) |>
    dplyr::pull(.data$composante)

  if (length(composantes_ajustables) == 0) {
    stop("Aucune composante ajustable disponible pour ce produit.", call. = FALSE)
  }

  verif_annuelle <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(
      n_cibles = dplyr::n_distinct(.data$valeur_annuelle),
      all_na = all(is.na(.data$valeur_annuelle)),
      .groups = "drop"
    )

  annuelles_invalides <- verif_annuelle |>
    dplyr::filter(.data$all_na | .data$n_cibles > 1)

  if (nrow(annuelles_invalides) > 0) {
    stop(
      "Cibles annuelles invalides pour au moins une composante ajustable.",
      call. = FALSE
    )
  }

  table_totaux <- data_prepared |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(
      ressources = sum(.data$valeur_trimestrielle[.data$type_bloc == "ressource"], na.rm = TRUE),
      emplois_figes = sum(
        .data$valeur_trimestrielle[
          .data$type_bloc == "emploi" & !.data$ajustable
        ],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(contrainte_contemp = .data$ressources - .data$emplois_figes)

  if (any(!is.finite(table_totaux$contrainte_contemp))) {
    stop("La contrainte contemporaine ne peut pas etre construite.", call. = FALSE)
  }

  table_totaux <- recentrer_contraintes_trimestrielles_par_annee(
    table_contrainte = table_totaux,
    tol_recentering = tol_recentering
  )

  debut <- data_prepared |>
    dplyr::arrange(.data$annee, .data$trimestre) |>
    dplyr::slice(1)

  start_ts <- c(debut$annee, debut$trimestre)

  .serie_trim <- function(df, col_valeur) {
    stats::ts(df[[col_valeur]], start = start_ts, frequency = 4)
  }

  contrainte_ts <- table_totaux |>
    dplyr::arrange(.data$annee, .data$trimestre) |>
    .serie_trim("contrainte_contemp")

  xlist <- list(CONTRAINTE_CONTEMP = contrainte_ts)

  for (comp in composantes_ajustables) {
    serie_comp <- data_prepared |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee, .data$trimestre)

    xlist[[comp]] <- .serie_trim(serie_comp, "valeur_trimestrielle")
  }

  tcvector <- purrr::map_chr(composantes_ajustables, function(comp) {
    y_name <- paste0("Y_", make.names(comp))
    paste0(y_name, " = sum(", comp, ")")
  })

  ccvector <- paste0(
    "CONTRAINTE_CONTEMP = ",
    paste(composantes_ajustables, collapse = " + ")
  )

  composantes_figees <- data_prepared |>
    dplyr::filter(.data$type_bloc == "emploi", !.data$ajustable) |>
    dplyr::distinct(.data$composante) |>
    dplyr::pull(.data$composante)

  list(
    code_produit = code_produit,
    xlist = xlist,
    tcvector = tcvector,
    ccvector = ccvector,
    table_contrainte = table_totaux,
    composantes_ajustables = composantes_ajustables,
    composantes_figees = composantes_figees
  )
}

#' Equilibrer un produit ERE via multivariate Cholette
#'
#' @description
#' Exécute l'équilibrage d'un produit ERE en combinant contraintes temporelles
#' annuelles et contrainte contemporaine trimestrielle.
#'
#' @param data_produit Table longue d'un produit (voir
#'   \code{preparer_donnees_equilibrage_ere_produit()}).
#' @param composantes_ajustables Vecteur des composantes emplois autorisées à
#'   absorber le déséquilibre.
#' @param tol_recentering Tolérance absolue pour le recentrage annuel de
#'   \code{CONTRAINTE_CONTEMP} lors de la préparation des contraintes.
#' @param call_cholette Fonction d'estimation. Par défaut,
#'   \code{rjd3bench::multivariatecholette}. Paramètre injectable pour tests.
#' @return Liste avec :
#' \describe{
#'   \item{series_ajustees}{Tibble des composantes ajustées par trimestre.}
#'   \item{diagnostic}{Tables de contrôle avant/après (écarts trimestriels,
#'   respect annuel, contrainte contemporaine).}
#'   \item{contraintes}{Objets \code{xlist/tcvector/ccvector} effectivement utilisés.}
#' }
#' @export
equilibrer_produit_ere_multivariatecholette <- function(
    data_produit,
    composantes_ajustables,
    tol_recentering = 1e-8,
    call_cholette = rjd3bench::multivariatecholette) {

  data_prepared <- preparer_donnees_equilibrage_ere_produit(
    data_produit = data_produit,
    composantes_ajustables = composantes_ajustables
  )

  prep <- preparer_contraintes_equilibrage_ere_produit(
    data_prepared = data_prepared,
    tol_recentering = tol_recentering
  )

  code_produit <- prep$code_produit

  cibles_annuelles <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle),
                     .groups = "drop")

  for (comp in prep$composantes_ajustables) {
    y_name <- paste0("Y_", make.names(comp))
    y_ann <- cibles_annuelles |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee)

    if (nrow(y_ann) == 0 || any(is.na(y_ann$valeur_annuelle))) {
      stop(
        "Une composante ajustable n'a pas de cible annuelle : ", comp,
        call. = FALSE
      )
    }

    prep$xlist[[y_name]] <- stats::ts(
      y_ann$valeur_annuelle,
      start = min(y_ann$annee),
      frequency = 1
    )
  }

  res_cholette <- tryCatch(
    call_cholette(
      xlist = prep$xlist,
      tcvector = prep$tcvector,
      ccvector = prep$ccvector
    ),
    error = function(e) {
      stop(
        "Echec multivariatecholette pour le produit ", code_produit,
        " : ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (is.null(res_cholette$result)) {
    stop(
      "multivariatecholette n'a pas retourne d'objet 'result' exploitable.",
      call. = FALSE
    )
  }

  index_trim <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  .as_num <- function(z) as.numeric(stats::ts(z))

  series_ajustees <- purrr::map_dfr(prep$composantes_ajustables, function(comp) {
    if (!comp %in% names(res_cholette$result)) {
      stop(
        "La composante ajustable ", comp,
        " est absente du resultat multivariatecholette.",
        call. = FALSE
      )
    }

    tibble::tibble(
      Code_Produit = code_produit,
      composante = comp,
      annee = index_trim$annee,
      trimestre = index_trim$trimestre,
      valeur_avant = data_prepared |>
        dplyr::filter(.data$composante == comp) |>
        dplyr::arrange(.data$annee, .data$trimestre) |>
        dplyr::pull(.data$valeur_trimestrielle),
      valeur_apres = .as_num(res_cholette$result[[comp]])
    )
  }) |>
    dplyr::mutate(delta = .data$valeur_apres - .data$valeur_avant)

  controle_contemporain <- series_ajustees |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(
      somme_ajustees = sum(.data$valeur_apres, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      prep$table_contrainte |>
        dplyr::select(.data$annee, .data$trimestre, contrainte_contemp = .data$contrainte_contemp),
      by = c("annee", "trimestre")
    ) |>
    dplyr::mutate(ecart_contemporain = .data$somme_ajustees - .data$contrainte_contemp)

  controle_annuel <- series_ajustees |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(
      somme_trim_apres = sum(.data$valeur_apres, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(cibles_annuelles,
      by = c("composante", "annee")
    ) |>
    dplyr::mutate(ecart_annuel = .data$somme_trim_apres - .data$valeur_annuelle)

  list(
    series_ajustees = series_ajustees,
    diagnostic = list(
      code_produit = code_produit,
      composantes_ajustables = prep$composantes_ajustables,
      composantes_figees = prep$composantes_figees,
      contrainte_contemp = prep$table_contrainte,
      controle_contemporain = controle_contemporain,
      controle_annuel = controle_annuel
    ),
    contraintes = list(
      xlist = prep$xlist,
      tcvector = prep$tcvector,
      ccvector = prep$ccvector
    ),
    resultat_brut = res_cholette
  )
}

#' Equilibrer plusieurs produits ERE (wrapper)
#'
#' @param data_ere Table longue multi-produits.
#' @param model_equil Table de paramétrage des modèles de bouclage avec colonnes
#'   minimales \code{Code_Produit} et \code{composante_ajustable}.
#' @param tol_recentering Tolérance absolue pour le recentrage annuel de
#'   \code{CONTRAINTE_CONTEMP} lors de la préparation des contraintes.
#' @param call_cholette Fonction d'estimation injectable.
#' @return Liste nommée par \code{Code_Produit}.
#' @export
equilibrer_ere_multivarie <- function(data_ere,
                                      model_equil,
                                      tol_recentering = 1e-8,
                                      call_cholette = rjd3bench::multivariatecholette) {

  if (!all(c("Code_Produit", "composante_ajustable") %in% names(model_equil))) {
    stop(
      "model_equil doit contenir au minimum : Code_Produit, composante_ajustable.",
      call. = FALSE
    )
  }

  codes <- unique(data_ere$Code_Produit)

  purrr::set_names(codes) |>
    purrr::map(function(code) {
      composantes_aj <- model_equil |>
        dplyr::filter(.data$Code_Produit == code) |>
        dplyr::pull(.data$composante_ajustable) |>
        unique()

      if (length(composantes_aj) == 0) {
        stop(
          "Aucun modele de bouclage trouve pour le produit ", code,
          call. = FALSE
        )
      }

      equilibrer_produit_ere_multivariatecholette(
        data_produit = dplyr::filter(data_ere, .data$Code_Produit == code),
        composantes_ajustables = composantes_aj,
        tol_recentering = tol_recentering,
        call_cholette = call_cholette
      )
    })
}
