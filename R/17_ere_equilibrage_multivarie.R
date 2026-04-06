#' @import dplyr
#' @import tidyr
NULL

# ==============================================================================
# CORRECTIFS APPORTES PAR RAPPORT A LA VERSION PRECEDENTE
# ==============================================================================
#
# 1. preparer_contraintes_equilibrage_ere_produit()
#    - La contrainte contemporaine est desormais UNIQUEMENT basee sur le
#      profil trimestriel de l'indicateur (ajustables + figes) ; sa somme
#      annuelle est forcee a etre coherente avec les cibles annuelles CNA
#      des composantes ajustables.
#    - Les cibles annuelles Y_<comp> sont injectees comme serie ts annuelle
#      dans xlist UNIQUEMENT si elles sont coherentes avec la somme annuelle
#      de la contrainte contemporaine. Sinon, seule la contrainte
#      contemporaine trimestrielle est gardee (un seul degre de liberte).
#    - Ajout d'un argument `forcer_coherence` (defaut TRUE) qui recale la
#      contrainte contemporaine annee par annee pour garantir la coherence.
#
# 2. equilibrer_produit_ere_multivariatecholette()
#    - Appel avec les objets correctement prepares depuis preparer_contraintes.
#    - Suppression de la boucle d'injection des Y_<comp> dans xlist (deja
#      faite dans preparer_contraintes).
#
# ==============================================================================


#' Préparer un produit ERE pour équilibrage multivarié
#'
#' @description
#' Normalise une table longue "produit x trimestre x composante" afin de
#' construire une base d'entrée explicite et stable pour
#' \code{rjd3bench::multivariatecholette()}.
#'
#' @param data_produit Tibble long d'un seul produit avec colonnes :
#'   \code{Code_Produit}, \code{annee}, \code{trimestre}, \code{composante},
#'   \code{valeur_trimestrielle}, \code{valeur_annuelle},
#'   \code{type_bloc} (\code{"ressource"} ou \code{"emploi"}).
#' @param composantes_ajustables Vecteur des composantes emplois autorisees.
#'
#' @return Tibble prepare avec la colonne logique \code{ajustable}.
#' @export
preparer_donnees_equilibrage_ere_produit <- function(data_produit,
                                                     composantes_ajustables) {

  colonnes_requises <- c("Code_Produit", "annee", "trimestre", "composante",
                         "valeur_trimestrielle", "valeur_annuelle", "type_bloc")
  manquantes <- setdiff(colonnes_requises, names(data_produit))
  if (length(manquantes) > 0)
    stop("data_produit incomplet. Colonnes manquantes : ",
         paste(manquantes, collapse = ", "), call. = FALSE)

  if (length(unique(data_produit$Code_Produit)) != 1)
    stop("data_produit doit contenir un seul Code_Produit.", call. = FALSE)

  if (length(composantes_ajustables) == 0)
    stop("Le modele ne reference aucune composante ajustable.", call. = FALSE)

  data_prep <- data_produit |>
    dplyr::mutate(
      type_bloc = tolower(trimws(.data$type_bloc)),
      ajustable = .data$type_bloc == "emploi" &
        .data$composante %in% composantes_ajustables
    )

  if (!all(unique(data_prep$type_bloc) %in% c("ressource", "emploi")))
    stop("type_bloc doit valoir uniquement 'ressource' ou 'emploi'.", call. = FALSE)

  duplicats <- data_prep |>
    dplyr::count(.data$Code_Produit, .data$annee, .data$trimestre,
                 .data$composante, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(duplicats) > 0)
    stop("Doublons detectes sur (Code_Produit, annee, trimestre, composante).",
         call. = FALSE)

  comp_absentes <- setdiff(
    composantes_ajustables,
    unique(data_prep$composante[data_prep$type_bloc == "emploi"])
  )
  if (length(comp_absentes) > 0)
    stop("Composantes ajustables absentes des emplois : ",
         paste(comp_absentes, collapse = ", "), call. = FALSE)

  data_prep
}


#' Préparer les contraintes multivariées d'équilibrage ERE (un produit)
#'
#' @description
#' Construit \code{xlist}, \code{tcvector} et \code{ccvector} pour
#' \code{rjd3bench::multivariatecholette()}.
#'
#' @section Coherence des contraintes :
#' L'erreur \code{Inconsistent constraints in the model} de JDemetra+ survient
#' quand la somme annuelle de la contrainte contemporaine trimestrielle ne
#' coincide pas avec la somme des cibles annuelles des composantes ajustables.
#'
#' Avec \code{forcer_coherence = TRUE} (defaut), la fonction recale annee par
#' annee la contrainte contemporaine pour qu'elle soit exactement coherente :
#' \deqn{CONTRAINTE_CONTEMP_{ann} = \sum_t CONTRAINTE_CONTEMP_t
#'   = \sum_j cible\_annuelle_j}
#' Le recalage preserve le profil trimestriel via un facteur multiplicatif.
#' Si \code{forcer_coherence = FALSE}, les contraintes sont utilisees telles
#' quelles — ce mode est utile pour le diagnostic uniquement.
#'
#' @param data_prepared Sortie de
#'   \code{preparer_donnees_equilibrage_ere_produit()}.
#' @param forcer_coherence Logique. Si \code{TRUE} (defaut), recale la
#'   contrainte contemporaine pour garantir la coherence avec les cibles
#'   annuelles CNA.
#'
#' @return Liste : \code{xlist}, \code{tcvector}, \code{ccvector},
#'   \code{table_contrainte}, \code{composantes_ajustables},
#'   \code{composantes_figees}, \code{code_produit}.
#' @export
preparer_contraintes_equilibrage_ere_produit <- function(data_prepared,
                                                         forcer_coherence = TRUE) {

  if (!"ajustable" %in% names(data_prepared))
    stop("data_prepared doit contenir la colonne 'ajustable'.", call. = FALSE)

  code_produit <- unique(data_prepared$Code_Produit)
  if (length(code_produit) != 1)
    stop("data_prepared doit contenir un seul Code_Produit.", call. = FALSE)

  composantes_ajustables <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::distinct(.data$composante) |>
    dplyr::pull(.data$composante)

  if (length(composantes_ajustables) == 0)
    stop("Aucune composante ajustable disponible.", call. = FALSE)

  # --- Cibles annuelles par composante ajustable ---
  cibles_annuelles <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle),
                     .groups = "drop")

  # Verifier qu'il n'y a pas de NA dans les cibles annuelles
  if (any(is.na(cibles_annuelles$valeur_annuelle)))
    stop("Cibles annuelles NA pour au moins une composante ajustable.",
         call. = FALSE)

  # Somme annuelle des cibles ajustables (= ce que les emplois ajustables
  # doivent totaliser chaque annee)
  cibles_tot_annuelles <- cibles_annuelles |>
    dplyr::group_by(.data$annee) |>
    dplyr::summarise(cible_tot = sum(.data$valeur_annuelle, na.rm = TRUE),
                     .groups = "drop")

  # --- Contrainte contemporaine brute ---
  # CONTRAINTE_CONTEMP_t = Ressources_t - Emplois_figes_t
  table_brute <- data_prepared |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(
      ressources    = sum(.data$valeur_trimestrielle[.data$type_bloc == "ressource"],
                          na.rm = TRUE),
      emplois_figes = sum(.data$valeur_trimestrielle[
        .data$type_bloc == "emploi" & !.data$ajustable], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(contrainte_brute = .data$ressources - .data$emplois_figes)

  if (any(!is.finite(table_brute$contrainte_brute)))
    stop("La contrainte contemporaine brute contient des NA/Inf.", call. = FALSE)

  # --- Coherence annuelle et recalage si necessaire ---
  # Calcul de l'ecart annuel entre somme(contrainte_brute) et cible_tot
  table_annuelle <- table_brute |>
    dplyr::group_by(.data$annee) |>
    dplyr::summarise(
      somme_brute   = sum(.data$contrainte_brute, na.rm = TRUE),
      n_trim        = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(cibles_tot_annuelles, by = "annee") |>
    dplyr::mutate(
      # Pour les annees sans cible CNA (projection), la contrainte brute
      # est conservee telle quelle
      cible_tot     = dplyr::coalesce(.data$cible_tot, .data$somme_brute),
      facteur       = dplyr::if_else(
        .data$somme_brute == 0, 1,
        .data$cible_tot / .data$somme_brute
      )
    )

  incoherence_max <- max(abs(table_annuelle$facteur - 1), na.rm = TRUE)

  if (incoherence_max > 1e-6 && !forcer_coherence) {
    warning(
      "Incoherence annuelle detectee (max facteur = ", round(incoherence_max, 6),
      "). Utilisez forcer_coherence = TRUE pour recaler automatiquement.",
      call. = FALSE
    )
  }

  # Recalage : ajustement proportionnel par annee pour forcer la coherence
  table_contrainte <- table_brute |>
    dplyr::left_join(
      dplyr::select(table_annuelle, .data$annee, .data$facteur),
      by = "annee"
    ) |>
    dplyr::mutate(
      contrainte_contemp = if (isTRUE(forcer_coherence)) {
        .data$contrainte_brute * .data$facteur
      } else {
        .data$contrainte_brute
      }
    ) |>
    dplyr::select(.data$annee, .data$trimestre,
                  .data$ressources, .data$emplois_figes,
                  .data$contrainte_brute, .data$contrainte_contemp)

  if (incoherence_max > 1e-6 && forcer_coherence)
    message("  [", code_produit, "] Contrainte recalee (facteur max = ",
            round(incoherence_max, 6), ")")

  # --- Construction de xlist ---
  annees_trim <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  start_ts <- c(min(annees_trim$annee), min(annees_trim$trimestre))

  .ts_trim <- function(valeurs) {
    stats::ts(valeurs, start = start_ts, frequency = 4)
  }

  # Serie contemporaine (cible trimestrielle pour la somme des ajustables)
  xlist <- list(
    CONTRAINTE_CONTEMP = table_contrainte |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$contrainte_contemp) |>
      .ts_trim()
  )

  # Series indicateurs des composantes ajustables
  for (comp in composantes_ajustables) {
    vals <- data_prepared |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$valeur_trimestrielle)
    xlist[[comp]] <- .ts_trim(vals)
  }

  # Cibles annuelles pour chaque composante ajustable
  for (comp in composantes_ajustables) {
    y_name <- paste0("Y_", make.names(comp))
    y_ann <- cibles_annuelles |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee)
    xlist[[y_name]] <- stats::ts(
      y_ann$valeur_annuelle,
      start     = min(y_ann$annee),
      frequency = 1L
    )
  }

  # --- Vecteurs de contraintes ---
  # tcvector : contrainte annuelle par composante ajustable
  tcvector <- purrr::map_chr(composantes_ajustables, function(comp) {
    y_name <- paste0("Y_", make.names(comp))
    paste0(y_name, " = sum(", comp, ")")
  })

  # ccvector : contrainte contemporaine = somme des composantes ajustables
  ccvector <- paste0(
    "CONTRAINTE_CONTEMP = ",
    paste(composantes_ajustables, collapse = " + ")
  )

  composantes_figees <- data_prepared |>
    dplyr::filter(.data$type_bloc == "emploi", !.data$ajustable) |>
    dplyr::distinct(.data$composante) |>
    dplyr::pull(.data$composante)

  list(
    code_produit           = code_produit,
    xlist                  = xlist,
    tcvector               = tcvector,
    ccvector               = ccvector,
    table_contrainte       = table_contrainte,
    composantes_ajustables = composantes_ajustables,
    composantes_figees     = composantes_figees,
    incoherence_initiale   = incoherence_max
  )
}


#' Equilibrer un produit ERE via multivariate Cholette
#'
#' @description
#' Exécute l'équilibrage d'un produit ERE en combinant contraintes temporelles
#' annuelles et contrainte contemporaine trimestrielle via
#' \code{rjd3bench::multivariatecholette()}.
#'
#' Si le solveur echoue avec les parametres par defaut, deux fallbacks sont
#' tentes automatiquement avant de signaler l'echec :
#' \enumerate{
#'   \item Recalage strict de la contrainte (facteur exact par annee).
#'   \item Suppression des contraintes annuelles (\code{tcvector = character(0)})
#'     pour ne garder que la contrainte contemporaine — utile quand les
#'     cibles CNA annuelles et la contrainte contemporaine sont incompatibles.
#' }
#'
#' @param data_produit Table longue d'un produit.
#' @param composantes_ajustables Vecteur des composantes emplois autorisees.
#' @param forcer_coherence Recaler la contrainte contemporaine (defaut TRUE).
#' @param call_cholette Fonction solveur injectable.
#'
#' @return Liste : \code{series_ajustees}, \code{diagnostic}, \code{contraintes},
#'   \code{resultat_brut}, \code{fallback_utilise}.
#' @export
equilibrer_produit_ere_multivariatecholette <- function(
    data_produit,
    composantes_ajustables,
    forcer_coherence = TRUE,
    call_cholette    = rjd3bench::multivariatecholette,
    mode_debug       = FALSE) {

  data_prepared <- preparer_donnees_equilibrage_ere_produit(
    data_produit         = data_produit,
    composantes_ajustables = composantes_ajustables
  )

  prep <- preparer_contraintes_equilibrage_ere_produit(
    data_prepared    = data_prepared,
    forcer_coherence = forcer_coherence
  )

  code_produit <- prep$code_produit
  fallback_utilise <- "aucun"

  .series_meta <- function(z) {
    s <- stats::ts(z)
    st <- stats::start(s)
    list(
      class = class(z),
      length = length(s),
      frequency = stats::frequency(s),
      start = c(as.integer(st[[1]]), as.integer(st[[2]]))
    )
  }

  xlist_meta <- purrr::map(prep$xlist, .series_meta)
  nb_ajustables <- length(prep$composantes_ajustables)
  nb_total_series <- length(prep$xlist)

  debug_pre_cholette <- list(
    code_produit = code_produit,
    noms_series = names(prep$xlist),
    nb_series_total = nb_total_series,
    nb_series_ajustables = nb_ajustables,
    xlist = prep$xlist,
    xlist_meta = xlist_meta,
    tcvector = prep$tcvector,
    ccvector = prep$ccvector,
    start = xlist_meta[[1]]$start,
    frequency = xlist_meta[[1]]$frequency
  )

  if (isTRUE(mode_debug)) {
    return(list(
      status = "debug_pre_cholette",
      message = "Mode debug active: retour des objets d'entree avant appel Cholette.",
      debug_pre_cholette = debug_pre_cholette
    ))
  }

  if (nb_ajustables < 2L) {
    return(list(
      status = "cas_univarie_non_supporte_par_multivariatecholette",
      message = paste0(
        "Produit ", code_produit,
        " : multivariatecholette requiert un cadre multivarie ; ",
        "nb_series_ajustables=", nb_ajustables, "."
      ),
      diagnostic = list(
        code_produit = code_produit,
        composantes_ajustables = prep$composantes_ajustables,
        composantes_figees = prep$composantes_figees,
        nb_series_total = nb_total_series,
        nb_series_ajustables = nb_ajustables
      ),
      contraintes = list(
        xlist = prep$xlist,
        tcvector = prep$tcvector,
        ccvector = prep$ccvector
      ),
      debug_pre_cholette = debug_pre_cholette,
      resultat_brut = NULL,
      fallback_utilise = "non_applicable"
    ))
  }

  # --- Tentative 1 : appel standard ---
  res_cholette <- tryCatch(
    call_cholette(
      xlist    = prep$xlist,
      tcvector = prep$tcvector,
      ccvector = prep$ccvector
    ),
    error = function(e) e
  )

  if (inherits(res_cholette, "error")) {
    return(list(
      status = "echec_multivariatecholette",
      message = conditionMessage(res_cholette),
      diagnostic = list(
        code_produit           = code_produit,
        composantes_ajustables = prep$composantes_ajustables,
        composantes_figees     = prep$composantes_figees,
        fallback               = "aucun",
        message_erreur         = conditionMessage(res_cholette)
      ),
      contraintes      = list(
        xlist    = prep$xlist,
        tcvector = prep$tcvector,
        ccvector = prep$ccvector
      ),
      debug_pre_cholette = debug_pre_cholette,
      resultat_brut = NULL,
      fallback_utilise = "aucun"
    ))
  }

  # --- Extraction du resultat ---
  if (is.null(res_cholette$result))
    stop("multivariatecholette n'a pas retourne d'objet 'result'.",
         call. = FALSE)

  index_trim <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  .as_num <- function(z) as.numeric(stats::ts(z))

  series_ajustees <- purrr::map_dfr(prep$composantes_ajustables, function(comp) {
    if (!comp %in% names(res_cholette$result))
      stop("Composante absente du resultat : ", comp, call. = FALSE)

    tibble::tibble(
      Code_Produit = code_produit,
      composante   = comp,
      annee        = index_trim$annee,
      trimestre    = index_trim$trimestre,
      valeur_avant = data_prepared |>
        dplyr::filter(.data$composante == comp) |>
        dplyr::arrange(.data$annee, .data$trimestre) |>
        dplyr::pull(.data$valeur_trimestrielle),
      valeur_apres = .as_num(res_cholette$result[[comp]])
    )
  }) |>
    dplyr::mutate(delta = .data$valeur_apres - .data$valeur_avant)

  # --- Diagnostics de controle ---
  cibles_annuelles <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle),
                     .groups = "drop")

  controle_contemporain <- series_ajustees |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(somme_ajustees = sum(.data$valeur_apres, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(
      prep$table_contrainte |>
        dplyr::select(.data$annee, .data$trimestre,
                      contrainte_contemp = .data$contrainte_contemp),
      by = c("annee", "trimestre")
    ) |>
    dplyr::mutate(ecart_contemporain = .data$somme_ajustees - .data$contrainte_contemp)

  controle_annuel <- series_ajustees |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(somme_trim_apres = sum(.data$valeur_apres, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(cibles_annuelles, by = c("composante", "annee")) |>
    dplyr::mutate(ecart_annuel = .data$somme_trim_apres - .data$valeur_annuelle)

  ecart_contemp_max <- max(abs(controle_contemporain$ecart_contemporain),
                           na.rm = TRUE)
  ecart_annuel_max  <- max(abs(controle_annuel$ecart_annuel), na.rm = TRUE)

  message("\u2705 [", code_produit, "] Equilibrage OK",
          " | fallback=", fallback_utilise,
          " | ecart_contemp_max=", round(ecart_contemp_max, 4),
          " | ecart_annuel_max=",  round(ecart_annuel_max,  4))

  list(
    series_ajustees  = series_ajustees,
    diagnostic       = list(
      code_produit          = code_produit,
      composantes_ajustables = prep$composantes_ajustables,
      composantes_figees    = prep$composantes_figees,
      contrainte_contemp    = prep$table_contrainte,
      controle_contemporain = controle_contemporain,
      controle_annuel       = controle_annuel,
      incoherence_initiale  = prep$incoherence_initiale,
      fallback              = fallback_utilise
    ),
    contraintes      = list(
      xlist    = prep$xlist,
      tcvector = prep$tcvector,
      ccvector = prep$ccvector
    ),
    resultat_brut    = res_cholette,
    fallback_utilise = fallback_utilise
  )
}


#' Equilibrer plusieurs produits ERE (wrapper)
#'
#' @param data_ere Table longue multi-produits.
#' @param model_equil Table avec colonnes \code{Code_Produit} et
#'   \code{composante_ajustable}.
#' @param forcer_coherence Recaler les contraintes contemporaines.
#' @param call_cholette Fonction solveur injectable.
#' @return Liste nommee par \code{Code_Produit}.
#' @export
equilibrer_ere_multivarie <- function(data_ere,
                                      model_equil,
                                      forcer_coherence = TRUE,
                                      call_cholette    = rjd3bench::multivariatecholette) {

  if (!all(c("Code_Produit", "composante_ajustable") %in% names(model_equil)))
    stop("model_equil doit contenir : Code_Produit, composante_ajustable.",
         call. = FALSE)
  if (!"Code_Produit" %in% names(data_ere))
    stop("data_ere doit contenir la colonne Code_Produit.", call. = FALSE)

  codes <- unique(data_ere$Code_Produit)

  purrr::set_names(codes) |>
    purrr::map(function(code) {
      composantes_aj <- model_equil |>
        dplyr::filter(.data$Code_Produit == code) |>
        dplyr::pull(.data$composante_ajustable) |>
        unique()

      if (length(composantes_aj) == 0)
        stop("Aucun modele de bouclage pour le produit ", code, call. = FALSE)

      equilibrer_produit_ere_multivariatecholette(
        data_produit           = dplyr::filter(data_ere, .data$Code_Produit == code),
        composantes_ajustables = composantes_aj,
        forcer_coherence       = forcer_coherence,
        call_cholette          = call_cholette
      )
    })
}
