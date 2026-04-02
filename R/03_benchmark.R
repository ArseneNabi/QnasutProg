#' Benchmarking trimestriel par code avec contrainte annuelle (Cholette)
#'
#' @description
#' Applique la methode de Cholette (\pkg{rjd3bench}) pour benchmarquer des series
#' trimestrielles (source) sur des contraintes annuelles (cibles), par \code{full_code}.
#'
#' @details
#' La serie calee suit les mouvements trimestriels de l'indicateur source tout
#' en respectant les contraintes annuelles CNA (principe de Cholette).
#'
#' Logique de repli par priorite decroissante :
#' \enumerate{
#'   \item Cholette proportionnel (lambda = 1, rho = 1) : \code{rjd3bench::cholette()}
#'     retourne directement un objet \code{ts}. On verifie que ce \code{ts}
#'     respecte bien les contraintes annuelles (somme par annee proche de la cible).
#'     Si le resultat est identique a l'indicateur source (JARs defectueux ou
#'     contrainte non appliquee), on bascule sur le repli.
#'   \item Cholette additif (lambda = 0) si le precedent n'a pas modifie la serie.
#'   \item Distribution plate (cible annuelle / 4) si :
#'     (a) Cholette echoue completement (erreur R/Java),
#'     (b) le resultat Cholette contient des negatifs non autorises, ou
#'     (c) le resultat est identique a l'indicateur source apres les deux
#'         tentatives Cholette (serie cible avec zeros intercales).
#'   \item Si aucune cible annuelle : code exclu du resultat (comportement strict),
#'     sauf si \code{garder_sans_cible = TRUE}.
#' }
#'
#' @param df_source Data frame/tibble source trimestriel. Doit contenir :
#'   \code{full_code}, \code{annee}, \code{trimestre}, et la colonne
#'   \code{value_col}. Si \code{type_filter} est fourni, doit aussi contenir
#'   \code{type_ind}.
#' @param df_target Data frame/tibble des contraintes annuelles. Doit contenir :
#'   \code{full_code}, \code{annee}, \code{valeur}.
#' @param type_filter Filtre optionnel applique a \code{df_source$type_ind}.
#'   Si \code{NULL}, pas de filtre.
#' @param value_col Nom de la colonne dans \code{df_source} a benchmarquer.
#'   Defaut : \code{"valeur"}.
#' @param rho Parametre de lissage de Cholette (entre 0 et 1). Defaut 1.
#' @param lambda Parametre du modele (1 proportionnel, 0 additif). Defaut 1.
#' @param bias Biais ("None", "Additive", "Multiplicative"). Defaut "None".
#' @param conversion Regle d'agregation ("Sum" pour des flux). Defaut "Sum".
#' @param garder_sans_cible Logique. Si \code{TRUE}, les codes sans aucune
#'   contrainte annuelle sont conserves dans le resultat avec
#'   \code{valeur_cal = valeur source}. Si \code{FALSE} (defaut strict), ils
#'   sont exclus du resultat.
#' @param codes_negatifs_autorises Vecteur de \code{full_code} pour lesquels
#'   des valeurs negatives sont acceptables (variations de stocks).
#'   Defaut : \code{character(0)}.
#' @param tol_contrainte Tolerance relative pour verifier que Cholette a bien
#'   applique les contraintes annuelles. Si l'ecart max entre la somme annuelle
#'   calee et la cible depasse ce seuil, la serie est consideree non calee et
#'   le repli est active. Defaut : \code{0.01} (1 %).
#'
#' @return Un tibble contenant les lignes de \code{df_source} traitees avec :
#'   \itemize{
#'     \item \code{valeur_cal} : serie benchmarkee.
#'     \item \code{methode_cal} : methode utilisee (\code{"cholette"},
#'       \code{"distribution_plate"} ou \code{"source"}).
#'   }
#'
#' @export
benchmark_groupe <- function(df_source,
                             df_target,
                             type_filter              = NULL,
                             value_col                = "valeur",
                             rho                      = 1,
                             lambda                   = 1,
                             bias                     = "None",
                             conversion               = "Sum",
                             garder_sans_cible        = FALSE,
                             codes_negatifs_autorises = character(0),
                             tol_contrainte           = 0.01) {

  # ------------------------------------------------------------------
  # Verifications minimales
  # ------------------------------------------------------------------
  cols_source_requises <- c("full_code", "annee", "trimestre", value_col)
  cols_target_requises <- c("full_code", "annee", "valeur")

  manquantes_source <- setdiff(cols_source_requises, names(df_source))
  manquantes_target <- setdiff(cols_target_requises, names(df_target))

  if (length(manquantes_source) > 0)
    stop("Colonnes absentes de `df_source` dans benchmark_groupe() : ",
         paste(manquantes_source, collapse = ", "), call. = FALSE)
  if (length(manquantes_target) > 0)
    stop("Colonnes absentes de `df_target` dans benchmark_groupe() : ",
         paste(manquantes_target, collapse = ", "), call. = FALSE)

  # ------------------------------------------------------------------
  # Filtrage eventuel par type d'indicateur
  # ------------------------------------------------------------------
  if (!is.null(type_filter)) {
    if (length(type_filter) != 1 || is.na(type_filter) ||
        trimws(type_filter) == "")
      stop("`type_filter` doit etre une chaine non vide.", call. = FALSE)
    if (!"type_ind" %in% names(df_source))
      stop("Colonne `type_ind` absente de `df_source`.", call. = FALSE)

    df_source   <- dplyr::mutate(
      df_source, type_ind = trimws(as.character(.data$type_ind)))
    type_filter <- trimws(as.character(type_filter))
    vals_dispo  <- sort(unique(df_source$type_ind))
    df_source   <- dplyr::filter(df_source, .data$type_ind == type_filter)

    if (nrow(df_source) == 0)
      stop("Aucune ligne pour type_filter = '", type_filter,
           "'. Valeurs disponibles : ", paste(vals_dispo, collapse = ", "),
           call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Helper : appeler cholette() et recuperer le ts resultant
  #
  # rjd3bench::cholette() retourne directement un objet ts (vecteur
  # numerique avec attributs start/frequency) quand rjd3jars est
  # correctement installe. On verifie que c'est bien un ts valide.
  # ------------------------------------------------------------------
  appeler_cholette <- function(s_ts, t_ts, lam) {
    res <- tryCatch(
      rjd3bench::cholette(s = s_ts, t = t_ts,
                          rho = rho, lambda = lam,
                          bias = bias, conversion = conversion),
      error = function(e) NULL
    )
    # rjd3bench retourne un ts directement
    if (!is.null(res) && inherits(res, "ts") && length(res) == length(s_ts))
      return(res)
    # Securite : si c'est une liste (versions futures eventuelles)
    if (is.list(res)) {
      for (nm in c("benchmarked", "result", "series", "s")) {
        if (!is.null(res[[nm]]) && inherits(res[[nm]], "ts"))
          return(res[[nm]])
      }
    }
    NULL
  }

  # ------------------------------------------------------------------
  # Helper : verifier que Cholette a bien applique les contraintes
  # On compare la somme annuelle calee a la cible pour chaque annee
  # couverte par t_filtre. Si l'ecart relatif max depasse tol_contrainte,
  # la serie est consideree non calee (JARs defectueux).
  # ------------------------------------------------------------------
  contraintes_respectees <- function(bench_ts, t_filtre) {
    ecarts <- vapply(t_filtre$annee, function(an) {
      trim_an <- stats::window(
        bench_ts,
        start = c(an, 1),
        end   = c(an, 4),
        extend = TRUE
      )
      somme  <- sum(trim_an, na.rm = TRUE)
      cible  <- t_filtre$valeur[t_filtre$annee == an]
      if (length(cible) != 1 || cible == 0) return(0)
      abs(somme - cible) / abs(cible)
    }, numeric(1))
    max(ecarts, na.rm = TRUE) <= tol_contrainte
  }

  # ------------------------------------------------------------------
  # Helper : distribution plate
  # Repartit chaque valeur annuelle CNA en 4 trimestres egaux.
  # Pour les annees sans cible (projection), reconduit la derniere
  # valeur CNA connue / 4.
  # ------------------------------------------------------------------
  distribuer_plate <- function(s_data, t_data) {
    annees_src     <- sort(unique(s_data$annee))
    derniere_cible <- if (nrow(t_data) > 0)
      t_data$valeur[which.max(t_data$annee)] / 4 else 0

    val_trim <- vapply(annees_src, function(an) {
      v <- t_data$valeur[t_data$annee == an]
      if (length(v) == 1L) v / 4 else derniere_cible
    }, numeric(1L))

    s_data |>
      dplyr::mutate(
        .v = val_trim[match(.data$annee, annees_src)],
        valeur_cal  = .data$.v,
        methode_cal = "distribution_plate"
      ) |>
      dplyr::select(-.data$.v)
  }

  # ------------------------------------------------------------------
  # Helper : convertir ts en data.frame (annee, trimestre, valeur_cal)
  # et realigner sur les periodes reelles de s_data
  # ------------------------------------------------------------------
  ts_vers_df <- function(bench_ts, s_data, val_source) {
    tt <- stats::time(bench_ts)
    yy <- floor(tt + 1e-9)
    qq <- round((tt - yy) * 4) + 1
    bench_df <- data.frame(
      annee     = as.integer(yy),
      trimestre = as.integer(qq),
      valeur_cal = as.numeric(bench_ts),
      stringsAsFactors = FALSE
    )
    # Jointure sur les periodes reelles de s_data
    src_df <- data.frame(
      annee      = s_data$annee,
      trimestre  = s_data$trimestre,
      valeur_src = val_source,
      stringsAsFactors = FALSE
    )
    merged <- merge(src_df, bench_df, by = c("annee", "trimestre"),
                    all.x = TRUE, sort = FALSE)
    merged <- merged[order(match(
      paste(merged$annee, merged$trimestre),
      paste(src_df$annee,  src_df$trimestre)
    )), ]
    # Periodes sans contrainte (projection) : repli sur source
    merged$valeur_cal <- ifelse(
      is.na(merged$valeur_cal), merged$valeur_src, merged$valeur_cal
    )
    merged$valeur_cal
  }

  # ------------------------------------------------------------------
  # Benchmarking code par code
  # ------------------------------------------------------------------
  codes_uniques <- unique(df_source$full_code)
  results_list  <- vector("list", length(codes_uniques))
  names(results_list) <- as.character(codes_uniques)

  for (code in codes_uniques) {

    s_data <- df_source |>
      dplyr::filter(.data$full_code == code) |>
      dplyr::arrange(.data$annee, .data$trimestre)

    if (nrow(s_data) == 0L) next

    t_data <- df_target |>
      dplyr::filter(.data$full_code == code) |>
      dplyr::arrange(.data$annee)

    # ----------------------------------------------------------------
    # Cas : aucune cible annuelle
    # ----------------------------------------------------------------
    if (nrow(t_data) == 0L) {
      if (isTRUE(garder_sans_cible))
        results_list[[code]] <- dplyr::mutate(
          s_data, valeur_cal = .data[[value_col]], methode_cal = "source")
      next
    }

    # ----------------------------------------------------------------
    # Series ts — toutes les series demarrent au meme trimestre
    # ----------------------------------------------------------------
    val_source <- s_data[[value_col]]
    s_ts <- stats::ts(val_source,
                      start     = c(min(s_data$annee), min(s_data$trimestre)),
                      frequency = 4L)

    annees_source <- min(s_data$annee):max(s_data$annee)
    t_filtre <- dplyr::filter(t_data, .data$annee %in% annees_source)

    if (nrow(t_filtre) == 0L) {
      if (isTRUE(garder_sans_cible))
        results_list[[code]] <- dplyr::mutate(
          s_data, valeur_cal = .data[[value_col]], methode_cal = "source")
      next
    }

    t_ts <- stats::ts(t_filtre$valeur,
                      start     = min(t_filtre$annee),
                      frequency = 1L)

    # ----------------------------------------------------------------
    # Tentative 1 : Cholette avec lambda fourni
    # ----------------------------------------------------------------
    bench_ts <- appeler_cholette(s_ts, t_ts, lambda)

    # ----------------------------------------------------------------
    # Tentative 2 : Cholette additif si lambda != 0 et echec/non-cale
    # ----------------------------------------------------------------
    if (!is.null(bench_ts) && lambda != 0 &&
        !contraintes_respectees(bench_ts, t_filtre)) {
      bench_ts2 <- appeler_cholette(s_ts, t_ts, 0)
      if (!is.null(bench_ts2) && contraintes_respectees(bench_ts2, t_filtre))
        bench_ts <- bench_ts2
    }

    # ----------------------------------------------------------------
    # Repli distribution plate si Cholette a echoue ou n'a pas cale
    # ----------------------------------------------------------------
    if (is.null(bench_ts) || !contraintes_respectees(bench_ts, t_filtre)) {
      message("  \u21b3 Distribution plate (Cholette non cale) : ", code)
      results_list[[code]] <- distribuer_plate(s_data, t_filtre)
      next
    }

    # ----------------------------------------------------------------
    # Realignement sur les periodes reelles
    # ----------------------------------------------------------------
    valeurs_cal <- ts_vers_df(bench_ts, s_data, val_source)

    if (length(valeurs_cal) != nrow(s_data))
      stop(sprintf("Alignement incoherent pour code=%s", code), call. = FALSE)

    # ----------------------------------------------------------------
    # Valeurs negatives non autorisees -> distribution plate
    # ----------------------------------------------------------------
    if (any(valeurs_cal < 0, na.rm = TRUE) &&
        !(code %in% codes_negatifs_autorises)) {
      message("  \u21b3 Distribution plate (negatifs Cholette) : ", code)
      results_list[[code]] <- distribuer_plate(s_data, t_filtre)
      next
    }

    # ----------------------------------------------------------------
    # Resultat Cholette valide
    # ----------------------------------------------------------------
    results_list[[code]] <- dplyr::mutate(s_data,
                                          valeur_cal  = valeurs_cal,
                                          methode_cal = "cholette")
  }

  dplyr::bind_rows(results_list)
}
