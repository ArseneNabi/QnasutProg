#' Benchmarking trimestriel par code avec contrainte annuelle (Cholette)
#'
#' @description
#' Applique la methode de Cholette (\pkg{rjd3bench}) pour benchmarquer des series
#' trimestrielles (source) sur des contraintes annuelles (cibles), par \code{full_code}.
#'
#' @details
#' La fonction applique la logique de repli suivante pour chaque code :
#' \enumerate{
#'   \item Benchmarking Cholette (lambda = 1, proportionnel).
#'   \item Si le resultat contient des valeurs negatives ET que le code n'est
#'     pas dans \code{codes_negatifs_autorises} : repli sur la
#'     \strong{distribution plate} (cible annuelle / 4 par trimestre).
#'     Ce cas survient typiquement quand la serie cible annuelle contient des
#'     zeros intercales (ex. : 0, 55000, 0, 0, 0) que Cholette ne peut pas
#'     distribuer sans produire de negatifs.
#'   \item Si Cholette echoue (erreur R), tentative avec lambda = 0 (additif),
#'     puis repli distribution plate en dernier recours.
#'   \item Si aucune cible annuelle n'est disponible : le code est exclu du
#'     resultat (comportement strict), sauf si \code{garder_sans_cible = TRUE}.
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
#'   des valeurs negatives sont acceptables (variations de stocks "VS").
#'   Pour ces codes, le resultat Cholette est conserve meme s'il contient des
#'   negatifs. Defaut : \code{character(0)}.
#'
#' @return Un tibble contenant les lignes de \code{df_source} traitees avec
#'   une colonne \code{valeur_cal} et une colonne \code{methode_cal} indiquant
#'   la methode utilisee : \code{"cholette"}, \code{"distribution_plate"} ou
#'   \code{"source"}.
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
                             codes_negatifs_autorises = character(0)) {

  # ------------------------------------------------------------------
  # Verifications minimales
  # ------------------------------------------------------------------
  cols_source_requises <- c("full_code", "annee", "trimestre", value_col)
  cols_target_requises <- c("full_code", "annee", "valeur")

  manquantes_source <- setdiff(cols_source_requises, names(df_source))
  manquantes_target <- setdiff(cols_target_requises, names(df_target))

  if (length(manquantes_source) > 0) {
    stop(
      "Colonnes absentes de `df_source` dans benchmark_groupe() : ",
      paste(manquantes_source, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(manquantes_target) > 0) {
    stop(
      "Colonnes absentes de `df_target` dans benchmark_groupe() : ",
      paste(manquantes_target, collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Filtrage eventuel par type d'indicateur
  # ------------------------------------------------------------------
  if (!is.null(type_filter)) {
    if (length(type_filter) != 1 || is.na(type_filter) || trimws(type_filter) == "") {
      stop("`type_filter` doit etre une chaine non vide lorsqu'il est fourni.",
           call. = FALSE)
    }
    if (!"type_ind" %in% names(df_source)) {
      stop("La colonne `type_ind` est absente de `df_source` dans benchmark_groupe().",
           call. = FALSE)
    }

    df_source   <- dplyr::mutate(df_source,
                                 type_ind = trimws(as.character(.data$type_ind)))
    type_filter <- trimws(as.character(type_filter))
    valeurs_dispo <- sort(unique(df_source$type_ind))

    df_source <- dplyr::filter(df_source, .data$type_ind == type_filter)

    if (nrow(df_source) == 0) {
      stop(
        "Aucune ligne trouvee dans `df_source` pour type_filter = '", type_filter,
        "'. Valeurs disponibles : ",
        paste(valeurs_dispo, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # ------------------------------------------------------------------
  # Helper : convertir un ts trimestriel en data.frame
  # ------------------------------------------------------------------
  tsq_to_df <- function(x_ts, value_name = "value") {
    tt <- stats::time(x_ts)
    yy <- floor(tt + 1e-9)
    qq <- round((tt - yy) * 4) + 1
    out <- data.frame(annee = as.integer(yy), trimestre = as.integer(qq),
                      stringsAsFactors = FALSE)
    out[[value_name]] <- as.numeric(x_ts)
    out
  }

  # ------------------------------------------------------------------
  # Helper : distribution plate
  # Repartit la valeur annuelle CNA en 4 trimestres egaux.
  # Pour les annees sans cible (projection), repete la derniere
  # valeur CNA connue / 4.
  # ------------------------------------------------------------------
  distribuer_plate <- function(s_data, t_data) {

    annees_source <- sort(unique(s_data$annee))

    derniere_cible <- if (nrow(t_data) > 0)
      t_data$valeur[which.max(t_data$annee)] / 4
    else
      0

    val_trim <- vapply(annees_source, function(an) {
      cible_an <- t_data$valeur[t_data$annee == an]
      if (length(cible_an) == 1) cible_an / 4 else derniere_cible
    }, numeric(1))

    s_data |>
      dplyr::mutate(
        .val_ann    = val_trim[match(.data$annee, annees_source)],
        valeur_cal  = .data$.val_ann,
        methode_cal = "distribution_plate"
      ) |>
      dplyr::select(-.data$.val_ann)
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

    if (nrow(s_data) == 0) next

    t_data <- df_target |>
      dplyr::filter(.data$full_code == code) |>
      dplyr::arrange(.data$annee)

    # ----------------------------------------------------------------
    # Cas : aucune cible annuelle
    # ----------------------------------------------------------------
    if (nrow(t_data) == 0) {
      if (isTRUE(garder_sans_cible)) {
        results_list[[as.character(code)]] <- dplyr::mutate(
          s_data,
          valeur_cal  = .data[[value_col]],
          methode_cal = "source"
        )
      }
      # Comportement strict : code exclu du resultat
      next
    }

    # ----------------------------------------------------------------
    # Construction des series ts
    # Toutes les series debutent a la meme periode donc pas de
    # correction d'alignement necessaire.
    # ----------------------------------------------------------------
    s_ts <- stats::ts(
      s_data[[value_col]],
      start     = c(min(s_data$annee), min(s_data$trimestre)),
      frequency = 4
    )

    annees_source <- min(s_data$annee):max(s_data$annee)
    t_data_filtre <- dplyr::filter(t_data, .data$annee %in% annees_source)

    if (nrow(t_data_filtre) == 0) {
      if (isTRUE(garder_sans_cible)) {
        results_list[[as.character(code)]] <- dplyr::mutate(
          s_data, valeur_cal = .data[[value_col]], methode_cal = "source"
        )
      }
      next
    }

    t_ts <- stats::ts(
      t_data_filtre$valeur,
      start     = min(t_data_filtre$annee),
      frequency = 1
    )

    # ----------------------------------------------------------------
    # Tentative Cholette (lambda fourni, puis lambda = 0)
    # ----------------------------------------------------------------
    res_obj <- tryCatch(
      rjd3bench::cholette(s = s_ts, t = t_ts, rho = rho, lambda = lambda,
                          bias = bias, conversion = conversion),
      error = function(e) {
        tryCatch(
          rjd3bench::cholette(s = s_ts, t = t_ts, rho = rho, lambda = 0,
                              bias = bias, conversion = conversion),
          error = function(e2) NULL
        )
      }
    )

    # ----------------------------------------------------------------
    # Cholette a echoue -> distribution plate directement
    # ----------------------------------------------------------------
    if (is.null(res_obj)) {
      message("  \u21b3 Distribution plate (echec Cholette) : ", code)
      results_list[[as.character(code)]] <- distribuer_plate(s_data, t_data_filtre)
      next
    }

    # ----------------------------------------------------------------
    # Extraction robuste de la serie benchmarkee
    # ----------------------------------------------------------------
    bench_ts <- res_obj
    if (is.list(res_obj)) {
      if      (!is.null(res_obj$result))      bench_ts <- res_obj$result
      else if (!is.null(res_obj$benchmarked)) bench_ts <- res_obj$benchmarked
      else if (!is.null(res_obj$series))      bench_ts <- res_obj$series
      else if (!is.null(res_obj$s))           bench_ts <- res_obj$s
    }

    bench_df <- tsq_to_df(bench_ts, value_name = "valeur_cal")

    # Realignement sur les periodes reelles de s_data
    src_df <- dplyr::select(s_data, "annee", "trimestre") |>
      dplyr::mutate(valeur_src = s_data[[value_col]])

    merged <- dplyr::left_join(src_df, bench_df, by = c("annee", "trimestre")) |>
      dplyr::mutate(
        valeur_cal = dplyr::if_else(
          is.na(.data$valeur_cal), .data$valeur_src, .data$valeur_cal
        )
      )

    if (nrow(merged) != nrow(s_data)) {
      stop(sprintf(
        "Alignement incoherent pour code=%s : merged=%d vs s_data=%d",
        code, nrow(merged), nrow(s_data)
      ), call. = FALSE)
    }

    valeurs_cal <- merged$valeur_cal

    # ----------------------------------------------------------------
    # Cholette a produit des negatifs sur un code non autorise
    # -> repli distribution plate
    # Ce cas arrive quand la serie cible annuelle contient des zeros
    # intercales que Cholette ne peut distribuer sans negatifs
    # (ex : cible 0, 55000, 0, 0, 0 ...)
    # Seule exception : les variations de stocks (codes_negatifs_autorises)
    # ----------------------------------------------------------------
    negatifs_presents  <- any(valeurs_cal < 0, na.rm = TRUE)
    negatifs_autorises <- code %in% codes_negatifs_autorises

    if (negatifs_presents && !negatifs_autorises) {
      message(
        "  \u21b3 Distribution plate (n\u00e9gatifs Cholette) : ", code
      )
      results_list[[as.character(code)]] <- distribuer_plate(s_data, t_data_filtre)
      next
    }

    # ----------------------------------------------------------------
    # Resultat Cholette valide
    # ----------------------------------------------------------------
    results_list[[as.character(code)]] <- dplyr::mutate(
      s_data,
      valeur_cal  = valeurs_cal,
      methode_cal = "cholette"
    )
  }

  dplyr::bind_rows(results_list)
}
