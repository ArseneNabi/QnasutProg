#' Benchmarking trimestriel par code avec contrainte annuelle (Cholette)
#'
#' @description
#' Applique la méthode de Cholette (\pkg{rjd3bench}) pour benchmarquer des séries
#' trimestrielles (source) sur des contraintes annuelles (cibles), par \code{full_code}.
#'
#' @details
#' - \code{s} (trimestriel) est construit à partir de \code{df_source}.
#' - \code{t} (annuel) est construit à partir de \code{df_target}.
#' - \code{t} peut être plus court que \code{s} (cas "pas de benchmark encore" pour les années récentes),
#'   conformément à la documentation \code{rjd3bench::cholette()}.
#' - La sortie est réalignée sur \code{(annee, trimestre)} et, si des périodes manquent,
#'   elles sont remplacées par la valeur source (comportement "pas de contrainte => pas de calage").
#'
#' @param df_source Data frame/tibble source trimestriel. Doit contenir :
#' \code{full_code}, \code{annee}, \code{trimestre}, et la colonne \code{value_col}.
#' Si \code{type_filter} est fourni, \code{df_source} doit aussi contenir \code{type_ind}.
#' @param df_target Data frame/tibble des contraintes annuelles. Doit contenir :
#' \code{full_code}, \code{annee}, \code{valeur}.
#' @param type_filter Filtre optionnel appliqué à \code{df_source$type_ind}. Si \code{NULL}, pas de filtre.
#' @param value_col Nom de la colonne dans \code{df_source} qui contient la série à benchmarquer.
#' @param rho Paramètre de lissage de Cholette (entre 0 et 1). Par défaut 1.
#' @param lambda Paramètre du modèle d'ajustement (1 proportionnel, 0 additif). Par défaut 1.
#' @param bias Biais ("None", "Additive", "Multiplicative"). Par défaut "None".
#' @param conversion Règle d'agrégation ("Sum" recommandé pour des flux). Par défaut "Sum".
#'
#' @return Un tibble contenant les lignes de \code{df_source} traitées avec une colonne \code{valeur_cal}.
#'
#' @export
benchmark_groupe <- function(df_source,
                             df_target,
                             type_filter = NULL,
                             value_col = "valeur",
                             rho = 1,
                             lambda = 1,
                             bias = "None",
                             conversion = "Sum") {

  # Filtrage \u00e9ventuel par type
  data_src <- if (!is.null(type_filter)) {
    dplyr::filter(df_source, .data$type_ind == type_filter)
  } else {
    df_source
  }

  codes_uniques <- unique(data_src$full_code)
  results_list <- vector("list", length(codes_uniques))
  names(results_list) <- as.character(codes_uniques)

  # Helper: convertir un ts trimestriel en (annee, trimestre, value)
  tsq_to_df <- function(x_ts, value_name = "value") {
    tt <- stats::time(x_ts)
    yy <- floor(tt + 1e-9)
    qq <- round((tt - yy) * 4) + 1
    out <- data.frame(
      annee = as.integer(yy),
      trimestre = as.integer(qq),
      stringsAsFactors = FALSE
    )
    out[[value_name]] <- as.numeric(x_ts)
    out
  }

  for (code in codes_uniques) {

    s_data <- data_src |>
      dplyr::filter(.data$full_code == code) |>
      dplyr::arrange(.data$annee, .data$trimestre)

    t_data <- df_target |>
      dplyr::filter(.data$full_code == code) |>
      dplyr::arrange(.data$annee)

    if (nrow(s_data) == 0 || nrow(t_data) == 0) next

    # S\u00e9rie source trimestrielle
    s_ts <- stats::ts(
      s_data[[value_col]],
      start = c(min(s_data$annee), min(s_data$trimestre)),
      frequency = 4
    )

    # S\u00e9rie contrainte annuelle (peut s'arr\u00eater avant la fin de s_ts, c'est OK)
    t_ts <- stats::ts(
      t_data$valeur,
      start = min(t_data$annee),
      frequency = 1
    )

    # Cholette (fallback lambda=0 si \u00e9chec)
    res_obj <- tryCatch(
      rjd3bench::cholette(
        s = s_ts, t = t_ts,
        rho = rho, lambda = lambda,
        bias = bias, conversion = conversion
      ),
      error = function(e) {
        tryCatch(
          rjd3bench::cholette(
            s = s_ts, t = t_ts,
            rho = rho, lambda = 0,
            bias = bias, conversion = conversion
          ),
          error = function(e2) NULL
        )
      }
    )
    if (is.null(res_obj)) next

    # Extraction robuste de la s\u00e9rie benchmark\u00e9e
    # (selon versions, \u00e7a peut \u00eatre directement un ts ou un objet liste)
    bench_ts <- res_obj
    if (is.list(res_obj)) {
      if (!is.null(res_obj$result)) bench_ts <- res_obj$result
      else if (!is.null(res_obj$benchmarked)) bench_ts <- res_obj$benchmarked
      else if (!is.null(res_obj$series)) bench_ts <- res_obj$series
      else if (!is.null(res_obj$s)) bench_ts <- res_obj$s
    }

    # Conversion en df et r\u00e9alignement sur s_data (annee/trimestre)
    bench_df <- tsq_to_df(bench_ts, value_name = "valeur_cal")
    src_df   <- dplyr::select(s_data, "annee", "trimestre") |>
      dplyr::mutate(valeur_src = s_data[[value_col]])

    merged <- dplyr::left_join(src_df, bench_df, by = c("annee", "trimestre"))

    # Si certaines p\u00e9riodes ne sont pas renvoy\u00e9es, on garde la valeur source
    merged <- dplyr::mutate(
      merged,
      valeur_cal = dplyr::if_else(is.na(.data$valeur_cal), .data$valeur_src, .data$valeur_cal)
    )

    # S\u00e9curit\u00e9 longueur
    if (nrow(merged) != nrow(s_data)) {
      stop(sprintf(
        "Alignement incoh\u00e9rent pour code=%s : merged=%d vs s_data=%d",
        code, nrow(merged), nrow(s_data)
      ))
    }

    results_list[[as.character(code)]] <- dplyr::mutate(s_data, valeur_cal = merged$valeur_cal)
  }

  dplyr::bind_rows(results_list)
}
