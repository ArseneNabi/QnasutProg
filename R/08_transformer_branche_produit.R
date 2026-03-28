#' Transformer une série par branche en série par produit à l'aide des poids du TRE
#'
#' @description
#' Transforme des agrégats trimestriels exprimés par branche en agrégats par produit
#' à l'aide des poids branche-produit issus du TRE.
#'
#' Les poids sont extraits de `poids_tbl` puis mis en format large avec :
#' - `Poids_Crt` pour les poids en prix courants
#' - `Poids_Vol` pour les poids en prix constants
#'
#' La fonction applique ensuite :
#' - la colonne courante aux poids courants
#' - la colonne volume / VPAP aux poids constants
#'
#' @param df_branches data.frame/tibble avec les valeurs par branche et période.
#' @param poids_tbl data.frame/tibble des poids, typiquement `res_trim$poids_trim`.
#' @param operation filtre sur la colonne `Operation` de `poids_tbl` (ex : "P1", "P2").
#' @param value_crt_col nom de la colonne de valeur courante dans `df_branches`.
#' @param value_vol_col nom de la colonne de valeur volume / VPAP dans `df_branches`.
#' @param annee_col nom de la colonne année dans `df_branches`.
#' @param trimestre_col nom de la colonne trimestre dans `df_branches`.
#' @param branche_col nom de la colonne branche dans `df_branches`.
#' @param produit_col nom de la colonne produit dans la sortie.
#' @param normalize si TRUE, renormalise séparément les poids courant et volume.
#'
#' @return tibble agrégé par année, trimestre et produit.
#' @export
transformer_branche_produit <- function(df_branches,
                                        poids_tbl,
                                        operation,
                                        value_crt_col,
                                        value_vol_col,
                                        annee_col = "annee",
                                        trimestre_col = "trimestre",
                                        branche_col = "Code_Branche",
                                        produit_col = "Code_Produit",
                                        normalize = TRUE) {

  w <- poids_tbl |>
    dplyr::filter(.data$Operation == operation) |>
    dplyr::mutate(
      Annee = as.integer(.data$Annee),
      Trimestre = as.integer(.data$Trimestre),
      Code_Branche = trimws(as.character(.data$Code_Branche)),
      Code_Produit = trimws(as.character(.data$Code_Produit)),
      Type_Prix = trimws(as.character(.data$Type_Prix)),
      Poids = as.numeric(.data$Poids)
    ) |>
    dplyr::mutate(
      Type_Prix = dplyr::case_when(
        .data$Type_Prix == "Courant"  ~ "Poids_Crt",
        .data$Type_Prix == "Constant" ~ "Poids_Vol",
        TRUE ~ .data$Type_Prix
      )
    ) |>
    tidyr::pivot_wider(
      names_from = .data$Type_Prix,
      values_from = .data$Poids,
      values_fill = 0
    )

  if (isTRUE(normalize)) {
    w <- w |>
      dplyr::group_by(.data$Annee, .data$Trimestre, .data$Code_Branche) |>
      dplyr::mutate(
        somme_crt = sum(.data$Poids_Crt, na.rm = TRUE),
        somme_vol = sum(.data$Poids_Vol, na.rm = TRUE),
        Poids_Crt = ifelse(somme_crt > 0, .data$Poids_Crt / somme_crt, .data$Poids_Crt),
        Poids_Vol = ifelse(somme_vol > 0, .data$Poids_Vol / somme_vol, .data$Poids_Vol)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-.data$somme_crt, -.data$somme_vol)
  }

  b <- df_branches |>
    dplyr::mutate(
      Annee = as.integer(.data[[annee_col]]),
      Trimestre = as.integer(.data[[trimestre_col]]),
      Code_Branche = trimws(as.character(.data[[branche_col]]))
    ) |>
    dplyr::select(
      .data$Annee,
      .data$Trimestre,
      .data$Code_Branche,
      valeur_crt = dplyr::all_of(value_crt_col),
      valeur_vol = dplyr::all_of(value_vol_col)
    )

  out <- b |>
    dplyr::inner_join(
      w |>
        dplyr::select(.data$Annee, .data$Trimestre, .data$Code_Branche,
                      .data$Code_Produit, .data$Poids_Crt, .data$Poids_Vol),
      by = c("Annee", "Trimestre", "Code_Branche")
    ) |>
    dplyr::mutate(
      valeur_crt = .data$valeur_crt * .data$Poids_Crt,
      valeur_vol = .data$valeur_vol * .data$Poids_Vol
    ) |>
    dplyr::group_by(.data$Annee, .data$Trimestre, .data$Code_Produit) |>
    dplyr::summarise(
      valeur_crt = sum(.data$valeur_crt, na.rm = TRUE),
      valeur_vol = sum(.data$valeur_vol, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(
      !!annee_col := .data$Annee,
      !!trimestre_col := .data$Trimestre,
      !!produit_col := .data$Code_Produit
    )

  out
}
