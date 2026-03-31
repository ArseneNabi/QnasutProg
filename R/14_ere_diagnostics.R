#' Diagnostiquer la cohérence interne du module ERE
#'
#' @description
#' Produit un diagnostic méthodologique complet du module ERE après benchmarking :
#' 
#' * écarts avant/après benchmarking (P1/P2, courant, VPAP, chaîné),
#' * conservation des sous-ensembles hors cible CNA,
#' * cohérence comptable \eqn{P1 - P2 = B1} au niveau branche et total,
#' * ratios \eqn{P2/P1}, contributions par produit et branche,
#' * anomalies (valeurs négatives et ruptures trimestrielles fortes).
#'
#' Conformément à la logique CNT (SCN 2008), le benchmarking est supposé être
#' appliqué sur les séries chaînées; les VPAP sont utilisés pour l'additivité
#' et doivent rester stables hors cible CNA.
#'
#' @param p1_ere_avant Liste ou tibble des séries P1 avant benchmarking.
#'   Si liste : éléments nommés \code{p1_ere_crt}, \code{p1_ere_ch}, \code{p1_ere_vol}.
#' @param p1_ere_apres Liste ou tibble des séries P1 après benchmarking.
#'   Même structure que \code{p1_ere_avant}.
#' @param p2_ere_avant Liste ou tibble des séries P2 avant benchmarking.
#'   Si liste : éléments nommés \code{p2_ere_crt}, \code{p2_ere_ch}, \code{p2_ere_vol}.
#' @param p2_ere_apres Liste ou tibble des séries P2 après benchmarking.
#'   Même structure que \code{p2_ere_avant}.
#' @param cna_ere_struct Liste CNA ERE (optionnel mais recommandé) pour
#'   identifier les couples produit-année couverts par des cibles.
#' @param comptes_branches Table des comptes de production par branche (optionnel)
#'   avec \code{Code_Branche} et au moins \code{P1_crt}, \code{P2_crt},
#'   \code{VA_crt} ou \code{B1_crt}.
#' @param seuil_rel_alerte Seuil relatif d'alerte pour les écarts de cohérence.
#' @param seuil_rupture Seuil absolu de variation trimestrielle (t/t-1) pour
#'   signaler une rupture forte.
#' @param export_excel Chemin de fichier Excel (optionnel). Si fourni,
#'   le diagnostic est exporté via \code{writexl::write_xlsx()}.
#'
#' @return Une liste \code{ere_diagnostics} avec les éléments :
#' \describe{
#'   \item{ecarts_benchmarking}{Écarts avant/après par composante, produit, période.}
#'   \item{stabilite_hors_cna}{Contrôle des sous-ensembles sans cible CNA.}
#'   \item{ratios_p2_sur_p1}{Ratios P2/P1 après benchmarking.}
#'   \item{contributions_produits}{Contributions des produits aux écarts globaux.}
#'   \item{contributions_branches}{Contributions par branche (si fourni).}
#'   \item{coherence_b1_branche}{Contrôle \eqn{P1-P2=B1} par branche (si fourni).}
#'   \item{coherence_b1_total}{Contrôle \eqn{P1-P2=B1} sur total trimestriel.}
#'   \item{anomalies_negatives}{Détection des valeurs négatives.}
#'   \item{anomalies_ruptures}{Détection des ruptures fortes t/t-1.}
#'   \item{alertes}{Messages d'alerte textuels.}
#' }
#' @export
diagnostiquer_ere <- function(p1_ere_avant,
                              p1_ere_apres,
                              p2_ere_avant,
                              p2_ere_apres,
                              cna_ere_struct = NULL,
                              comptes_branches = NULL,
                              seuil_rel_alerte = 0.05,
                              seuil_rupture = 0.30,
                              export_excel = NULL) {

  .as_tbl <- function(x, col_val) {
    if (is.list(x) && !is.data.frame(x)) {
      return(x[[col_val]])
    }
    x
  }

  .prepare_pair <- function(df_avant, df_apres, col_avant, col_apres, agregat, type_prix) {
    df_avant |>
      dplyr::rename(valeur_avant = dplyr::all_of(col_avant)) |>
      dplyr::inner_join(
        df_apres |> dplyr::rename(valeur_apres = dplyr::all_of(col_apres)),
        by = c("annee", "trimestre", "Code_Produit")
      ) |>
      dplyr::mutate(
        agregat = agregat,
        type_prix = type_prix,
        ecart_abs = valeur_apres - valeur_avant,
        ecart_rel = dplyr::if_else(
          is.na(valeur_avant) | valeur_avant == 0,
          NA_real_,
          ecart_abs / valeur_avant
        )
      )
  }

  .cna_long <- function(cna_ere_struct, composante) {
    if (is.null(cna_ere_struct)) {
      return(tibble::tibble(annee = numeric(), Code_Produit = character()))
    }
    pivoter_ere_long(
      cna_ere_struct[[composante]][["CnaErECrt"]],
      "CnaErECrt",
      composante
    ) |>
      dplyr::filter(valeur != 0) |>
      dplyr::distinct(annee, Code_Produit)
  }

  p1_crt_av <- .as_tbl(p1_ere_avant, "p1_ere_crt")
  p1_ch_av  <- .as_tbl(p1_ere_avant, "p1_ere_ch")
  p1_vol_av <- .as_tbl(p1_ere_avant, "p1_ere_vol")
  p1_crt_ap <- .as_tbl(p1_ere_apres, "p1_ere_crt")
  p1_ch_ap  <- .as_tbl(p1_ere_apres, "p1_ere_ch")
  p1_vol_ap <- .as_tbl(p1_ere_apres, "p1_ere_vol")

  p2_crt_av <- .as_tbl(p2_ere_avant, "p2_ere_crt")
  p2_ch_av  <- .as_tbl(p2_ere_avant, "p2_ere_ch")
  p2_vol_av <- .as_tbl(p2_ere_avant, "p2_ere_vol")
  p2_crt_ap <- .as_tbl(p2_ere_apres, "p2_ere_crt")
  p2_ch_ap  <- .as_tbl(p2_ere_apres, "p2_ere_ch")
  p2_vol_ap <- .as_tbl(p2_ere_apres, "p2_ere_vol")

  ecarts_benchmarking <- dplyr::bind_rows(
    .prepare_pair(p1_crt_av, p1_crt_ap, "P1_crt", "P1_crt", "P1", "crt"),
    .prepare_pair(p1_ch_av,  p1_ch_ap,  "P1_ch",  "P1_ch",  "P1", "ch"),
    .prepare_pair(p1_vol_av, p1_vol_ap, "P1_vol", "P1_vol", "P1", "vpap"),
    .prepare_pair(p2_crt_av, p2_crt_ap, "P2_crt", "P2_crt", "P2", "crt"),
    .prepare_pair(p2_ch_av,  p2_ch_ap,  "P2_ch",  "P2_ch",  "P2", "ch"),
    .prepare_pair(p2_vol_av, p2_vol_ap, "P2_vol", "P2_vol", "P2", "vpap")
  )

  cna_p1 <- .cna_long(cna_ere_struct, "PRODUCTION")
  cna_p2 <- .cna_long(cna_ere_struct, "CI Prix d'acquisition")

  stabilite_hors_cna <- dplyr::bind_rows(
    ecarts_benchmarking |>
      dplyr::filter(agregat == "P1") |>
      dplyr::anti_join(cna_p1, by = c("annee", "Code_Produit")),
    ecarts_benchmarking |>
      dplyr::filter(agregat == "P2") |>
      dplyr::anti_join(cna_p2, by = c("annee", "Code_Produit"))
  ) |>
    dplyr::mutate(stable = dplyr::near(ecart_abs, 0, tol = 1e-10))

  ratios_p2_sur_p1 <- dplyr::inner_join(
    p1_crt_ap,
    p2_crt_ap,
    by = c("annee", "trimestre", "Code_Produit")
  ) |>
    dplyr::mutate(
      ratio_p2_p1 = dplyr::if_else(P1_crt == 0, NA_real_, P2_crt / P1_crt),
      b1_crt = P1_crt - P2_crt
    )

  contributions_produits <- ecarts_benchmarking |>
    dplyr::group_by(agregat, type_prix, Code_Produit) |>
    dplyr::summarise(
      ecart_abs_total = sum(ecart_abs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(agregat, type_prix) |>
    dplyr::mutate(
      contribution = dplyr::if_else(
        sum(abs(ecart_abs_total), na.rm = TRUE) == 0,
        0,
        ecart_abs_total / sum(abs(ecart_abs_total), na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup()

  contributions_branches <- tibble::tibble()
  coherence_b1_branche <- tibble::tibble()
  coherence_b1_total <- tibble::tibble()

  if (!is.null(comptes_branches)) {
    col_b1 <- if ("B1_crt" %in% names(comptes_branches)) "B1_crt" else "VA_crt"

    coherence_b1_branche <- comptes_branches |>
      dplyr::mutate(
        b1_calcule = P1_crt - P2_crt,
        b1_reference = .data[[col_b1]],
        ecart_abs = b1_calcule - b1_reference,
        ecart_rel = dplyr::if_else(
          b1_reference == 0,
          NA_real_,
          ecart_abs / b1_reference
        )
      ) |>
      dplyr::select(
        annee, trimestre, Code_Branche,
        P1_crt, P2_crt, b1_calcule, b1_reference, ecart_abs, ecart_rel
      )

    coherence_b1_total <- coherence_b1_branche |>
      dplyr::group_by(annee, trimestre) |>
      dplyr::summarise(
        P1_total = sum(P1_crt, na.rm = TRUE),
        P2_total = sum(P2_crt, na.rm = TRUE),
        B1_calcule_total = sum(b1_calcule, na.rm = TRUE),
        B1_reference_total = sum(b1_reference, na.rm = TRUE),
        ecart_abs = B1_calcule_total - B1_reference_total,
        ecart_rel = dplyr::if_else(
          B1_reference_total == 0,
          NA_real_,
          ecart_abs / B1_reference_total
        ),
        .groups = "drop"
      )

    contributions_branches <- coherence_b1_branche |>
      dplyr::group_by(Code_Branche) |>
      dplyr::summarise(
        ecart_abs_total = sum(ecart_abs, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        contribution = dplyr::if_else(
          sum(abs(ecart_abs_total), na.rm = TRUE) == 0,
          0,
          ecart_abs_total / sum(abs(ecart_abs_total), na.rm = TRUE)
        )
      )
  }

  anomalies_negatives <- dplyr::bind_rows(
    p1_crt_ap |>
      dplyr::transmute(agregat = "P1", type_prix = "crt", annee, trimestre, Code_Produit, valeur = P1_crt),
    p2_crt_ap |>
      dplyr::transmute(agregat = "P2", type_prix = "crt", annee, trimestre, Code_Produit, valeur = P2_crt),
    p1_ch_ap |>
      dplyr::transmute(agregat = "P1", type_prix = "ch", annee, trimestre, Code_Produit, valeur = P1_ch),
    p2_ch_ap |>
      dplyr::transmute(agregat = "P2", type_prix = "ch", annee, trimestre, Code_Produit, valeur = P2_ch)
  ) |>
    dplyr::filter(valeur < 0)

  anomalies_ruptures <- dplyr::bind_rows(
    p1_crt_ap |>
      dplyr::arrange(Code_Produit, annee, trimestre) |>
      dplyr::group_by(Code_Produit) |>
      dplyr::mutate(
        agregat = "P1",
        type_prix = "crt",
        variation_t_t1 = P1_crt / dplyr::lag(P1_crt) - 1
      ) |>
      dplyr::ungroup() |>
      dplyr::transmute(agregat, type_prix, annee, trimestre, Code_Produit, variation_t_t1),
    p2_crt_ap |>
      dplyr::arrange(Code_Produit, annee, trimestre) |>
      dplyr::group_by(Code_Produit) |>
      dplyr::mutate(
        agregat = "P2",
        type_prix = "crt",
        variation_t_t1 = P2_crt / dplyr::lag(P2_crt) - 1
      ) |>
      dplyr::ungroup() |>
      dplyr::transmute(agregat, type_prix, annee, trimestre, Code_Produit, variation_t_t1)
  ) |>
    dplyr::filter(!is.na(variation_t_t1), abs(variation_t_t1) > seuil_rupture)

  alertes <- character(0)

  if (any(!stabilite_hors_cna$stable, na.rm = TRUE)) {
    alertes <- c(
      alertes,
      "Alerte: des sous-ensembles hors cible CNA ont été modifiés après benchmarking."
    )
  }

  if (nrow(coherence_b1_branche) > 0 &&
      any(abs(coherence_b1_branche$ecart_rel) > seuil_rel_alerte, na.rm = TRUE)) {
    alertes <- c(
      alertes,
      "Alerte: incohérences P1-P2=B1 détectées au niveau branche."
    )
  }

  if (nrow(coherence_b1_total) > 0 &&
      any(abs(coherence_b1_total$ecart_rel) > seuil_rel_alerte, na.rm = TRUE)) {
    alertes <- c(
      alertes,
      "Alerte: incohérences P1-P2=B1 détectées sur les totaux trimestriels."
    )
  }

  if (nrow(anomalies_negatives) > 0) {
    alertes <- c(alertes, "Alerte: des valeurs négatives ont été détectées.")
  }

  if (nrow(anomalies_ruptures) > 0) {
    alertes <- c(alertes, "Alerte: des ruptures trimestrielles fortes ont été détectées.")
  }

  ere_diagnostics <- list(
    ecarts_benchmarking = ecarts_benchmarking,
    stabilite_hors_cna = stabilite_hors_cna,
    ratios_p2_sur_p1 = ratios_p2_sur_p1,
    contributions_produits = contributions_produits,
    contributions_branches = contributions_branches,
    coherence_b1_branche = coherence_b1_branche,
    coherence_b1_total = coherence_b1_total,
    anomalies_negatives = anomalies_negatives,
    anomalies_ruptures = anomalies_ruptures,
    alertes = alertes
  )

  if (!is.null(export_excel)) {
    tables_export <- ere_diagnostics[names(ere_diagnostics) != "alertes"]
    tables_export$alertes <- tibble::tibble(message = ere_diagnostics$alertes)
    writexl::write_xlsx(tables_export, path = export_excel)
    message("✅ Diagnostic ERE exporté : ", export_excel)
  }

  if (length(alertes) == 0) {
    message("✅ Diagnostic ERE: aucun écart critique détecté.")
  } else {
    message("⚠️ Diagnostic ERE: ", length(alertes), " alerte(s) détectée(s).")
  }

  ere_diagnostics
}
