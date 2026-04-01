# ==============================================================================
# 15_diagnostic_equilibre_ere.R
# Diagnostic de l'equilibre Ressources = Emplois de l'ERE
# par produit et par trimestre
# ==============================================================================
#' Diagnostiquer l'equilibre Ressources - Emplois de l'ERE
#'
#' @description
#' Calcule, pour chaque produit ERE et chaque trimestre, l'ecart entre les
#' ressources totales et les emplois totaux :
#' \deqn{Ecart = Ressources\_tot - Emplois\_tot}
#'
#' Produit plusieurs niveaux de diagnostic :
#' \itemize{
#'   \item equilibre global par produit et trimestre ;
#'   \item detail des ressources par composante ;
#'   \item detail des emplois par composante ;
#'   \item alertes sur les desequilibres ;
#'   \item syntheses annuelles et par produit ;
#'   \item export Excel optionnel.
#' }
#'
#' @details
#' Objets attendus :
#' \describe{
#'   \item{\code{ressources_crt}}{Sortie \code{executer_ressources_ere()$ressources_crt}.
#'   Colonnes attendues : \code{annee}, \code{trimestre}, \code{Code_Produit},
#'   \code{composante}, \code{valeur_composante}.}
#'
#'   \item{\code{emplois_vpap}}{Sortie \code{executer_emplois_ere()$emplois_vpap}.
#'   Colonnes attendues : \code{annee}, \code{trimestre}, \code{Code_Produit},
#'   \code{composante}, \code{valeur_crt}, \code{valeur_ch}, \code{valeur_vpap}.}
#'
#'   \item{\code{ressources_chaine}}{Sortie \code{executer_ressources_ere()$ressources_chaine}.
#'   Colonnes attendues : \code{annee}, \code{trimestre}, \code{Code_Produit},
#'   \code{valeur_crt}, \code{valeur_vpap}, \code{valeur_ch}.}
#'
#'   \item{\code{ressources_vpap}}{Sortie optionnelle
#'   \code{executer_ressources_ere()$ressources_vpap}.
#'   Colonnes attendues : \code{annee}, \code{trimestre}, \code{Code_Produit},
#'   \code{composante}, \code{valeur_composante}.}
#' }
#'
#' @param ressources_crt Tibble des ressources ERE en courant par composante.
#' @param emplois_vpap Tibble des emplois ERE avec courant, chaine et VPAP.
#' @param ressources_chaine Tibble des ressources ERE agregees
#'   (courant, VPAP, chaine).
#' @param ressources_vpap Tibble optionnel des ressources ERE en volume VPAP
#'   par composante.
#' @param seuil_absolu Seuil absolu au-dela duquel un ecart est signale.
#' @param seuil_relatif Seuil relatif au-dela duquel un ecart est signale.
#' @param annees_filtre Vecteur d'annees a analyser. Si \code{NULL},
#'   toutes les annees disponibles sont retenues.
#' @param produits_filtre Vecteur de codes produit ERE a analyser. Si
#'   \code{NULL}, tous les produits sont retenus.
#' @param export_excel Chemin vers un fichier \code{.xlsx} pour exporter
#'   le diagnostic complet. Si \code{NULL}, aucun export n'est realise.
#'
#' @return Une liste nommee contenant :
#' \describe{
#'   \item{\code{equilibre_global}}{Table par produit et trimestre avec les
#'   totaux de ressources et d'emplois, les ecarts courant, VPAP et chaine,
#'   ainsi qu'un indicateur d'alerte.}
#'
#'   \item{\code{detail_ressources}}{Table longue par composante ressource
#'   avec les valeurs courantes et les parts dans le total.}
#'
#'   \item{\code{detail_emplois}}{Table longue par composante emploi avec
#'   les valeurs courantes, chainees, VPAP et leur part dans le total courant.}
#'
#'   \item{\code{alertes}}{Table des couples produit-trimestre en desequilibre
#'   au-dela des seuils definis.}
#'
#'   \item{\code{synthese_annuelle}}{Agregation annuelle des ecarts par produit.}
#'
#'   \item{\code{synthese_par_produit}}{Classement des produits selon le nombre
#'   d'alertes et l'importance moyenne des ecarts.}
#'
#'   \item{\code{parametres}}{Liste des parametres utilises pour le diagnostic.}
#' }
#'
#' @examples
#' \dontrun{
#' diag <- diagnostiquer_equilibre_ere(
#'   ressources_crt    = ere_res$ressources_crt,
#'   emplois_vpap      = ere_emp$emplois_vpap,
#'   ressources_chaine = ere_res$ressources_chaine,
#'   ressources_vpap   = ere_res$ressources_vpap
#' )
#'
#' diag$alertes
#' diag$synthese_par_produit
#'
#' inspecter_produit_ere(diag, "AZ001", annee = 2023)
#'
#' resumer_equilibre_par_trimestre(diag)
#' }
#'
#' @export
diagnostiquer_equilibre_ere <- function(ressources_crt,
                                        emplois_vpap,
                                        ressources_chaine,
                                        ressources_vpap   = NULL,
                                        seuil_absolu      = 1,
                                        seuil_relatif     = 0.001,
                                        annees_filtre     = NULL,
                                        produits_filtre   = NULL,
                                        export_excel      = NULL) {

  # ============================================================================
  # 0. VALIDATION DES ENTREES
  # ============================================================================

  .verifier_colonnes <- function(df, cols_requises, nom_arg) {
    manquantes <- setdiff(cols_requises, names(df))
    if (length(manquantes) > 0) {
      stop(
        "Colonnes manquantes dans `", nom_arg, "` : ",
        paste(manquantes, collapse = ", "), ".\n",
        "Colonnes disponibles : ", paste(names(df), collapse = ", "),
        call. = FALSE
      )
    }
  }

  .verifier_colonnes(
    ressources_crt,
    c("annee", "trimestre", "Code_Produit", "composante", "valeur_composante"),
    "ressources_crt"
  )
  .verifier_colonnes(
    emplois_vpap,
    c("annee", "trimestre", "Code_Produit", "composante",
      "valeur_crt", "valeur_ch", "valeur_vpap"),
    "emplois_vpap"
  )
  .verifier_colonnes(
    ressources_chaine,
    c("annee", "trimestre", "Code_Produit", "valeur_crt", "valeur_vpap", "valeur_ch"),
    "ressources_chaine"
  )
  if (!is.null(ressources_vpap)) {
    .verifier_colonnes(
      ressources_vpap,
      c("annee", "trimestre", "Code_Produit", "composante", "valeur_composante"),
      "ressources_vpap"
    )
  }

  message("\u25b6 Diagnostic ERE \u2014 \u00e9quilibre Ressources = Emplois...")

  # ============================================================================
  # 1. FILTRAGE OPTIONNEL
  # ============================================================================

  .filtrer <- function(df) {
    if (!is.null(annees_filtre))
      df <- dplyr::filter(df, .data$annee %in% annees_filtre)
    if (!is.null(produits_filtre))
      df <- dplyr::filter(df, .data$Code_Produit %in% produits_filtre)
    df
  }

  res_crt  <- .filtrer(ressources_crt)
  emp_vpap <- .filtrer(emplois_vpap)
  res_ch   <- .filtrer(ressources_chaine)

  # ============================================================================
  # 2. TOTAUX RESSOURCES PAR PRODUIT-TRIMESTRE
  # ============================================================================

  total_res_crt <- res_crt |>
    dplyr::group_by(.data$annee, .data$trimestre, .data$Code_Produit) |>
    dplyr::summarise(
      total_ressources_crt = sum(.data$valeur_composante, na.rm = TRUE),
      .groups = "drop"
    )

  total_res_vpap_ch <- res_ch |>
    dplyr::select(
      .data$annee, .data$trimestre, .data$Code_Produit,
      total_ressources_vpap = .data$valeur_vpap,
      total_ressources_ch   = .data$valeur_ch
    )

  # ============================================================================
  # 3. TOTAUX EMPLOIS PAR PRODUIT-TRIMESTRE
  # ============================================================================

  total_emp_crt <- emp_vpap |>
    dplyr::group_by(.data$annee, .data$trimestre, .data$Code_Produit) |>
    dplyr::summarise(
      total_emplois_crt  = sum(.data$valeur_crt,  na.rm = TRUE),
      total_emplois_vpap = sum(.data$valeur_vpap, na.rm = TRUE),
      total_emplois_ch   = sum(.data$valeur_ch,   na.rm = TRUE),
      .groups = "drop"
    )

  # ============================================================================
  # 4. EQUILIBRE GLOBAL
  # ============================================================================

  equilibre_global <- total_res_crt |>
    dplyr::full_join(total_res_vpap_ch, by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::full_join(total_emp_crt,     by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("total_"), ~ tidyr::replace_na(.x, 0)),

      ecart_crt  = .data$total_ressources_crt  - .data$total_emplois_crt,
      ecart_vpap = .data$total_ressources_vpap - .data$total_emplois_vpap,
      ecart_ch   = .data$total_ressources_ch   - .data$total_emplois_ch,

      ecart_rel_crt = dplyr::if_else(
        .data$total_ressources_crt == 0, NA_real_,
        abs(.data$ecart_crt) / abs(.data$total_ressources_crt)
      ),
      ecart_rel_vpap = dplyr::if_else(
        .data$total_ressources_vpap == 0, NA_real_,
        abs(.data$ecart_vpap) / abs(.data$total_ressources_vpap)
      ),
      ecart_rel_ch = dplyr::if_else(
        .data$total_ressources_ch == 0, NA_real_,
        abs(.data$ecart_ch) / abs(.data$total_ressources_ch)
      ),

      flag_alerte = (
        abs(.data$ecart_crt) > seuil_absolu |
          (!is.na(.data$ecart_rel_crt) & .data$ecart_rel_crt > seuil_relatif)
      )
    ) |>
    dplyr::arrange(.data$annee, .data$trimestre, .data$Code_Produit)

  # ============================================================================
  # 5. DETAIL COMPOSANTES — Ressources
  # ============================================================================

  detail_ressources <- res_crt |>
    dplyr::left_join(
      total_res_crt,
      by = c("annee", "trimestre", "Code_Produit")
    ) |>
    dplyr::mutate(
      valeur_crt_det      = .data$valeur_composante,
      pct_ressources_crt  = dplyr::if_else(
        .data$total_ressources_crt == 0, NA_real_,
        .data$valeur_composante / .data$total_ressources_crt * 100
      )
    ) |>
    dplyr::select(
      .data$annee, .data$trimestre, .data$Code_Produit,
      .data$composante,
      valeur_crt = .data$valeur_crt_det,
      .data$pct_ressources_crt
    ) |>
    dplyr::arrange(.data$annee, .data$trimestre, .data$Code_Produit, .data$composante)

  # Ajout VPAP si disponible
  if (!is.null(ressources_vpap)) {
    res_vpap_f <- .filtrer(ressources_vpap)

    total_res_vpap_comp <- res_vpap_f |>
      dplyr::group_by(.data$annee, .data$trimestre, .data$Code_Produit) |>
      dplyr::summarise(
        total_res_vpap_comp = sum(.data$valeur_composante, na.rm = TRUE),
        .groups = "drop"
      )

    detail_ressources <- detail_ressources |>
      dplyr::left_join(
        res_vpap_f |>
          dplyr::select(
            .data$annee, .data$trimestre, .data$Code_Produit,
            .data$composante,
            valeur_vpap = .data$valeur_composante
          ),
        by = c("annee", "trimestre", "Code_Produit", "composante")
      ) |>
      dplyr::left_join(
        total_res_vpap_comp,
        by = c("annee", "trimestre", "Code_Produit")
      ) |>
      dplyr::mutate(
        pct_ressources_vpap = dplyr::if_else(
          .data$total_res_vpap_comp == 0, NA_real_,
          tidyr::replace_na(.data$valeur_vpap, 0) / .data$total_res_vpap_comp * 100
        )
      ) |>
      dplyr::select(-.data$total_res_vpap_comp)
  }

  # ============================================================================
  # 6. DETAIL COMPOSANTES — Emplois
  # ============================================================================

  detail_emplois <- emp_vpap |>
    dplyr::left_join(
      total_emp_crt |>
        dplyr::select(.data$annee, .data$trimestre, .data$Code_Produit,
                      .data$total_emplois_crt),
      by = c("annee", "trimestre", "Code_Produit")
    ) |>
    dplyr::mutate(
      pct_emplois_crt = dplyr::if_else(
        .data$total_emplois_crt == 0, NA_real_,
        .data$valeur_crt / .data$total_emplois_crt * 100
      )
    ) |>
    dplyr::select(
      .data$annee, .data$trimestre, .data$Code_Produit, .data$composante,
      .data$valeur_crt, .data$valeur_vpap, .data$valeur_ch,
      .data$pct_emplois_crt
    ) |>
    dplyr::arrange(.data$annee, .data$trimestre, .data$Code_Produit, .data$composante)

  # ============================================================================
  # 7. ALERTES
  # ============================================================================

  alertes <- equilibre_global |>
    dplyr::filter(.data$flag_alerte) |>
    dplyr::mutate(
      type_alerte = dplyr::case_when(
        abs(.data$ecart_crt) > seuil_absolu &
          !is.na(.data$ecart_rel_crt) &
          .data$ecart_rel_crt > seuil_relatif ~ "Absolu ET Relatif",
        abs(.data$ecart_crt) > seuil_absolu   ~ "Absolu uniquement",
        TRUE                                   ~ "Relatif uniquement"
      ),
      sens_ecart = dplyr::if_else(
        .data$ecart_crt > 0,
        "Ressources > Emplois",
        "Emplois > Ressources"
      )
    ) |>
    dplyr::select(
      .data$annee, .data$trimestre, .data$Code_Produit,
      .data$total_ressources_crt, .data$total_emplois_crt,
      .data$ecart_crt, .data$ecart_rel_crt,
      .data$ecart_vpap, .data$ecart_ch,
      .data$type_alerte, .data$sens_ecart
    ) |>
    dplyr::arrange(dplyr::desc(abs(.data$ecart_crt)))

  # ============================================================================
  # 8. SYNTHESE ANNUELLE
  # ============================================================================

  synthese_annuelle <- equilibre_global |>
    dplyr::group_by(.data$annee, .data$Code_Produit) |>
    dplyr::summarise(
      ressources_annuelles_crt = sum(.data$total_ressources_crt,  na.rm = TRUE),
      emplois_annuels_crt      = sum(.data$total_emplois_crt,     na.rm = TRUE),
      ecart_annuel_crt         = sum(.data$ecart_crt,             na.rm = TRUE),
      ecart_annuel_vpap        = sum(.data$ecart_vpap,            na.rm = TRUE),
      ecart_annuel_ch          = sum(.data$ecart_ch,              na.rm = TRUE),
      nb_trim_en_alerte        = sum(.data$flag_alerte,           na.rm = TRUE),
      ecart_rel_max_crt        = max(abs(.data$ecart_rel_crt),    na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ecart_rel_annuel_crt = dplyr::if_else(
        .data$ressources_annuelles_crt == 0, NA_real_,
        abs(.data$ecart_annuel_crt) / abs(.data$ressources_annuelles_crt)
      )
    ) |>
    dplyr::arrange(.data$annee, dplyr::desc(abs(.data$ecart_annuel_crt)))

  # ============================================================================
  # 9. SYNTHESE PAR PRODUIT
  # ============================================================================

  synthese_par_produit <- equilibre_global |>
    dplyr::group_by(.data$Code_Produit) |>
    dplyr::summarise(
      nb_trimestres_total   = dplyr::n(),
      nb_trimestres_alerte  = sum(.data$flag_alerte,          na.rm = TRUE),
      pct_trimestres_alerte = mean(.data$flag_alerte,         na.rm = TRUE) * 100,
      ecart_moyen_abs_crt   = mean(abs(.data$ecart_crt),      na.rm = TRUE),
      ecart_max_abs_crt     = max(abs(.data$ecart_crt),       na.rm = TRUE),
      ecart_total_crt       = sum(.data$ecart_crt,            na.rm = TRUE),
      ecart_moyen_rel_crt   = mean(.data$ecart_rel_crt,       na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$nb_trimestres_alerte),
      dplyr::desc(.data$ecart_moyen_abs_crt)
    )

  # ============================================================================
  # 10. MESSAGES CONSOLE
  # ============================================================================

  n_prod    <- dplyr::n_distinct(equilibre_global$Code_Produit)
  n_trim    <- dplyr::n_distinct(
    paste(equilibre_global$annee, equilibre_global$trimestre)
  )
  n_obs     <- nrow(equilibre_global)
  n_alertes <- nrow(alertes)
  pct_alert <- if (n_obs > 0) round(n_alertes / n_obs * 100, 1) else 0
  ecart_max <- if (n_alertes > 0) round(max(abs(alertes$ecart_crt)), 2) else 0
  prod_max  <- if (n_alertes > 0)
    alertes$Code_Produit[which.max(abs(alertes$ecart_crt))] else "\u2014"

  message(
    "\u2705 Diagnostic ERE equilibre | ",
    n_prod, " produits | ", n_trim, " trimestres | ", n_obs, " observations"
  )
  if (n_alertes == 0) {
    message("\u2705 Aucune alerte : tous les produits sont en equilibre.")
  } else {
    message(
      "\u26a0\ufe0f  ", n_alertes, " alerte(s) (", pct_alert, "% des obs.) | ",
      "ecart max : ", ecart_max, " (", prod_max, ")"
    )
    prods_alertes <- unique(alertes$Code_Produit)
    message("   Produits concernes : ", paste(sort(prods_alertes), collapse = ", "))
  }

  # ============================================================================
  # 11. EXPORT EXCEL (optionnel)
  # ============================================================================

  if (!is.null(export_excel)) {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      warning(
        "Le package 'writexl' est requis pour l'export Excel. ",
        "Installez-le avec install.packages('writexl').",
        call. = FALSE
      )
    } else {
      eq_export <- equilibre_global |>
        dplyr::mutate(
          ecart_rel_crt_pct  = round(.data$ecart_rel_crt  * 100, 3),
          ecart_rel_vpap_pct = round(.data$ecart_rel_vpap * 100, 3),
          ecart_rel_ch_pct   = round(.data$ecart_rel_ch   * 100, 3),
          flag_alerte        = dplyr::if_else(.data$flag_alerte, "OUI", "non")
        ) |>
        dplyr::select(
          -.data$ecart_rel_crt, -.data$ecart_rel_vpap, -.data$ecart_rel_ch
        )

      writexl::write_xlsx(
        list(
          `0_Lisez-moi`         = .ere_lisez_moi(seuil_absolu, seuil_relatif),
          `1_Equilibre_global`  = eq_export,
          `2_Alertes`           = alertes,
          `3_Synthese_annuelle` = synthese_annuelle,
          `4_Synthese_produits` = synthese_par_produit,
          `5_Detail_ressources` = detail_ressources,
          `6_Detail_emplois`    = detail_emplois
        ),
        path = export_excel
      )
      message("\u2705 Diagnostic export\u00e9 : ", export_excel)
    }
  }

  # ============================================================================
  # 12. RETOUR
  # ============================================================================

  list(
    equilibre_global     = equilibre_global,
    detail_ressources    = detail_ressources,
    detail_emplois       = detail_emplois,
    alertes              = alertes,
    synthese_annuelle    = synthese_annuelle,
    synthese_par_produit = synthese_par_produit,
    parametres           = list(
      seuil_absolu    = seuil_absolu,
      seuil_relatif   = seuil_relatif,
      annees_filtre   = annees_filtre,
      produits_filtre = produits_filtre,
      n_produits      = n_prod,
      n_trimestres    = n_trim,
      n_alertes       = n_alertes
    )
  )
}


# ==============================================================================
# HELPERS INTERNES
# ==============================================================================

#' @noRd
.ere_lisez_moi <- function(seuil_absolu, seuil_relatif) {
  tibble::tribble(
    ~Rubrique,            ~Description,
    "Objet",              "Diagnostic equilibre Ressources = Emplois ERE par produit x trimestre",
    "Equation",           "Ecart = Total_Ressources - Total_Emplois (doit etre = 0)",
    "Composantes Res.",   "PRODUCTION, IMPORTATIONS, IMPOT Import/export, MARGE commerce/transport, TVA, IMPOT produit, Subventions",
    "Composantes Emp.",   "CI, EXPORTATIONS, CFmarch, CFnmarch, CFapu, CFisblsm, FBCF, VS, AOV",
    "Seuil absolu",       paste0("Ecart > ", seuil_absolu, " => alerte"),
    "Seuil relatif",      paste0("Ecart / Ressources > ", round(seuil_relatif * 100, 3), "% => alerte"),
    "Feuille 1",          "Equilibre_global : ecart R-E par produit x trimestre (crt, vpap, chaine)",
    "Feuille 2",          "Alertes : produits-trimestres depassant au moins un seuil",
    "Feuille 3",          "Synthese_annuelle : agregation annuelle des ecarts par produit",
    "Feuille 4",          "Synthese_produits : classement par nombre d'alertes et ecart moyen",
    "Feuille 5",          "Detail_ressources : contribution de chaque composante ressource",
    "Feuille 6",          "Detail_emplois : contribution de chaque composante emploi",
    "Interpretation",     "Un ecart persistant indique un defaut de bouclage. Verifier : ratios ERE, methode solde, benchmarking imports/exports.",
    "Contact",            "Service des Comptes Nationaux"
  )
}


# ==============================================================================
# FONCTIONS COMPLEMENTAIRES D'EXPLORATION
# ==============================================================================

#' Inspecter l'equilibre ERE pour un produit donne
#'
#' Affiche un resume tabulaire pour un produit precis : ressources et emplois
#' par trimestre et par composante, avec l'ecart ligne a ligne.
#'
#' @param diag Sortie de \code{diagnostiquer_equilibre_ere()}.
#' @param code_produit Code produit ERE a inspecter (ex : \code{"AZ001"}).
#' @param annee Annee a inspecter. Si \code{NULL}, toutes les annees.
#'
#' @return Tibble : annee, trimestre, composante, cote (Ressource/Emploi/Ecart),
#'   valeur_crt, pct.
#' @export
inspecter_produit_ere <- function(diag, code_produit, annee = NULL) {

  if (!code_produit %in% unique(diag$equilibre_global$Code_Produit)) {
    stop(
      "Produit '", code_produit, "' absent du diagnostic.\n",
      "Produits disponibles : ",
      paste(sort(unique(diag$equilibre_global$Code_Produit)), collapse = ", "),
      call. = FALSE
    )
  }

  .f <- function(df) {
    df <- dplyr::filter(df, .data$Code_Produit == code_produit)
    if (!is.null(annee)) df <- dplyr::filter(df, .data$annee == annee)
    df
  }

  res_f <- .f(diag$detail_ressources) |>
    dplyr::transmute(
      .data$annee, .data$trimestre, .data$composante,
      cote = "Ressource", valeur_crt = .data$valeur_crt, pct = .data$pct_ressources_crt
    )

  emp_f <- .f(diag$detail_emplois) |>
    dplyr::transmute(
      .data$annee, .data$trimestre, .data$composante,
      cote = "Emploi", valeur_crt = .data$valeur_crt, pct = .data$pct_emplois_crt
    )

  eq_f <- .f(diag$equilibre_global) |>
    dplyr::transmute(
      .data$annee, .data$trimestre,
      composante = "== ECART ==", cote = "Ecart (R-E)",
      valeur_crt = .data$ecart_crt,
      pct = .data$ecart_rel_crt * 100
    )

  dplyr::bind_rows(res_f, emp_f, eq_f) |>
    dplyr::arrange(.data$annee, .data$trimestre, .data$cote, .data$composante)
}


#' Resumer les ecarts ERE par trimestre (tous produits confondus)
#'
#' Agrege les ecarts de tous les produits par trimestre pour une vue
#' macroeconomique du bouclage ERE.
#'
#' @param diag Sortie de \code{diagnostiquer_equilibre_ere()}.
#'
#' @return Tibble : annee, trimestre, ressources_total_crt, emplois_total_crt,
#'   ecart_total_crt, ecart_total_vpap, ecart_total_ch, nb_produits_alerte,
#'   ecart_rel_total_crt (en \%), periode.
#' @export
resumer_equilibre_par_trimestre <- function(diag) {

  diag$equilibre_global |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(
      ressources_total_crt = sum(.data$total_ressources_crt,  na.rm = TRUE),
      emplois_total_crt    = sum(.data$total_emplois_crt,     na.rm = TRUE),
      ecart_total_crt      = sum(.data$ecart_crt,             na.rm = TRUE),
      ecart_total_vpap     = sum(.data$ecart_vpap,            na.rm = TRUE),
      ecart_total_ch       = sum(.data$ecart_ch,              na.rm = TRUE),
      nb_produits_alerte   = sum(.data$flag_alerte,           na.rm = TRUE),
      nb_produits_total    = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ecart_rel_total_crt = dplyr::if_else(
        .data$ressources_total_crt == 0, NA_real_,
        abs(.data$ecart_total_crt) / abs(.data$ressources_total_crt) * 100
      ),
      periode = paste0(.data$annee, "T", .data$trimestre)
    ) |>
    dplyr::arrange(.data$annee, .data$trimestre)
}
