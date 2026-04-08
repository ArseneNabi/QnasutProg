#' @import dplyr
#' @import tidyr
NULL

.norm_text_ere_equilibrage <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  gsub("[[:space:]]+", " ", x)
}

.referentiel_composantes_ere <- function() {
  tibble::tribble(
    ~composante_canonique, ~alias,
    "PRODUCTION", "PRODUCTION",
    "IMPORTATIONS", "IMPORTATIONS",
    "IMPOT sur Import", "IMPOT sur Import",
    "IMPOT sur export", "IMPOT sur export",
    "MARGE de commerce", "MARGE de commerce",
    "MARGE de transport", "MARGE de transport",
    "TVA", "TVA",
    "IMPOT sur produit", "IMPOT sur produit",
    "Subventions", "Subventions",
    "CI Prix d'acquisition", "CI",
    "CI Prix d'acquisition", "CI Prix d'acquisition",
    "CF Marchande Menage Prix d'acquisition", "CFmarch",
    "CF Marchande Menage Prix d'acquisition", "CF Marchande Menage Prix d'acquisition",
    "CF Non Marchande Menage Prix d'acquisition", "CFnmarch",
    "CF Non Marchande Menage Prix d'acquisition", "CF Non Marchande Menage Prix d'acquisition",
    "CF Non Marchande APU Prix d'acquisition", "CFapu",
    "CF Non Marchande APU Prix d'acquisition", "CF Non Marchande APU Prix d'acquisition",
    "CF Non Marchande ISBL Prix d'acquisition", "CFisblsm",
    "CF Non Marchande ISBL Prix d'acquisition", "CF Non Marchande ISBL Prix d'acquisition",
    "FBCF Prix d'acquisition", "FBCF",
    "FBCF Prix d'acquisition", "FBCF Prix d'acquisition",
    "VS Prix d'acquisition", "VS",
    "VS Prix d'acquisition", "VS Prix d'acquisition",
    "Exportation Prix d'acquisition", "EXPORTATIONS",
    "Exportation Prix d'acquisition", "Exportation Prix d'acquisition",
    "Exportation Prix d'acquisition", "Exportations de biens et services",
    "Aquisition moyen cession de origen de valeur Prix d'acquisition", "AOV",
    "Aquisition moyen cession de origen de valeur Prix d'acquisition", "Aquisition moyen cession de origen de valeur Prix d'acquisition"
  ) |>
    dplyr::mutate(alias_norm = .norm_text_ere_equilibrage(.data$alias))
}

.standardiser_composante_ere <- function(x) {
  ref <- .referentiel_composantes_ere()
  x_chr <- as.character(x)
  x_norm <- .norm_text_ere_equilibrage(x_chr)
  idx <- match(x_norm, ref$alias_norm)

  x_std <- trimws(gsub("[[:space:]]+", " ", x_chr))
  x_std[!is.na(idx)] <- ref$composante_canonique[idx[!is.na(idx)]]
  x_std[is.na(x)] <- NA_character_
  x_std
}

.standardiser_colonne_composante_ere <- function(df, col = "composante") {
  if (!col %in% names(df)) {
    return(df)
  }

  df[[col]] <- .standardiser_composante_ere(df[[col]])
  df
}

.trouver_nom_cna_ere <- function(cna_ere_struct, composante) {
  noms <- names(cna_ere_struct)
  if (length(noms) == 0) {
    return(NA_character_)
  }

  ref <- .referentiel_composantes_ere()
  comp_std <- .standardiser_composante_ere(composante)
  alias_candidats <- ref |>
    dplyr::filter(.data$composante_canonique == comp_std) |>
    dplyr::pull(.data$alias)

  candidats_norm <- unique(.norm_text_ere_equilibrage(c(comp_std, composante, alias_candidats)))
  noms_norm <- .norm_text_ere_equilibrage(noms)

  idx <- match(candidats_norm, noms_norm, nomatch = 0L)
  idx <- idx[idx > 0]
  if (length(idx) > 0) {
    return(noms[idx[1]])
  }

  idx <- which(vapply(noms_norm, function(nm) {
    any(grepl(nm, candidats_norm, fixed = TRUE) |
          grepl(candidats_norm, nm, fixed = TRUE))
  }, logical(1)))

  if (length(idx) == 1) {
    return(noms[idx])
  }

  NA_character_
}

.harmoniser_periodes_produit_ere <- function(data_produit) {
  composantes <- data_produit |>
    dplyr::distinct(.data$type_bloc, .data$composante)
  n_comp <- nrow(composantes)

  periodes_avant <- data_produit |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  periodes_communes <- data_produit |>
    dplyr::count(.data$annee, .data$trimestre, name = "n_comp_presentes") |>
    dplyr::filter(.data$n_comp_presentes == n_comp) |>
    dplyr::select(annee, trimestre)

  if (nrow(periodes_communes) == 0) {
    stop("Aucune periode commune complete entre les composantes du produit.",
         call. = FALSE)
  }

  if (nrow(periodes_communes) < nrow(periodes_avant)) {
    n_trim_sup <- nrow(periodes_avant) - nrow(periodes_communes)
    derniers <- periodes_avant |>
      dplyr::anti_join(periodes_communes, by = c("annee", "trimestre")) |>
      dplyr::arrange(.data$annee, .data$trimestre)

    message(
      "  Harmonisation des periodes : ", n_trim_sup,
      " trimestre(s) incomplet(s) supprime(s) [",
      paste(paste0(derniers$annee, "T", derniers$trimestre), collapse = ", "),
      "]"
    )
  }

  data_produit |>
    dplyr::inner_join(periodes_communes, by = c("annee", "trimestre"))
}

.choisir_lambda_cholette_ere <- function(data_prepared, lambda = NULL) {
  if (!is.null(lambda)) {
    return(lambda)
  }

  vals_trim <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::pull(.data$valeur_trimestrielle)

  vals_ann <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::pull(.data$valeur_annuelle)

  if (any(vals_trim <= 0, na.rm = TRUE) || any(vals_ann <= 0, na.rm = TRUE)) {
    return(0)
  }

  0.8
}

.appeler_cholette_multivarie <- function(call_cholette, xlist, tcvector,
                                         ccvector, rho, lambda) {
  args <- list(
    xlist = xlist,
    tcvector = tcvector,
    ccvector = ccvector
  )

  fml <- tryCatch(names(formals(call_cholette)), error = function(e) character(0))
  if ("rho" %in% fml) {
    args$rho <- rho
  }
  if ("lambda" %in% fml) {
    args$lambda <- lambda
  }

  do.call(call_cholette, args)
}

# ==============================================================================
# MODULE D'EQUILIBRAGE ERE MULTIVARIÉ — QnaSut
# ==============================================================================
#
# PRINCIPE MÉTIER
# ===============
# Après benchmarking, les ressources et emplois ERE sont calés sur leurs
# cibles CNA annuelles respectives. Il subsiste néanmoins des déséquilibres
# trimestriels produit par produit :
#   z(t) = Ressources_tot(t) - Emplois_figés(t)  ≠  somme_j x_j(t)
#
# Ce module répartit ces déséquilibres sur les composantes emplois
# "ajustables" définies dans le modèle de bouclage (Methode_ERE.xlsx).
#
# STRUCTURE DES DONNÉES D'ENTRÉE (issues du pipeline CNT)
# ========================================================
# ere_res$ressources_crt / ressources_vpap : tibble long
#   colonnes : annee, trimestre, Code_Produit, valeur_composante, composante
#
# ere_emp$emplois_crt / emplois_vpap : tibble long
#   colonnes : annee, trimestre, Code_Produit, valeur_composante, composante
#
# cna_ere_struct : liste de listes, accès via
#   cna_ere_struct[[composante]]$CnaErECrt  (courant)
#   cna_ere_struct[[composante]]$CnaErEVol  (volume VPAP)
#
# DEUX TYPES DE PRIX TRAITÉS EN PARALLÈLE
# ========================================
# - "crt"  : prix courants  (ere_res$ressources_crt  / ere_emp$emplois_crt)
# - "vpap" : volumes VPAP   (ere_res$ressources_vpap / ere_emp$emplois_vpap)
#
# DEUX MÉTHODES D'ÉQUILIBRAGE
# ============================
# N ≥ 2 composantes ajustables : multivariatecholette (rjd3bench)
# N = 1 composante ajustable   : ajustement direct  x_j(t) ← x_j(t) + z(t)
#
# RECENTRAGE DES CONTRAINTES
# ===========================
# Précondition Cholette : sum_t z(t) = sum_j Y_j(annee)  pour chaque année.
# Si cette condition n'est pas satisfaite, on applique un offset additif :
#   offset = [sum_t z(t) - sum_j Y_j(annee)] / n_trim
#   z_corr(t) = z(t) - offset
# (NE PAS utiliser de distribution plate ni de recalage multiplicatif)
# ==============================================================================


# ==============================================================================
# 0. FONCTIONS UTILITAIRES INTERNES
# ==============================================================================

#' Recentrer les contraintes trimestrielles par année (offset additif)
#'
#' Garantit que sum_t(z_t) = cible_annuelle pour chaque année, via :
#' offset = (sum_t(z_t) - cible) / n ; z_corr(t) = z(t) - offset.
#'
#' @param table_contrainte Tibble avec \code{annee}, \code{trimestre},
#'   \code{contrainte_contemp}.
#' @param cibles_tot Tibble avec \code{annee}, \code{cible_tot}. Si NULL,
#'   la cible est 0.
#' @param tol Tolérance : pas de correction si |écart| <= tol.
#'
#' @return Tibble enrichi avec \code{contrainte_initiale}, \code{offset},
#'   \code{contrainte_contemp} (corrigée), \code{somme_avant},
#'   \code{somme_apres}, \code{cible_tot_ann}.
#' @export
recentrer_contraintes_trimestrielles_par_annee <- function(table_contrainte,
                                                           cibles_tot = NULL,
                                                           tol = 1e-6) {
  if (!all(c("annee", "trimestre", "contrainte_contemp") %in%
           names(table_contrainte)))
    stop("table_contrainte : colonnes requises = annee, trimestre, contrainte_contemp.",
         call. = FALSE)

  ann <- table_contrainte |>
    dplyr::group_by(.data$annee) |>
    dplyr::summarise(somme_avant = sum(.data$contrainte_contemp, na.rm = TRUE),
                     n_trim      = dplyr::n(), .groups = "drop")

  if (!is.null(cibles_tot)) {
    ann <- dplyr::left_join(ann, cibles_tot, by = "annee") |>
      dplyr::mutate(cible_tot_ann = dplyr::coalesce(.data$cible_tot,
                                                    .data$somme_avant))
  } else {
    ann <- dplyr::mutate(ann, cible_tot_ann = 0)
  }

  ann <- ann |>
    dplyr::mutate(
      ecart  = .data$somme_avant - .data$cible_tot_ann,
      offset = dplyr::if_else(abs(.data$ecart) > tol,
                              .data$ecart / .data$n_trim, 0)
    )

  n_corr <- sum(abs(ann$offset) > 0, na.rm = TRUE)
  if (n_corr > 0)
    message("  Recentrage offset : ", n_corr, " annee(s) | offset max = ",
            round(max(abs(ann$offset), na.rm = TRUE), 4))

  table_contrainte |>
    dplyr::rename(contrainte_initiale = contrainte_contemp) |>
    dplyr::left_join(
      dplyr::select(ann, annee, offset, cible_tot_ann, somme_avant, n_trim),
      by = "annee"
    ) |>
    dplyr::mutate(
      contrainte_contemp = .data$contrainte_initiale - .data$offset
    ) |>
    dplyr::group_by(.data$annee) |>
    dplyr::mutate(somme_apres = sum(.data$contrainte_contemp, na.rm = TRUE)) |>
    dplyr::ungroup()
}


# Construire le retour standardisé
.construire_retour_equilibrage <- function(series_ajustees, prep,
                                           cibles_annuelles,
                                           code_produit, methode,
                                           res_brut) {
  ctrl_contemp <- series_ajustees |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(somme_aj = sum(.data$valeur_apres, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(
      dplyr::select(prep$table_contrainte, annee, trimestre, contrainte_contemp),
      by = c("annee", "trimestre")
    ) |>
    dplyr::mutate(ecart_contemporain = .data$somme_aj - .data$contrainte_contemp)

  ctrl_ann <- series_ajustees |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(somme_apres = sum(.data$valeur_apres, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(cibles_annuelles, by = c("composante", "annee")) |>
    dplyr::mutate(
      contrainte_temporelle_active = .data$annee %in%
        prep$annees_contrainte_temporelle,
      ecart_annuel = .data$somme_apres - .data$valeur_annuelle
    )

  list(
    series_ajustees = series_ajustees,
    diagnostic = list(
      code_produit           = code_produit,
      methode                = methode,
      composantes_ajustables = prep$composantes_ajustables,
      composantes_figees     = prep$composantes_figees,
      recentrage_applique    = prep$recentrage_applique,
      table_contrainte       = prep$table_contrainte,
      controle_contemporain  = ctrl_contemp,
      controle_annuel        = ctrl_ann,
      annees_contrainte_temporelle = prep$annees_contrainte_temporelle
    ),
    contraintes = list(xlist    = prep$xlist,
                       tcvector = prep$tcvector,
                       ccvector = prep$ccvector),
    resultat_brut = res_brut,
    methode       = methode
  )
}


# ==============================================================================
# 1. CONSTRUCTION DE data_ere DEPUIS LES RÉSULTATS BENCHMARKÉS
# ==============================================================================

#' Construire la table longue ERE pour l'équilibrage (un type de prix)
#'
#' @description
#' Transforme les résultats benchmarkés des ressources et emplois ERE en une
#' table longue au format attendu par
#' \code{equilibrer_produit_ere_multivariatecholette()}.
#'
#' Les données trimestrielles sont celles directement issues du pipeline
#' (\code{ere_res$ressources_crt} et \code{ere_emp$emplois_crt}). Les cibles annuelles
#' CNA sont extraites de \code{cna_ere_struct} pour alimenter le \code{tcvector}.
#'
#' @param ressources_bench Tibble long des ressources benchmarkées pour un
#'   type de prix. Colonnes attendues : \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{composante}, \code{valeur_composante}.
#' @param emplois_bench Tibble long des emplois benchmarkés. Mêmes colonnes.
#' @param cna_ere_struct Liste de structures CNA ERE (issue de
#'   \code{charger_donnees_cnt()$cna_ere_struct}).
#' @param type_cna Clé CNA dans \code{cna_ere_struct} pour les cibles
#'   annuelles : \code{"CnaErECrt"} (courant) ou \code{"CnaErEVol"} (volume).
#' @param codes_produits Vecteur optionnel de \code{Code_Produit} à retenir.
#'   Si \code{NULL}, tous les produits présents dans les données sont traités.
#'
#' @return Tibble long avec colonnes : \code{Code_Produit}, \code{annee},
#'   \code{trimestre}, \code{composante}, \code{valeur_trimestrielle},
#'   \code{valeur_annuelle}, \code{type_bloc}.
#' @export
construire_data_ere_depuis_pipeline <- function(ressources_bench,
                                                emplois_bench,
                                                cna_ere_struct,
                                                type_cna    = "CnaErECrt",
                                                codes_produits = NULL) {

  cols_cles <- c("annee", "trimestre", "Code_Produit", "composante")

  # Detecter la colonne de valeur (peut s'appeler valeur_composante,
  # valeur_cal, valeur, etc. selon la fonction qui a produit les donnees)
  .detecter_col_valeur <- function(df, nm_df) {
    candidats <- c("valeur_composante", "valeur_cal", "valeur_vpap", "valeur_vpap", "valeur", "valeur_trim")
    trouve    <- intersect(candidats, names(df))
    if (length(trouve) == 0)
      stop(nm_df, " : aucune colonne de valeur trouvee parmi : ",
           paste(candidats, collapse = ", "),
           "\n  Colonnes disponibles : ", paste(names(df), collapse = ", "),
           call. = FALSE)
    if (length(trouve) > 1)
      message("  ", nm_df, " : plusieurs colonnes de valeur detectees, ",
              "utilisation de '", trouve[1], "'")
    trouve[1]
  }

  for (nm in c("ressources_bench", "emplois_bench")) {
    df   <- get(nm)
    manq <- setdiff(cols_cles, names(df))
    if (length(manq) > 0)
      stop(nm, " : colonnes manquantes : ", paste(manq, collapse = ", "),
           call. = FALSE)
    .detecter_col_valeur(df, nm)  # valide silencieusement
  }

  col_val_res <- .detecter_col_valeur(ressources_bench, "ressources_bench")
  col_val_emp <- .detecter_col_valeur(emplois_bench,    "emplois_bench")

  ressources_bench <- .standardiser_colonne_composante_ere(ressources_bench)
  emplois_bench    <- .standardiser_colonne_composante_ere(emplois_bench)

  if (!is.null(codes_produits)) {
    ressources_bench <- dplyr::filter(ressources_bench,
                                      .data$Code_Produit %in% codes_produits)
    emplois_bench    <- dplyr::filter(emplois_bench,
                                      .data$Code_Produit %in% codes_produits)
  }

  # --- Cibles annuelles CNA par composante et produit ---
  .extraire_cible_ann <- function(composante, type_c) {
    composante_std <- .standardiser_composante_ere(composante)
    nom_reel <- .trouver_nom_cna_ere(cna_ere_struct, composante_std)
    if (is.na(nom_reel)) return(tibble::tibble())
    sub      <- cna_ere_struct[[nom_reel]][[type_c]]
    if (is.null(sub)) return(tibble::tibble())
    tryCatch(
      QnaSut::pivoter_ere_long(sub, type_c, nom_reel) |>
        dplyr::group_by(.data$Code_Produit, .data$annee) |>
        dplyr::summarise(valeur_annuelle = sum(.data$valeur, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate(composante = composante_std),
      error = function(e) tibble::tibble()
    )
  }

  # Composantes présentes dans les données
  comps_res <- unique(ressources_bench$composante)
  comps_emp <- unique(emplois_bench$composante)
  all_comps <- union(comps_res, comps_emp)

  cibles_ann <- purrr::map_dfr(all_comps, .extraire_cible_ann, type_c = type_cna)

  # --- Assemblage ---
  .to_long <- function(df, col_val, type_b) {
    df |>
      dplyr::rename(valeur_trimestrielle = dplyr::all_of(col_val)) |>
      dplyr::mutate(type_bloc = type_b)
  }

  data_long <- dplyr::bind_rows(
    .to_long(ressources_bench, col_val_res, "ressource"),
    .to_long(emplois_bench,    col_val_emp, "emploi")
  )

  # Jointure avec cibles annuelles
  if (nrow(cibles_ann) > 0) {
    data_long <- dplyr::left_join(
      data_long,
      cibles_ann,
      by = c("Code_Produit", "annee", "composante")
    )
  } else {
    data_long <- dplyr::mutate(data_long, valeur_annuelle = NA_real_)
  }

  # Pour les composantes sans cible CNA : utiliser la somme annuelle
  # des valeurs benchmarkées comme proxy de cible (garantit tcvector ≈ trivial)
  data_long <- data_long |>
    dplyr::group_by(.data$Code_Produit, .data$composante, .data$annee) |>
    dplyr::mutate(
      valeur_annuelle = dplyr::if_else(
        is.na(.data$valeur_annuelle),
        sum(.data$valeur_trimestrielle, na.rm = TRUE),
        .data$valeur_annuelle
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(Code_Produit, annee, trimestre, composante,
                  valeur_trimestrielle, valeur_annuelle, type_bloc) |>
    dplyr::arrange(.data$Code_Produit, .data$annee, .data$trimestre,
                   .data$type_bloc, .data$composante)

  message("\u2705 data_ere construit | ",
          nrow(data_long), " lignes | ",
          dplyr::n_distinct(data_long$Code_Produit), " produits | ",
          "type_cna = ", type_cna)

  data_long
}


# ==============================================================================
# 2. PRÉPARATION DES DONNÉES D'UN PRODUIT
# ==============================================================================

#' Préparer un produit ERE pour l'équilibrage multivarié
#'
#' @param data_produit Tibble long d'un seul produit. Colonnes requises :
#'   \code{Code_Produit}, \code{annee}, \code{trimestre}, \code{composante},
#'   \code{valeur_trimestrielle}, \code{valeur_annuelle}, \code{type_bloc}.
#' @param composantes_ajustables Vecteur des composantes emplois autorisées.
#' @return Tibble enrichi avec colonne \code{ajustable} (logique).
#' @export
preparer_donnees_equilibrage_ere_produit <- function(data_produit,
                                                     composantes_ajustables) {
  cols_req <- c("Code_Produit", "annee", "trimestre", "composante",
                "valeur_trimestrielle", "valeur_annuelle", "type_bloc")
  manq <- setdiff(cols_req, names(data_produit))
  if (length(manq) > 0)
    stop("Colonnes manquantes : ", paste(manq, collapse = ", "), call. = FALSE)
  if (length(unique(data_produit$Code_Produit)) != 1)
    stop("data_produit : un seul Code_Produit attendu.", call. = FALSE)
  if (length(composantes_ajustables) == 0)
    stop("Aucune composante ajustable fournie.", call. = FALSE)

  data_prep <- data_produit |>
    dplyr::mutate(
      type_bloc = tolower(trimws(.data$type_bloc)),
      ajustable = .data$type_bloc == "emploi" &
        .data$composante %in% composantes_ajustables
    )

  if (!all(unique(data_prep$type_bloc) %in% c("ressource", "emploi")))
    stop("type_bloc : seules 'ressource' et 'emploi' sont acceptées.", call. = FALSE)

  dups <- data_prep |>
    dplyr::count(.data$Code_Produit, .data$annee, .data$trimestre,
                 .data$composante, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(dups) > 0)
    stop("Doublons sur (Code_Produit, annee, trimestre, composante).", call. = FALSE)

  data_prep <- .harmoniser_periodes_produit_ere(data_prep)

  annees_incompletes <- data_prep |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::count(.data$annee, name = "n_trim") |>
    dplyr::filter(.data$n_trim < 4L) |>
    dplyr::pull(.data$annee)

  if (length(annees_incompletes) > 0) {
    message(
      "  Cibles annuelles recalculees sur periodes retenues pour annee(s) incomplete(s) : ",
      paste(annees_incompletes, collapse = ", ")
    )

    data_prep <- data_prep |>
      dplyr::group_by(.data$Code_Produit, .data$type_bloc,
                      .data$composante, .data$annee) |>
      dplyr::mutate(
        valeur_annuelle = dplyr::if_else(
          .data$annee %in% annees_incompletes,
          sum(.data$valeur_trimestrielle, na.rm = TRUE),
          .data$valeur_annuelle
        )
      ) |>
      dplyr::ungroup()
  }

  comp_abs <- setdiff(composantes_ajustables,
                      unique(data_prep$composante[data_prep$type_bloc == "emploi"]))
  if (length(comp_abs) > 0)
    stop("Composantes ajustables absentes des emplois : ",
         paste(comp_abs, collapse = ", "), call. = FALSE)

  data_prep
}


# ==============================================================================
# 3. PRÉPARATION DES CONTRAINTES
# ==============================================================================

#' Préparer les contraintes d'équilibrage ERE (un produit)
#'
#' @description
#' Construit \code{xlist}, \code{tcvector} et \code{ccvector} pour
#' \code{rjd3bench::multivariatecholette()}, avec recentrage par offset
#' additif si nécessaire.
#'
#' @section Contrainte contemporaine :
#' \deqn{z(t) = \text{Ressources}_{tot}(t) - \text{Emplois\_figes}(t)}
#' Les ressources totales incluent toutes les composantes ressources ERE
#' (PRODUCTION, IMPORTATIONS, impôts, marges, taxes, subventions).
#'
#' @section Cohérence obligatoire :
#' \deqn{\sum_t z(t) = \sum_j Y_j(\text{annee}) \quad \forall \text{annee}}
#' Si cette condition n'est pas vérifiée, un offset additif est appliqué par
#' année pour recentrer les contraintes trimestrielles.
#'
#' @param data_prepared Sortie de
#'   \code{preparer_donnees_equilibrage_ere_produit()}.
#' @param tol_recentrage Tolérance pour le recentrage. Défaut \code{1e-6}.
#'
#' @return Liste : \code{xlist}, \code{tcvector}, \code{ccvector},
#'   \code{table_contrainte}, \code{composantes_ajustables},
#'   \code{composantes_figees}, \code{code_produit},
#'   \code{recentrage_applique}.
#' @export
preparer_contraintes_equilibrage_ere_produit <- function(data_prepared,
                                                         tol_recentrage = 1e-6) {
  if (!"ajustable" %in% names(data_prepared))
    stop("data_prepared doit contenir la colonne 'ajustable'.", call. = FALSE)

  code_produit <- unique(data_prepared$Code_Produit)
  if (length(code_produit) != 1)
    stop("data_prepared : un seul Code_Produit attendu.", call. = FALSE)

  annees_contrainte_temporelle <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::count(.data$annee, name = "n_trim") |>
    dplyr::filter(.data$n_trim == 4L) |>
    dplyr::pull(.data$annee)

  annees_incompletes <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::count(.data$annee, name = "n_trim") |>
    dplyr::filter(.data$n_trim < 4L) |>
    dplyr::pull(.data$annee)

  if (length(annees_incompletes) > 0) {
    message(
      "  Contrainte temporelle complete non active pour annee(s) incomplete(s) : ",
      paste(annees_incompletes, collapse = ", ")
    )
  }

  composantes_ajustables <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::distinct(.data$composante) |>
    dplyr::pull(.data$composante)
  if (length(composantes_ajustables) == 0)
    stop("Aucune composante ajustable dans data_prepared.", call. = FALSE)

  # --- Cibles annuelles CNA par composante ajustable ---
  cibles_annuelles <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle),
                     .groups = "drop")

  if (any(is.na(cibles_annuelles$valeur_annuelle)))
    stop("Cibles annuelles NA pour une composante ajustable.", call. = FALSE)

  cibles_tot <- cibles_annuelles |>
    dplyr::group_by(.data$annee) |>
    dplyr::summarise(cible_tot = sum(.data$valeur_annuelle, na.rm = TRUE),
                     .groups = "drop")

  # --- Contrainte contemporaine brute ---
  # z(t) = Ressources_tot(t) - Emplois_figés(t)
  table_brute <- data_prepared |>
    dplyr::group_by(.data$annee, .data$trimestre) |>
    dplyr::summarise(
      ressources    = sum(.data$valeur_trimestrielle[
        .data$type_bloc == "ressource"], na.rm = TRUE),
      emplois_figes = sum(.data$valeur_trimestrielle[
        .data$type_bloc == "emploi" & !.data$ajustable], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(contrainte_contemp = .data$ressources - .data$emplois_figes)

  if (any(!is.finite(table_brute$contrainte_contemp)))
    stop("Contrainte contemporaine contient NA/Inf.", call. = FALSE)

  # --- Recentrage par offset additif ---
  table_contrainte <- recentrer_contraintes_trimestrielles_par_annee(
    table_contrainte = table_brute,
    cibles_tot       = cibles_tot,
    tol              = tol_recentrage
  )

  recentrage_applique <- any(abs(table_contrainte$offset) > 0, na.rm = TRUE)

  # --- Vérification cohérence résiduelle ---
  ecart_res <- table_contrainte |>
    dplyr::group_by(.data$annee) |>
    dplyr::summarise(
      ecart = abs(dplyr::first(.data$somme_apres) -
                    dplyr::first(.data$cible_tot_ann)),
      .groups = "drop"
    )
  if (max(ecart_res$ecart, na.rm = TRUE) > tol_recentrage * 1000)
    warning("[", code_produit, "] Ecart residuel apres recentrage : ",
            round(max(ecart_res$ecart, na.rm = TRUE), 4), call. = FALSE)

  # --- Construction de xlist ---
  annees_trim <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  start_ts <- c(min(annees_trim$annee), min(annees_trim$trimestre))
  .ts4 <- function(v) stats::ts(v, start = start_ts, frequency = 4L)

  # Contrainte contemporaine (z)
  xlist <- list(
    CONTRAINTE_CONTEMP = table_contrainte |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$contrainte_contemp) |>
      .ts4()
  )

  # Indicateurs sources (composantes ajustables)
  for (comp in composantes_ajustables) {
    vals <- data_prepared |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$valeur_trimestrielle)
    xlist[[comp]] <- .ts4(vals)
  }

  # Cibles annuelles (séries annuelles)
  for (comp in composantes_ajustables) {
    y_nm  <- paste0("Y_", make.names(comp))
    y_ann <- dplyr::filter(cibles_annuelles, .data$composante == comp) |>
      dplyr::arrange(.data$annee)
    xlist[[y_nm]] <- stats::ts(y_ann$valeur_annuelle,
                               start = min(y_ann$annee), frequency = 1L)
  }

  # --- Vecteurs de contraintes ---
  tcvector <- purrr::map_chr(composantes_ajustables, function(comp)
    paste0("Y_", make.names(comp), " = sum(", comp, ")")
  )
  ccvector <- paste0("CONTRAINTE_CONTEMP = ",
                     paste(composantes_ajustables, collapse = " + "))

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
    recentrage_applique    = recentrage_applique,
    annees_contrainte_temporelle = annees_contrainte_temporelle
  )
}


# ==============================================================================
# 4. ÉQUILIBRAGE D'UN PRODUIT
# ==============================================================================

#' Équilibrer un produit ERE (courant ou VPAP)
#'
#' @description
#' Deux méthodes selon le nombre de composantes ajustables :
#'
#' \strong{N ≥ 2 composantes} : \code{rjd3bench::multivariatecholette()}.
#' Si l'appel standard échoue, un second essai est tenté sans \code{tcvector}.
#' En cas d'échec définitif, le produit est marqué \code{"echec"} — aucune
#' distribution plate n'est appliquée.
#'
#' \strong{N = 1 composante} : ajustement direct
#' \eqn{x_j(t)_{\text{après}} = x_j(t)_{\text{avant}} + z(t)}.
#'
#' @param data_produit Table longue d'un seul produit (pipeline benchmarké).
#' @param composantes_ajustables Vecteur des composantes emplois autorisées.
#' @param tol_recentrage Tolérance pour le recentrage des contraintes.
#' @param rho Paramètre de lissage transmis à \code{multivariatecholette()}.
#' @param lambda Paramètre du modèle d'ajustement transmis à
#'   \code{multivariatecholette()}. Si \code{NULL}, la fonction choisit
#'   automatiquement \code{0} dès qu'une composante ajustable contient une
#'   valeur trimestrielle ou annuelle non positive, sinon \code{0.8}.
#' @param call_cholette Fonction solveur (injectable pour tests unitaires).
#'
#' @return Liste : \code{series_ajustees}, \code{diagnostic},
#'   \code{contraintes}, \code{resultat_brut}, \code{methode}.
#' @export
equilibrer_produit_ere_multivariatecholette <- function(
    data_produit,
    composantes_ajustables,
    tol_recentrage = 1e-6,
    rho = 1,
    lambda = NULL,
    call_cholette  = rjd3bench::multivariatecholette) {

  data_prepared <- preparer_donnees_equilibrage_ere_produit(
    data_produit, composantes_ajustables)

  prep <- preparer_contraintes_equilibrage_ere_produit(
    data_prepared, tol_recentrage)

  code_produit <- prep$code_produit

  index_trim <- data_prepared |>
    dplyr::distinct(.data$annee, .data$trimestre) |>
    dplyr::arrange(.data$annee, .data$trimestre)

  cibles_annuelles <- data_prepared |>
    dplyr::filter(.data$ajustable) |>
    dplyr::group_by(.data$composante, .data$annee) |>
    dplyr::summarise(valeur_annuelle = dplyr::first(.data$valeur_annuelle),
                     .groups = "drop")

  lambda_eff <- .choisir_lambda_cholette_ere(data_prepared, lambda)
  if (is.null(lambda) && identical(lambda_eff, 0)) {
    message("  [", code_produit, "] Cholette additif (lambda=0) : valeurs non positives detectees.")
  }

  # ================================================================
  # CAS 1 — UNE SEULE COMPOSANTE : ajustement direct
  # ================================================================
  if (length(prep$composantes_ajustables) == 1) {
    comp    <- prep$composantes_ajustables
    methode <- "ajustement_direct"

    avant <- data_prepared |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$valeur_trimestrielle)

    z_t <- prep$table_contrainte |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$contrainte_contemp)

    series_ajustees <- tibble::tibble(
      Code_Produit = code_produit,
      composante   = comp,
      annee        = index_trim$annee,
      trimestre    = index_trim$trimestre,
      valeur_avant = avant,
      valeur_apres = avant + z_t,
      delta        = z_t
    )

    message("\u2705 [", code_produit, "] Ajustement direct | comp : ", comp,
            " | recentrage=", prep$recentrage_applique)

    return(.construire_retour_equilibrage(
      series_ajustees, prep, cibles_annuelles,
      code_produit, methode, NULL
    ))
  }

  # ================================================================
  # CAS 2 — PLUSIEURS COMPOSANTES : multivariatecholette
  # ================================================================
  methode <- "multivariatecholette"

  res_cholette <- tryCatch(
    .appeler_cholette_multivarie(
      call_cholette = call_cholette,
      xlist = prep$xlist,
      tcvector = prep$tcvector,
      ccvector = prep$ccvector,
      rho = rho,
      lambda = lambda_eff
    ),
    error = function(e) e
  )

  # Fallback sans tcvector
  if (inherits(res_cholette, "error")) {
    message("  [", code_produit, "] Echec standard : ",
            conditionMessage(res_cholette))
    message("  [", code_produit, "] Tentative sans tcvector...")
    methode <- "multivariatecholette_sans_tcvector"

    noms_y    <- paste0("Y_", make.names(prep$composantes_ajustables))
    xlist_fb  <- prep$xlist[!names(prep$xlist) %in% noms_y]

    res_cholette <- tryCatch(
      .appeler_cholette_multivarie(
        call_cholette = call_cholette,
        xlist = xlist_fb,
        tcvector = character(0),
        ccvector = prep$ccvector,
        rho = rho,
        lambda = lambda_eff
      ),
      error = function(e) e
    )
  }

  # Echec définitif : pas de distribution plate
  if (inherits(res_cholette, "error")) {
    msg <- conditionMessage(res_cholette)
    message("  [", code_produit, "] ECHEC : ", msg)
    return(list(
      series_ajustees = tibble::tibble(),
      diagnostic = list(
        code_produit           = code_produit,
        methode                = "echec",
        raison_echec           = msg,
        composantes_ajustables = prep$composantes_ajustables,
        composantes_figees     = prep$composantes_figees
      ),
      contraintes   = list(xlist    = prep$xlist,
                           tcvector = prep$tcvector,
                           ccvector = prep$ccvector),
      resultat_brut = NULL,
      methode       = "echec"
    ))
  }

  if (is.null(res_cholette) || length(res_cholette) == 0)
    stop("[", code_produit, "] multivariatecholette : sortie vide.",
         call. = FALSE)

  .as_num <- function(z) as.numeric(stats::ts(z))

  series_ajustees <- purrr::map_dfr(prep$composantes_ajustables, function(comp) {
    if (!comp %in% names(res_cholette))
      stop("Composante absente du result : ", comp, call. = FALSE)

    valeur_avant <- data_prepared |>
      dplyr::filter(.data$composante == comp) |>
      dplyr::arrange(.data$annee, .data$trimestre) |>
      dplyr::pull(.data$valeur_trimestrielle)

    valeur_apres <- .as_num(res_cholette[[comp]])

    if (length(valeur_avant) != nrow(index_trim)) {
      stop(
        "[", code_produit, "] longueur incoherente pour `valeur_avant` (",
        comp, ") : ", length(valeur_avant), " vs ", nrow(index_trim),
        call. = FALSE
      )
    }
    if (length(valeur_apres) != nrow(index_trim)) {
      stop(
        "[", code_produit, "] longueur incoherente pour `valeur_apres` (",
        comp, ") : ", length(valeur_apres), " vs ", nrow(index_trim),
        call. = FALSE
      )
    }

    tibble::tibble(
      Code_Produit = code_produit,
      composante   = comp,
      annee        = index_trim$annee,
      trimestre    = index_trim$trimestre,
      valeur_avant = valeur_avant,
      valeur_apres = valeur_apres
    )
  }) |>
    dplyr::mutate(delta = .data$valeur_apres - .data$valeur_avant)

  message("\u2705 [", code_produit, "] ", methode,
          " | recentrage=", prep$recentrage_applique,
          " | lambda=", lambda_eff)

  .construire_retour_equilibrage(
    series_ajustees, prep, cibles_annuelles,
    code_produit, methode, res_cholette
  )
}


# ==============================================================================
# 5. WRAPPER COMPLET : TOUS LES PRODUITS, DEUX TYPES DE PRIX
# ==============================================================================

#' Exécuter l'équilibrage ERE complet (courant + VPAP, tous produits)
#'
#' @description
#' Fonction principale à appeler dans le pipeline CNT après
#' \code{executer_emplois_ere()} et \code{executer_ressources_ere()}.
#'
#' Pour chaque type de prix (courant et VPAP), pour chaque produit du modèle
#' de bouclage, la fonction :
#' \enumerate{
#'   \item Sélectionne les données benchmarkées du produit.
#'   \item Applique le modèle de bouclage (composantes ajustables).
#'   \item Récentre les contraintes trimestrielles si nécessaire.
#'   \item Appelle \code{multivariatecholette} (N ≥ 2) ou ajustement direct
#'     (N = 1).
#'   \item Retourne les séries ajustées et les diagnostics.
#' }
#'
#' @param ere_res Liste retournée par \code{executer_ressources_ere()}.
#'   Doit contenir \code{$ressources_crt} et \code{$ressources_vpap}.
#' @param ere_emp Liste retournée par \code{executer_emplois_ere()}.
#'   Doit contenir \code{$emplois_crt} et \code{$emplois_vpap}.
#' @param modele_equilibrage Objet \code{ModeleEquilibrageERE} retourné par
#'   \code{charger_modele_equilibrage_ere()}.
#' @param cna_ere_struct Liste des structures CNA ERE
#'   (\code{donnees$cna_ere_struct}).
#' @param tol_recentrage Tolérance pour le recentrage. Défaut \code{1e-6}.
#' @param call_cholette Fonction solveur injectable.
#' @param rho ParamÃ¨tre de lissage transmis Ã  \code{multivariatecholette()}.
#' @param lambda ParamÃ¨tre d'ajustement transmis Ã  \code{multivariatecholette()}.
#'   Si \code{NULL}, le choix est automatique produit par produit.
#'
#' @return Liste avec :
#'   \describe{
#'     \item{\code{crt}}{Liste nommée par Code_Produit, résultats courant.}
#'     \item{\code{vpap}}{Liste nommée par Code_Produit, résultats VPAP.}
#'     \item{\code{synthese_crt}}{Tibble synthèse courant.}
#'     \item{\code{synthese_vpap}}{Tibble synthèse VPAP.}
#'     \item{\code{emplois_ere_crt_equilibres}}{Table longue emplois courant ajustés.}
#'     \item{\code{emplois_ere_vpap_equilibres}}{Table longue emplois VPAP ajustés.}
#'   }
#' @export
executer_equilibrage_ere <- function(
    ere_res,
    ere_emp,
    modele_equilibrage,
    cna_ere_struct,
    tol_recentrage = 1e-6,
    rho = 1,
    lambda = NULL,
    call_cholette  = rjd3bench::multivariatecholette) {

  # Vérifications
  for (nm in c("ressources_crt", "ressources_vpap"))
    if (is.null(ere_res[[nm]]))
      stop("ere_res$", nm, " manquant.", call. = FALSE)
  for (nm in c("emplois_crt", "emplois_vpap"))
    if (is.null(ere_emp[[nm]]))
      stop("ere_emp$", nm, " manquant.", call. = FALSE)

  # Table modele : Code_Produit x composante_ajustable
  model_equil <- modele_equilibrage$produits_composantes_autorisees |>
    dplyr::filter(.data$autorise) |>
    dplyr::transmute(
      Code_Produit         = .data$Code_Produit,
      composante_ajustable = .standardiser_composante_ere(.data$Composante)
    ) |>
    dplyr::distinct()

  codes_produits <- unique(model_equil$Code_Produit)
  message("\u25b6 Equilibrage ERE : ", length(codes_produits),
          " produits | 2 types de prix (crt + vpap)")

  # ----------------------------------------------------------------
  # Fonction interne : traiter un type de prix
  # ----------------------------------------------------------------
  .traiter_type <- function(ressources, emplois, type_cna, label) {
    message("\n--- Type : ", label, " ---")

    ressources <- .standardiser_colonne_composante_ere(ressources)
    emplois    <- .standardiser_colonne_composante_ere(emplois)

    data_ere <- construire_data_ere_depuis_pipeline(
      ressources_bench = ressources,
      emplois_bench    = emplois,
      cna_ere_struct   = cna_ere_struct,
      type_cna         = type_cna,
      codes_produits   = codes_produits
    )

    produits_dispo <- intersect(codes_produits,
                                unique(data_ere$Code_Produit))
    produits_abs   <- setdiff(codes_produits,
                              unique(data_ere$Code_Produit))
    if (length(produits_abs) > 0)
      message("  Produits absents des donnees : ",
              paste(produits_abs, collapse = ", "))

    resultats <- purrr::set_names(produits_dispo) |>
      purrr::map(function(code) {
        comp_aj <- model_equil |>
          dplyr::filter(.data$Code_Produit == code) |>
          dplyr::pull(.data$composante_ajustable) |>
          unique()

        dp <- dplyr::filter(data_ere, .data$Code_Produit == code)
        if (nrow(dp) == 0) {
          message("  [", code, "] Aucune donnee — produit ignore.")
          return(list(series_ajustees = tibble::tibble(),
                      methode = "absent",
                      diagnostic = list(code_produit = code,
                                        methode = "absent")))
        }

        tryCatch(
          equilibrer_produit_ere_multivariatecholette(
            data_produit           = dp,
            composantes_ajustables = comp_aj,
            tol_recentrage         = tol_recentrage,
            rho                    = rho,
            lambda                 = lambda,
            call_cholette          = call_cholette
          ),
          error = function(e) {
            message("  [", code, "] ERREUR : ", conditionMessage(e))
            list(series_ajustees = tibble::tibble(),
                 methode         = "erreur",
                 diagnostic      = list(code_produit  = code,
                                        methode       = "erreur",
                                        raison_echec  = conditionMessage(e)))
          }
        )
      })

    # Synthèse
    synthese <- purrr::imap_dfr(resultats, function(res, code) {
      tibble::tibble(
        Code_Produit      = code,
        methode           = res$methode %||% "inconnu",
        recentrage        = isTRUE(res$diagnostic$recentrage_applique),
        n_comp_aj         = length(res$diagnostic$composantes_ajustables %||%
                                     character(0)),
        echec             = res$methode %in% c("echec", "erreur", "absent"),
        raison_echec      = if (res$methode %in% c("echec", "erreur"))
          res$diagnostic$raison_echec %||% NA_character_ else NA_character_,
        ecart_annuel_max  = tryCatch({
          ctrl_ann <- res$diagnostic$controle_annuel
          if (is.null(ctrl_ann) || nrow(ctrl_ann) == 0) {
            return(NA_real_)
          }

          if ("contrainte_temporelle_active" %in% names(ctrl_ann)) {
            ctrl_ann <- dplyr::filter(ctrl_ann, .data$contrainte_temporelle_active)
          }

          vals <- abs(ctrl_ann$ecart_annuel)
          vals <- vals[is.finite(vals)]
          if (length(vals) == 0) NA_real_ else max(vals)
        }, error = function(e) NA_real_),
        ecart_contemp_max = tryCatch(
          max(abs(res$diagnostic$controle_contemporain$ecart_contemporain),
              na.rm = TRUE),
          error = function(e) NA_real_
        )
      )
    })

    # Table longue des emplois ajustés
    emplois_ajust <- purrr::map_dfr(resultats, function(res) {
      if (nrow(res$series_ajustees) == 0) return(tibble::tibble())
      res$series_ajustees
    })

    list(resultats     = resultats,
         synthese      = synthese,
         emplois_ajust = emplois_ajust)
  }

  # ----------------------------------------------------------------
  # Traitement courant
  # ----------------------------------------------------------------
  res_crt <- .traiter_type(ere_res$ressources_crt, ere_emp$emplois_crt,
                           "CnaErECrt", "COURANT")

  # ----------------------------------------------------------------
  # Traitement VPAP
  # ----------------------------------------------------------------
  res_vpap <- .traiter_type(ere_res$ressources_vpap, ere_emp$emplois_vpap,
                            "CnaErEVol", "VPAP")

  # ----------------------------------------------------------------
  # Synthèse globale
  # ----------------------------------------------------------------
  n_ok_crt  <- sum(!res_crt$synthese$echec,  na.rm = TRUE)
  n_ok_vpap <- sum(!res_vpap$synthese$echec, na.rm = TRUE)
  n_prod     <- length(codes_produits)

  message("\n\u2705 Equilibrage ERE termine")
  message("  Courant : ", n_ok_crt, "/", n_prod,
          " produits equilibres")
  message("  VPAP    : ", n_ok_vpap, "/", n_prod,
          " produits equilibres")

  list(
    crt                       = res_crt$resultats,
    vpap                      = res_vpap$resultats,
    synthese_crt              = res_crt$synthese,
    synthese_vpap             = res_vpap$synthese,
    emplois_ere_crt_equilibres  = res_crt$emplois_ajust,
    emplois_ere_vpap_equilibres = res_vpap$emplois_ajust
  )
}

# Opérateur null-coalescing (usage interne)
`%||%` <- function(a, b) if (!is.null(a)) a else b
