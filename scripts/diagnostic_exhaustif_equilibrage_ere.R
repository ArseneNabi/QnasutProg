#!/usr/bin/env Rscript

# ============================================================
# Diagnostic exhaustif de l'equilibrage ERE (tous produits)
# - Faisabilite pre-Cholette
# - Execution produit par produit (multivarie / fallback univarie)
# - Controle de neutralite annuelle du desequilibre trimestriel
# - Exports Excel + CSV pour exploitation metier
# ============================================================

suppressPackageStartupMessages({
  library(QnaSut)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(writexl)
  library(tibble)
})

# ---------------------------
# 0) Parametres
# ---------------------------
params <- list(
  tol = 1e-8,
  output_dir_name = "Diagnostics_ERE",
  output_prefix = paste0("diag_equilibrage_ere_", format(Sys.Date(), "%Y%m%d")),
  path_data_ere_rds = NULL,          # optionnel ; auto-detection si NULL
  utiliser_diagnostic_global = TRUE, # lance aussi diagnostiquer_faisabilite_cholette_ere_tous_produits()
  tester_cholette_dans_diag = FALSE  # TRUE = plus lent, utile pour audit complet
)

message("\n=== Diagnostic exhaustif equilibrage ERE : demarrage ===")

# ---------------------------
# 1) Chargement config + donnees + modele
# ---------------------------
cfg <- load_config()
donnees_cnt <- charger_donnees_cnt(cfg)
modele_obj <- charger_modele_equilibrage_ere()

if (!"produits_composantes_autorisees" %in% names(modele_obj)) {
  stop("Le modele charge est invalide : 'produits_composantes_autorisees' manquant.", call. = FALSE)
}

tbl_modele <- modele_obj$produits_composantes_autorisees

col_composante <- c("Composante", "composante", "composante_ajustable") |>
  purrr::keep(~ .x %in% names(tbl_modele)) |>
  dplyr::first()

if (is.na(col_composante) || is.null(col_composante)) {
  stop(
    "Impossible d'identifier la colonne composante du modele. ",
    "Colonnes attendues : Composante / composante / composante_ajustable.",
    call. = FALSE
  )
}

model_equil <- tbl_modele |>
  filter(.data$autorise) |>
  transmute(
    Code_Produit = as.character(.data$Code_Produit),
    composante_ajustable = as.character(.data[[col_composante]])
  ) |>
  distinct()

if (nrow(model_equil) == 0) {
  stop("Aucune composante ajustable autorisee dans le modele d'equilibrage.", call. = FALSE)
}

# ---------------------------
# 2) Chargement de la table longue ERE a equilibrer
# ---------------------------
# Attendu : colonnes
# Code_Produit, annee, trimestre, composante,
# valeur_trimestrielle, valeur_annuelle, type_bloc

path_data_ere <- params$path_data_ere_rds
if (is.null(path_data_ere)) {
  candidats <- c(
    file.path(cfg$root_dir, "ERE_Equilibrage_Input.rds"),
    file.path(cfg$root_dir, "Output", "ERE_Equilibrage_Input.rds"),
    file.path(getwd(), "ERE_Equilibrage_Input.rds")
  )
  existants <- candidats[file.exists(candidats)]
  path_data_ere <- if (length(existants) > 0) existants[[1]] else NA_character_
}

if (!is.na(path_data_ere) && file.exists(path_data_ere)) {
  message("[INFO] Chargement data_ere depuis : ", path_data_ere)
  data_ere <- readRDS(path_data_ere)
} else {
  message("[INFO] Aucun .rds data_ere trouve. Construction automatique depuis cna_ere_struct (repartition annuelle / 4).")

  if (is.null(donnees_cnt$cna_ere_struct) || length(donnees_cnt$cna_ere_struct) == 0) {
    stop(
      "Impossible de construire data_ere : cna_ere_struct absent/vide dans donnees_cnt ",
      "et aucun fichier .rds fourni.",
      call. = FALSE
    )
  }

  composantes_ressources <- c("PRODUCTION", "IMPORTATIONS")
  composantes_emploi_modele <- model_equil |>
    dplyr::pull(.data$composante_ajustable) |>
    unique()

  composantes_cibles <- unique(c(composantes_ressources, composantes_emploi_modele))

  extraire_annuel <- function(composante) {
    bloc_comp <- donnees_cnt$cna_ere_struct[[composante]]
    if (is.null(bloc_comp) || is.null(bloc_comp$CnaErECrt)) {
      return(tibble::tibble())
    }

    QnaSut::pivoter_ere_long(bloc_comp$CnaErECrt, "CnaErECrt", composante) |>
      dplyr::select(.data$annee, .data$Code_Produit, valeur_annuelle = .data$valeur, .data$composante) |>
      dplyr::mutate(
        annee = as.integer(.data$annee),
        Code_Produit = as.character(.data$Code_Produit),
        valeur_annuelle = as.numeric(.data$valeur_annuelle)
      ) |>
      dplyr::filter(!is.na(.data$annee), !is.na(.data$Code_Produit), .data$Code_Produit != "")
  }

  data_ere <- purrr::map_dfr(composantes_cibles, function(comp) {
    ann <- extraire_annuel(comp)
    if (nrow(ann) == 0) {
      return(tibble::tibble())
    }

    ann |>
      tidyr::crossing(trimestre = 1:4) |>
      dplyr::mutate(
        valeur_trimestrielle = .data$valeur_annuelle / 4,
        type_bloc = dplyr::if_else(.data$composante %in% composantes_ressources, "ressource", "emploi")
      ) |>
      dplyr::select(
        .data$Code_Produit, .data$annee, .data$trimestre, .data$composante,
        .data$valeur_trimestrielle, .data$valeur_annuelle, .data$type_bloc
      )
  })

  if (nrow(data_ere) == 0) {
    stop(
      "Construction automatique de data_ere impossible : aucune composante exploitable ",
      "dans cna_ere_struct pour les ressources/emplois du modele.",
      call. = FALSE
    )
  }
}

colonnes_requises <- c(
  "Code_Produit", "annee", "trimestre", "composante",
  "valeur_trimestrielle", "valeur_annuelle", "type_bloc"
)
manquantes <- setdiff(colonnes_requises, names(data_ere))
if (length(manquantes) > 0) {
  stop("data_ere incomplet. Colonnes manquantes : ", paste(manquantes, collapse = ", "), call. = FALSE)
}

# Normalisation defensive

data_ere <- data_ere |>
  mutate(
    Code_Produit = as.character(.data$Code_Produit),
    composante = as.character(.data$composante),
    type_bloc = tolower(trimws(as.character(.data$type_bloc))),
    annee = as.integer(.data$annee),
    trimestre = as.integer(.data$trimestre),
    valeur_trimestrielle = as.numeric(.data$valeur_trimestrielle),
    valeur_annuelle = as.numeric(.data$valeur_annuelle)
  ) |>
  filter(!is.na(.data$Code_Produit), .data$Code_Produit != "")

# ---------------------------
# 3) Grilles de sortie (jamais NULL)
# ---------------------------
diag_produit <- tibble(
  Code_Produit = character(),
  nb_composantes_ajustables = integer(),
  nb_composantes_figees = integer(),
  verdict_faisabilite = character(),
  motifs_faisabilite = character(),
  max_abs_desequilibre_trim = numeric(),
  max_abs_somme_deseq_annuel = numeric(),
  ok_somme_deseq_annuel = logical(),
  ok_contrainte_contemp = logical(),
  ok_faisabilite = logical(),
  statut_execution = character(),
  message_execution = character(),
  priorite_correction = character(),
  score_priorite = integer(),
  tol = numeric()
)

diag_annee <- tibble(
  Code_Produit = character(),
  annee = integer(),
  somme_desequilibre_annuel = numeric(),
  max_abs_deseq_trim_annee = numeric(),
  ok_somme_deseq_annuel = logical(),
  nb_trim = integer(),
  tol = numeric()
)

diag_trimestre <- tibble(
  Code_Produit = character(),
  annee = integer(),
  trimestre = integer(),
  contrainte_contemp = numeric(),
  somme_ajustables = numeric(),
  desequilibre_trim = numeric(),
  abs_desequilibre_trim = numeric(),
  ok_contrainte_contemp = logical(),
  statut_execution = character(),
  tol = numeric()
)

# ---------------------------
# 4) Boucle produit par produit
# ---------------------------
codes_produits <- sort(unique(data_ere$Code_Produit))
message("[INFO] Nombre total de produits detectes : ", length(codes_produits))

for (code in codes_produits) {
  message("\n[TRAITEMENT] Produit ", code)

  data_produit <- data_ere |>
    filter(.data$Code_Produit == code) |>
    arrange(.data$annee, .data$trimestre, .data$composante)

  composantes_aj <- model_equil |>
    filter(.data$Code_Produit == code) |>
    pull(.data$composante_ajustable) |>
    unique()

  # Valeurs par defaut (defensif)
  verdict_faisabilite <- "non_faisable_probable"
  motifs_faisabilite <- "modele_absent"
  max_abs_deseq_trim <- NA_real_
  max_abs_somme_deseq_annuel <- NA_real_
  ok_somme_deseq_annuel_global <- FALSE
  ok_contrainte_contemp_global <- FALSE
  ok_faisabilite <- FALSE
  statut_execution <- "modele_absent"
  message_execution <- "Aucune composante ajustable parametree pour ce produit."
  nb_aj <- 0L
  nb_fig <- 0L

  # Table details trimestriels/annuels du desequilibre pre-Cholette
  detail_trim <- tibble(
    Code_Produit = code,
    annee = integer(),
    trimestre = integer(),
    contrainte_contemp = numeric(),
    somme_ajustables = numeric(),
    desequilibre_trim = numeric(),
    abs_desequilibre_trim = numeric(),
    ok_contrainte_contemp = logical(),
    statut_execution = character(),
    tol = params$tol
  )

  detail_ann <- tibble(
    Code_Produit = code,
    annee = integer(),
    somme_desequilibre_annuel = numeric(),
    max_abs_deseq_trim_annee = numeric(),
    ok_somme_deseq_annuel = logical(),
    nb_trim = integer(),
    tol = params$tol
  )

  if (length(composantes_aj) > 0) {
    # 4.1 Diagnostic faisabilite pre-Cholette
    diag <- tryCatch(
      diagnostiquer_faisabilite_cholette_ere_produit(
        data_produit = data_produit,
        composantes_ajustables = composantes_aj,
        code_produit = code,
        tol = params$tol,
        tester_cholette = FALSE
      ),
      error = function(e) e
    )

    if (!inherits(diag, "error")) {
      verdict_faisabilite <- diag$verdict$statut
      motifs_faisabilite <- paste(diag$verdict$motifs, collapse = "; ")
      nb_aj <- as.integer(diag$infos_generales$nb_series_ajustables)
      nb_fig <- as.integer(length(diag$infos_generales$composantes_figees))

      # 4.2 Desequilibre trimestriel (definition demandee)
      # desequilibre_t = contrainte_contemp_t - somme(composantes_ajustables)_t
      prep_data <- tryCatch(
        preparer_donnees_equilibrage_ere_produit(data_produit, composantes_aj),
        error = function(e) NULL
      )
      prep_ctr <- if (!is.null(prep_data)) {
        tryCatch(preparer_contraintes_equilibrage_ere_produit(prep_data), error = function(e) NULL)
      } else {
        NULL
      }

      if (!is.null(prep_data) && !is.null(prep_ctr)) {
        somme_ajustables_trim <- prep_data |>
          filter(.data$ajustable) |>
          group_by(.data$annee, .data$trimestre) |>
          summarise(somme_ajustables = sum(.data$valeur_trimestrielle, na.rm = TRUE), .groups = "drop")

        detail_trim <- prep_ctr$table_contrainte |>
          select(.data$annee, .data$trimestre, .data$contrainte_contemp) |>
          left_join(somme_ajustables_trim, by = c("annee", "trimestre")) |>
          mutate(
            Code_Produit = code,
            somme_ajustables = coalesce(.data$somme_ajustables, 0),
            desequilibre_trim = .data$contrainte_contemp - .data$somme_ajustables,
            abs_desequilibre_trim = abs(.data$desequilibre_trim),
            ok_contrainte_contemp = .data$abs_desequilibre_trim <= params$tol,
            statut_execution = "pre_cholette",
            tol = params$tol
          ) |>
          select(
            .data$Code_Produit, .data$annee, .data$trimestre,
            .data$contrainte_contemp, .data$somme_ajustables,
            .data$desequilibre_trim, .data$abs_desequilibre_trim,
            .data$ok_contrainte_contemp, .data$statut_execution, .data$tol
          ) |>
          arrange(.data$annee, .data$trimestre)

        detail_ann <- detail_trim |>
          group_by(.data$Code_Produit, .data$annee) |>
          summarise(
            somme_desequilibre_annuel = sum(.data$desequilibre_trim, na.rm = TRUE),
            max_abs_deseq_trim_annee = max(.data$abs_desequilibre_trim, na.rm = TRUE),
            nb_trim = dplyr::n(),
            .groups = "drop"
          ) |>
          mutate(
            ok_somme_deseq_annuel = abs(.data$somme_desequilibre_annuel) <= params$tol,
            tol = params$tol
          )

        max_abs_deseq_trim <- suppressWarnings(max(detail_trim$abs_desequilibre_trim, na.rm = TRUE))
        max_abs_somme_deseq_annuel <- suppressWarnings(max(abs(detail_ann$somme_desequilibre_annuel), na.rm = TRUE))

        if (!is.finite(max_abs_deseq_trim)) max_abs_deseq_trim <- NA_real_
        if (!is.finite(max_abs_somme_deseq_annuel)) max_abs_somme_deseq_annuel <- NA_real_

        ok_somme_deseq_annuel_global <- nrow(detail_ann) > 0 && all(detail_ann$ok_somme_deseq_annuel)
        ok_contrainte_contemp_global <- nrow(detail_trim) > 0 && all(detail_trim$ok_contrainte_contemp)
      }
    } else {
      verdict_faisabilite <- "non_faisable_probable"
      motifs_faisabilite <- paste("erreur_diagnostic", conditionMessage(diag), sep = " : ")
      message_execution <- conditionMessage(diag)
      statut_execution <- "erreur_diagnostic"
    }

    # 4.3 Execution produit (multivarie ou cas univarie)
    run_res <- tryCatch(
      equilibrer_produit_ere_multivariatecholette(
        data_produit = data_produit,
        composantes_ajustables = composantes_aj,
        forcer_coherence = TRUE
      ),
      error = function(e) e
    )

    if (inherits(run_res, "error")) {
      statut_execution <- "echec_runtime"
      message_execution <- conditionMessage(run_res)
    } else {
      if (!is.null(run_res$status)) {
        if (identical(run_res$status, "cas_univarie_non_supporte_par_multivariatecholette")) {
          statut_execution <- "cas_univarie"
          message_execution <- run_res$message
        } else if (identical(run_res$status, "echec_multivariatecholette")) {
          statut_execution <- "echec_cholette"
          message_execution <- run_res$message
        } else {
          statut_execution <- run_res$status
          message_execution <- if (!is.null(run_res$message)) run_res$message else "Statut d'execution specifique."
        }
      } else {
        statut_execution <- "ok_multivarie"
        message_execution <- "Equilibrage multivarie execute avec succes."
      }

      # controle_contemporain post-Cholette (si disponible)
      if (!is.null(run_res$diagnostic$controle_contemporain)) {
        ok_contrainte_contemp_global <- all(abs(run_res$diagnostic$controle_contemporain$ecart_contemporain) <= params$tol)
      }
    }
  }

  # 4.4 Regle globale faisabilite
  ok_faisabilite <- isTRUE(verdict_faisabilite %in% c("faisable_probable", "a_verifier")) &&
    isTRUE(ok_somme_deseq_annuel_global)

  # 4.5 Score/priorite de correction
  score_priorite <- 0L
  if (statut_execution %in% c("echec_cholette", "echec_runtime", "erreur_diagnostic", "modele_absent")) score_priorite <- score_priorite + 5L
  if (statut_execution == "cas_univarie") score_priorite <- score_priorite + 3L
  if (!ok_somme_deseq_annuel_global) score_priorite <- score_priorite + 3L
  if (!ok_contrainte_contemp_global) score_priorite <- score_priorite + 2L
  if (isTRUE(verdict_faisabilite == "non_faisable_probable")) score_priorite <- score_priorite + 2L
  if (is.finite(max_abs_somme_deseq_annuel) && !is.na(max_abs_somme_deseq_annuel) && max_abs_somme_deseq_annuel > 1) score_priorite <- score_priorite + 1L

  priorite <- dplyr::case_when(
    score_priorite >= 7 ~ "elevee",
    score_priorite >= 4 ~ "moyenne",
    TRUE ~ "faible"
  )

  diag_produit <- bind_rows(
    diag_produit,
    tibble(
      Code_Produit = code,
      nb_composantes_ajustables = nb_aj,
      nb_composantes_figees = nb_fig,
      verdict_faisabilite = verdict_faisabilite,
      motifs_faisabilite = motifs_faisabilite,
      max_abs_desequilibre_trim = max_abs_deseq_trim,
      max_abs_somme_deseq_annuel = max_abs_somme_deseq_annuel,
      ok_somme_deseq_annuel = ok_somme_deseq_annuel_global,
      ok_contrainte_contemp = ok_contrainte_contemp_global,
      ok_faisabilite = ok_faisabilite,
      statut_execution = statut_execution,
      message_execution = message_execution,
      priorite_correction = priorite,
      score_priorite = score_priorite,
      tol = params$tol
    )
  )

  diag_annee <- bind_rows(diag_annee, detail_ann)
  diag_trimestre <- bind_rows(
    diag_trimestre,
    detail_trim |>
      mutate(statut_execution = statut_execution)
  )
}

# ---------------------------
# 5) Resume global et tables cibles
# ---------------------------
resume_global <- diag_produit |>
  summarise(
    nb_total_produits = dplyr::n(),
    nb_faisables_multivaries = sum(.data$statut_execution == "ok_multivarie", na.rm = TRUE),
    nb_univaries = sum(.data$statut_execution == "cas_univarie", na.rm = TRUE),
    nb_echec = sum(.data$statut_execution %in% c("echec_cholette", "echec_runtime", "erreur_diagnostic", "modele_absent"), na.rm = TRUE),
    nb_violant_somme_annuelle_nulle = sum(!.data$ok_somme_deseq_annuel, na.rm = TRUE),
    tol = first(.data$tol)
  )

# Tables demandees
produits_problematiques <- diag_produit |>
  arrange(desc(.data$score_priorite), desc(.data$max_abs_somme_deseq_annuel), desc(.data$max_abs_desequilibre_trim), .data$Code_Produit)

produits_univaries <- diag_produit |>
  filter(.data$statut_execution == "cas_univarie") |>
  arrange(desc(.data$score_priorite), .data$Code_Produit)

produits_somme_annuelle_non_nulle <- diag_annee |>
  filter(!.data$ok_somme_deseq_annuel) |>
  arrange(desc(abs(.data$somme_desequilibre_annuel)), .data$Code_Produit, .data$annee)

# Option : diagnostic global existant dans le package
resultat_diag_global <- NULL
if (isTRUE(params$utiliser_diagnostic_global)) {
  message("\n[INFO] Lancement du diagnostic global package (optionnel)...")
  resultat_diag_global <- tryCatch(
    diagnostiquer_faisabilite_cholette_ere_tous_produits(
      data_ere = data_ere,
      model_equil = model_equil,
      tol = params$tol,
      tester_cholette = params$tester_cholette_dans_diag
    ),
    error = function(e) e
  )
}

# ---------------------------
# 6) Exports
# ---------------------------
output_dir <- file.path(cfg$root_dir, params$output_dir_name)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

path_excel <- file.path(output_dir, paste0(params$output_prefix, ".xlsx"))
path_diag_produit <- file.path(output_dir, paste0(params$output_prefix, "_diag_produit.csv"))
path_diag_annee <- file.path(output_dir, paste0(params$output_prefix, "_diag_annee.csv"))
path_diag_trim <- file.path(output_dir, paste0(params$output_prefix, "_diag_trimestre.csv"))
path_resume <- file.path(output_dir, paste0(params$output_prefix, "_resume_global.csv"))

write_csv(diag_produit, path_diag_produit)
write_csv(diag_annee, path_diag_annee)
write_csv(diag_trimestre, path_diag_trim)
write_csv(resume_global, path_resume)

feuilles <- list(
  resume_global = resume_global,
  diag_produit = diag_produit,
  diag_annee = diag_annee,
  diag_trimestre = diag_trimestre,
  top_produits_problematiques = produits_problematiques,
  produits_univaries = produits_univaries,
  produits_somme_annuelle_non_nulle = produits_somme_annuelle_non_nulle
)

if (!inherits(resultat_diag_global, "error") && !is.null(resultat_diag_global)) {
  feuilles$diag_pkg_resume_produits <- resultat_diag_global$resume_produits
  feuilles$diag_pkg_stats_globales <- resultat_diag_global$statistiques_globales$verdict
  feuilles$diag_pkg_motifs <- resultat_diag_global$statistiques_globales$motifs
}

write_xlsx(feuilles, path_excel)

# ---------------------------
# 7) Messages de synthese
# ---------------------------
message("\n=== DIAGNOSTIC TERMINE ===")
print(resume_global)

message("\nTop produits problematiques (10 premiers) :")
print(head(produits_problematiques, 10))

message("\nProduits univaries : ", nrow(produits_univaries))
message("Produits avec somme annuelle desequilibre non nulle : ", nrow(produits_somme_annuelle_non_nulle))

message("\nFichiers exportes :")
message(" - ", path_excel)
message(" - ", path_diag_produit)
message(" - ", path_diag_annee)
message(" - ", path_diag_trim)
message(" - ", path_resume)

invisible(list(
  diag_produit = diag_produit,
  diag_annee = diag_annee,
  diag_trimestre = diag_trimestre,
  resume_global = resume_global,
  top_produits_problematiques = produits_problematiques,
  produits_univaries = produits_univaries,
  produits_somme_annuelle_non_nulle = produits_somme_annuelle_non_nulle,
  export_excel = path_excel
))
