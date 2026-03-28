#' Nettoie le code complet pour trouver la cible CNA
#' @param code le code concaténé dont on veut extraire seulement le code branche
#' Ex: "Ind1_PRIVE*A01007" -> "PRIVE*A01007"
#' @export
extract_target_code <- function(code) {
  # Enlève tout ce qui est avant le premier underscore
  sub("^[^_]+_", "", code)
}

#' Extrait la branche fine pour le Prix
#' @param code le code concaténé dont on veut extraire seulement le code branche
#' Ex: "Ind1_PRIVE*A01007" -> "A01007"
#' Ex: "A01007" -> "A01007" (Gère le cas où c'est déjà propre)
#' @export
extract_price_branch <- function(code) {
  # Regex : ".*" (tout) suivi de "[\\*\\.]" (séparateur)
  # On remplace tout ce bloc par "" (vide).
  # sub est natif à R, pas besoin de package supplémentaire.
  trimws(sub(".*[\\*\\.]", "", code))
}

#' Extraire le code branche depuis un code composite (après le dernier "*")
#'
#' @description
#' Fonction utilitaire qui extrait la partie droite d'un code composite séparé
#' par "\code{*}". Typiquement utilisée pour passer de \code{"TypeInd*CodeBranche"}
#' à \code{"CodeBranche"}.
#'
#' Exemples :
#' \itemize{
#'   \item \code{"Ind1_PRIVE*A01007"} → \code{"A01007"}
#'   \item \code{"CI_TOTAL*B01001"}   → \code{"B01001"}
#'   \item \code{"A01007"}            → \code{"A01007"} (déjà propre)
#' }
#'
#' @param x Vecteur de caractères contenant les codes composites.
#' @return Vecteur de caractères avec uniquement le code après le dernier \code{*}.
#' @seealso \code{\link{extract_price_branch}}, \code{\link{extract_target_code}}
#' @export
extract_branch_code <- function(x) {
  trimws(sub(".*\\*", "", x))
}

#' Consolidation des Résultats de benchmarking
#' @param df la base de fichers benchmarkés
#' @param type_traitement est le type de traitement concerné
#' @export
clean_res <- function(df, type_traitement) {
  df |>
    dplyr::select(annee, trimestre, periode, branche = full_code, valeur_finale = valeur_cal) |>
    dplyr::mutate(Traitement = type_traitement)
}


# ==============================================================================
# OPTIQUE DEPENSES : Application des ratios ERE sur les agregats
# ==============================================================================

#' Appliquer les ratios ERE trimestriels sur un agregat de base
#'
#' Multiplie pour chaque trimestre et chaque produit la valeur de l'agregat
#' de base par le ratio ERE trimestrialise correspondant.
#'
#' @param df_base Tibble avec colonnes \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{valeur}. Represente l'agregat de base
#'   (importations, ou production+importations selon la composante).
#' @param ratios_trim Sortie de \code{trimestrialiser_ratios_ere()}, filtrée
#'   sur la composante et le type_prix souhaites.
#' @param composante Nom de la composante a filtrer dans \code{ratios_trim}.
#' @param type_prix Type de prix a filtrer dans \code{ratios_trim}.
#'
#' @return Tibble avec colonnes \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{composante}, \code{valeur_composante}.
#' @export
appliquer_ratios_ere <- function(df_base, ratios_trim, composante, type_prix) {
  ratios_f <- ratios_trim |>
    dplyr::filter(composante == .env$composante, type_prix == .env$type_prix) |>
    dplyr::select(annee, trimestre, Code_Produit, ratio_trim)

  df_base |>
    dplyr::inner_join(ratios_f, by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::mutate(valeur_composante = valeur * ratio_trim, composante = .env$composante) |>
    dplyr::select(annee, trimestre, Code_Produit, composante = composante,
                  valeur_composante)
}

# ==============================================================================
# OPTIQUE DEPENSES : Assemblage des ressources et emplois ERE
# ==============================================================================

#' Assembler les composantes ressources ERE (marges, taxes) par application de ratios
#'
#' Applique les ratios trimestrialises sur les bases importations et
#' production+importations pour obtenir les 7 composantes de marges et taxes,
#' puis les assemble avec la production et les importations.
#'
#' @param p1_ere Tibble production ERE trimestrielle (colonnes : annee, trimestre, Code_Produit, valeur).
#' @param imp_ere Tibble importations ERE trimestrielles (colonnes : annee, trimestre, Code_Produit, valeur).
#' @param ratios_trim Ratios ERE trimestrialises (issu de trimestrialiser_ratios_ere()).
#' @param type_prix Type de prix : \code{"CnaErECrt"} ou \code{"CnaErEVol"}.
#' @param noms_ere Vecteur des noms des composantes ERE (names(cna_ere_struct)).
#' @param composante_p1 Nom de la colonne valeur dans p1_ere (defaut : \code{"P1_crt"}).
#' @param composante_imp Nom de la colonne valeur dans imp_ere (defaut : \code{"imp_crt"}).
#' @return Tibble avec colonnes annee, trimestre, Code_Produit, composante, valeur_composante.
#' @export
assembler_ressources_ere <- function(p1_ere, imp_ere, ratios_trim, type_prix,
                                     noms_ere,
                                     composante_p1  = "P1_crt",
                                     composante_imp = "imp_crt") {

  base_imp <- imp_ere |>
    dplyr::rename(valeur = dplyr::all_of(composante_imp))

  base_prod_imp <- dplyr::bind_rows(
    p1_ere  |> dplyr::rename(valeur = dplyr::all_of(composante_p1)),
    base_imp
  ) |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(valeur = sum(valeur, na.rm = TRUE), .groups = "drop")

  comps_base_imp  <- c("IMPOT sur Import", "IMPOT sur export")
  comps_base_both <- c("MARGE de commerce", "MARGE de transport", "TVA",
                       "IMPOT sur produit", "Subventions")

  res_marges <- dplyr::bind_rows(
    lapply(comps_base_imp, function(comp) {
      nm <- noms_ere[grepl(comp, noms_ere, ignore.case = TRUE)][1]
      if (is.na(nm)) return(NULL)
      appliquer_ratios_ere(base_imp, ratios_trim, nm, type_prix)
    }),
    lapply(comps_base_both, function(comp) {
      nm <- noms_ere[grepl(comp, noms_ere, ignore.case = TRUE)][1]
      if (is.na(nm)) return(NULL)
      appliquer_ratios_ere(base_prod_imp, ratios_trim, nm, type_prix)
    })
  )

  dplyr::bind_rows(
    p1_ere  |> dplyr::rename(valeur_composante = dplyr::all_of(composante_p1))  |>
      dplyr::mutate(composante = "PRODUCTION"),
    imp_ere |> dplyr::rename(valeur_composante = dplyr::all_of(composante_imp)) |>
      dplyr::mutate(composante = "IMPORTATIONS"),
    res_marges
  )
}


#' Lire et normaliser les methodes d estimation ERE depuis Methode_ERE.xlsx
#'
#' @param path_methode_ere Chemin vers le fichier Methode_ERE.xlsx.
#' @param map_feuille_cna Tibble de correspondance feuille <-> composante_cna
#'   (colonnes : feuille, composante_cna).
#' @return Tibble avec colonnes feuille, Code_Produit, Methode, composante_cna.
#' @export
lire_methodes_ere <- function(path_methode_ere, map_feuille_cna) {
  map_feuille_cna$feuille |>
    purrr::set_names() |>
    purrr::map_dfr(function(f) {
      readxl::read_excel(path_methode_ere, sheet = f) |>
        dplyr::select(Code_Produit = Code, Methode) |>
        dplyr::mutate(feuille = f)
    }) |>
    dplyr::left_join(map_feuille_cna, by = "feuille") |>
    dplyr::mutate(Methode = tolower(trimws(Methode))) |>
    dplyr::mutate(Methode = dplyr::case_when(
      grepl("lissage",               Methode) ~ "lissage",
      grepl("indicateur.*ressource", Methode) ~ "ind_ressources",
      grepl("ind apu",               Methode) ~ "ind_apu",
      grepl("prod",                  Methode) ~ "prod",
      grepl("solde",                 Methode) ~ "solde",
      TRUE                                    ~ "zero"
    ))
}


#' Estimer toutes les composantes emplois ERE par benchmarking
#'
#' Applique estimer_composante_emploi() sur toutes les composantes de
#' map_feuille_cna et retourne un tibble consolide.
#'
#' @param map_feuille_cna Tibble feuille <-> composante_cna.
#' @param methodes_ere Table des methodes (issue de lire_methodes_ere()).
#' @param cna_ere_struct Liste des composantes ERE annuelles.
#' @param grille_trim Grille trimestres x produits.
#' @param ind_ressources_trim Indicateur ressources totales trimestriel.
#' @param ind_prod_trim Indicateur production trimestriel.
#' @param ind_apu_trim Indicateur APU trimestriel.
#' @param type_prix \code{"CnaErECrt"} (defaut) ou \code{"CnaErECh"}.
#' @return Tibble consolide de toutes les composantes benchmarkees.
#' @export
estimer_emplois_ere <- function(map_feuille_cna, methodes_ere, cna_ere_struct,
                                grille_trim, ind_ressources_trim,
                                ind_prod_trim, ind_apu_trim,
                                type_prix = "CnaErECrt") {
  map_feuille_cna$feuille |>
    purrr::set_names() |>
    purrr::map_dfr(function(f) {
      cna_nm <- map_feuille_cna$composante_cna[map_feuille_cna$feuille == f]
      estimer_composante_emploi(f, cna_nm, methodes_ere, cna_ere_struct,
                                grille_trim, ind_ressources_trim,
                                ind_prod_trim, ind_apu_trim,
                                type_prix = type_prix)
    })
}

