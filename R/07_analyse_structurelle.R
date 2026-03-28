#' @import dplyr
NULL

# ==============================================================================
# CALCUL DES COEFFICIENTS TECHNIQUES (CI / PROD)
# ==============================================================================

#' Calculer les Coefficients Techniques par Branche
#'
#' Cette fonction calcule le rapport CI / Production pour chaque branche.
#' Elle utilise les lignes "Total" générées lors de l'import.
#'
#' @param df_tre Le data.frame issu de Base_TRE_Historique.rds
#' @return Un data.frame avec le Coefficient Technique par Branche, Année et Type de Prix.
#' @export
calculer_coef_technique <- function(df_tre) {

  # On filtre uniquement sur les lignes de TOTAUX pour P1 et P2
  # Car le coef technique est un ratio macroéconomique de la branche
  df_agregats <- df_tre |>
    filter(Code_Produit == "Total",
           Operation %in% c("P1", "P2")) |>
    select(Annee, Type_Prix, Code_Branche, Operation, Valeur)

  # Pivot pour avoir P1 et P2 en colonnes
  df_coef <- df_agregats |>
    tidyr::pivot_wider(
      names_from = Operation,
      values_from = Valeur,
      values_fill = 0
    ) |>
    mutate(
      Coef_Technique = ifelse(P1 == 0, 0, P2 / P1) # Gestion division par zéro
    ) |>
    select(Annee, Type_Prix, Code_Branche, P1_Total = P1, P2_Total = P2, Coef_Technique)

  return(df_coef)
}

# ==============================================================================
# CALCUL DES POIDS (STRUCTURE DES MATRICES)
# ==============================================================================

#' Calculer la Structure (Poids) d'une Matrice
#'
#' Calcule la part de chaque produit dans le total de la branche.
#' Poids = Valeur(Branche, Produit) / Valeur(Branche, Total)
#'
#' @param df_tre Le data.frame complet.
#' @param op_cible L'opération à analyser (ex: "P1" pour Prod, "P2" pour CI).
#' @return Un data.frame détaillé avec une colonne 'Poids'.
#' @export
calculer_poids_matrice <- function(df_tre, op_cible = "P1") {

  # 1. Extraction des totaux par branche pour l'opération cible
  df_totaux <- df_tre |>
    filter(Operation == op_cible, Code_Produit == "Total") |>
    select(Annee, Type_Prix, Code_Branche, Total_Branche = Valeur)

  # 2. Extraction des détails (Tous les produits sauf le Total)
  df_details <- df_tre |>
    filter(Operation == op_cible, Code_Produit != "Total")

  # 3. Jointure et Calcul
  df_structure <- df_details |>
    left_join(df_totaux, by = c("Annee", "Type_Prix", "Code_Branche")) |>
    mutate(
      Poids = ifelse(Total_Branche == 0, 0, Valeur / Total_Branche)
    ) |>
    select(Annee, Type_Prix, Operation, Code_Branche, Code_Produit, Valeur, Total_Branche, Poids)

  return(df_structure)
}

#' @import dplyr
NULL

# ... (Fonction coef technique inchangée) ...

#' Calculer la Structure (Poids) d'une Matrice
#' @param op_cible "P1" pour Production, "P2" pour CI
#' @export
calculer_poids_matrice <- function(df_tre, op_cible = "P1") {

  # 1. Totaux
  df_totaux <- df_tre |>
    filter(Operation == op_cible, Code_Produit == "Total") |>
    select(Annee, Type_Prix, Code_Branche, Total_Branche = Valeur)

  # 2. Détails
  df_details <- df_tre |>
    filter(Operation == op_cible, Code_Produit != "Total")

  # 3. Calcul
  df_structure <- df_details |>
    left_join(df_totaux, by = c("Annee", "Type_Prix", "Code_Branche")) |>
    mutate(
      Poids = ifelse(Total_Branche == 0, 0, Valeur / Total_Branche)
    ) |>
    select(Annee, Type_Prix, Operation, Code_Branche, Code_Produit, Valeur, Total_Branche, Poids)

  return(df_structure)
}
