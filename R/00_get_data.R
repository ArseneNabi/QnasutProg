#' Récupérer un TRE spécifique pour calculs
#' @param annee_cible Année voulue
#' @param type_prix_cible "Courant" ou "Constant"
#' @param matrice "Production", "CI", "Demande_Finale", "VAB"
#' @export
get_tre_matrix <- function(annee_cible, type_prix_cible = "Courant", matrice = "Production") {

  # 1. Chargement Base
  path_db <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/Data_Historique/TRE/Base_TRE_Historique.rds"
  if(!file.exists(path_db)) stop("Base TRE introuvable")

  df <- readRDS(path_db) |>
    dplyr::filter(Annee == annee_cible, Type_Prix == type_prix_cible)

  # 2. Filtrage selon le besoin
  if (matrice == "Production") {
    res <- df |> dplyr::filter(Operation == "P1")
  } else if (matrice == "CI") {
    res <- df |> dplyr::filter(Operation == "P2")
  } else if (matrice == "VAB") {
    # Calcul P1 - Somme(P2) ou extraction directe si disponible dans les comptes
    # Ici on extrait les lignes de revenus
    res <- df |> dplyr::filter(Operation == "Revenus")
  }

  # 3. Retourne un format "Large" (Matrice) pour calculs
  res |>
    tidyr::pivot_wider(names_from = Code_Branche_Ou_Op, values_from = Valeur)
}
