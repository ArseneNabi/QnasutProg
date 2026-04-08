#' Nettoie le code complet pour trouver la cible CNA
#' @param code le code concatene dont on veut extraire seulement le code branche
#' Ex: "Ind1_PRIVE*A01007" -> "PRIVE*A01007"
#' @export
extract_target_code <- function(code) {
  # Enleve tout ce qui est avant le premier underscore
  sub("^[^_]+_", "", code)
}

#' Extrait la branche fine pour le Prix
#' @param code le code concatene dont on veut extraire seulement le code branche
#' Ex: "Ind1_PRIVE*A01007" -> "A01007"
#' Ex: "A01007" -> "A01007" (gere le cas ou c'est deja propre)
#' @export
extract_price_branch <- function(code) {
  # Regex : ".*" (tout) suivi de "[\\*\\.]" (separateur)
  # On remplace tout ce bloc par "" (vide).
  # sub est natif a R, pas besoin de package supplementaire.
  trimws(sub(".*[\\*\\.]", "", code))
}

#' Extraire le code branche depuis un code composite (apres le dernier "*")
#'
#' @description
#' Fonction utilitaire qui extrait la partie droite d'un code composite separe
#' par "\code{*}". Typiquement utilisee pour passer de \code{"TypeInd*CodeBranche"}
#' a \code{"CodeBranche"}.
#'
#' Exemples :
#' \itemize{
#'   \item \code{"Ind1_PRIVE*A01007"} -> \code{"A01007"}
#'   \item \code{"CI_TOTAL*B01001"}   -> \code{"B01001"}
#'   \item \code{"A01007"}            -> \code{"A01007"} (deja propre)
#' }
#'
#' @param x Vecteur de caracteres contenant les codes composites.
#' @return Vecteur de caracteres avec uniquement le code apres le dernier \code{*}.
#' @seealso \code{\link{extract_price_branch}}, \code{\link{extract_target_code}}
#' @export
extract_branch_code <- function(x) {
  trimws(sub(".*\\*", "", x))
}

#' Consolidation des Resultats de benchmarking
#' @param df la base de fichers benchmarkes
#' @param type_traitement est le type de traitement concerne
#' @export
clean_res <- function(df, type_traitement) {
  df |>
    dplyr::select(annee, trimestre, periode, branche = full_code, valeur_finale = valeur_cal) |>
    dplyr::mutate(Traitement = type_traitement)
}
