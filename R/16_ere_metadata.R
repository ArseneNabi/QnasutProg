#' Construire les métadonnées statiques de l'optique dépenses ERE
#'
#' Centralise les objets de configuration ERE historiquement définis dans
#' le Rmd :
#' \itemize{
#'   \item \code{path_methode_ere}
#'   \item \code{map_feuille_cna}
#'   \item \code{map_apu_cols}
#' }
#'
#' Cette fonction ne modifie pas la logique de calcul ; elle expose les mêmes
#' valeurs et structures attendues par \code{executer_emplois_ere()}.
#'
#' @param root_dir Chemin racine du dossier de travail (issu de
#'   \code{cfg$root_dir}).
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{\code{path_methode_ere}}{Chemin vers \code{Methode_ERE.xlsx}.}
#'   \item{\code{map_feuille_cna}}{Tibble de correspondance feuille
#'     \eqn{\rightarrow} composante CNA (colonnes \code{feuille},
#'     \code{composante_cna}).}
#'   \item{\code{map_apu_cols}}{Tibble de correspondance code produit ERE
#'     \eqn{\rightarrow} colonne APU des indicateurs trimestriels
#'     (colonnes \code{Code_Produit}, \code{col_apu}).}
#' }
#'
#' @export
construire_metadonnees_ere <- function(root_dir) {

  path_methode_ere <- file.path(root_dir, "Methode_ERE.xlsx")

  map_feuille_cna <- tibble::tribble(
    ~feuille,    ~composante_cna,
    "CFmarch",   "CF Marchande Menage   Prix d'acquisition",
    "CFnmarch",  "CF Non Marchande Menage Prix d'acquisition",
    "CFapu",     "CF Non Marchande APU Prix d'acquisition",
    "CFisblsm",  "CF Non Marchande  ISBL Prix d'acquisition",
    "FBCF",      "FBCF Prix d'acquisition",
    "VS",        "VS Prix d'acquisition",
    "AOV",       "Aquisition moyen cession de origen de valeur Prix d'acquisition"
  )

  map_apu_cols <- tibble::tribble(
    ~Code_Produit, ~col_apu,
    "JZ000", "Ind1_APU*JZ000",
    "KZ000", "Ind1_APU*K21004",
    "OZ000", "Ind1_APU*L22000",
    "PZ000", "Ind1_APU*P26000",
    "QZ000", "Ind1_APU*QZ000",
    "RS001", "Ind1_APU*RS001"
  )

  list(
    path_methode_ere = path_methode_ere,
    map_feuille_cna  = map_feuille_cna,
    map_apu_cols     = map_apu_cols
  )
}
