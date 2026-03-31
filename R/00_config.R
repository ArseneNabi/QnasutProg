#' Charger la configuration depuis un fichier config.yml
#'
#' @description
#' Lit le fichier \code{config.yml} situé dans le répertoire courant (ou à un
#' chemin spécifié) et retourne les paramètres de configuration sous forme de
#' liste.
#'
#' @details
#' Le fichier \code{config.yml} doit contenir une section \code{default:} avec
#' les clés suivantes :
#' \itemize{
#'   \item \code{root_dir} : répertoire principal de travail
#'   \item \code{data_hist_dir} : répertoire des données historiques TRE
#'   \item \code{nomen_dir} : répertoire des nomenclatures
#'   \item \code{derniere_annee_definitif} : dernière année CNA définitive
#'   \item \code{annee_fin_projection} : année de fin de projection
#' }
#'
#' @param config_path Chemin vers le fichier \code{config.yml}.
#'   Par défaut, cherche \code{config.yml} dans le répertoire courant
#'   (\code{getwd()}).
#'
#' @return Une liste nommée avec les paramètres de configuration.
#'
#' @examples
#' \dontrun{
#'   cfg <- load_config()
#'   root_dir  <- cfg$root_dir
#'   data_hist_dir <- cfg$data_hist_dir
#' }
#'
#' @export
load_config <- function(config_path = NULL) {

  # V\u00e9rifier que yaml est disponible
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "Le package 'yaml' est requis pour lire config.yml.\n",
      "Installez-le avec : install.packages('yaml')",
      call. = FALSE
    )
  }

  # Chemin par d\u00e9faut : config.yml dans le r\u00e9pertoire courant
  if (is.null(config_path)) {
    config_path <- file.path(getwd(), "config.yml")
  }

  # V\u00e9rifier que le fichier existe
  if (!file.exists(config_path)) {
    stop(
      "Fichier config.yml introuvable : ", config_path, "\n",
      "Cr\u00e9ez ce fichier en vous basant sur le mod\u00e8le fourni avec le package.\n",
      "Voir ?load_config pour la structure attendue.",
      call. = FALSE
    )
  }

  # Lire le fichier YAML
  raw <- yaml::read_yaml(config_path)

  # Extraire la section "default"
  cfg <- raw[["default"]]
  if (is.null(cfg)) {
    stop(
      "Le fichier config.yml doit contenir une section 'default:'.\n",
      "V\u00e9rifiez la structure du fichier : ", config_path,
      call. = FALSE
    )
  }

  # Validation des cl\u00e9s obligatoires
  required_keys <- c("root_dir", "data_hist_dir", "nomen_dir",
                     "derniere_annee_definitif", "annee_fin_projection")
  missing_keys <- setdiff(required_keys, names(cfg))
  if (length(missing_keys) > 0) {
    stop(
      "Cl\u00e9s manquantes dans config.yml : ",
      paste(missing_keys, collapse = ", "), "\n",
      "V\u00e9rifiez la structure du fichier : ", config_path,
      call. = FALSE
    )
  }

  # Validation des chemins (existence)
  dir_keys <- c("root_dir", "data_hist_dir", "nomen_dir")
  for (key in dir_keys) {
    if (!dir.exists(cfg[[key]])) {
      warning(
        "Le r\u00e9pertoire '", key, "' n'existe pas : ", cfg[[key]], "\n",
        "V\u00e9rifiez le chemin dans config.yml.",
        call. = FALSE
      )
    }
  }

  message("\u2705 Configuration charg\u00e9e depuis : ", config_path)
  cfg
}
