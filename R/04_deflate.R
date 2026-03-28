#' Appliquer Prix sur résultat de Benchmarking
#'
#' @description
#' Cette fonction réalise l'opération de déflation ou d'inflation.
#' Elle est compatible avec les anciens formats (branche_prix/prix) et les nouveaux (Code_Produit/IP).
#'
#' @param df_bench Données de benchmarking (doit contenir 'full_code')
#' @param df_prix Données de prix (Code_Produit/IP ou branche_prix/prix)
#' @param operation "deflate" ou "inflate"
#' @export
apply_price_to_bench <- function(df_bench, df_prix, operation = c("deflate", "inflate")) {

  op <- match.arg(operation)

  # --- 1. ADAPTATEUR DE PRIX (Le Pont entre Nouveau et Ancien Syst\u00e8me) ---
  # Objectif : Standardiser df_prix pour qu'il ait toujours 'branche_prix' et 'prix'

  df_prix_std <- df_prix
  col_noms <- names(df_prix)

  # Si c'est le nouveau format (Code_Produit / IP)
  if ("Code_Produit" %in% col_noms) {
    df_prix_std <- df_prix_std |>
      dplyr::rename(
        branche_prix = Code_Produit,
        prix = IP # On renomme IP en prix pour la suite du script
      )
  }
  # Sinon, on suppose que c'est d\u00e9j\u00e0 'branche_prix' et 'prix' (Ancien format)


  # --- 2. PR\u00c9PARATION DU BENCHMARKING (Logique conserv\u00e9e) ---
  # On extrait la cl\u00e9 de jointure depuis le full_code
  df_bench <- df_bench |>
    dplyr::mutate(
      # Note : Assure-toi que la fonction extract_price_branch est bien disponible dans le package
      branche_prix = extract_price_branch(full_code),
      annee = as.numeric(annee),
      trimestre = as.numeric(trimestre)
    )

  # --- 3. PR\u00c9PARATION DES PRIX (Ta logique de robustesse conserv\u00e9e) ---
  # On nettoie, on type et on agr\u00e8ge les doublons \u00e9ventuels
  df_prix_clean <- df_prix_std |>
    dplyr::mutate(
      branche_prix = trimws(branche_prix),
      annee = as.numeric(annee),
      trimestre = as.numeric(trimestre),
      # Gestion : si la colonne s'appelle 'valeur' au lieu de 'prix' (s\u00e9curit\u00e9 suppl\u00e9mentaire)
      prix = as.numeric(if ("prix" %in% names(df_prix_std)) prix else valeur)
    ) |>
    # S\u00e9curit\u00e9 : On ne garde que les lignes avec un prix valide
    dplyr::filter(!is.na(prix)) |>
    # Agr\u00e9gation : On ne garde qu'une seule valeur par cl\u00e9 (Moyenne)
    dplyr::group_by(branche_prix, annee, trimestre) |>
    dplyr::summarize(prix = mean(prix, na.rm = TRUE), .groups = "drop")


  # --- 4. JOINTURE STRICTE ---
  res <- df_bench |>
    dplyr::inner_join(df_prix_clean, by = c("branche_prix", "annee", "trimestre")) |>
    dplyr::mutate(
      valeur_estimee = if (op == "deflate") valeur_cal / prix else valeur_cal * prix
    )

  # --- 5. V\u00c9RIFICATION ET SORTIE ---
  if (nrow(res) == 0) {
    warning("\u26a0\ufe0f ALERTE : Aucune correspondance trouv\u00e9e entre Benchmarking et Prix. \n",
            "  - V\u00e9rifiez que 'extract_price_branch(full_code)' donne bien des codes pr\u00e9sents dans le fichier de prix.\n",
            "  - Codes Bench (exemple) : ", paste(utils::head(unique(df_bench$branche_prix), 3), collapse=", "), "\n",
            "  - Codes Prix (exemple)  : ", paste(utils::head(unique(df_prix_clean$branche_prix), 3), collapse=", "))
  }

  res |>
    dplyr::select(
      annee, trimestre, periode, full_code,
      dplyr::any_of(c("branche_macro", "type_ind")),
      valeur_cal, prix, valeur_estimee
    )
}
