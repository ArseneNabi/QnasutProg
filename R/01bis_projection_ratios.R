#' @import dplyr
#' @import tidyr
#' @import rjd3bench
#' @importFrom purrr map_dfr set_names
NULL

# ==============================================================================
# 1. AGREGATION DU TRE (Niv 2 -> Niv Etalonnage)
# ==============================================================================

#' Agréger le TRE vers la nomenclature d'étalonnage
#' @param db_tre La base TRE brute (Niveau 2 Branches)
#' @param table_passage Dataframe avec colonnes : Code_Branche_Niv2, Code_Branche_Etal
#' @export
agreger_tre_etalonnage <- function(db_tre, table_passage) {

  message("\u1f504 Agr\u00e9gation du TRE vers la nomenclature d'\u00e9talonnage...")

  # 1. Nettoyage et Jointure
  df_agg <- db_tre |>
    # --- CORRECTIF CRITIQUE ---
    # On supprime les lignes o\u00f9 le Code_Produit est "Total" (insensible \u00e0 la casse)
    # Cela force le re-calcul du total bas\u00e9 uniquement sur les produits \u00e9l\u00e9mentaires
    filter(!grepl("^total$", trimws(Code_Produit), ignore.case = TRUE)) |>

    # Jointure pour r\u00e9cup\u00e9rer le code Etalonnage
    inner_join(table_passage, by = c("Code_Branche" = "Code_Branche_Niv2")) |>

    # 2. Agr\u00e9gation (Somme des doublons \u00e9ventuels dus au passage Niv2->Etal)
    group_by(Annee, Type_Prix, Operation, Code_Branche_Etal, Code_Produit) |>
    summarise(
      Valeur = sum(Valeur, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(Code_Branche = Code_Branche_Etal) # On renomme pour la suite

  return(df_agg)
}

# ==============================================================================
# 2. FONCTIONS DE CALCUL DES RATIOS
# ==============================================================================

#' Calculer les Coefficients Techniques et Poids (Base Agrégée)
#' @param db_tre_etal Base TRE agrégée au niveau étalonnage (Annee, Type_Prix, Operation, Code_Branche, Code_Produit, Valeur).
#' @export
calculer_ratios_annuels_etal <- function(db_tre_etal) {

  # 1. Calcul des Totaux par Branche (D\u00e9nominateur recalcul\u00e9 proprement)
  db_totaux <- db_tre_etal |>
    group_by(Annee, Type_Prix, Code_Branche, Operation) |>
    summarise(Total_Branche = sum(Valeur, na.rm = TRUE), .groups = "drop")

  # 2. Calcul des Poids (Matrice P1 et P2)
  # Poids = Valeur Produit / Total Branche recalcul\u00e9
  df_poids <- db_tre_etal |>
    left_join(db_totaux, by = c("Annee", "Type_Prix", "Code_Branche", "Operation")) |>
    mutate(
      # S\u00e9curit\u00e9 division par z\u00e9ro
      Poids = if_else(Total_Branche == 0, 0, Valeur / Total_Branche)
    )

  # 3. Calcul des Coeffs Techniques (Total P2 / Total P1)
  df_p1 <- db_totaux |> filter(Operation == "P1") |> rename(P1 = Total_Branche) |> select(-Operation)
  df_p2 <- db_totaux |> filter(Operation == "P2") |> rename(P2 = Total_Branche) |> select(-Operation)

  df_ct <- df_p1 |>
    left_join(df_p2, by = c("Annee", "Type_Prix", "Code_Branche")) |>
    mutate(
      P2 = replace_na(P2, 0),
      Coef_Technique = if_else(P1 == 0, 0, P2 / P1)
    )

  return(list(
    poids = df_poids, # Contient P1 et P2
    ct = df_ct
  ))
}

# ==============================================================================
# 3. MOTEUR DE TRIMESTRIALISATION ET PROJECTION
# ==============================================================================

#' Trimestrialisation Complète (Historique + Projection)
#' @param ratios_annuels les ratios annuels deja calculé du TRE
#' @param df_prix_niv3 les indices de prix au niveau le plus detaillé
#' @param derniere_annee_cna la dernière année des comptes definitifs
#' @param annee_fin_proj l'année de compilation
#' @export
trimestrialiser_cnt_complet <- function(ratios_annuels, df_prix_niv3, derniere_annee_cna, annee_fin_proj) {

  # --- A. INITIALISATION ---
  df_poids_annuel <- ratios_annuels$poids
  df_ct_annuel    <- ratios_annuels$ct

  annees_totales <- min(df_poids_annuel$Annee):annee_fin_proj

  res_poids_list <- list()
  res_ct_list    <- list()
  res_ip_list    <- list()

  message("\u1f680 D\u00e9marrage de la trimestrialisation (Hist + Proj)...")

  for (an in annees_totales) {

    est_historique <- (an <= derniere_annee_cna)
    trimestres <- 1:4

    # R\u00e9cup\u00e9ration des Prix de l'ann\u00e9e en cours
    prix_an <- df_prix_niv3 |> filter(annee == an)
    if(nrow(prix_an) == 0) stop(paste("Prix manquants pour l'ann\u00e9e", an))

    # ---------------------------------------------------------
    # 1. DETERMINATION DES POIDS EN VOLUME (Q_Vol)
    # ---------------------------------------------------------

    if (est_historique) {
      # CAS HISTORIQUE : Volume Trim = Volume Annuel Constant
      w_vol_ref <- df_poids_annuel |>
        filter(Annee == an, Type_Prix == "Constant") |>
        select(Code_Branche, Code_Produit, Operation, Poids_Vol = Poids)

    } else {
      # CAS PROJECTION (2023, 2024...)
      # R\u00e8gle : Volume Trim N = Structure COURANTE Annuelle N-1
      an_ref <- an - 1

      if (an_ref <= derniere_annee_cna) {
        # Si N-1 est historique, on prend le Poids Courant de N-1
        w_vol_ref <- df_poids_annuel |>
          filter(Annee == an_ref, Type_Prix == "Courant") |>
          select(Code_Branche, Code_Produit, Operation, Poids_Vol = Poids)
      } else {
        # Si N-1 est projet\u00e9, on r\u00e9cup\u00e8re la moyenne des poids courants calcul\u00e9s
        prev_res <- res_poids_list[[as.character(an_ref)]]

        w_vol_ref <- prev_res |>
          filter(Type_Prix == "Courant") |>
          group_by(Code_Branche, Code_Produit, Operation) |>
          summarise(Poids_Vol = mean(Poids, na.rm = TRUE), .groups="drop")
      }
    }

    # Extension sur 4 trimestres
    w_vol_trim <- w_vol_ref |>
      crossing(Trimestre = trimestres) |>
      mutate(Annee = an, Type_Prix = "Constant") |>
      rename(Poids = Poids_Vol)

    # ---------------------------------------------------------
    # 2. CALCUL DES PRIX BRANCHES & POIDS COURANTS
    # ---------------------------------------------------------

    w_calc <- w_vol_trim |>
      inner_join(prix_an, by = c("Annee" = "annee", "Trimestre" = "trimestre", "Code_Produit"))

    # A. IP Branches (Moyenne pond\u00e9r\u00e9e : Somme(Poids_Vol * Prix))
    # Note : Comme Somme(Poids_Vol) = 1 (gr\u00e2ce au correctif "Total"), c'est bien une moyenne pond\u00e9r\u00e9e
    ip_branches <- w_calc |>
      group_by(Annee, Trimestre, Code_Branche, Operation) |>
      summarise(IP_Agregat = sum(Poids * IP, na.rm = TRUE), .groups="drop")

    ip_prod <- ip_branches |> filter(Operation == "P1") |> rename(IP_Prod = IP_Agregat) |> select(-Operation)
    ip_ci   <- ip_branches |> filter(Operation == "P2") |> rename(IP_CI = IP_Agregat) |> select(-Operation)

    # B. Poids Courants (Repond\u00e9ration)
    w_crt_trim <- w_calc |>
      left_join(ip_branches, by = c("Annee", "Trimestre", "Code_Branche", "Operation")) |>
      mutate(
        Type_Prix = "Courant",
        Val_Relative = Poids * IP,
        Poids_Crt = if_else(IP_Agregat == 0, 0, Val_Relative / IP_Agregat)
      ) |>
      select(Annee, Trimestre, Type_Prix, Code_Branche, Code_Produit, Operation, Poids = Poids_Crt)

    res_poids_list[[as.character(an)]] <- bind_rows(w_vol_trim, w_crt_trim)
    res_ip_list[[as.character(an)]]    <- bind_rows(ip_prod, ip_ci)

    # ---------------------------------------------------------
    # 3. COEFFICIENTS TECHNIQUES (CT)
    # ---------------------------------------------------------

    # A. CT Volume
    if (est_historique) {
      ct_vol_ref <- df_ct_annuel |>
        filter(Annee == an, Type_Prix == "Constant") |>
        select(Code_Branche, CT_Vol = Coef_Technique)
    } else {
      an_ref <- an - 1
      if (an_ref <= derniere_annee_cna) {
        ct_vol_ref <- df_ct_annuel |>
          filter(Annee == an_ref, Type_Prix == "Courant") |>
          select(Code_Branche, CT_Vol = Coef_Technique)
      } else {
        prev_ct <- res_ct_list[[as.character(an_ref)]]
        ct_vol_ref <- prev_ct |>
          filter(Type_Prix == "Courant") |>
          group_by(Code_Branche) |>
          summarise(CT_Vol = mean(Coef_Technique, na.rm=TRUE), .groups="drop")
      }
    }

    ct_vol_trim <- ct_vol_ref |>
      crossing(Trimestre = trimestres) |>
      mutate(Annee = an, Type_Prix = "Constant", Coef_Technique = CT_Vol)

    # B. CT Courant (Calcul\u00e9 : CT_Vol * IP_CI / IP_Prod)
    ct_crt_calc <- ct_vol_trim |>
      left_join(ip_prod, by = c("Annee", "Trimestre", "Code_Branche")) |>
      left_join(ip_ci,   by = c("Annee", "Trimestre", "Code_Branche")) |>
      mutate(
        Type_Prix = "Courant",
        Coef_Technique_Calc = Coef_Technique * (IP_CI / IP_Prod)
      ) |>
      select(Annee, Trimestre, Type_Prix, Code_Branche, Coef_Technique = Coef_Technique_Calc)

    # C. Benchmarking CT Courant
    if (est_historique) {
      ct_target <- df_ct_annuel |>
        filter(Annee == an, Type_Prix == "Courant") |>
        select(Code_Branche, CT_Target = Coef_Technique)

      ct_crt_final <- ct_crt_calc |>
        left_join(ct_target, by = "Code_Branche") |>
        group_by(Code_Branche) |>
        mutate(
          Coef_Technique = bench_denton_simple(Coef_Technique, CT_Target[1])
        ) |>
        ungroup() |>
        select(-CT_Target)
    } else {
      ct_crt_final <- ct_crt_calc
    }

    res_ct_list[[as.character(an)]] <- bind_rows(ct_vol_trim, ct_crt_final)

  } # Fin Boucle Ann\u00e9e

  message("\u2705 Trimestrialisation termin\u00e9e.")

  return(list(
    poids_trim = bind_rows(res_poids_list),
    ct_trim    = bind_rows(res_ct_list),
    ip_branches = bind_rows(res_ip_list)
  ))
}

#' Fonction utilitaire pour Denton
#' @noRd
bench_denton_simple <- function(serie_trim, cible_annuelle) {
  if (all(is.na(serie_trim)) || is.na(cible_annuelle)) return(serie_trim)
  if (all(serie_trim == 0)) return(serie_trim)

  ts_indic <- stats::ts(serie_trim, frequency = 4)
  ts_target <- stats::ts(cible_annuelle * 4, frequency = 1) # Denton pr\u00e9serve la somme

  tryCatch({
    res <- rjd3bench::denton(s = ts_indic, t = ts_target, d = 1, freq = 4)
    return(as.numeric(res$final))
  }, error = function(e) {
    facteur <- (cible_annuelle * 4) / sum(serie_trim)
    return(serie_trim * facteur)
  })
}


# ==============================================================================
# OPTIQUE DEPENSES : Ratios ERE par produit (marges, impots, TVA)
# ==============================================================================

#' Calculer les ratios annuels ERE par produit
#'
#' Pour chaque composante ERE (marges, impots, TVA, subventions), calcule le
#' ratio annuel : valeur_composante / valeur_base, ou la base est selon la
#' composante :
#' \itemize{
#'   \item Impot sur import, impot sur export : base = IMPORTATIONS
#'   \item Marges commerce, transport, TVA, impot produit, subventions : base = PRODUCTION + IMPORTATIONS
#' }
#'
#' @param cna_ere_struct Liste issue de \code{import_cna_ere_structured()}.
#' @param type_prix Type de prix pour le calcul : \code{"CnaErECrt"} ou
#'   \code{"CnaErEVol"}. Par defaut \code{"CnaErECrt"}.
#'
#' @return Tibble long avec colonnes \code{annee}, \code{Code_Produit},
#'   \code{composante}, \code{type_prix}, \code{ratio}.
#' @export
calculer_ratios_ere <- function(cna_ere_struct, type_prix = "CnaErECrt") {

  # Bases de calcul
  prod_ann <- cna_ere_struct[["PRODUCTION"]][[type_prix]]
  imp_ann  <- cna_ere_struct[["IMPORTATIONS"]][[type_prix]]

  # Base 1 : IMPORTATIONS seules (impots sur import/export)
  base_imp <- pivoter_ere_long(imp_ann, type_prix, "IMPORTATIONS") |>
    dplyr::select(annee, Code_Produit, base = valeur)

  # Base 2 : PRODUCTION + IMPORTATIONS (marges, TVA, impots produits, subventions)
  base_prod_imp <- dplyr::bind_rows(
    pivoter_ere_long(prod_ann, type_prix, "PRODUCTION"),
    pivoter_ere_long(imp_ann,  type_prix, "IMPORTATIONS")
  ) |>
    dplyr::group_by(annee, Code_Produit) |>
    dplyr::summarise(base = sum(valeur, na.rm = TRUE), .groups = "drop")

  # Composantes et leurs bases respectives
  composantes_base_imp <- c("IMPOT sur Import", "IMPOT sur export")
  composantes_base_prod_imp <- c(
    "MARGE de commerce", "MARGE de transport",
    "TVA Non Deductible", "IMPOT sur produit", "Subventions sur produits"
  )

  # Noms reels dans cna_ere_struct (gestion apostrophes/accents)
  noms_disponibles <- names(cna_ere_struct)
  trouver_nom <- function(pattern) {
    noms_disponibles[grepl(pattern, noms_disponibles, ignore.case = TRUE)][1]
  }

  map_composantes <- list(
    list(nom = trouver_nom("IMPOT sur Import"),      base = base_imp),
    list(nom = trouver_nom("IMPOT sur export"),      base = base_imp),
    list(nom = trouver_nom("MARGE de commerce"),     base = base_prod_imp),
    list(nom = trouver_nom("MARGE de transport"),    base = base_prod_imp),
    list(nom = trouver_nom("TVA"),                   base = base_prod_imp),
    list(nom = trouver_nom("IMPOT sur produit"),     base = base_prod_imp),
    list(nom = trouver_nom("Subventions"),           base = base_prod_imp)
  )

  # Calcul des ratios pour chaque composante
  purrr::map_dfr(map_composantes, function(comp) {
    if (is.null(comp$nom) || is.null(cna_ere_struct[[comp$nom]])) return(NULL)
    df_comp <- cna_ere_struct[[comp$nom]][[type_prix]]
    if (is.null(df_comp)) return(NULL)
    pivoter_ere_long(df_comp, type_prix, comp$nom) |>
      dplyr::inner_join(comp$base, by = c("annee", "Code_Produit")) |>
      dplyr::mutate(
        ratio = dplyr::if_else(base > 0, valeur / base, 0)
      ) |>
      dplyr::select(annee, Code_Produit, composante, type_prix, ratio)
  })
}


#' Trimestrialiser les ratios ERE par produit
#'
#' Interpole lineairement les ratios ERE annuels au niveau trimestriel.
#' Le ratio etant une moyenne (et non un flux), chaque trimestre prend
#' la valeur du ratio annuel correspondant. Pour les annees de projection
#' (apres la derniere annee CNA), le dernier ratio connu est extrapole.
#'
#' @param ratios_ere Sortie de \code{calculer_ratios_ere()}.
#' @param derniere_annee_cna Derniere annee des CNA definitifs.
#' @param annee_fin_proj Annee de fin de projection (incluse).
#'
#' @return Tibble long avec colonnes \code{annee}, \code{trimestre},
#'   \code{Code_Produit}, \code{composante}, \code{type_prix}, \code{ratio_trim}.
#' @export
trimestrialiser_ratios_ere <- function(ratios_ere, derniere_annee_cna,
                                       annee_fin_proj) {
  annees_proj <- seq(derniere_annee_cna + 1, annee_fin_proj)

  ratios_ere |>
    dplyr::group_by(Code_Produit, composante, type_prix) |>
    dplyr::group_modify(function(df, keys) {
      annees_hist <- df$annee
      ratios_hist <- df$ratio

      # Extrapolation : repeter le dernier ratio pour les annees de projection
      ratio_proj <- dplyr::last(ratios_hist)
      annees_all <- c(annees_hist, annees_proj)
      ratios_all <- c(ratios_hist, rep(ratio_proj, length(annees_proj)))

      # Le ratio est une moyenne : chaque trimestre = ratio annuel
      # (pas de Denton ici car le ratio n'est pas un flux)
      tibble::tibble(
        annee      = rep(annees_all, each = 4),
        trimestre  = rep(1:4, length(annees_all)),
        ratio_trim = rep(ratios_all, each = 4)
      )
    }) |>
    dplyr::ungroup()
}


# ==============================================================================
# OPTIQUE DEPENSES : Estimation des composantes emplois ERE
# ==============================================================================

#' Estimer une composante emploi ERE par benchmarking
#'
#' Construit l indicateur trimestriel selon la methode definie dans les
#' metadonnees (lissage, ind_ressources, prod, ind_apu, zero), puis benchmarke
#' sur la cible CNA annuelle. Les produits a methode solde sont exclus.
#'
#' @param feuille_nm Nom de la feuille dans Methode_ERE.xlsx.
#' @param composante_cna_nm Nom exact de la composante dans cna_ere_struct.
#' @param methodes_ere Table longue des methodes (feuille, Code_Produit, Methode).
#' @param cna_ere_struct Liste des composantes ERE annuelles.
#' @param grille_trim Tibble annee x trimestre x Code_Produit.
#' @param ind_ressources_trim Indicateur ressources totales trimestriel.
#' @param ind_prod_trim Indicateur production trimestriel.
#' @param ind_apu_trim Indicateur APU trimestriel.
#' @param type_prix Type de prix pour la cible CNA : \code{"CnaErECrt"} (defaut,
#'   prix courants) ou \code{"CnaErECh"} (volume chaine).
#' @return Tibble : annee, trimestre, Code_Produit, valeur_cal, composante.
#' @export
estimer_composante_emploi <- function(feuille_nm, composante_cna_nm,
                                      methodes_ere, cna_ere_struct,
                                      grille_trim, ind_ressources_trim,
                                      ind_prod_trim, ind_apu_trim,
                                      type_prix = "CnaErECrt") {

  meth_comp        <- dplyr::filter(methodes_ere, feuille == feuille_nm, Methode != "solde")
  produits_methodes <- dplyr::select(meth_comp, Code_Produit, Methode)

  sous_element <- if (type_prix == "CnaErECh") "CnaErECh" else "CnaErECrt"
  libelle_prix <- if (type_prix == "CnaErECh") "Ch" else "Crt"

  cna_target <- pivoter_ere_long(
    cna_ere_struct[[composante_cna_nm]][[sous_element]], libelle_prix, feuille_nm
  ) |> dplyr::select(annee, Code_Produit, valeur)

  get_prods <- function(m) dplyr::pull(dplyr::filter(produits_methodes, Methode == m), Code_Produit)

  source_trim <- dplyr::bind_rows(
    { p <- get_prods("lissage")
      if (length(p) > 0) dplyr::mutate(dplyr::filter(grille_trim, Code_Produit %in% p), valeur = 1)
      else dplyr::tibble() },
    { p <- get_prods("ind_ressources")
      if (length(p) > 0) dplyr::filter(ind_ressources_trim, Code_Produit %in% p)
      else dplyr::tibble() },
    { p <- get_prods("prod")
      if (length(p) > 0) dplyr::filter(ind_prod_trim, Code_Produit %in% p)
      else dplyr::tibble() },
    { p <- get_prods("ind_apu")
      if (length(p) > 0) dplyr::filter(ind_apu_trim, Code_Produit %in% p)
      else dplyr::tibble() }
  )

  if (nrow(source_trim) == 0) return(dplyr::tibble())

  source_bench <- dplyr::mutate(source_trim,
    full_code = Code_Produit, type_ind = feuille_nm,
    periode   = paste0(annee, "T", trimestre))
  target_bench <- dplyr::rename(
    dplyr::filter(cna_target, Code_Produit %in% unique(source_trim$Code_Produit)),
    full_code = Code_Produit)

  res_bench <- benchmark_groupe(source_bench, target_bench,
                                type_filter = feuille_nm, value_col = "valeur")

  prods_zero <- get_prods("zero")
  zero_trim  <- dplyr::mutate(dplyr::filter(grille_trim, Code_Produit %in% prods_zero),
                              valeur_cal = 0, full_code = Code_Produit)

  dplyr::bind_rows(
    dplyr::select(res_bench, annee, trimestre, Code_Produit = full_code, valeur_cal),
    dplyr::select(zero_trim, annee, trimestre, Code_Produit, valeur_cal)
  ) |> dplyr::mutate(composante = feuille_nm)
}


#' Calculer une composante emploi ERE par la methode du solde
#'
#' Construit d abord un indicateur trimestriel residuel :
#' \deqn{solde_{trim} = ressources_{trim} - \sum autres\_emplois_{trim}}
#' puis benchmarke ce solde trimestriel sur la cible annuelle CNA de la
#' composante correspondante. Les annees sans cible CNA conservent le profil de
#' l indicateur trimestriel (comportement standard de \code{benchmark_groupe()}).
#'
#' @param feuille_nm Nom de la composante solde (ex. "VS", "CFmarch").
#' @param methodes_ere Table longue des methodes (feuille, Code_Produit, Methode).
#' @param ind_ressources_trim Ressources totales trimestrielles par produit.
#' @param emplois_non_solde Emplois deja estimes (annee, trimestre, Code_Produit, valeur_cal, composante).
#' @param composantes_autres Vecteur des composantes a soustraire pour obtenir le solde.
#' @param map_feuille_cna Tibble feuille <-> composante_cna.
#' @param cna_ere_struct Liste des composantes ERE annuelles.
#' @param type_prix Type de prix pour la cible CNA : \code{"CnaErECrt"} (defaut)
#'   ou \code{"CnaErECh"}.
#' @return Tibble : annee, trimestre, Code_Produit, valeur_cal, composante.
#' @export
calculer_solde_ere <- function(feuille_nm, methodes_ere, ind_ressources_trim,
                               emplois_non_solde, composantes_autres,
                               map_feuille_cna, cna_ere_struct,
                               type_prix = "CnaErECrt") {

  prods_solde <- dplyr::pull(
    dplyr::filter(methodes_ere, feuille == feuille_nm, Methode == "solde"),
    Code_Produit)

  if (length(prods_solde) == 0) return(dplyr::tibble())

  autres_agg <- emplois_non_solde |>
    dplyr::filter(Code_Produit %in% prods_solde, composante %in% composantes_autres) |>
    dplyr::group_by(annee, trimestre, Code_Produit) |>
    dplyr::summarise(autres_emplois = sum(valeur_cal, na.rm = TRUE), .groups = "drop")

  solde_indicateur <- ind_ressources_trim |>
    dplyr::filter(Code_Produit %in% prods_solde) |>
    dplyr::rename(total_ressources = valeur) |>
    dplyr::left_join(autres_agg, by = c("annee", "trimestre", "Code_Produit")) |>
    dplyr::mutate(
      valeur_ind = total_ressources - tidyr::replace_na(autres_emplois, 0),
      composante = feuille_nm
    ) |>
    dplyr::select(annee, trimestre, Code_Produit, valeur_ind, composante)

  composante_cna_nm <- map_feuille_cna$composante_cna[map_feuille_cna$feuille == feuille_nm][1]
  if (is.na(composante_cna_nm) || is.null(cna_ere_struct[[composante_cna_nm]])) {
    return(dplyr::transmute(solde_indicateur, annee, trimestre, Code_Produit,
                            valeur_cal = valeur_ind, composante))
  }

  sous_element <- if (type_prix == "CnaErECh") "CnaErECh" else "CnaErECrt"
  libelle_prix <- if (type_prix == "CnaErECh") "Ch" else "Crt"

  cna_target <- pivoter_ere_long(
    cna_ere_struct[[composante_cna_nm]][[sous_element]], libelle_prix, feuille_nm
  ) |>
    dplyr::filter(Code_Produit %in% prods_solde) |>
    dplyr::select(annee, Code_Produit, valeur)

  if (nrow(cna_target) == 0) {
    return(dplyr::transmute(solde_indicateur, annee, trimestre, Code_Produit,
                            valeur_cal = valeur_ind, composante))
  }

  source_bench <- solde_indicateur |>
    dplyr::mutate(
      full_code = Code_Produit,
      type_ind = feuille_nm,
      periode = paste0(annee, "T", trimestre)
    )
  target_bench <- dplyr::rename(cna_target, full_code = Code_Produit)

  res_bench <- benchmark_groupe(source_bench, target_bench,
                                type_filter = feuille_nm, value_col = "valeur_ind")

  if (nrow(res_bench) == 0) {
    return(dplyr::transmute(solde_indicateur, annee, trimestre, Code_Produit,
                            valeur_cal = valeur_ind, composante))
  }

  dplyr::select(res_bench, annee, trimestre, Code_Produit = full_code,
                valeur_cal, composante)
}
