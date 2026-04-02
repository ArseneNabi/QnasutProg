utils::globalVariables(c(
  # ── Generiques dplyr / tidyr ──────────────────────────────────────────────
  ".", "key", "key_temp", "col_id", "Header_Col", "n",

  # ── Temporel ─────────────────────────────────────────────────────────────
  "Annee", "annee", "Trimestre", "trimestre", "periode",

  # ── Codes produit / branche ───────────────────────────────────────────────
  "full_code", "Code_Branche", "Code_Produit", "branche", "produit",
  "Code_Prod_N3", "Code_Prod_N2", "Code_Prod_Etal", "Code_Prod_Ct", "Code_Prod_Tpub",
  "Code_Bran_N2", "Code_Bran_Etal", "Code_Bran_Ct", "Code_Bran_Tpub",
  "Code_Branche_Etal", "Code_Branche_Ou_Op",
  "Code",          # lire_methodes_ere : colonne Code dans Methode_ERE.xlsx

  # ── Libelles ──────────────────────────────────────────────────────────────
  "Libelle_N3", "Libelle_N2", "Libelle_Tpub", "Type_Prod",

  # ── Valeurs et prix ───────────────────────────────────────────────────────
  "valeur", "valeur_cal", "valeur_estimee",
  "Valeur", "Poids", "Poids_Vol", "Poids_Crt",
  "IP", "IP_Agregat", "IP_Prod", "IP_CI", "prix", "branche_prix",
  "valeur_crt", "valeur_ch", "valeur_vol", "valeur_vpap",

  # ── Intermediaires calcul deflation / benchmarking ────────────────────────
  "valeur_src",        # benchmark_groupe : colonne temporaire
  "original_val",      # benchmarker_p1/p2_ere : .restaurer_hors_cible
  "valeur_avant",      # diagnostiquer_ere : .prepare_pair
  "valeur_apres",      # diagnostiquer_ere : .prepare_pair
  "valeur_ind",        # calculer_solde_ere
  "valeur_crt_cal",    # integrer_ind4_dans_p1, calculer_ind4_depuis_ci
  "valeur_ch_cal",     # calculer_ind4_depuis_ci
  "valeur_vpap_cal",   # calculer_ind4_depuis_ci

  # ── Ratios et coefficients ────────────────────────────────────────────────
  "Type_Prix", "Coef_Technique", "Coef_Technique_Calc",
  "CT_Vol", "CT_Crt",            # calculer_comptes_production
  "CT_Target", "Val_Relative",
  "Courant", "Constant",         # calculer_ci_branches : pivot_wider names

  # ── Production P1 ─────────────────────────────────────────────────────────
  "P1", "P1_crt_agg", "P1_vpap_agg",   # calculer_ci_branches, comptes
  "P1_vol",                              # calculer_p1_ere, ind5, ressources_ere
  "P1_ch",                               # calculer_p1_ere, calculer_comptes_production
  "P1_vpap",                             # calculer_comptes_production
  "P1_crt",                              # export_results_excel (evol_P1_crt_pct)

  # ── Consommation intermediaire P2 ─────────────────────────────────────────
  "P2",
  "P2_crt",     "P2_vol",     "P2_ch",     # calculer_p2_ere, benchmarker_p2, comptes
  "P2_crt_cal", "P2_vpap_cal", "P2_ch_cal", # calculer_ci_branches, calculer_comptes_production
  "P2_ch_est",                              # calculer_ci_branches : est_p2_chained
  "P2_vpap",                                # calculer_comptes_production
  "Ind_P2_crt", "Ind_P2_vpap",             # calculer_ci_branches : df_calcul_p2

  # ── Valeur ajoutee ────────────────────────────────────────────────────────
  "VA_crt", "VA_vpap", "VA_ch",
  "B1_vpap",                               # calculer_comptes_branches
  "b1_calcule", "b1_reference",            # diagnostiquer_ere
  "B1_calcule_total", "B1_reference_total",# diagnostiquer_ere

  # ── ERE imports / exports ─────────────────────────────────────────────────
  "imp_crt", "imp_vol", "imp_vpap",        # executer_ressources_ere, ind5
  "exp_crt", "exp_vpap",                   # executer_emplois_ere

  # ── ERE ressources / emplois : intermediaires ─────────────────────────────
  "base_crt", "base_vol",                  # calculer_ind5_depuis_production
  "marge",                                 # calculer_ind5_depuis_production : .calc_marge
  "val_crt", "val_ch",                     # calculer_ind5_depuis_production (Ind5 VPAP)
  "valeur_composante",                     # assembler_ressources_ere, calculer_ratios_ere

  # ── ERE ratios ────────────────────────────────────────────────────────────
  "ratio", "ratio_trim", "composante", "feuille",
  "type_prix", "autres_emplois", "total_ressources",
  "base",                                  # calculer_ratios_ere : base de calcul
  ".env",                                  # appliquer_ratios_ere : pronoun

  # ── Methodes ERE ──────────────────────────────────────────────────────────
  "Methode",                               # lire_methodes_ere

  # ── Diagnostics ERE (diagnostiquer_ere) ───────────────────────────────────
  "agregat",           # diagnostiquer_ere : colonne ajoutee dans ecarts_benchmarking
  "ecart_abs",         # diagnostiquer_ere : dans .prepare_pair et contributions
  "ecart_abs_total",   # diagnostiquer_ere : contributions_produits
  "ecart_rel",         # diagnostiquer_ere : coherence_b1_branche
  "P2_crt",            # diagnostiquer_ere : ratios_p2_sur_p1  (deja declare mais repete)
  "P1_ch", "P2_ch",    # diagnostiquer_ere : anomalies_ruptures (idem)
  "variation_t_t1",    # diagnostiquer_ere : anomalies_ruptures

  # ── Agregats production / CI ──────────────────────────────────────────────
  "Total_Branche", "Agregat",
  "somme_crt", "somme_vol",
  "ci_ch_agg",                             # calculer_ci_branches

  # ── Consolidation production complete (11_pipeline_production_directe) ────
  # valeur_crt et valeur_ch deja declares plus haut

  # ── Operateurs / flux ─────────────────────────────────────────────────────
  "Operation", "Nom_Operation", "Flux", "Produit", "Serie",
  "type_ind", "chaine_trim",

  # ── ggplot2 (plot_benchmark_compare) ─────────────────────────────────────
  "aes", "theme_minimal", "theme", "element_text",

  # ── data.table ────────────────────────────────────────────────────────────
  ":="
))
