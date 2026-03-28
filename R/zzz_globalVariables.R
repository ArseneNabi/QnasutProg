utils::globalVariables(c(
  # Génériques dplyr / tidyr
  ".", "key", "key_temp", "col_id", "Header_Col",

  # Temporel
  "Annee", "annee", "Trimestre", "trimestre", "periode",

  # Codes
  "full_code", "Code_Branche", "Code_Produit", "branche", "produit",
  "Code_Prod_N3", "Code_Prod_N2", "Code_Prod_Etal", "Code_Prod_Ct", "Code_Prod_Tpub",
  "Code_Bran_N2", "Code_Bran_Etal", "Code_Bran_Ct", "Code_Bran_Tpub",
  "Code_Branche_Etal", "Code_Branche_Ou_Op",

  # Libellés
  "Libelle_N3", "Libelle_N2", "Libelle_Tpub", "Type_Prod",

  # Valeurs et prix
  "valeur", "valeur_cal", "valeur_estimee",
  "Valeur", "Poids", "Poids_Vol", "Poids_Crt",
  "IP", "IP_Agregat", "IP_Prod", "IP_CI", "prix", "branche_prix",

  # Ratios et coefficients
  "Type_Prix", "Coef_Technique", "Coef_Technique_Calc",
  "CT_Vol", "CT_Crt", "CT_Target", "Val_Relative",

  # Agrégats production / CI
  "P1", "P2", "Total_Branche", "Agregat",
  "somme_crt", "somme_vol",
  "B1_crt", "P1_crt",

  # Opérations / flux
  "Operation", "Nom_Operation", "Flux", "Produit", "Serie",
  "type_ind", "chaine_trim",

  # ggplot2 (plot_benchmark_compare)
  "aes", "theme_minimal", "theme", "element_text",

  # data.table
  ":=",

  # ERE emplois
  ".env", "Methode", "autres_emplois", "base", "composante", "feuille", "ratio", "ratio_trim", "total_ressources", "type_prix", "valeur_composante"
))
