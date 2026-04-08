library(readxl)
library(dplyr)

project_dir <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut"
pre_file <- file.path(project_dir, "Diagnostic_PreCholette_ERE_20260408.xlsx")
global_file <- file.path(project_dir, "Diagnostic_Global_Prix_Volume_ERE_20260408.xlsx")

cat("PRE_SHEETS\n")
print(excel_sheets(pre_file))
cat("GLOBAL_SHEETS\n")
print(excel_sheets(global_file))

pre_prod <- read_excel(pre_file, sheet = "Synthese_Calage_Produit")
pre_comp <- read_excel(pre_file, sheet = "Synthese_Calage_Composante")
pre_alert <- read_excel(pre_file, sheet = "Alertes_Calage")
global_stages <- read_excel(global_file, sheet = "Synthese_Stages")
global_top <- read_excel(global_file, sheet = "Top_Anomalies")
global_calage <- read_excel(global_file, sheet = "Calage_PreCholette")
global_alert <- read_excel(global_file, sheet = "Alertes_Calage_CRT_VPAP")

cat("COLS_PRE_PROD\n")
print(names(pre_prod))
cat("COLS_PRE_COMP\n")
print(names(pre_comp))
cat("COLS_PRE_ALERT\n")
print(names(pre_alert))
cat("COLS_GLOBAL_CALAGE\n")
print(names(global_calage))
cat("COLS_GLOBAL_ALERT\n")
print(names(global_alert))

cat("PRE_PROD_TOP20\n")
print(head(pre_prod, 20))

cat("PRE_COMP_TOP20\n")
print(head(pre_comp, 20))

cat("PRE_ALERT_TOP20\n")
print(head(pre_alert, 20))

cat("GLOBAL_STAGE_SUMMARY\n")
print(global_stages)

cat("GLOBAL_TOP_ANOMALIES_TOP30\n")
print(head(global_top, 30))

cat("GLOBAL_CALAGE_TOP30\n")
print(head(global_calage, 30))

cat("GLOBAL_ALERT_TOP30\n")
print(head(global_alert, 30))

cat("PRE_ALERT_STATUS\n")
print(pre_alert |> count(bloc, type_prix, statut_calage, sort = TRUE))

cat("PRE_ALERT_COMPLETE_YEARS\n")
print(
  pre_alert |>
    filter(annee_complete, cible_presente) |>
    count(bloc, type_prix, statut_calage, sort = TRUE)
)

cat("PRE_ALERT_TOP_COMPONENTS\n")
print(
  pre_alert |>
    filter(annee_complete, cible_presente) |>
    count(bloc, type_prix, composante_source, sort = TRUE) |>
    group_by(bloc, type_prix) |>
    slice_head(n = 10) |>
    ungroup()
)

cat("PRE_ALERT_TOP_PRODUCTS\n")
print(
  pre_alert |>
    filter(annee_complete, cible_presente) |>
    count(bloc, type_prix, Code_Produit, sort = TRUE) |>
    group_by(bloc, type_prix) |>
    slice_head(n = 10) |>
    ungroup()
)

cat("GLOBAL_CALAGE_STATUS\n")
print(global_calage |> count(bloc, type_prix, statut_calage, sort = TRUE))

cat("GLOBAL_CALAGE_COMPLETE_YEARS\n")
print(
  global_calage |>
    filter(annee_complete, cible_presente) |>
    count(bloc, type_prix, statut_calage, sort = TRUE)
)

cat("GLOBAL_CALAGE_TOP_COMPONENTS\n")
print(
  global_calage |>
    filter(annee_complete, cible_presente, !cale_annuel) |>
    count(bloc, type_prix, composante_source, sort = TRUE) |>
    group_by(bloc, type_prix) |>
    slice_head(n = 15) |>
    ungroup()
)

cat("GLOBAL_CALAGE_TOP_PRODUCTS\n")
print(
  global_calage |>
    filter(annee_complete, cible_presente, !cale_annuel) |>
    count(bloc, type_prix, Code_Produit, sort = TRUE) |>
    group_by(bloc, type_prix) |>
    slice_head(n = 15) |>
    ungroup()
)

cat("GLOBAL_CALAGE_MAX_ECARTS\n")
print(
  global_calage |>
    filter(annee_complete, cible_presente) |>
    group_by(bloc, type_prix) |>
    summarise(
      n = n(),
      ecart_abs_max = max(ecart_abs, na.rm = TRUE),
      ecart_abs_med = median(ecart_abs, na.rm = TRUE),
      ecart_rel_max = max(abs(ecart_relatif), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(ecart_abs_max))
)
