library(readxl)
library(dplyr)

project_dir <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut"

global_file <- file.path(project_dir, "Diagnostic_Global_Prix_Volume_ERE_20260408.xlsx")
pre_file <- file.path(project_dir, "Diagnostic_PreCholette_ERE_20260408.xlsx")

cat("GLOBAL_SHEETS=\n")
print(excel_sheets(global_file))
cat("PRE_SHEETS=\n")
print(excel_sheets(pre_file))

synthese_stages <- read_excel(global_file, sheet = "Synthese_Stages")
egalites_post2015 <- read_excel(global_file, sheet = "Egalites_Post2015")
top_anomalies <- read_excel(global_file, sheet = "Top_Anomalies")

cat("N_EGALITES_POST2015=", nrow(egalites_post2015), "\n", sep = "")
cat("COLS_SYNTH_STAGES=\n")
print(names(synthese_stages))
cat("TOP_STAGES_BY_EQ_SHARE=\n")
print(
  synthese_stages |>
    mutate(part_eq = n_egalites_crt_vpap / pmax(n_post_base, 1)) |>
    arrange(desc(part_eq)) |>
    select(stage, n_post_base, n_egalites_crt_vpap, part_eq, abs_delta_crt_vpap_max)
)

cat("TOP_20_EGALITES_POST2015=\n")
print(head(egalites_post2015, 20))

cat("TOP_ANOMALIES=\n")
print(head(top_anomalies, 30))

calage_produit <- read_excel(pre_file, sheet = "Synthese_Calage_Produit")
alertes_calage <- read_excel(pre_file, sheet = "Alertes_Calage")
crt_vpap_pre <- read_excel(pre_file, sheet = "Synthese_CRT_VPAP")

cat("PRE_N_ALERTES_CALAGE=", nrow(alertes_calage), "\n", sep = "")
cat("PRE_TOP_CALAGE_PRODUIT=\n")
print(
  calage_produit |>
    arrange(desc(abs_ecart_annuel_max)) |>
    head(20)
)

cat("PRE_SYNTH_CRT_VPAP=\n")
print(
  crt_vpap_pre |>
    arrange(desc(part_trimestres_egaux)) |>
    head(30)
)
