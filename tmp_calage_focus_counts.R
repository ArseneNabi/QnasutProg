library(readxl)
library(dplyr)

project_dir <- "C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut"
global_file <- file.path(project_dir, "Diagnostic_Global_Prix_Volume_ERE_20260408.xlsx")
global_calage <- read_excel(global_file, sheet = "Calage_PreCholette")

focus <- global_calage |>
  filter(annee_complete, cible_presente, !cale_annuel)

cat("ZERO_TARGET_COUNTS\n")
print(
  focus |>
    mutate(cible_zero = abs(valeur_annuelle_cible) < 1e-12) |>
    count(bloc, type_prix, cible_zero, sort = TRUE),
  n = Inf
)

cat("RESOURCE_COMPONENT_COUNTS\n")
print(
  focus |>
    filter(bloc == "ressource") |>
    count(type_prix, composante_source, sort = TRUE),
  n = Inf
)

cat("EMPLOI_COMPONENT_COUNTS\n")
print(
  focus |>
    filter(bloc == "emploi") |>
    count(type_prix, composante_source, sort = TRUE),
  n = Inf
)

cat("RESOURCE_PRODUCT_COUNTS\n")
print(
  focus |>
    filter(bloc == "ressource") |>
    count(type_prix, Code_Produit, sort = TRUE),
  n = Inf
)

cat("EMPLOI_PRODUCT_COUNTS\n")
print(
  focus |>
    filter(bloc == "emploi") |>
    count(type_prix, Code_Produit, sort = TRUE),
  n = Inf
)

cat("RESOURCE_MAX_ECARTS\n")
print(
  focus |>
    filter(bloc == "ressource") |>
    group_by(type_prix, composante_source) |>
    summarise(
      n = n(),
      ecart_abs_max = max(ecart_abs, na.rm = TRUE),
      ecart_abs_med = median(ecart_abs, na.rm = TRUE),
      ecart_rel_max = max(abs(ecart_relatif), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(ecart_abs_max)),
  n = Inf
)

cat("EMPLOI_MAX_ECARTS\n")
print(
  focus |>
    filter(bloc == "emploi") |>
    group_by(type_prix, composante_source) |>
    summarise(
      n = n(),
      ecart_abs_max = max(ecart_abs, na.rm = TRUE),
      ecart_abs_med = median(ecart_abs, na.rm = TRUE),
      ecart_rel_max = max(abs(ecart_relatif), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(ecart_abs_max)),
  n = Inf
)
