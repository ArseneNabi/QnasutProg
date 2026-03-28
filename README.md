# QnaSut <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **Outils pour les Comptes Nationaux Trimestriels du Burkina Faso**

<!-- badges: start -->
![Version](https://img.shields.io/badge/version-0.0.0.9000-blue)
![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-brightgreen)
![License](https://img.shields.io/badge/license-MIT-green)
![Tests](https://img.shields.io/badge/tests-59%20pass-success)
<!-- badges: end -->

---

## 📋 Description

`QnaSut` est un package R dédié à la production des **Comptes Nationaux Trimestriels (CNT) du Burkina Faso**. Il fournit une chaîne de traitement complète allant de l'importation des indicateurs trimestriels jusqu'à l'export des résultats, en passant par le benchmarking, le chaînage volume/prix et la déflation/inflation.

### Fonctionnalités principales

- 📥 **Importation** de données depuis des fichiers Excel structurés (indicateurs, CNA, prix, TRE)
- 📐 **Benchmarking** trimestriel par la méthode Cholette (via `rjd3bench`)
- 💱 **Déflation / Inflation** des séries trimestrielles avec indices de prix
- 🔗 **Chaînage** des valeurs en volume (VPAP — Valeur aux Prix de l'Année Précédente)
- 🏗️ **Projection** des ratios structurels TRE (coefficients techniques)
- 📤 **Export Excel** multi-feuilles enrichi avec taux de croissance trimestriels
- ⚙️ **Configuration portable** via `config.yml` (aucun chemin codé en dur)

---

## ⚙️ Prérequis

### R et Java

```r
# R >= 4.1.0 requis (pour le pipe natif |>)
R.version$major  # doit être >= 4
```

> ⚠️ **Java est requis** pour le package `rjd3bench`. Assurez-vous d'avoir une JDK installée et configurée avant d'utiliser ce package.

### Installation de rjd3bench

`rjd3bench` n'est pas sur le CRAN. Installez-le depuis GitHub :

```r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3bench")
```

---

## 📦 Installation

```r
# Depuis le dossier local (développement)
devtools::install("C:/chemin/vers/QnaSut")

# Ou en mode développement (recommandé)
devtools::load_all("C:/chemin/vers/QnaSut")
```

---

## 🚀 Démarrage rapide

### 1. Créer le fichier de configuration

Copiez et adaptez `config.yml` dans votre dossier de travail :

```yaml
# config.yml
default:
  root_dir: "C:/votre/chemin/OutilCntBfa"
  data_hist_dir: "C:/votre/chemin/Data_Historique/TRE"
  nomen_dir: "C:/votre/chemin/Data_Historique/Nomenclatures"
  derniere_annee_definitif: 2022
  annee_fin_projection: 2024
```

### 2. Charger la configuration

```r
library(QnaSut)

# Charger la config (cherche config.yml dans le répertoire courant)
cfg <- load_config()

root_dir      <- cfg$root_dir
data_hist_dir <- cfg$data_hist_dir
nomen_dir     <- cfg$nomen_dir
```

### 3. Importer les données

```r
path_ind  <- file.path(root_dir, "R_Indicateur.xlsx")
path_cna  <- file.path(root_dir, "R_CNA.xlsx")
path_prix <- file.path(root_dir, "R_Prix.xlsx")

# Indicateurs trimestriels
ind_crt  <- import_matrix_cnt(path_ind, "IndCrt")
prod_crt <- import_matrix_cnt(path_cna, "ProdCrt")

# Prix
prix_etal <- import_matrix_prix(path_prix, "Prix_Etal") |> prepare_price_data()
```

### 4. Benchmarking

```r
# Benchmarking trimestriel par méthode Cholette
cnt_ind1_crt <- benchmark_groupe(
  df_source   = ind_crt,
  df_target   = prod_crt,
  type_filter = "Ind1",
  value_col   = "valeur"
)
# → Retourne un tibble avec la colonne valeur_cal (valeurs benchmarkées)
```

### 5. Déflation / Inflation

```r
# Déflater une série courante en volume
est_vol <- apply_price_to_bench(
  df_bench  = cnt_ind2_crt,
  df_prix   = prix_etal,
  operation = "deflate"
)

# Inflater une série volume en courant
est_crt <- apply_price_to_bench(
  df_bench  = cnt_ind3_ch,
  df_prix   = prix_etal,
  operation = "inflate"
)
```

### 6. Chaînage volume/prix

```r
# Calcul de la valeur aux prix de l'année précédente (VPAP)
prod_complete <- prod_complete |>
  group_by(full_code) |>
  mutate(
    valeur_vpap = dechainer_valeurs(valeur_crt, valeur_ch, trim = TRUE)
  ) |>
  ungroup()
```

### 7. Export Excel

```r
# Export multi-feuilles avec taux de croissance automatiques
export_results_excel(
  list(
    "Synthese_Comptes" = df_comptes_finaux,
    "Ind1_Crt"         = cnt_ind1_crt,
    "Ind1_Ch"          = cnt_ind1_ch
  ),
  file_name = "Resultats_CNT_2024.xlsx"
)
```

---

## 📚 Fonctions principales

| Fonction | Description |
|---|---|
| `load_config()` | Lit `config.yml` et valide les paramètres |
| `import_matrix_cnt()` | Importe une matrice d'indicateurs ou de CNA depuis Excel |
| `import_matrix_prix()` | Importe les indices de prix depuis Excel |
| `import_prix_niv3()` | Importe les prix au niveau 3 de nomenclature |
| `benchmark_groupe()` | Benchmarking Cholette trimestriel par groupe de codes |
| `apply_price_to_bench()` | Déflation ou inflation d'une série benchmarkée |
| `dechainer_valeurs()` | Calcul VPAP (trimestriel ou annuel) |
| `calcul_valeur_chainee_trim()` | Valeur chaînée à partir d'indices trimestriels |
| `calcul_valeur_chainee_annuel()` | Valeur chaînée à partir d'indices annuels |
| `agreger_tre_etalonnage()` | Agrège le TRE vers la nomenclature d'étalonnage |
| `trimestrialiser_cnt_complet()` | Trimestrialise les ratios structurels TRE |
| `transformer_branche_produit()` | Transforme des agrégats branches → produits |
| `export_results_excel()` | Export Excel enrichi avec taux de croissance |
| `extract_branch_code()` | Extrait le code branche depuis un code composite |

---

## 🗂️ Structure des données attendues

### Fichiers Excel sources

| Fichier | Feuilles clés | Description |
|---|---|---|
| `R_Indicateur.xlsx` | `IndCrt`, `IndCst`, `IndCE` | Indicateurs trimestriels courant/constant |
| `R_CNA.xlsx` | `ProdCrt`, `ProdCh`, `CiCrt`, `CiVol`, `CiCh`, `CnaEre` | Comptes annuels |
| `R_Prix.xlsx` | `Prix_Etal`, `Prix_Niv3` | Indices de prix |
| `R_Serie_TRE.xlsx` | — | Séries TRE trimestrielles |

### Fichiers RDS

| Fichier | Description |
|---|---|
| `Base_TRE_Historique.rds` | Base TRE historique détaillée |
| `Table_Passage_Niv2_Etal.rds` | Table de correspondance Niv2 → Étalonnage |
| `Map_Branches.rds` | Nomenclature des branches |
| `Map_Produits.rds` | Nomenclature des produits |

### Format des codes composites

Les codes utilisés dans le package suivent la convention :

```
"TypeIndicateur_Secteur*CodeBranche"
# Exemples :
"Ind1_PRIVE*A01001"
"Ind3_INFORMEL*B04003"
"Ind4_TOTAL*GZ002"
```

Utilisez `extract_branch_code()` pour extraire `"A01001"` depuis `"Ind1_PRIVE*A01001"`.

---

## 🧪 Tests

Le package dispose d'une suite de **59 tests unitaires** couvrant les fonctions clés :

```r
devtools::test()
# ✔ | F W  S  OK | Context
# ✔ |         12 | benchmark
# ✔ |         24 | chainage
# ✔ |          8 | config
# ✔ |         15 | utils
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 59 ]
```

---

## 🔁 Programme principal

La chaîne de calcul complète est documentée dans **`ProgCntBFa.Rmd`** (dossier `OutilCntBfa/`).

```
Section 0.1 → Configuration (load_config)
Section 0.2 → Importation des données (safe_import + rm(db_tre))
Section 0.3 → Ratios TRE et trimestrialisation
Section 1.1 → Benchmarking Ind1 / Ind2 / Ind3
Section 1.2 → Déflation (Ind2) / Inflation (Ind3)
Section 1.3 → Benchmarking final
Section 1.4 → Déchaînage VPAP
Section 1.5 → Calcul CI (P2) par branche
Section 1.6 → Indicateurs Ind4
Section 1.7 → Production et CI par produit
Section 1.8 → Export Excel (10 feuilles)
```

**Résultats** : 58 branches, période 2015 T1 → 2024 T3.

---

## 👤 Auteur

**Arsène NABI**
📧 arsenefilsnabi@gmail.com

---

## 📄 Licence

MIT — voir le fichier [LICENSE](LICENSE) pour les détails.
