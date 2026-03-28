# AGENTS.md — Mémo projet QnaSut (Comptes Nationaux Trimestriels - Burkina Faso)
*Dernière mise à jour : 27 mars 2026*

## 🎯 Description du projet

**QnaSut** est un package R développé pour la production des **Comptes Nationaux Trimestriels (CNT) du Burkina Faso**. Il implémente une chaîne complète de calcul trimestriel : importation des indicateurs → benchmarking (méthode Cholette via `rjd3bench`) → déflation/inflation → déchaînage → export Excel.

Le programme principal est **`ProgCntBFa_Evol.Rmd`**, situé dans le dossier `OutilCntBfa/`.

---

## 📁 Structure des fichiers

```
C:/CnaBfaScn08/CntBfaV4/07P_Outils/
├── QnaSut/                          ← Package R (développement RStudio)
│   ├── R/
│   │   ├── 00_config.R              ← load_config() : lecture config.yml
│   │   ├── 00_get_data.R
│   │   ├── 00_import_nomenclatures.R
│   │   ├── 00_import_tre_detail.R
│   │   ├── 01_import.R              ← import_matrix_cnt(), import_matrix_prix(), pivoter_ere_long()
│   │   ├── 01bis_projection_ratios.R← trimestrialiser_cnt_complet(), calculer_ratios_ere(), trimestrialiser_ratios_ere()
│   │   ├── 02_utils.R               ← extract_branch_code(), extract_target_code(), appliquer_ratios_ere()
│   │   ├── 03_benchmark.R           ← benchmark_groupe() : méthode Cholette
│   │   ├── 04_deflate.R             ← apply_price_to_bench()
│   │   ├── 05_outputs.R             ← export_results_excel() : export Excel enrichi
│   │   ├── 06_chainage.R            ← dechainer_valeurs(), calcul_valeur_chainee_trim(), etc.
│   │   ├── 07_analyse_structurelle.R
│   │   ├── 08_transformer_branche_produit.R ← transformer_branche_produit()
│   │   ├── 09_ind5_marges.R         ← integrer_ind4_dans_p1(), calculer_ind5_depuis_production()
│   │   ├── 10_comptes_production.R  ← calculer_comptes_production()
│   │   ├── 11_ere_imports_exports.R ← executer_benchmarking_imp_exp() (avec repli distribution plate)
│   │   ├── 12_ere_ressources.R      ← calculer_p1_ere(), benchmarker_p1_ere(), benchmarker_p2_ere(),
│   │   │                               calculer_p2_ere(), executer_ressources_ere()
│   │   ├── 13_ere_emplois.R         ← executer_emplois_ere()
│   │   └── zzz_globalVariables.R
│   ├── tests/testthat/              ← 59 tests unitaires (0 échec, 0 warning)
│   │   ├── test-benchmark.R         ← 12 tests benchmark_groupe()
│   │   ├── test-chainage.R          ← 24 tests fonctions chaînage
│   │   ├── test-config.R            ← 8 tests load_config()
│   │   └── test-utils.R             ← 15 tests fonctions utilitaires
│   └── DESCRIPTION                  ← yaml dans Imports, testthat+withr dans Suggests
│
├── OutilCntBfa/                     ← Programme principal
│   ├── ProgCntBFa_Evol.Rmd          ← Chaîne de calcul CNT complète (version active)
│   ├── ProgCntBFa.Rmd               ← Ancienne version (référence)
│   ├── config.yml                   ← ⚠️ FICHIER À ADAPTER PAR POSTE
│   ├── R_Indicateur.xlsx            ← Indicateurs trimestriels (IndCrt, IndCst)
│   ├── R_CNA.xlsx                   ← Comptes annuels (ProdCrt, ProdCh, CiCrt, etc.)
│   ├── R_Prix.xlsx                  ← Prix (Prix_Etal, Prix_Niv3)
│   └── R_Serie_TRE.xlsx             ← Séries TRE
│
└── Data_Historique/
    ├── TRE/
    │   ├── Base_TRE_Historique.rds
    │   └── Table_Passage_Niv2_Etal.rds
    └── Nomenclatures/
        ├── Map_Branches.rds
        └── Map_Produits.rds
```

---

## 🚀 Comment démarrer une session

### Dans Positron (assistant IA)

```r
# 1. Se placer dans le bon répertoire
setwd("C:/CnaBfaScn08/CntBfaV4/07P_Outils/OutilCntBfa")

# 2. Recharger le package (après modifications dans RStudio)
devtools::load_all("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")

# 3. Charger la config et les données
library(QnaSut); library(dplyr); library(tidyr); library(readxl); library(rjd3bench)
cfg <- load_config()
# → lit config.yml dans le répertoire courant
```

### Workflow hybride recommandé
- **RStudio** : développement du package (`devtools::load_all()`, `devtools::check()`, `devtools::test()`)
- **Positron** : exécution de `ProgCntBFa.Rmd`, diagnostic, corrections assistées par IA

---

## ⚙️ config.yml — Structure attendue

```yaml
default:
  root_dir: "C:/..."                  # Dossier OutilCntBfa
  data_hist_dir: "C:/..."             # Dossier Data_Historique/TRE
  nomen_dir: "C:/..."                 # Dossier Data_Historique/Nomenclatures
  derniere_annee_definitif: 2022      # Dernière année CNA définitive
  annee_fin_projection: 2024          # Année de fin de projection
```

> ⚠️ Seul `config.yml` est à modifier sur un nouveau poste. Le Rmd et le package ne contiennent plus aucun chemin absolu.

---

## 🔧 Fonctions clés du package

### Fonctions générales

| Fonction | Fichier | Description |
|---|---|---|
| `load_config()` | `00_config.R` | Lit `config.yml`, valide les clés, retourne une liste |
| `benchmark_groupe()` | `03_benchmark.R` | Benchmarking Cholette trimestriel par groupe de codes |
| `apply_price_to_bench()` | `04_deflate.R` | Déflation ou inflation d'une série benchmarkée |
| `export_results_excel()` | `05_outputs.R` | Export Excel multi-feuilles avec taux de croissance par code |
| `dechainer_valeurs()` | `06_chainage.R` | Calcul VPAP (valeur aux prix de l'année précédente) |
| `calcul_valeur_chainee_trim()` | `06_chainage.R` | Valeur chaînée trimestrielle |
| `calcul_valeur_chainee_annuel()` | `06_chainage.R` | Valeur chaînée annuelle |
| `extract_branch_code()` | `02_utils.R` | Extrait le code après `*` dans un code composite |
| `agreger_tre_etalonnage()` | `00_import_tre_detail.R` | Agrège le TRE vers la nomenclature d'étalonnage |
| `trimestrialiser_cnt_complet()` | `01bis_projection_ratios.R` | Trimestrialise les ratios TRE (hist + proj) |
| `transformer_branche_produit()` | `08_transformer_branche_produit.R` | Désagrège une grandeur branche → produits via poids TRE |
| `integrer_ind4_dans_p1()` | `09_ind5_marges.R` | Remplace les branches Ind4 dans `p1_agreg` par leurs estimations finales calibrées |
| `calculer_ind5_depuis_production()` | `09_ind5_marges.R` | Chaîne complète Ind5 : VPAP → marges → benchmarking → intégration dans `p1_agreg_complet` |
| `calculer_comptes_production()` | `10_comptes_production.R` | Complète la CI pour les branches manquantes (CT), chaîne P1/P2, calcule VA = P1 − P2 |

### Fonctions optique dépenses (ERE)

| Fonction | Fichier | Description |
|---|---|---|
| `pivoter_ere_long()` | `01_import.R` | Convertit un tableau ERE large (colonnes = codes produit) en format long (`Code_Produit`, `valeur`) |
| `calculer_ratios_ere()` | `01bis_projection_ratios.R` | Calcule les ratios annuels pour 7 composantes ERE à partir de `cna_ere_struct` |
| `trimestrialiser_ratios_ere()` | `01bis_projection_ratios.R` | Trimestrialise les ratios ERE (interpolation linéaire + extrapolation constante) |
| `appliquer_ratios_ere()` | `02_utils.R` | Applique les ratios trimestriels à une base (importations ou production+importations) |
| `calculer_p1_ere()` | `12_ere_ressources.R` | Désagrège `p1_agreg_complet` (60 branches) en produits N3 via TRE puis agrège vers ERE |
| `benchmarker_p1_ere()` | `12_ere_ressources.R` | Recale P1 produit ERE sur les cibles CNA annuelles (garantit égalité annuelle Res=Emp) |
| `calculer_p2_ere()` | `12_ere_ressources.R` | Agrège la CI N3 (`p2_par_produit_n3`) vers la nomenclature ERE |
| `benchmarker_p2_ere()` | `12_ere_ressources.R` | Recale CI produit ERE sur les cibles CNA annuelles (garantit égalité annuelle Res=Emp) |
| `executer_benchmarking_imp_exp()` | `11_ere_imports_exports.R` | Benchmarking Cholette imports/exports avec repli automatique en distribution plate |
| `executer_ressources_ere()` | `12_ere_ressources.R` | Assemble les 9 composantes ressources ERE (courant, VPAP, chaîné) |
| `executer_emplois_ere()` | `13_ere_emplois.R` | Assemble les 9 composantes emplois ERE (courant, VPAP, chaîné) |

#### Détail `calculer_ratios_ere()`
- **Entrée** : `cna_ere_struct` (liste 19 composantes), `type_prix` (`"CnaErECrt"`, `"CnaErECh"`, `"CnaErEVol"`)
- **7 composantes calculées** :
  - Base = IMPORTATIONS : `IMPOT sur Import`, `IMPOT sur export`
  - Base = PRODUCTION + IMPORTATIONS : `MARGE de commerce`, `MARGE de transport`, `TVA`, `IMPOT sur produit`, `Subventions`
- **Sortie** : `annee`, `Code_Produit`, `composante`, `type_prix`, `ratio`

#### Détail `trimestrialiser_ratios_ere()`
- **Principe** : ratios traités comme des **moyennes** (non des flux) → chaque trimestre = ratio annuel
- **Interpolation** : linéaire entre années CNA connues, extrapolation constante pour les années de projection
- **Paramètres** : `derniere_annee_cna`, `annee_fin_proj`

---

## 📊 Structure de la chaîne ProgCntBFa_Evol.Rmd

### Section 0 — Étapes préparatoires ✅ COMPLET

| Section | Description | Variables produites |
|---|---|---|
| **0.1** | Setup : packages + `load_config()` + chemins | `cfg`, `root_dir`, `path_*` |
| **0.2** | Import données via `charger_donnees_cnt(cfg)` | `ind_crt`, `ind_cst`, `prod_crt`, `prod_ch`, `ci_*`, `ind_ce_struct`, `cna_ere_struct`, `prix_*`, `Map_*` |
| **0.3** | Calcul ratios TRE + trimestrialisation via `executer_module_tre()` | `ratios_annuels`, `res_trim` (liste : `poids_trim`, `ct_trim`, `ip_branches`) |

### Section 1 — Optique Production ✅ COMPLET

| Section | Description | Variables produites |
|---|---|---|
| **1.1** | Benchmarking Ind1/Ind2/Ind3 via `executer_optique_production_directe()` | `cnt_ind1/2/3_crt/ch`, `est_ind2_vol`, `est_ind3_val`, `prod_complete` |
| **1.2.1** | Calcul CI (P2) par branche via `calculer_ci_branches()` | `p1_agreg`, `p2_final_cal`, `bench_p2_crt/ch` |
| **1.2.2** | Indicateurs Ind4 indirects via `calculer_ind4_depuis_ci()` | `cnt_ind4_crt/ch`, `ind4_final`, `p2_par_produit_n3` |
| **1.2.3** | Indicateurs Ind5 (commerce + transport) via `calculer_ind5_depuis_production()` | voir détail ci-dessous |

#### Détail section 1.2.3 — `calculer_ind5_depuis_production()`

Chaîne en 7 étapes encapsulée dans le package :

| Étape | Action | Sortie |
|---|---|---|
| 1 | Intégration Ind4 dans `p1_agreg` (`integrer_ind4_dans_p1`) | `p1_agreg_complet` (58 branches) |
| 2 | Désagrégation branche → produit N3 (`transformer_branche_produit`) | `p1_par_produit` (7 995 lignes) |
| 3 | Agrégation N3 → ERE + importations brutes (`ind_ce_struct`) | `p1_ere_crt`, `p1_ere_vol`, bases P1+M |
| 4 | Ratios ERE annuels + trimestrialisation (courant et volume) | `ratios_ere_trim_crt`, `ratios_ere_trim_vol` |
| 5 | Ind5 = Σ_produits (P1+M)_trim × taux_marge_trim | `ind5_marges` (78 lignes, 0 NA) |
| 6 | Benchmarking Cholette sur cibles CNA (`prod_crt`, `prod_ch`) | `cnt_ind5_crt`, `cnt_ind5_ch` |
| 7 | VPAP (`dechainer_valeurs`) + intégration dans `p1_agreg_complet` | `p1_agreg_complet` (**60 branches**) |

**Paramètres clés** : `map_branches = c("MARGE de commerce" = "GZ001", "MARGE de transport" = "HZ001")`

**Variables exposées dans le Rmd** après déstructuration de `ind5` :
`p1_agreg_complet`, `p1_par_produit`, `p1_ere_crt`, `p1_ere_vol`, `ratios_ere_trim_crt`, `ratios_ere_trim_vol`, `ind5_marges`, `cnt_ind5_crt`, `cnt_ind5_ch`

### Section 1.3 — Comptes de production (P1, P2, VA) ✅ COMPLET

| Section | Description | Variables produites |
|---|---|---|
| **1.3** | CI complète (GZ001/HZ001 via CT) + VA par branche via `calculer_comptes_production()` | `df_comptes_finaux` (2 340 lignes, 60 branches, 0 NA) |

`df_comptes_finaux` contient pour chaque branche × trimestre :
`P1_crt`, `P1_vpap`, `P1_ch`, `P2_crt`, `P2_vpap`, `P2_ch`, `VA_crt`, `VA_vpap`, `VA_ch`

Export : `Comptes_Production_<date>.xlsx` (4 feuilles : Courant, VPAP, Chaine, Synthese_annuelle)

### Section 2 — Optique Dépenses (ERE) ✅ COMPLET

| Section | Description | Variables produites |
|---|---|---|
| **2.1** | P1 branche→produit ERE (60 branches) + benchmarking CNA + CI ERE + benchmarking CI + Imports/Exports | `p1_ere_crt`, `p1_ere_vol`, `p2_ere_crt`, `p2_ere_vol`, `cnt_imp_final`, `cnt_exp_final` |
| **2.2** | Assemblage ressources ERE (courant, VPAP, chaîné) | `ere_ressources_completes` (13 338 lignes, 9 composantes, 39 produits, 0 NA) |
| **2.3** | Assemblage emplois ERE (courant, VPAP, chaîné) | `emplois_ere_complets` (13 783 lignes, 9 composantes, 0 NA), `emplois_ere_vol`, `emplois_ere_vpap` |

#### Pipeline complet de la section 2.1 (ordre d'appel dans le Rmd)

```r
# 1. P1 : branche → produit N3 → ERE (60 branches, inclut GZ001/HZ001)
p1_final     <- calculer_p1_ere(p1_agreg_complet, res_trim$poids_trim, Map_Produits)
# 2. P1 : benchmarking produit vs cibles CNA annuelles
p1_ere_bench <- benchmarker_p1_ere(p1_final$p1_ere_crt, p1_final$p1_ere_vol, cna_ere_struct)
p1_ere_crt   <- p1_ere_bench$p1_ere_crt    # colonnes : annee, trimestre, Code_Produit, P1_crt
p1_ere_vol   <- p1_ere_bench$p1_ere_vol    # colonnes : annee, trimestre, Code_Produit, P1_vol

# 3. CI : agrégation N3 → ERE
p2_ere       <- calculer_p2_ere(p2_par_produit_n3, Map_Produits)
# 4. CI : benchmarking produit vs cibles CNA annuelles
p2_ere_bench <- benchmarker_p2_ere(p2_ere$p2_ere_crt, p2_ere$p2_ere_vol, cna_ere_struct)
p2_ere_crt   <- p2_ere_bench$p2_ere_crt   # colonnes : annee, trimestre, Code_Produit, P2_crt
p2_ere_vol   <- p2_ere_bench$p2_ere_vol

# 5. Imports/Exports : benchmarking Cholette avec repli distribution plate
imp_exp      <- executer_benchmarking_imp_exp(ind_ce_struct, cna_ere_struct)
cnt_imp_final <- imp_exp$cnt_imp_final
cnt_exp_final <- imp_exp$cnt_exp_final
```

> **⚠️ Important** : `p1_par_produit` retourné par `ind5` (section 1.2.3) est basé sur 58 branches et n'est utile que pour le calcul interne des marges Ind5. Le `p1_par_produit` exposé dans le Rmd est celui de `calculer_p1_ere()` (60 branches). De même, les `p1_ere_crt`/`p1_ere_vol` de la section 1.2.3 sont écrasés en section 2.1.

#### Composantes de `ere_ressources_completes`
1. `PRODUCTION` — de `p1_ere_crt` (benchmarké vs CNA)
2. `IMPORTATIONS` — de `cnt_imp_final` (Cholette ou distribution plate)
3. `IMPOT sur Import` — ratio × importations
4. `IMPOT sur export` — ratio × importations
5. `MARGE de commerce` — ratio × (production + importations)
6. `MARGE de transport` — ratio × (production + importations)
7. `TVA` — ratio × (production + importations)
8. `IMPOT sur produit` — ratio × (production + importations)
9. `Subventions` — ratio × (production + importations)

---

## 🗂️ Structures de données clés

### `cna_ere_struct` — liste de 19 composantes ERE annuelles
- Chaque élément est une liste avec 3 sous-éléments : `CnaErECh`, `CnaErECrt`, `CnaErEVol`
- Format : tableau **large** — colonnes = codes produit ERE (`AA000`, `AB000`, ..., `XZ000`), lignes = années
- Noms des composantes (ex.) : `"PRODUCTION"`, `"IMPORTATIONS"`, `"Exportations de biens et services"`, `"MARGE de commerce"`, ...

### `ind_ce_struct` — liste indicateurs Import/Export trimestriels
- Structure : `ind_ce_struct$Import$Crt`, `ind_ce_struct$Import$Ch`, `ind_ce_struct$Export$Crt`, `ind_ce_struct$Export$Ch`
- Format : tableau **large** — colonnes = codes produit, lignes = trimestres (subset `Ch`/`Crt`/`Vol`)

### `res_trim` — liste de 4 éléments (ratios TRE trimestrialisés)
- `coef_tech_trim` : coefficients techniques par branche/produit
- `poids_trim` : poids pour `transformer_branche_produit()`
- `prix_trim` : indices de prix trimestriels
- `ratio_val_trim` : ratios valeur par branche

### `Map_Produits` — table de correspondance produits [205 × 9]
- `Code_Prod_N3` : code nomenclature niveau 3 (utilisé dans TRE)
- `Code_Prod_Ct` : code nomenclature ERE (39 produits : `AA000`, `AB000`, ..., `XZ000`)
- `Code_Prod_Etal` : code nomenclature d'étalonnage (pour Ind4)

### `Map_Branches` — table de correspondance branches [121 × 6]

---

## ✅ Corrections déjà appliquées (ne pas refaire)

1. **Chemins absolus** → remplacés par `config.yml` + `load_config()`
2. **`magrittr` (`%>%`)** → remplacé par pipe natif `|>` (210 occurrences) + `magrittr` retiré du `DESCRIPTION`
3. **Bug `.` magrittr** → corrigé dans `04_deflate.R` (ligne `names(.)`) et `01_import.R`
4. **`clean_code_fct`** → déplacée dans `02_utils.R` comme `extract_branch_code()`
5. **`rename(Coef_Technique = Coef_Technique)`** → supprimé (§0.3 du Rmd)
6. **`rm(db_tre)`** → ajouté après agrégation (libère ~880k lignes)
7. **`safe_import()`** → ajouté autour de tous les imports critiques (§0.2)
8. **`@importFrom magrittr |>`** → annotations obsolètes supprimées dans 4 fichiers
9. **`.data$` dans `dplyr::select()`** → corrigé dans `03_benchmark.R`
10. **`export_results_excel()`** → améliorée avec taux de croissance par `full_code` (group_by)
11. **Tests unitaires** → 59 tests dans `tests/testthat/` (0 échec, 0 warning)
12. **`calculer_p1_ere()` ajoutée** (`12_ere_ressources.R`) → désagrège `p1_agreg_complet` (60 branches) vers produits N3 puis ERE. Remplace le `p1_ere_crt` interne à `calculer_ind5_depuis_production()`, qui était basé sur seulement 58 branches (GZ001/HZ001 manquants).
13. **`benchmarker_p1_ere()` ajoutée** (`12_ere_ressources.R`) → recale P1 produit ERE sur les cibles CNA annuelles via Cholette. Garantit l'égalité annuelle Ressources = Emplois pour les années CNA.
14. **`benchmarker_p2_ere()` ajoutée** (`12_ere_ressources.R`) → recale CI produit ERE sur les cibles CNA annuelles via Cholette. Corrige le sous-estimé systématique de CI pour les produits de services (DZ000, HZ001, KZ000, MN002, CC007, HZ002) hérité de la désagrégation TRE branche → produit.
15. **`executer_benchmarking_imp_exp()` améliorée** (`11_ere_imports_exports.R`) → repli automatique en **distribution plate** (CNA/4, dernière valeur extrapolée pour la projection) dans trois cas : (1) produit absent des indicateurs trimestriels (AA000, AB000, AC000), (2) benchmarking Cholette retourne un résultat vide (OZ000, EZ000), (3) valeurs négatives après Cholette (AE000, CC003, CC004, CC006, CD000). Passe de 36 à 39 produits couverts. `inner_join` → `full_join` avec `replace_na(0)` pour garantir la symétrie crt/ch.

---

## ⚠️ Points d'attention

### Doublon CI pour les branches Ind4 — C12003 corrigé ✅

**Problème détecté** : Pour la branche C12003 (Ind4), deux entrées coexistaient dans `ci_crt` / `ci_vol` / `ci_ch` de `R_CNA.xlsx` :
- `Ind3_PRIVE*C12003` (colonne 60) — composante partielle
- `Ind4_TOTAL*C12003` (colonne 109) — total consolidé

`calculer_ci_branches()` sommait les deux comme des composantes additives, doublant le CI de C12003 (P2_trim_sum ≈ 2 × Ind4_TOTAL). Cela produisait des VA négatives à partir de 2020, s'aggravant jusqu'en 2024.

**Correction appliquée** : Suppression de la colonne `Ind3_PRIVE*C12003` dans les 3 feuilles CI de `R_CNA.xlsx` (col. 60 dans CiCrt, CiVol, CiCh). Après correction, P2 annuel = exactement la cible `Ind4_TOTAL*C12003` (écart = 0).

**⚠️ Ne pas refaire** : Ne jamais rajouter de colonne `Ind3_PRIVE*C12003` dans les feuilles CI de `R_CNA.xlsx`. Pour les branches Ind4 (B04003, C12003, MN001, MN002), seule la colonne `Ind4_TOTAL*<branche>` doit être présente dans les feuilles CI.

> **Règle générale** : Vérifier que pour chaque branche Ind4, une seule entrée `Ind4_TOTAL*<code>` apparaît dans `ci_crt`. Toute entrée `Ind3_*` ou `Ind2_*` pour ces mêmes branches serait redondante.

### NA dans les données sources
- `ind_crt` et `ind_cst` contiennent des NA pour **Ind3, Ind4, Ind5** → **NORMAL**
- Ces indicateurs sont construits indirectement par le programme (sections 1.2.1, 1.2.2, 1.2.3)
- Les résultats finaux (`valeur_cal`, `cnt_ind5_crt/ch`) contiennent **0 NA**

### Ind5 : base de calcul des marges
- Les ratios `"MARGE de commerce"` et `"MARGE de transport"` sont calculés par `calculer_ratios_ere()` avec base = **PRODUCTION + IMPORTATIONS**
- L'indicateur trimestriel utilise donc `(P1_ere + imp_ere)_trim × ratio_trim`, cohérent avec la définition des ratios annuels CNA
- Les importations trimestrielles proviennent de `ind_ce_struct$Import$Crt/Ch` (indicateurs bruts, non benchmarkés à ce stade)

### Doublon emplois ERE — filtre `bind_rows` corrigé ✅

**Problème détecté** : Dans les chunks d'assemblage des emplois ERE (sections 2.5 et 2.7 du Rmd), deux appels `filter()` séparés sur `emplois_bench` (ou `emplois_bench_ch`) étaient passés comme composantes distinctes dans `bind_rows()` :

```r
# ❌ MAUVAIS : double toutes les lignes non-VS-solde ET non-CFmarch-solde
dplyr::filter(emplois_bench, !(composante == "VS"      & Code_Produit %in% prods_solde_vs)),
dplyr::filter(emplois_bench, !(composante == "CFmarch" & Code_Produit %in% prods_solde_cfmarch)),
```

Chaque filtre conservait presque toutes les lignes de `emplois_bench`, leur union dans `bind_rows` les dupliquait. Résultat : `emplois_ere_complets` avait ~23 556 lignes au lieu de 13 689 (9 867 doublons).

**Correction appliquée** : un seul filtre combinant les deux exclusions :

```r
# ✅ CORRECT : un seul filter avec deux conditions
emplois_bench |>
  dplyr::filter(
    !(composante == "VS"      & Code_Produit %in% prods_solde_vs),
    !(composante == "CFmarch" & Code_Produit %in% prods_solde_cfmarch)
  ),
```

**Règle générale** : dans un `bind_rows()`, ne jamais appliquer deux filtres séparés sur la même table source. Combiner toujours les exclusions dans un seul `filter()`.

### Production GZ001/HZ001 manquante dans p1_ere_crt — corrigé ✅

**Problème détecté** : `calculer_ind5_depuis_production()` calcule `p1_par_produit` et `p1_ere_crt` à l'étape 2, avant l'intégration de GZ001/HZ001 (étape 7). Ces deux branches représentent ~1,1 M (GZ001) et ~300 K (HZ001) de production annuelle, soit ~96% manquants dans les ressources ERE pour leurs produits.

**Correction** : `calculer_p1_ere()` est appelée en section 2.1 *après* que `p1_agreg_complet` est finalisé à 60 branches. Elle écrase les valeurs intermédiaires de `p1_ere_crt`/`p1_ere_vol`.

**Règle** : Ne jamais utiliser `p1_ere_crt` produit par `ind5$p1_ere_crt` pour les ressources ERE. Toujours utiliser celui de `calculer_p1_ere()` + `benchmarker_p1_ere()`.

### Désagrégation TRE branche → produit : sous-estimation structurelle pour les services

**Problème** : Pour les produits de services (DZ000 énergie, HZ001 transport, KZ000 finances, MN002 immobilier, CC007 tabac, HZ002 transport routier), la ventilation TRE des CI branche → produit sous-estime systématiquement le CI produit ERE de 14–22% par rapport aux cibles CNA. Même phénomène pour la production (P1) de BD000.

**Cause** : La matrice TRE alloue le CI par branche vers les produits selon les coefficients techniques, mais cette allocation ne coïncide pas avec l'utilisation réelle des produits comme inputs au niveau ERE.

**Correction** : `benchmarker_p1_ere()` et `benchmarker_p2_ere()` recalent les agrégats ERE sur les cibles CNA annuelles, garantissant l'égalité annuelle Ressources = Emplois pour les années couvertes par les CNA.

**⚠️ Ne jamais supprimer ces deux étapes de benchmarking** — sans elles, des déséquilibres annuels de 15–25% réapparaissent sur ces produits.

### Imports manquants pour AA000, AB000, AC000

**Problème** : Ces trois produits agricoles n'ont pas d'indicateur trimestriel dans `R_Indicateur.xlsx`, bien que le CNA enregistre des imports non nuls (AA000 : 81–121K/an). Sans traitement spécifique, `executer_benchmarking_imp_exp()` les ignorait.

**Correction** : Repli automatique en distribution plate (`cna_annuel / 4`) avec extrapolation constante de la dernière valeur CNA pour les années de projection.

### Repli distribution plate dans executer_benchmarking_imp_exp()

Trois cas déclenchent le repli (signalés dans le message de la fonction) :

| Cas | Exemple produits | Traitement |
|---|---|---|
| Pas d'indicateur trimestriel | AA000, AB000, AC000 (imports) | Distribution plate CNA/4 |
| Benchmark Cholette échoué (résultat vide) | OZ000 (imports), EZ000 (exports) | Distribution plate CNA/4 |
| Valeurs négatives après Cholette | AE000, CC003, CC004, CC006, CD000 | Distribution plate CNA/4 |

**Extrapolation en projection** : pour les années au-delà de la dernière année CNA, la distribution plate utilise la dernière valeur CNA connue divisée par 4. Si la dernière valeur CNA est 0 (ex. OZ000 en 2022), la projection vaut 0 — ce qui est correct.

### NA dans evol_trim_pct (Excel exporté)
- La **première ligne de chaque code** a `evol_trim_pct = NA` → **NORMAL**
- `dplyr::lag()` n'a pas de valeur précédente pour le premier trimestre

### NA dans VPAP des imports/exports
- Produits avec importations nulles sur toute la période → ratio indéfini → VPAP = NA
- **Traitement** : remplacé par 0 avec `tidyr::replace_na(..., 0)` dans sections 2.2

### VA négative sur C05014 — cas structurel

La branche C05014 présente une VA négative sur **8 trimestres (2017 T1–2018 T4)**, avec un minimum de −6 185 en courant. Cela est **confirmé par les données CNA sources** : le CT de C05014 dépasse 1 en 2017 (CT = 1,34) et 2018 (CT = 1,12), signifiant que la CI a légitimement excédé la production ces années-là. Ce n'est **pas une erreur de calcul**.

### Résultats attendus (optique production)
- **60 branches** dans `p1_agreg_complet` (58 Ind1/2/3/4 + GZ001 commerce + HZ001 transport)
- **Période** : 2015 T1 → 2024 T3 (39 trimestres)
- `p1_par_produit` : 7 995 lignes (205 produits N3 × 39 trimestres)
- `df_comptes_finaux` : 2 340 lignes (60 branches × 39 trimestres), colonnes P1/P2/VA en courant, VPAP et chaîné
- **10 feuilles** dans l'Excel de sortie CNT + 4 feuilles dans `Comptes_Production_<date>.xlsx`

### Résultats attendus (optique dépenses — ERE)
- **39 produits** ERE (`Code_Prod_Ct`) dans toutes les tables
- **9 composantes** ressources dans `ere_ressources_completes` ; 9 composantes emplois dans `emplois_ere_complets`
- **13 338 lignes** dans `ere_ressources_completes`
- **13 783 lignes** dans `emplois_ere_complets`
- **0 NA** dans toutes les tables ERE (ressources, emplois, VPAP)
- Imports : **39 produits** dans `cnt_imp_final` (dont AA000/AB000/AC000 en distribution plate)
- Exports : **39 produits** dans `cnt_exp_final`

### Validation Ressources = Emplois ERE

La validation doit être faite en deux blocs distincts :

**Bloc 1 — Années CNA (≤ `derniere_annee_definitif`, ici 2022)**
- L'égalité annuelle doit être quasi-parfaite (résidu < 1 en valeur absolue)
- **Résultat actuel** : 289/304 lignes produit×année avec |solde annuel| < 1
- Résidus résiduels (< 3.5%) uniquement sur EZ000, QZ000 en 2018 — cas structurel

**Bloc 2 — Années de projection (> `derniere_annee_definitif`, ici 2023-2024)**
- Des déséquilibres annuels sont attendus (pas de contrainte CNA)
- Des déséquilibres trimestriels plus importants sont normaux
- Produits avec écart projection > 5% : RS001, CC007, XZ000, KZ000, HZ002, AA000, DZ000, EZ000

---

## 🧪 Lancer les tests

```r
devtools::test("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
# Résultat attendu : FAIL 0 | WARN 0 | SKIP 0 | PASS 59
```

---

## 📦 Dépendances principales

- `rjd3bench` — Méthode Cholette (nécessite **Java**)
- `dplyr`, `tidyr`, `readxl`, `writexl`
- `yaml` — Lecture de `config.yml`
- `purrr` — Utilisé dans section 2.4 pour `map_dfr()` sur les composantes ERE
- `testthat`, `withr` — Tests unitaires (Suggests)
