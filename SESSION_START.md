# SESSION_START.md

Mémo court pour démarrer une session RStudio propre sur `QnaSut`.

## 1. Ouvrir le bon projet

Toujours ouvrir :

`C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut/QnaSut.Rproj`

Ne pas travailler avec `Project: (None)`.

## 2. Réglages RStudio recommandés

Dans `Tools > Global Options > General` :

- `Restore .RData into workspace at startup` : décoché
- `Save workspace to .RData on exit` : `Never`

Si le démarrage affiche une erreur liée à Posit Assistant et au réseau :

- désactiver Posit Assistant dans les préférences RStudio
- ou ignorer ce module si tu travailles hors ligne / derrière proxy

## 3. Démarrage package propre

Dans la console R :

```r
setwd("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
devtools::document()
devtools::load_all("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
devtools::test("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
```

Workflow normal pendant le développement :

- modifier le code
- lancer `devtools::document()`
- lancer `devtools::load_all(...)`
- relancer le test ou le chunk utile

## 4. Exécuter le pipeline CNT

Quand tu veux lancer le Rmd ou utiliser `config.yml` :

```r
setwd("C:/CnaBfaScn08/CntBfaV4/07P_Outils/OutilCntBfa")
devtools::load_all("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
cfg <- load_config()
```

Ensuite seulement :

- exécuter les chunks de `ProgCntBFa_Evol.Rmd`
- lancer les diagnostics
- exporter les classeurs

## 5. Procédure d'installation la plus stable

Éviter `devtools::install()` sur cette machine quand ce n'est pas nécessaire.

Utiliser plutôt dans le terminal RStudio ou PowerShell :

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\x64\Rcmd.exe' INSTALL 'C:\CnaBfaScn08\CntBfaV4\07P_Outils\QnaSut'
```

Cette commande évite les problèmes `pak/processx` rencontrés avec `devtools::install()`.

## 6. Précautions importantes

- ne pas laisser ouverts dans Excel les fichiers de diagnostic placés dans le dossier du package
- éviter de garder des fichiers temporaires `~$...xlsx`
- préférer `devtools::load_all()` à une réinstallation complète pendant le travail courant
- faire `install` seulement pour vérifier le package installé

## 7. Séquence minimale recommandée

Si tu veux juste reprendre vite :

```r
setwd("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
devtools::load_all("C:/CnaBfaScn08/CntBfaV4/07P_Outils/QnaSut")
setwd("C:/CnaBfaScn08/CntBfaV4/07P_Outils/OutilCntBfa")
cfg <- load_config()
```

Puis exécuter le chunk ou le script qui t'intéresse.
