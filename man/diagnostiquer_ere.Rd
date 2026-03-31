% Generated manually (Rscript/roxygen2 unavailable in this environment)
\name{diagnostiquer_ere}
\alias{diagnostiquer_ere}
\title{Diagnostiquer la cohérence interne du module ERE}
\usage{
diagnostiquer_ere(
  p1_ere_avant,
  p1_ere_apres,
  p2_ere_avant,
  p2_ere_apres,
  cna_ere_struct = NULL,
  comptes_branches = NULL,
  seuil_rel_alerte = 0.05,
  seuil_rupture = 0.3,
  export_excel = NULL
)
}
\arguments{
\item{p1_ere_avant}{Liste ou tibble des séries P1 avant benchmarking.}
\item{p1_ere_apres}{Liste ou tibble des séries P1 après benchmarking.}
\item{p2_ere_avant}{Liste ou tibble des séries P2 avant benchmarking.}
\item{p2_ere_apres}{Liste ou tibble des séries P2 après benchmarking.}
\item{cna_ere_struct}{Liste CNA ERE (optionnel) pour identifier les cibles.}
\item{comptes_branches}{Table des comptes de production par branche (optionnel).}
\item{seuil_rel_alerte}{Seuil relatif d'alerte pour la cohérence comptable.}
\item{seuil_rupture}{Seuil absolu de variation trimestrielle pour détecter les ruptures.}
\item{export_excel}{Chemin de sortie Excel (optionnel).}
}
\value{Une liste \code{ere_diagnostics} contenant les tables de diagnostic,
les contrôles \eqn{P1-P2=B1}, les anomalies détectées et les alertes.}
\description{Produit un diagnostic méthodologique complet du module ERE après benchmarking:
écarts avant/après, stabilité hors cible CNA, ratios \eqn{P2/P1}, contributions,
et anomalies.}
