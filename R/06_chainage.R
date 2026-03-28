#' Agréger un flux trimestriel en total annuel répété sur les trimestres
#'
#' @description
#' Pour chaque année complète (4 trimestres), calcule le total annuel et le
#' répète sur ses 4 trimestres.
#' Les trimestres d'une année incomplète en fin de série sont mis à NA.
#'
#' @param x Vecteur numérique trimestriel.
#' @return Vecteur même longueur que \code{x}.
#' @export
agregat_annuel_repete <- function(x) {
  n <- length(x)
  if (n == 0) return(numeric(0))

  n_annees <- floor(n / 4)
  reste <- n %% 4

  if (n_annees == 0) return(rep(NA_real_, n))

  x_ok <- x[seq_len(n_annees * 4)]
  mat <- matrix(x_ok, nrow = 4, byrow = FALSE)

  # Total annuel ; si les 4 trimestres sont NA => total NA (pas 0)
  na_all <- colSums(is.na(mat)) == 4
  tot <- colSums(mat, na.rm = TRUE)
  tot[na_all] <- NA_real_

  out <- rep(tot, each = 4)
  if (reste > 0) out <- c(out, rep(NA_real_, reste))
  out
}

#' Agrégat annuel répété (alias pour compatibilité)
#'
#' @param x Vecteur trimestriel.
#' @return Vecteur de même longueur.
#' @export
agregat_annuel <- function(x) agregat_annuel_repete(x)

#' Calculer les indices de volume (annuel)
#'
#' @description
#' Maillons annuels pour une série de flux :
#' I_1 = 1 ; I_t = Vvol_t / Vcrt_(t-1).
#'
#' @param IndCrt Flux annuels à prix courants.
#' @param IndVol Flux annuels en volume aux prix de l'année précédente.
#' @return Vecteur des maillons annuels.
#' @export
calcul_indice_volume_annuel <- function(IndCrt, IndVol) {
  stopifnot(length(IndCrt) == length(IndVol))
  n <- length(IndCrt)
  if (n == 0) return(numeric(0))

  out <- rep(NA_real_, n)
  out[1] <- 1

  if (n >= 2) {
    denom <- IndCrt[seq_len(n - 1)]
    num   <- IndVol[2:n]
    ok <- !is.na(num) & !is.na(denom) & denom != 0
    out[2:n][ok] <- num[ok] / denom[ok]
  }
  out
}

#' Chaîner des indices (produit cumulatif)
#'
#' @param indices Vecteur de maillons.
#' @return Vecteur d'indices chaînés.
#' @export
chainer_indices <- function(indices) {
  n <- length(indices)
  if (n == 0) return(numeric(0))

  out <- rep(NA_real_, n)
  out[1] <- 1
  if (n == 1) return(out)

  for (t in 2:n) {
    if (is.na(indices[t]) || is.na(out[t - 1])) out[t] <- NA_real_
    else out[t] <- indices[t] * out[t - 1]
  }
  out
}

#' Calculer une valeur chaînée annuelle (flux)
#'
#' @param IndCrt Flux annuels à prix courants.
#' @param IndVol Flux annuels en volume aux prix de l'année précédente.
#' @param base_position Position de l'année de base dans le vecteur.
#' @return Valeurs chaînées annuelles.
#' @export
calcul_valeur_chainee_annuel <- function(IndCrt, IndVol, base_position = 1) {
  stopifnot(length(IndCrt) == length(IndVol))
  n <- length(IndCrt)
  if (n == 0) return(numeric(0))
  stopifnot(base_position >= 1, base_position <= n)

  indices <- calcul_indice_volume_annuel(IndCrt, IndVol)
  chaine  <- chainer_indices(indices)

  base_val <- IndCrt[base_position]
  if (is.na(base_val)) return(rep(NA_real_, n))

  chaine * base_val
}

#' Calculer les indices de volume trimestriels (flux)
#'
#' @description
#' i_q = (Vvol_q / Vcrt_ann(année-1)) * 4
#' avec pour l'année de base : denom = Vcrt_ann(base).
#'
#' @param IndCrt Flux trimestriels courants.
#' @param IndVol Flux trimestriels aux prix de l'année précédente.
#' @return Maillons trimestriels i_q.
#' @export
calcul_indice_volume_trim <- function(IndCrt, IndVol) {
  stopifnot(length(IndCrt) == length(IndVol))
  n <- length(IndCrt)
  if (n == 0) return(numeric(0))

  crt_ann_rep <- agregat_annuel_repete(IndCrt)

  denom <- crt_ann_rep
  if (n > 4) denom[5:n] <- crt_ann_rep[1:(n - 4)]

  out <- rep(NA_real_, n)
  ok <- !is.na(IndVol) & !is.na(denom) & denom != 0
  out[ok] <- (IndVol[ok] / denom[ok]) * 4
  out
}

#' Calculer une valeur chaînée trimestrielle (flux) - conforme CNT avec année incomplète
#'
#' @description
#' Point clé : si la dernière année est incomplète (ex: 2024Q1-Q3),
#' on NE calcule PAS le maillon annuel de 2024.
#' On prolonge simplement l'indice annuel chaîné de la dernière année complète
#' (ex: 2023) sur les trimestres disponibles de 2024.
#'
#' @param IndCrt Flux trimestriels courants.
#' @param IndVol Flux trimestriels aux prix de l'année précédente.
#' @param base_position Position (index) du trimestre de base (défaut 1).
#' @return Valeurs chaînées trimestrielles.
#' @export
calcul_valeur_chainee_trim <- function(IndCrt, IndVol, base_position = 1) {
  stopifnot(length(IndCrt) == length(IndVol))
  n <- length(IndCrt)
  if (n == 0) return(numeric(0))
  stopifnot(base_position >= 1, base_position <= n)

  # Totaux annuels r\u00e9p\u00e9t\u00e9s (ann\u00e9e incompl\u00e8te => NA sur ses trimestres)
  crt_ann_rep <- agregat_annuel_repete(IndCrt)
  vol_ann_rep <- agregat_annuel_repete(IndVol)

  # Nombre d'ann\u00e9es compl\u00e8tes (sur la longueur du vecteur)
  n_full_years <- floor(n / 4)
  last_full_end <- n_full_years * 4  # index du dernier trimestre de la derni\u00e8re ann\u00e9e compl\u00e8te

  # 1) Maillons annuels r\u00e9p\u00e9t\u00e9s (idx_ann) CALCUL\u00c9S UNIQUEMENT sur ann\u00e9es compl\u00e8tes
  idx_ann <- rep(NA_real_, n)
  idx_ann[1:min(4, n)] <- 1

  if (last_full_end >= 8) {
    # pour t=5..last_full_end : idx_ann[t] = vol_ann_rep[t] / crt_ann_rep[t-4]
    denom <- crt_ann_rep[1:(last_full_end - 4)]
    num   <- vol_ann_rep[5:last_full_end]
    ok <- !is.na(num) & !is.na(denom) & denom != 0
    idx_ann[5:last_full_end][ok] <- num[ok] / denom[ok]
  }

  # 2) Cha\u00eenage annuel r\u00e9p\u00e9t\u00e9 (ch_ann) sur ann\u00e9es compl\u00e8tes, puis PROLONGEMENT sur fin incompl\u00e8te
  ch_ann <- rep(NA_real_, n)
  ch_ann[1:min(4, n)] <- 1

  if (last_full_end >= 8) {
    for (t in 5:last_full_end) {
      if (is.na(idx_ann[t]) || is.na(ch_ann[t - 4])) ch_ann[t] <- NA_real_
      else ch_ann[t] <- idx_ann[t] * ch_ann[t - 4]
    }
  }

  # Prolonger l'indice annuel cha\u00een\u00e9 de la derni\u00e8re ann\u00e9e compl\u00e8te sur les trimestres restants
  if (last_full_end < n) {
    carry <- if (last_full_end >= 1) ch_ann[last_full_end] else 1
    ch_ann[(last_full_end + 1):n] <- carry
  }

  # 3) Maillons trimestriels et indice trimestriel cha\u00een\u00e9
  idx_trim <- calcul_indice_volume_trim(IndCrt, IndVol)
  ch_trim  <- idx_trim * ch_ann

  # 4) Base : annuel courant de l'ann\u00e9e de base (r\u00e9p\u00e9t\u00e9)
  base_ann_crt <- crt_ann_rep[base_position]
  if (is.na(base_ann_crt)) return(rep(NA_real_, n))

  ch_trim * (base_ann_crt / 4)
}

#' Déchaînage des valeurs (VPAP / PaP implicite)
#'
#' @description
#' Calcule les valeurs aux prix de l'année précédente (VPAP) à partir de :
#' - valeurs courantes CntCrt
#' - valeurs chaînées CntVolchaine
#'
#' Logique trimestrielle CNT :
#' VPAP_q = CntVolchaine_q * PaP_(annee-1)
#' oou PaP_(annee-1) = (TotalAnnuelCrt_(annee-1)) / (TotalAnnuelCh_(annee-1))
#'
#' => Pour une année incomplète (ex: 2024), on utilise PaP de 2023 pour 2024Q1-Q3.
#'
#' @param CntCrt Valeurs courantes (trimestrielles si trim=TRUE).
#' @param CntVolchaine Valeurs chaînées (trimestrielles si trim=TRUE).
#' @param trim TRUE pour trimestriel, FALSE pour annuel.
#' @return VPAP (même longueur).
#' @export
dechainer_valeurs <- function(CntCrt, CntVolchaine, trim = TRUE) {
  if (length(CntCrt) != length(CntVolchaine)) {
    stop("Les vecteurs doivent avoir la m\u00eame taille.")
  }
  n <- length(CntCrt)
  if (n == 0) return(numeric(0))

  k <- if (isTRUE(trim)) 4 else 1

  # Agr\u00e9gats annuels r\u00e9p\u00e9t\u00e9s (ann\u00e9e incompl\u00e8te => NA sur ses trimestres)
  agregat_crt    <- agregat_annuel_repete(CntCrt)
  agregat_chaine <- agregat_annuel_repete(CntVolchaine)

  # PaP implicite (r\u00e9p\u00e9t\u00e9 par trimestre)
  pap <- rep(NA_real_, n)
  ok <- !is.na(agregat_crt) & !is.na(agregat_chaine) & agregat_chaine != 0
  pap[ok] <- agregat_crt[ok] / agregat_chaine[ok]

  # D\u00e9cha\u00eenage : base = valeurs cha\u00een\u00e9es sur la p\u00e9riode de base
  result <- rep(NA_real_, n)
  result[1:min(k, n)] <- CntVolchaine[1:min(k, n)]

  if (n > k) {
    for (i in (k + 1):n) {
      # pap[i-k] correspond \u00e0 l'ann\u00e9e pr\u00e9c\u00e9dente (trimestriel) ou p\u00e9riode pr\u00e9c\u00e9dente (annuel)
      if (is.na(pap[i - k]) || is.na(CntVolchaine[i])) result[i] <- NA_real_
      else result[i] <- CntVolchaine[i] * pap[i - k]
    }
  }

  result
}
