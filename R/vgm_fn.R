#' Variogram function
#' 
#' R implementation of gstat variogram functions.
#' 
#' @param model gstat model type. Currenty implemented: "Exp", "Sph", "Cir", "Gau", "Mat", "Bes", "Ste"
#' @param dist Distance (h)
#' @param range Range parameter of the variogram model.
#' @param psill Partial sill of the variogram model.
#' @param nugget Nugget parameter of the variogram model (default = 0)
#' @param kappa Kappa parameter for Matern class variogram models (default = 0.5)
#' @export

vgm_fn <- function(model, dist, range, psill, nugget = 0, kappa = 0.5, ...) {
  
  args <- as.list(match.call()[-1])
  
  switch(model,
         "Exp" = do.call(coldpool:::exp_vgm, args = args),
         "Sph" = do.call(coldpool:::sph_vgm, args = args),
         "Cir" = do.call(coldpool:::cir_vgm, args = args),
         "Gau" = do.call(coldpool:::gau_vgm, args = args),
         "Mat" = do.call(coldpool:::mat_vgm, args = args),
         "Bes" = do.call(coldpool:::bes_vgm, args = args),
         "Ste" = do.call(coldpool:::ste_vgm, args = args)
  )
  
}

#' Exponential variogram
#' @noRd

exp_vgm <- function(nugget = 0, psill, range, dist, a = 1, ...) {
  a <- range * a
  return(nugget + psill *(1-exp(-1*dist/a)))
}

#' Spherical variogram
#' @noRd

sph_vgm <- function(nugget = 0, psill, range, dist, a = 1, ...) {
  a <- range*a
  val <- numeric(length = length(dist))
  val[dist > a] <- nugget + psill
  val[dist <= a] <- nugget + psill * (1.5*dist[dist <= a]/a - 0.5 * (dist[dist <= a]/a)^3)
  return(val)
  
}

#' Circular variogram
#' @noRd

cir_vgm <- function(nugget = 0, psill, range, dist, a = 1, ...) {
  a <- range*a
  val <- numeric(length = length(dist))
  val[dist > a] <- nugget + psill
  val[dist <= a] <- nugget + psill * ((2*dist[dist <= a])/(pi*a) * sqrt(1-(dist[dist <= a]/a)^2)+(2/pi)*asin(dist[dist <= a]/a))
  return(val)
}

#' Gaussian variogram
#' @noRd

gau_vgm <- function(nugget = 0, psill, range, dist, a = 1, ...) {
  a <- range*a
  return(nugget + psill*(1-exp(-(dist/a)^2)))
}

#' Matern variogram
#' @noRd

mat_vgm <- function(nugget = 0, psill, dist, range, kappa = 0.5, ...) {
  
  dist <- dist/range
  
  if(kappa == .5) {
    return(nugget+psill*exp(-dist))
  }
  if(kappa == 1.5) {
    return(nugget+psill*(1+dist)*exp(-dist))
  }
  if(kappa == 2.5) {
    return(nugget+psill*(1+dist+dist^2/3)*exp(-dist))
  }
  
  con <- 1/(2^(kappa - 1)) * gamma(kappa)
  
  dist[dist == 0] <- 1e-10
  
  return(nugget + psill * con * (dist^kappa) * besselK(x = dist, nu = kappa))
}

#' Bessel variogram
#' @noRd

bes_vgm <- function(nugget = 0, psill, range, dist, a = 1, ...) {
  a <- range*a
  
  dist[dist == 0] <- 1e-10
  val <- nugget + psill*(1-dist/a*besselK(x = dist/a, nu = 1))
  
  return(val)
  
}

#' Stein's Matern variogram
#' @noRd

ste_vgm <- function(nugget = 0, dist, range, kappa, a = 1, ...) {
  
  a <- range/a
  
  matern <- besselK(x = 2 * kappa^(1/2) * dist / a, nu = kappa)
  multipl <- 1 / (2^(kappa - 1) * gamma(kappa))*(2 * kappa^(1/2) * dist/a)^kappa
  val <- ifelse(matern == 0 | !is.finite(multipl), 
                0, 
                ifelse(!is.finite(matern),
                       1,
                       multipl*matern))
  
  return(nugget + val)
}