library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)

region = "AI"

kriging_methods <- c("Exp", "Cir", "Gau", "Sph", "Mat", "Bes", "Ste")

# UTM zones based on the most frequent zone among survey samples.
crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                            utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                            aeacrs = "EPSG:3338")

profile_dat <- readRDS(here::here("data", region, paste0("cv_data_", region, ".rds")))


dir.create(here::here("output", region), showWarnings = FALSE)

unique_years <- sort(unique(profile_dat$YEAR))

cv_fits <- data.frame()

ii <- 1

with(ii, mean())
  
  sel_profile <- dplyr::filter(profile_dat, 
                               YEAR == unique_years[ii]) #|>
    # dplyr::filter(CAST == "bottom")
  
  x = sel_profile
  kriging_formula = TEMPERATURE ~ 1
  location_formula = ~ LONGITUDE + LATITUDE + DEPTH
  nm = Inf
  maxdist = Inf
  
  

mod_vertical <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = x,
                        nmax = Inf,
                        maxdist = ceiling(max(x$DEPTH)))

mod_horizontal <- gstat::gstat(formula = kriging_formula, 
                             locations = location_formula,
                             data = x,
                             nmax = Inf,
                             maxdist = maxdist)

(vgm_horizontal <- gstat::variogram(mod_horizontal, 
                                   beta = 0,
                                   tol.ver = 0))

(vgm_vertical <- gstat::variogram(mod_vertical, 
                                 beta = 90,
                                 cutoff = 500,
                                 tol.hor = 0))

fit_vg_horizontal <- gstat::fit.variogram(vgm_vertical, 
                     gstat::vgm(kriging_methods[ii]))

fit_vg_vertical <- gstat::fit.variogram(vgm_horizontal, 
                     gstat::vgm(kriging_methods[ii]))


mod <- gstat::gstat(formula = kriging_formula, 
                    locations = location_formula,
                    data = x,
                    nmax = Inf,
                    maxdist = ceiling(max(x$DEPTH)),
                    model = fit_vg_horizontal) |>
  gstat::gstat(formula = kriging_formula, 
               locations = location_formula,
               data = x,
               nmax = Inf,
               maxdist = maxdist,
               model = fit_vg_vertical)

vgm_fn <- function(range, psill, nugget, dist, model) {
  
  sel_fn <- switch(model,
                   "Exp" = coldpool::exp_vgm,
                   "Sph" = coldpool::sph_vgm,
                   "Cir" = coldpool::cir_vgm,
                   "Gau" = coldpool::gau_vgm,
                   "Mat" = coldpool::mat_vgm,
                   "Bes" = coldpool::bes_vgm,
                   "Ste" = coldpool::ste_vgm
  )
  
  return(out)
}

exp_vgm <- function(nugget = 0, psill, range, dist, a = 1) {
  a <- range * a
  return(nugget + psill *(1-exp(-1*dist/a)))
}

sph_vgm <- function(nugget = 0, psill, range, dist, a = 1) {
  a <- range*a
  val <- numeric(length = length(dist))
  val[dist > a] <- nugget + psill
  val[dist <= a] <- nugget + psill * (1.5*dist[dist <= a]/a - 0.5 * (dist[dist <= a]/a)^3)
  return(val)
  
}

cir_vgm <- function(nugget = 0, psill, range, dist, a = 1) {
  a <- range*a
  val <- numeric(length = length(dist))
  val[dist > a] <- nugget + psill
  val[dist <= a] <- nugget + psill * ((2*dist[dist <= a])/(pi*a) * sqrt(1-(dist[dist <= a]/a)^2)+(2/pi)*asin(dist[dist <= a]/a))
  return(val)
}


gau_vgm <- function(nugget = 0, psill, range, dist, a = 1) {
  a <- range*a
  return(nugget + psill*(1-exp(-(dist/a)^2)))
}

mat_vgm <- function(nugget = 0, psill, dist, range, kappa = 0.5) {
  
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

bes_vgm <- function(nugget = 0, psill, range, dist, a = 1) {
  a <- range*a
  
  dist[dist == 0] <- 1e-10
  val <- nugget + psill*(1-dist/a*besselK(x = dist/a, nu = 1))
  
  return(val)
  
}

ste_vgm <- function(nugget = 0, dist, range, kappa, a = 1) {
  
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


optim_exp_z <- function(z_start, kriging_formula, location_formula, anis = c(0,0,0,1,1), nmax, maxdist, data, model = "Exp") {
  
  
  z_fn <- function(zz, 
                   kriging_formula, 
                   location_formula,
                   data,
                   nmax,
                   maxdist, 
                   model) {
    
    zz <- exp(zz)
    
    print(zz)
    vert_max_dist <- ceiling(max(data[all.vars(location_formula)[[3]]]))
    
    mod_vertical <- gstat::gstat(formula = kriging_formula, 
                                 locations = location_formula,
                                 data = data,
                                 nmax = nmax,
                                 maxdist = vert_max_dist)
    
    vgm_vertical <- gstat::variogram(mod_vertical, 
                                     beta = 90, # Vertical
                                     cutoff = vert_max_dist,
                                     tol.hor = 0)
    
    mod_horizontal <- gstat::gstat(formula = kriging_formula, 
                                   locations = location_formula,
                                   data = data,
                                   nmax = nmax,
                                   maxdist = maxdist)
    
    vgm_horizontal <- gstat::variogram(mod_horizontal, 
                                        beta = 0, #
                                        tol.ver = 0)
    
    fit_vg_vertical <- gstat::fit.variogram(vgm_vertical, 
                                            gstat::vgm("Mat"))
    
    fit_vg_horizontal <- gstat::fit.variogram(vgm_horizontal, 
                                              gstat::vgm(model))
    
    plot(vgm_horizontal, fit_vg_horizontal)
    plot(seq(min(vgm_horizontal$dist), max(vgm_horizontal$dist), 12000), h1, col = "red", type = 'l')
    
    h1 <- exp_vgm(range = fit_vg_horizontal$range[2], 
                 psill = fit_vg_horizontal$psill[2], 
                 nugget = fit_vg_horizontal$psill[1],
                 dist = seq(min(vgm_horizontal$dist), max(vgm_horizontal$dist), 12000))#,
                 model = model)
    
    h2 <- vgm_fn(range = fit_vg_vertical$range[2], 
                 psill = fit_vg_vertical$psill[2], 
                 nugget = fit_vg_vertical$psill[1],
                 dist = vgm_horizontal$dist/zz,
                 model = model)
    
    return(sum((h2-h1)^2))
    
  }
  
  
  best_z <- optim(par = log(z_start),
        fn = z_fn,
        kriging_formula = kriging_formula, 
        location_formula = location_formula,
        data = data,
        nmax = nmax,
        maxdist = maxdist,
        model = model,
        method = "Brent",
        lower = c(log(1e-5)),
        upper = c(log(5e5)),
        control = list(trace = 1))
  
  best_z$par <- exp(best_z$par)
 
  return(best_z) 
  
}
  
  


z_start = 10
kriging_formula = TEMPERATURE ~ 1
location_formula = ~ LONGITUDE + LATITUDE + DEPTH
nmax = Inf
maxdist = Inf
data = x
model = "Exp"
  





  z_fn_3d <- function(zz, 
                      kriging_formula, 
                      location_formula,
                      data,
                      nmax,
                      maxdist, 
                      model) {
    
  mod_3d <- gstat::gstat(formula = kriging_formula, 
                      locations = location_formula,
                      data = dplyr::mutate(data, DEPTH = DEPTH * zz),
                      nmax = nmax,
                      maxdist = nmax)
  
  (vgm_3d <- gstat::variogram(mod_3d))
  
  vgm_fit_3d <- gstat::fit.variogram(vgm_3d, 
                                     gstat::vgm(model))

  return(sum(vgm_fit_3d$psill))
  
  }
  
  
  best_z <- optim(par = log(zz),
                  fn = z_fn_3d,
                  kriging_formula = kriging_formula, 
                  location_formula = location_formula,
                  data = data,
                  nmax = nmax,
                  maxdist = nmax,
                  model = model,
                  method = "Brent",
                  lower = c(1e-5),
                  upper = c(5e5),
                  control = list(trace = 6))
  
  
  mod_3d <- gstat::gstat(formula = kriging_formula, 
                         locations = location_formula,
                         data = dplyr::mutate(data, DEPTH = DEPTH * best_z$par),
                         nmax = nmax,
                         maxdist = nmax)
  
  (vgm_3d <- gstat::variogram(mod_3d))
  
  vgm_fit_3d <- gstat::fit.variogram(vgm_3d, 
                                     gstat::vgm(model))

  plot(vgm_3d, vgm_fit_3d)
  
gstat::fit.variogram.reml(formula = kriging_formula, 
                          location = location_formula, 
                          data = x, 
                          model = vgm_fit_3d)




vgm_mod <- gstat::fit.variogram(gstat::variogram(mod_idw, 
                                                 width = vgm_width), 
                                gstat::vgm(kriging_methods[ii]))

test <- gstat::fit.variogram.reml(formula = I(TEMPERATURE) ~1, 
                          location = ~ LONGITUDE + LATITUDE + DEPTH, 
                          data = x, 
                          model = vgm_mod)

mod <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = x,
                        set = list(idp = 2), 
                        nmax = Inf,
                        model = test,
                        maxdist = maxdist)

vgm_mod <- gstat::fit.variogram(gstat::variogram(mod_idw, 
                                                 width = vgm_width,
                                                 alpha = c(0, 45, 90, 135)), 
                                gstat::vgm(model = kriging_methods[ii],
                                           anis = anis_pars,
                                           nugget = vgm_mod$psill[1]))



