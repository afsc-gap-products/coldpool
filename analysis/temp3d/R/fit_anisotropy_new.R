fit_anisotropy_new <- function(anis_pars,
                               x_mod, 
                               kriging_method, 
                               kriging_formula,
                               location_formula,
                               nm,
                               vgm_width,
                               vgm_directions = c(0, 45, 90, 135), 
                               dat,
                               kfold = 10, 
                               seed = 19673) {
  
  if(length(anis_pars) == 5) {
    anis_pars <- c(anis_pars[1], 0, 0, anis_pars[2], 0)
  }
  
  vgm_mod <- gstat::fit.variogram(gstat::variogram(x_mod, 
                                                   width = vgm_width), 
                                  gstat::vgm(kriging_method))
  
  vgm_mod_2 <- gstat::fit.variogram(gstat::variogram(x_mod, 
                                                   width = vgm_width,
                                                   alpha = vgm_directions), 
                                  gstat::vgm(model = kriging_method,
                                             anis = anis_pars,
                                             nugget = vgm_mod$psill[1]))
  
  
  mod <- gstat::gstat(formula = kriging_formula, 
                      locations = location_formula,
                      data = dat,
                      model = vgm_mod_2, 
                      nmax = nm)
  
  set.seed(seed)
  ssq <- sum(gstat::gstat.cv(object = mod, 
                              verbose = FALSE, 
                              debug.level = 0,
                              nfold = kfold)$residual^2)
  
  return(ssq)
  
}
