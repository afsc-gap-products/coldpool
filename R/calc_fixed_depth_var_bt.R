#' Estimate value of a profile variable at a fixed depth
#' 
#' Algorithm used since 2013 to estimate temperature at 1-m (RACE_DATA.TEMPERATURE_METHOD.TEMPERATURE_METHOD_ID = 14) from temperature-depth recorders deployed on RACE bottom trawl surveys. Returns the variable value corresponding with the first depth value at or shallower than the reference depth. If no depth values are equal to the reference depth, the variable value is estimated by interpolating from the variable values above and below the reference depth.
#' 
#' @param depth Numeric vector of depth values
#' @param var vector of variables
#' @param ref_depth Reference depth for estimating 1L numeric vector
#' @export
#' @author Sean Rohan

calc_fixed_depth_var_bt <- 
  function(depth,
           var,
           ref_depth) {
    
    # Case when there's no data or surface data are missing
    if(min(depth) > (ref_depth + 5) | length(depth) < 1) {
      return(NA)
    }
    
    # Select only data from below the surface
    var <- var[depth >= 0]
    depth <- depth[depth >= 0]
    
    # Case where there's no data shallower than the reference depth, but data within 5 m
    if(length(depth) > 1 & min(depth) > ref_depth) {
      return(var[depth == min(depth)])
    }
    
    # Find the first value where depth is equal to or shallower than the reference depth
    index <- min(which(depth <= ref_depth)) 
    sel_depth <- depth[index]
    sel_var <- var[index]
    
    # Case where there is an observation at the reference depth
    if(sel_depth == ref_depth) {
      return(sel_var)
    }
    
    # Case where there isn't an observation at the reference depth (interpolation)
    sel_var <- var[(index-1):index]
    sel_depth <- depth[(index-1):index]
    sel_var <- sel_var[1] + ( ( ( sel_var[2] - sel_var[1]) / ( sel_depth[2] - sel_depth[1]) ) * ( ref_depth - sel_depth[1]) )
    
    return(sel_var)
    
  }