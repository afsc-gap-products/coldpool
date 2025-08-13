#' Estimate value of a profile variable at a fixed depth
#' 
#' Algorithm used since 2013 to estimate temperature at 1-m (RACE_DATA.TEMPERATURE_METHOD.TEMPERATURE_METHOD_ID = 14) from temperature-depth recorders deployed on RACE bottom trawl surveys. Returns the variable value corresponding with the first depth value at or shallower than the reference depth. If no depth values are equal to the reference depth, the variable value is estimated by interpolating from the variable values above and below the reference depth.
#' 
#' @param depth Numeric vector of depth values
#' @param var vector of variables
#' @param ref_depth Reference depth for estimating 1L numeric vector
#' @param ref_buffer Optional. Depth range around ref_depth to use for interpolation (e.g., ref_depth = 20 and ref_buffer = 10 means observations from depth 10-30 could be used for interpolation). All observations are used when NULL (the default).
#' @export
#' @author Sean Rohan

calc_fixed_depth_var_bt <- 
  function(depth,
           var,
           ref_depth,
           ref_buffer = NULL) {
    
    if(!is.null(ref_buffer)) {
      var <- var[depth >= ref_depth - ref_buffer & depth <= ref_depth + ref_buffer]
      depth <- depth[depth >= ref_depth - ref_buffer & depth <= ref_depth + ref_buffer]
      
      min_gap <- ref_buffer
    } else {
      min_gap <- 5
    }

    
    if(length(depth) < 1) {
      warning("calc_fixed_depth_var_bt: No data. Returning NA.")
      return(NA)
    }
    
    # Case when there's no data or surface data are missing
    if(min(depth) > (ref_depth + min_gap) | length(depth) < 1) {
      return(NA)
    }
    
    # Select only data from below the surface
    var <- var[depth >= 0]
    depth <- depth[depth >= 0]
    
    # Case where there's no data shallower than the reference depth, but data within buffer window
    if(length(depth) > 0 & min(depth) > ref_depth) {
      return(var[depth == min(depth)][1])
    }
    
    # Find the first value where depth is equal to or shallower than the reference depth
    index <- min(which(depth <= ref_depth)) 
    sel_depth <- depth[index]
    sel_var <- var[index]
    
    # Case where there is an observation at the reference depth
    if(sel_depth == ref_depth) {
      return(sel_var[1])
    }
    
    # Case where there isn't an observation at the reference depth (interpolation)
    sel_var <- var[(index-1):index]
    sel_depth <- depth[(index-1):index]
    sel_var <- sel_var[1] + ( ( ( sel_var[2] - sel_var[1]) / ( sel_depth[2] - sel_depth[1]) ) * ( ref_depth - sel_depth[1]) )
    
    return(sel_var[1])
    
  }
