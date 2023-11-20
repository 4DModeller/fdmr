# here goes nothing
# i have written everything in python except for the things i have not.
# this will then be converted to non-python later

#' @export

function_generator <- function(outcome_variable, fixed_effects, process_effects) {
  process_names <- process_effects.keys()
  # process_vars <- process_effects.values()
  # produce the naive fit equation
  func <- outcome_variable + ' ~ 1' 
  
  # add the fixed effects to the fit_equation
  for fe in fixed_effects:
    fixed_effects_func <- str(fe) + "({fe}, kind='linear')"
    func = func + ' + ' fixed_effects_func
    
  # fixed_effects_func <- [' + ' + str(fe) for fe in fixed_effects]
  # add the SPDE processes to the 
  for pn in process_names:
    process_vars = process_effects[pn]
    func = func + str(pn) + "(" + [str(pv) + ', ' for pv in process_vars] 
    + "kind='spde')"
    
    return func
}
