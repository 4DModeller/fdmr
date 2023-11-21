
#' @export

fit_model <- function(y, fixed_x, process_x, process_coords, data) {
# y : str - outcome variable name
# fixed_x : list - list of fixed effects variable names
# process_x : list - list of process effects variable names
# process_coords : list - list of coordinates associated with process
# data : data.frame - data.frame of data

# TODO : make fixed effects formula string

# TODO : make process effects formula string

    # TODO : make process mesh for specific process

    # TODO : make spde using mesh for specific process

    # TODO : make specific formula string for this specific process

# TODO : combine all formula strings to make main formula string
formula <- 'y ~ 1 + everything'

bru(formula, data, family='gaussian')
}
