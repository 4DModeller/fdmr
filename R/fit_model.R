
#' @export

fit_model <- function(y, fixed_x, process_x, process_coords, data) {
# y : str - outcome variable name
# fixed_x : list - list of fixed effects variable names
# process_x : list - list of process effects variable names
# process_coords : list - list of coordinates associated with process
# data : data.frame - data.frame of data

# TODO : make fixed effects formula string
fe_formula <- ""
for (i in 1:length(fixed_x)) {
    fe_formula <- paste(fe_formula, fixed_x[i], sep = "")

    if (i < length(fixed_x)) {
        fe_formula <- paste(fe_formula, " + ", sep= "")
    }
}

# TODO : make process effects formula string

    # TODO : make process mesh for specific process

    # TODO : make spde using mesh for specific process

    # TODO : make specific formula string for this specific process

# TODO : combine all formula strings to make main formula string
formula <- 'y ~ 1 + everything'

bru(formula, data, family='gaussian')
}
