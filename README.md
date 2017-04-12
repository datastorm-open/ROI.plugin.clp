# ROI.plugin.clp

### ROI.plugin.clp — 'Clp (Coin-or linear programming)' Plugin for the 'R' Optimization Interface

# Installation

```` 
install.packages("ROI.plugin.clp")

# devtools::install_github("datastorm-open/ROI.plugin.clp") for developpement version
````

# Use

````
require(ROI)
require(ROI.plugin.clp)

## Simple linear program.
## maximize:   2 x_1 + 4 x_2 + 3 x_3
## subject to: 3 x_1 + 4 x_2 + 2 x_3 <= 60
##             2 x_1 +   x_2 + 2 x_3 <= 40
##               x_1 + 3 x_2 + 2 x_3 <= 80
##               x_1, x_2, x_3 are non-negative real numbers

LP <- ROI::OP(c(2, 4, 3),
  ROI::L_constraint(L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
    dir = c("<=", "<=", "<="),
    rhs = c(60, 40, 80)),
  max = TRUE)
  
res_lp <- ROI::ROI_solve(x = LP, solver = "clp")
res_lp$solution
res_lp$objval
````
