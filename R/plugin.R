## ROI plugin: clp
## based on clpAPI interface
## SOLVER METHODS

#' @import ROI clpAPI slam methods
#' @importFrom Matrix sparseMatrix
#' @importFrom stats terms
solve_OP <- function( x, control) {
  
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    # type control
    if(!is.null(x$types)){
      if(any(!x$types %in% "C")){
        stop("COIN-OR Clp solver can only treat continuous objective variables")
      }
    }
    
    ## handle control args
    main_args <- list( obj = stats::terms(objective(x))[["L"]],
                       mat = constraints(x)$L,
                       dir = constraints(x)$dir,
                       rhs = constraints(x)$rhs,
                       bounds = bounds(x),
                       max = x$maximum )
    
    ## handle STMs
    if( slam::is.simple_triplet_matrix(main_args$obj))
      obj_coef <- as.matrix( main_args$obj )[1, ]
    
    ##
    if( slam::is.simple_triplet_matrix(main_args$mat)){
      s_mat <- Matrix::sparseMatrix(i = main_args$mat$i, j = main_args$mat$j, x = main_args$mat$v)
    }
    ia <- s_mat@i
    ja <-  s_mat@p
    ar <- s_mat@x
    ncols <- ncol(s_mat)
    nrows <- nrow(s_mat)
    
    # range of rows
    rlb <- ifelse(main_args$dir %in% c("<="), -Inf, main_args$rhs)
    rub <- ifelse(main_args$dir %in% c(">="), Inf, main_args$rhs)
    
    # range of column
    lb <- NULL
    ub <- NULL
    if(!is.null(main_args$bounds)){
      if(length(main_args$bounds$lower$ind) > 0){
        lb <- rep(0, ncols)
        lb[main_args$bounds$lower$ind] <- main_args$bounds$lower$val
      }
      if(length(main_args$bounds$upper$ind) > 0){
        ub <- rep(Inf, ncols)
        ub[main_args$bounds$upper$ind] <- main_args$bounds$upper$val
      }
    }
    
    # preparing the model
    lp <- clpAPI::initProbCLP()
    
    # log level ?
    if("amount"%in%names(control)){
      stopifnot(control$amount %in% 0:4)
      setLogLevelCLP(lp, control$amount)
      control$amount <- NULL
    }
    
    # minimize
    lpdir <- ifelse(main_args$max, -1, 1)
    clpAPI::setObjDirCLP(lp, lpdir)
    
    clpAPI::loadProblemCLP(lp, ncols = ncols, nrows = nrows, ia, ja, ar,
                   lb = lb, ub = ub, obj_coef = obj_coef,
                   rlb = rlb, rub = rub)
    
    
    # solve lp problem
    clpAPI::solveInitialCLP(lp)
    
    ## ROI format
    ROI_sol <- ROI_plugin_canonicalize_solution(solution = clpAPI::getColPrimCLP(lp),
                                 optimum = clpAPI::getObjValCLP(lp),
                                 status = clpAPI::getSolStatusCLP(lp),
                                 solver = solver)
    
    clpAPI::delProbCLP(lp)
    
    ROI_sol
}

## STATUS CODES
.add_status_codes <- function( ) {

    ## SEE clpAPI::status_codeCLP
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db(solver,
                                0L,
                                "solution is optimal",
                                "solution is optimal",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                1L,
                                "solution is primal infeasible",
                                "solution is primal infeasible"
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                2L,
                                "solution is dual infeasible",
                                "solution is dual infeasible"
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                3L,
                                "stopped on iterations etc",
                                "stopped on iterations etc"
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                4L,
                                "stopped due to errors",
                                "stopped due to errors"
                                )

    invisible(TRUE)
}

# ## SOLVER CONTROLS
.add_controls <- function(){
    solver <- ROI_plugin_get_solver_name(getPackageName())

    ROI_plugin_register_solver_control( solver, "amount", "X")

    invisible( TRUE )
}
