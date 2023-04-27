#' Disaggregates Enduse and Carrier Shares into combined enduse-carrier shares
#' via a quadratic optimization for an under-determined linear set of equations.
#'
#' @param data data.frame containing enduse-carrier shares for first estimation
#' in optimization problem. Columns 'enduse', 'carrier' and 'value' are required.
#' @param sharesEU data.frame containing aggregated enduse shares
#' @param sharesEC data.frame containing aggregated carrier shares
#'
#' @author Hagen Tockhorn
#'
#' @importFrom quadprog solve.QP
#' @importFrom quitte factor.data.frame
#' @export

toolDisaggregate <- function(data, sharesEU, sharesEC, showCheck = TRUE) {
  # ==== Explanation
  # Determine the matrix coefficient based on sharesEU and sharesEC,
  # looking for the solution closest to the data estimates.
  #
  #                El           Solids        Heat   |ShareEC
  #       cooking  5 (5/11)     6 (6/11)      0      |11
  # space heating 20            5             35     |60
  #       cooling 29            0             0      |29
  #--------------------------------------------------|-------
  #      ShareEU 54            11            35     |100
  #
  # Figures between brackets : from 'data'

  # In other words, an underdetermined linear problem of A*x = y is given, where
  #   A : matrix containing the constraints
  #   x : combined shares (tbd)
  #   y : estimate of combined shares
  # such that a solution for x has to be found.


  #--- Quadratic Optimization
  sol <- function(a, b, w, nEQ) {
    # nolint start
    #---Goal
    # The goal is to solve the minimization of f(x) where
    #        f(x) = norm2(Ax-y)
    # and
    #         x : vector (n,); estimate of disaggregated shares
    #         y : vector (m,); real aggregated shares (shareEU1, ... , shareEC1, ...)
    #         A : matrix (m,n); contains constraints to the problem
    #
    # A is necessary to ensure all disaggregated values (w.r.t EU) add up to
    # the respective EC share.

    # The solver takes the function to be minimized as´
    #   f(x) = norm2(A*x - y) = 1/2*xT*D*x -2*yT*A*x = 1/2*xT*D*x - xT*b
    # with
    #   b = 2*yT*A
    # and
    #   D : matrix (n*m,n*m), to be minimized, comes down to unity in this case
    #   b : vector (,n+m), containing aggregated non-combined shares
    #   x : vector (,n*m),containing estimated combined shares
    #   A : matrix (n+m,n*m), containing the constraints
    # for
    #   n end-uses, m carriers
    # and
    #   nEQ equality constraints



    # Useful websites:
    # https://vismor.com/documents/network_analysis/matrix_algorithms/S3.SS2.php
    # http://math.stackexchange.com/questions/271794/solution-to-underdetermined-linear-equations
    # http://stackoverflow.com/questions/9817001/optimization-with-constraints#9817442
    # http://stackoverflow.com/questions/16365723/find-positive-solutions-to-underdetermined-linear-system-of-equations
    # nolint end

    # Considering the quadratic problem, it is clear that D must be the identity matrix.
    d <- diag(length(w))

    # Feed the solver
    r <- solve.QP(d, w, t(a), b, meq = nEQ)

    zFinal <- r$solution

    # It might be that zero is approximated with very small values
    zFinal[abs(zFinal) < 1e-10] <- 0

    return(zFinal)
  }

#---Compute the disaggregated shares
  computeEUEC <- function(reg, per) {
    #--- computes the shares of EUEC, which are the closest from
    #--- the EUEC shares estimated with the Odyssee Database and
    #--- which solve the EC and EU equations with only positive parameters
    #---------------------------
    #--- params
    #------- reg  : region of consideration
    #------- per  : time period (year) of consideration
    #------- euec : combined shares of consideration; rest will default to 0


    tmp <- table[table[["region"]] == reg & table[["period"]] == per, ]
    tmp <- factor.data.frame(tmp)

    # Select only combined shares of interest
    tmpFilter <- tmp %>%
      semi_join(euec, by = c("carrier", "enduse")) %>%
      mutate(index = seq_along(.data[["region"]]))

    eu <- levels(tmpFilter[["enduse"]])
    nEU <- length(eu)
    ec <- levels(tmpFilter[["carrier"]])
    nEC <- length(ec)

    # Exclude Countries with NaN entries
    if (any(is.na(tmpFilter$shareEU)) || any(is.na(tmpFilter$shareEC))) {
      tmpFilter$index <- NULL
      return(tmpFilter)
    } else {
      input <- tmpFilter[tmpFilter[["index"]], ][["shareEUEC"]]


      # Transform the information in the DF into a linear system
      # where the sharesEUEC correspond to the unknowns.

      #---Equality Constraints (A %*% w == x)
      nEQ <- nEU + nEC
      numCol <- nrow(tmpFilter)
      x <- matrix(0, nrow = nEU  + nEC, ncol = numCol)
      y <- numeric(length = nEU + nEC)

      ### =====Build the Constraint Matrix x and Solution y

      i <- 1
      for (u in eu) {
        # first fill for the equations Sum(EUEC) = EU
        index <- tmpFilter[tmpFilter[["enduse"]] == u, ][["index"]]
        x[i, index] <- 1
        y[i] <- unique(tmpFilter[tmpFilter[["enduse"]] == u, ][["shareEU"]])
        i <- i + 1
      }
      for (c in ec) {
        # second fill for the equations Sum(EUEC) = EC
        index <- tmpFilter[tmpFilter[["carrier"]] == c, ][["index"]]
        x[i, index] <- 1
        y[i] <- unique(tmpFilter[tmpFilter[["carrier"]] == c, ][["shareEC"]])
        i <- i + 1
      }

      #---Inequality Constraints (A %*% w >= 0)
      x <- rbind(x, diag(nrow = numCol, ncol = numCol))
      y <- c(y, replicate(n = numCol, 0))

      # find the solution the closest from input, our estimate of EUEC
      output <- sol(x, y, input, nEQ)

      tmpFilter[tmpFilter[["index"]], "shareEUEC"] <- output

      #--- Join DataFrames to obtain full range of combined shares again / add zero shares
      tmp <- tmp %>%
        left_join(tmpFilter %>% select(-"region", -"period", -"model", -"scenario", -"variable",
                                       -"unit", -"shareEC", -"shareEU", -"index"),
                  by = c("enduse", "carrier")) %>%
        mutate(shareEUEC = ifelse(is.na(.data[["shareEUEC.y"]]),
                                  .data[["shareEUEC.x"]],
                                  .data[["shareEUEC.y"]])) %>%
        select(-"shareEUEC.x", -"shareEUEC.y")

      tmp$index <- NULL
      return(tmp)
    }
  }


  #------ END OF INTERNAL FUNCTIONS


  #----- internal mappings and parameters---
  years <- intersect(getPeriods(data), getPeriods(sharesEU))
  #-----


  #------- PROCESS DATA

  # Compute carrier-enduse shares as optimization estimate
  dataShares <- data %>%
    group_by(across(all_of(c("region", "period")))) %>%
    filter(any(!is.na(.data[["value"]]))) %>%
    ungroup() %>%
    group_by(across(all_of(c("carrier", "enduse")))) %>%
    summarise(value = mean(.data[["value"]]))

  # Combined Shares of interest
  euec <- dataShares %>%
    select(-"value")

  # Transform to quitte objects
  sharesEC <- as.quitte(sharesEC)
  sharesEU <- as.quitte(sharesEU)

  # Reduce the set of years in the data to the one in sharesEU
  data <- data %>% filter(.data[["period"]] %in% years)
  sharesEU <- sharesEU %>% filter(.data[["period"]] %in% years)


  #---------------- THERMAL PART --------------------

  # change the name of the value column
  sharesEU   <- rename(sharesEU,   shareEU = .data[["value"]])
  sharesEC   <- rename(sharesEC,   shareEC = .data[["value"]])

  # Put the information on shares together
  tableShares <- sharesEC %>%
    select(-"variable", -"model", -"scenario", -"unit") %>%
    left_join(sharesEU, by = c("region", "period")) %>%
    left_join(dataShares, by = c("carrier", "enduse")) %>%
    mutate(shareEUEC = .data[["value"]],
           shareEUEC = replace_na(.data[["shareEUEC"]], 0)) %>%
    select(-"value")


  #----------------

  # add an index column which gives the number of the row inside each group:
  # this is useful for building the matrix inside computeEUEC

  table <- tableShares %>%
    group_by(across(all_of(c("region", "period")))) %>%
    mutate(index = seq_along(.data[["region"]])) %>%
    ungroup()


  table <- do.call(rbind,
                  Map(computeEUEC,
                         unique(table[c("region", "period")])[[1]],
                         unique(table[c("region", "period")])[[2]]
                         # SIMPLIFY = FALSE
                  ))

  table <- table %>% mutate(value = .data[["shareEUEC"]]) %>% select(-"shareEUEC")


  #--------- END OF THERMAL PART


  return(table)
}
