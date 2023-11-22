#' Disaggregates Enduse and Carrier Shares into combined enduse-carrier shares
#' via a quadratic optimization for an under-determined linear set of equations.
#'
#' @param data data.frame containing data on carrier-specific energy consumption.
#'  Columns 'carrier' and 'value' are required.
#' @param sharesEU data.frame containing enduse shares
#' @param etpEU data.frame containing enduse energy data
#' @param exclude list containing "enduse-carrier" pairs to be excluded
#' @param dataReplace data.frame containing existing disaggregated carrier-enduse shares
#' @param correct boolean that determines whether values shall be corrected for
#'
#' @author Hagen Tockhorn
#'
#' @importFrom quadprog solve.QP
#' @importFrom dplyr cross_join
#' @export

toolDisaggregate <- function(data,
                             sharesEU,
                             etpEU,
                             exclude = NULL,
                             dataReplace = NULL) {
  # ==== Explanation
  # Determine the matrix coefficient based on sharesEU and sharesEC,
  # looking for the solution closest to the data estimates.
  #
  #                El           Solids        Heat   |ShareEC
  #       cooking  5            6             0      |11
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


  # NOTE: Since datasets are mixed here with different regional resolutions,
  # the above-mentioned approach was adjusted. The datasets will be disaggregated
  # in terms of energy quantities not shares.
  # Further, since enduse-specific data (sharesEU / etpEU) only delivers
  # regionally aggregated data points, the constraint was loosened such that all
  # of the encompassed regions are required to sum up to the given value stated
  # in etpEU for the region from the mapping.
  # In contrast, every region is still required to sum up to its corresponding
  # carrier-specific value stated in data.

  # NOTE:
  # Unfortunately, this function is still under maintenance since the constraints
  # are still determined to be infeasible in a strict optimization only using
  # equality constraints.
  #
  # The sum of the carrier quantities is smaller than the sum of the enduse
  # quantities on an aggregated region level. This, of course, makes if impossible
  # to stay within the defined constraints. The deviation comes from the fact that
  # two independent datasets are used which deviate from each other.
  # It is possible to define regionally aggregated EU by multiplying the aggregated
  # carrier quantities with an EU share value (see line 271), however, this leads
  # to large deviations such that e.g. "space_cooling" is significantly smaller in
  # energy quantity than hat would be expected according to etpEU.


  # FUNCTIONS ------------------------------------------------------------------

  computeShares <- function(data){
    group_cols = setdiff(colnames(data), c("value","carrier"))
    tmp = data %>% group_by(across(all_of(group_cols))) %>%
      mutate(value = value /sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(shareEC = "value")
    return(tmp)
  }

  #--- Quadratic Optimization
  sol <- function(a, b, w, nEQ) {
    # nolint start

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
    #   D : matrix   (n*m,n*m), to be minimized, comes down to unity in this case
    #   b : vector   (,n+m),    containing aggregated non-combined shares
    #   x : vector   (,n*m),    containing estimated combined shares
    #   A : matrix   (n+m,n*m), containing the constraints
    # for
    #   n end-uses, m carriers
    # and
    #   nEQ equality constraints
    # ------------------------------
    # Useful websites:
    # https://vismor.com/documents/network_analysis/matrix_algorithms/S3.SS2.php
    # http://math.stackexchange.com/questions/271794/solution-to-underdetermined-linear-equations
    # http://stackoverflow.com/questions/9817001/optimization-with-constraints#9817442
    # http://stackoverflow.com/questions/16365723/find-positive-solutions-to-underdetermined-linear-system-of-equations
    # nolint end

    d <- diag(length(w))

    # Feed the solver
    # r <- solve.QP(d, w, t(a), b, meq = nEQ)
    r <- solve.QP(d, w, t(a), b)
    # NOTE: This approach works but obviously does not take the equality
    # constraints into consideration. Needs to be fixed.


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


    tmp <- table[table[["regionAgg"]] == reg & table[["period"]] == per, ]
    tmp <- quitte::factor.data.frame(tmp)


    # Exclude Countries with NaN entries
    if (any(is.na(tmp$etpEU)) || any(is.na(tmp$dataFE))) {
      tmp$index <- NULL
      return(tmp)
    } else {
      input <- tmp[tmp[["index"]], ][["shareEUEC"]]

      eu <- levels(tmp[["enduse"]])
      nEU <- length(eu)
      ec <- levels(tmp[["carrier"]])
      nEC <- length(ec)
      regs <- unique(tmp[["region"]])
      nRegs <- length(regs)


      # Transform the information in the DF into a linear system
      # where the sharesEUEC correspond to the unknowns.

      #---Equality Constraints (A %*% w == x)

      # NOTE: only the EC values are taken as equality constraints
      # This was supposed to fix the feasibility issues, but didn't. For a full
      # run, it would be wishful to consider all equality constraints, meaning
      # nEQ <- nEC * Regs + nEU
      nEQ <- nEC * nRegs

      numCol <- nrow(tmp)
      x <- matrix(0, nrow = nEU  + (nEC * nRegs), ncol = numCol)
      y <- numeric(length = nEU + (nEC * nRegs))


      # Build the Constraint Matrix x and Solution y
      i <- 1

      # second fill for the country-specific equations Sum(EUEC) = EC
      for (r in regs) {
        tmpReg <- filter(tmp, .data[["region"]] == r)
        for (c in ec) {
          index <- tmpReg[tmpReg[["carrier"]] == c, ][["index"]]
          x[i, index] <- 1
          y[i] <- unique(tmpReg[tmpReg[["carrier"]] == c, ][["dataFE"]])
          i <- i + 1
        }
      }

      # first fill for the equations Sum(EUEC) = EU
      for (u in eu) {
        index <- tmp[tmp[["enduse"]] == u, ][["index"]]
        x[i, index] <- 1
        y[i] <- unique(tmp[tmp[["enduse"]] == u, ][["etpEU"]])
        i <- i + 1
      }

      #---Inequality Constraints (A %*% w >= 0)
      # x <- rbind(x, diag(nrow = numCol, ncol = numCol)) #nolint
      # y <- c(y, replicate(n = numCol, 0))               #nolint

      # find the solution the closest from input, our estimate of EUEC
      output <- sol(x, y, input, nEQ)

      tmp[tmp[["index"]], "shareEUEC"] <- output

      tmp$index <- NULL
      return(tmp)
    }
  }



  # PARAMETERS -----------------------------------------------------------------

  # Overlapping Periods w/ input data
  years <- intersect(getPeriods(data), getPeriods(etpEU))

  # Replacement Regions
  replaceRegs <- dataReplace %>%
    filter(!is.na(.data[["value"]])) %>%
    select("region") %>%
    unique()



  # PROCESS DATA ---------------------------------------------------------------

  # Filter periods in common
  data <- data %>% filter(.data[["period"]] %in% years)
  etpEU <- etpEU %>% filter(.data[["period"]] %in% years)
  sharesEU <- sharesEU %>% filter(.data[["period"]] %in% years)

  dataDis <- data %>%
    filter(.data[["unit"]] == "fe") %>%
    select("region", "period", "variable", "value") %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    rename(carrier = "variable")

  ecShares <- computeShares(dataDis) %>%
    mutate(shareEC = ifelse(.data[["shareEC"]] == 0,
                            NA,
                            .data[["shareEC"]]))


  # Filter out regions later to be replaced
  etpEU <- filter(etpEU, !(.data[["region"]] %in% replaceRegs$region))
  dataDis <- filter(dataDis, !(.data[["region"]] %in% replaceRegs$region))
  sharesEU <- filter(sharesEU, !(.data[["region"]] %in% replaceRegs$region))


  # change the name of the value column
  etpEU   <- rename(etpEU, etpEU = .data[["value"]])
  sharesEU   <- rename(sharesEU, shareEU = .data[["value"]])


  tableShares <- dataDis %>%
    rename(dataFE = "value") %>%
    left_join(etpEU, by = c("region", "period")) %>%
    left_join(sharesEU %>%
                select("region", "period", "enduse", "shareEU"),
              by = c("region", "period", "enduse")) %>%
    mutate(shareEU = replace_na(.data[["shareEU"]], 0))


  # Exclude unwanted EU-EC Combinations
  if (!is.null(exclude)) {
      tableShares <- tableShares %>%
        unite(col = "EUEC", .data[["enduse"]], .data[["carrier"]], sep = "-", remove = FALSE) %>%
        anti_join(data.frame("EUEC" = exclude), by = "EUEC") %>%
        select(-"EUEC")

      if (!is.null(dataReplace)) {
        dataReplace <- dataReplace %>%
          unite(col = "EUEC", .data[["enduse"]], .data[["carrier"]], sep = "-", remove = FALSE) %>%
          anti_join(data.frame("EUEC" = exclude), by = "EUEC") %>%
          select(-"EUEC") %>%
          group_by(across(all_of(c("region", "period")))) %>%
          mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
          ungroup()
      }
  }


  # Calculate Shared FE Proportion and Aggregate Enduse Data
  tableShares <- tableShares %>%
    mutate(shareEUEC = .data[["dataFE"]] * .data[["shareEU"]]) %>%
    group_by(across(all_of(c("period", "EEAReg", "enduse")))) %>%
    mutate(etpEU = sum(unique(.data[["etpEU"]]), na.rm = TRUE)) %>%
    ungroup() %>%
    rename(regionAgg = "EEAReg") %>%
    select(-"shareEU")


  # Add an index column which gives the number of the row inside each group:
  # this is useful for building the matrix inside computeEUEC
  table <- tableShares %>%
    group_by(across(all_of(c("regionAgg", "period")))) %>%
    mutate(index = seq_along(.data[["regionAgg"]])) %>%
    ungroup()


  # Iteratively calculate Shares
  table <- do.call(rbind,
                   mapply(computeEUEC, # nolint
                          unique(table[c("regionAgg", "period")])[[1]],
                          unique(table[c("regionAgg", "period")])[[2]],
                          SIMPLIFY = FALSE
                   ))


  # Replace with existing Shares and re-normalize due to excluded EUEC
  if (!is.null(dataReplace)) {
    dataReplace <- dataReplace %>%
      select("region", "period", "carrier", "enduse", "value") %>%
      rename(shareEUEC = "value") %>%
      na.omit(cols = "shareEUEC")

    table <- table %>%
      select("region", "period", "carrier", "enduse", "shareEUEC") %>%
      rbind(dataReplace) %>%
      group_by(across(all_of(c("region", "period")))) %>%
      mutate(shareEUEC = .data[["shareEUEC"]] / sum(.data[["shareEUEC"]], na.rm = TRUE)) %>%
      ungroup()
  }


  #-----------------------------------------------------------------------------
  # NOTE: this bit of code is only to force a result of this function. The EC
  # shares should be calculated in advance.

  ecShares <- table %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    summarise(value = sum(.data[["shareEUEC"]], na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(across(all_of(c("region", "period")))) %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]])) %>%
    ungroup() %>%
    rename(shareEC = "value")

  # Scale Shares to Carrier Level
  table <- table %>%
    left_join(ecShares, by = c("region", "period", "carrier")) %>%
    mutate(share = .data[["shareEUEC"]] / .data[["shareEC"]],
           share = ifelse(is.finite(.data[["share"]]), .data[["share"]], 0)) %>%
    select(-"shareEUEC")
  #-----------------------------------------------------------------------------


  # Add excluded Carrier-Enduse combinations as 0's
  tableFull <- table %>%
    select("region", "period") %>%
    unique() %>%
    dplyr::cross_join(data.frame("EUEC" = exclude)) %>%
    mutate(share = 0) %>%
    separate(col = "EUEC", into = c("enduse", "carrier"), sep = "-") %>%
    rbind(table %>% select("region", "period", "enduse", "carrier", "share"))


  # Apply Shares on FE Data
  data <- data %>%
    rename(carrier = "variable") %>%
    left_join(tableFull %>%
                select("region", "period", "carrier", "enduse", "share"),
              by = c("region", "period", "carrier")) %>%
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    select(-"share")


  return(data)
}
