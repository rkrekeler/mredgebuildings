#' HDD and CDD scenario and historic data
#'
#' smoothes the HDD and CDD scenario data
#'
#' @param tlimit Temperature threshold for computing HDD and CDD
#'
#' @author Antoine Levesque
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass setNames setYears as.magpie getNames lowpass getYears mbind
#' @importFrom dplyr %>%
#' @importFrom quitte as.quitte
#' @importFrom utils tail
#' @export

calcHDDCDD <- function(tlimit = 18) {

  stopifnot(as.character(tlimit) %in% as.character(17:25))

  data <- readSource("HDDCDD", subtype = tlimit)

  # yearsScenario = all years for which history is either NA or 0
  index <- apply(data[, , paste0("history.HDD.history.", tlimit)], c(2, 3),
                 function(x) {
                   return(all(is.na(x) | x == 0))
                 })

  yearsScenario <- c(gsub("y", "", attributes(index)$dimnames$period[index]) %>% as.integer(), 2100)
  yearsHistory <- setdiff(getYears(data, as.integer = TRUE), yearsScenario)

  convergenceYear <- 2030
  convergenceYears <- c(max(yearsHistory), yearsScenario[yearsScenario <= convergenceYear])
  afterConvergenceYears <- yearsScenario[!yearsScenario %in% convergenceYears]
  lambda <- c(rep(1, length(yearsHistory)),
              tail(seq(1, 0, length.out = length(convergenceYears)), -1),
              rep(0, length(afterConvergenceYears)))
  names(lambda) <- c(yearsHistory, yearsScenario)
  lambda <- as.magpie(lambda)

  rcps     <- grep("history", getNames(data, TRUE)$rcp,      value = TRUE, invert = TRUE)
  ssps     <- grep("history", getNames(data, TRUE)$scenario, value = TRUE, invert = TRUE)
  variable <- grep("history", getNames(data, TRUE)$variable, value = TRUE, invert = TRUE)

  data[is.na(data)] <- 0
  data <- mbind(data, (1 / 3) * (setYears(data[, 2099, ], 2100) +
                                 setYears(data[, 2098, ], 2100) +
                                 setYears(data[, 2097, ], 2100))) # give a point for 2100

  # distribute the history between all scenarios
  data <- do.call("mbind", lapply(rcps, function(rcp) {
    do.call("mbind", lapply(ssps, function(ssp) {
      tmp <- data[, , rcp][, , ssp]
      tmp[, yearsHistory, ] <- setNames(data[, yearsHistory, "history"][, , "history"],
                                        getNames(tmp[, yearsHistory, ]))
      return(tmp)
    }))
  }))


  # Smooth the trajectories
  dataSmoothed <- lapply(ssps, function(scenario) {
    lapply(rcps, function(rcp) {
      thirdDim <- paste(scenario, variable, rcp, tlimit, sep = ".")
      out <- lowpass(data[, , thirdDim], i = 100)
      return(out)
    })
  })
  dataSmoothed <- do.call("mbind", unlist(dataSmoothed, recursive = FALSE))


  # Smooth of the transition between historical and scenario values
  data <- data * lambda + dataSmoothed * (1 - lambda)

  # replace negative values with 0 (large neg values only  for  small countries)
  data[which(data < 0)] <- 0

  # Set regions where there is either no value for history or for scenarios to 0
  data["GRL", , ] <- 0
  data[c("ASM", "FSM", "GUM", "MDV", "MHL", "TUV", "TKL", "SJM"), , ] <- 0

  # Add the no Climate Change scenario
  noC <- data[, , "rcp2p6"]

  # Fill with the mean of the five last periods
  noC[, yearsScenario, ] <-
    dimSums(noC[, c((max(yearsHistory) - 4):max(yearsHistory)), ], dim = 2) / 5
  getNames(noC) <- gsub("rcp2p6", "rcpNoC", getNames(noC))
  data <- mbind(data, noC)

  pop <- calcOutput("Population", aggregate = FALSE, FiveYearSteps = FALSE)
  getNames(pop) <- tolower(sub("pop_(....).?$", "\\1", getNames(pop)))
  pop <- pop[, , ssps]
  y <- intersect(getYears(data), getYears(pop))
  data <- data[, y, ]
  pop <- pop[, y, ]

  return(list(x = data,
              weight = pop,
              unit = "dK/yr",
              description = "Heating degree days or cooling degree days"))
}
