calcRenovationAlternatives <- function() {
  # I test models to predict specific renovation cost based on the EC report on renovation
  renovation <- readSource("EuropeanCommissionRenovation") %>%
    as.quitte() %>%
    filter(!is.na(value))
  gdp <- calcOutput("GDPPast", aggregate = FALSE) %>%
    as.quitte()
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()
  gdppop <- rbind(gdp, pop) %>%
    filter(.data[["period"]] %in% 2012:2016) %>%
    group_by(across(all_of(c("region", "variable")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    calc_addVariable(gdppop = "`gdp in constant 2005 Int$PPP` / `population`",
                     units = "USD2005/cap", only.new = TRUE) %>%
    spread("variable", "value")

  # GDP controlled -------------------------------------------------------------
  # specificRenovationCost_r / gdppop_r = beta1 + beta2 * relativePeSaving_r
  # poor prediction power: R2 = 0.45
  data <- renovation %>%
    select(-"unit", -"model", -"scenario", -"period") %>%
    filter(.data[["renovation"]] %in% paste("Energy related -", c("below Threshold", "Light", "Medium", "Deep"))) %>%
    spread("variable", "value") %>%
    left_join(gdppop, by = "region") %>%
    mutate(x = .data[["relative PE savings"]],
           y = .data[["specific investment"]] / .data[["gdppop"]])

  model <- lapply(unique(data$subsector), function(subsec) {
    lm(y ~ x, filter(data, .data[["subsector"]] == subsec))
  })

  # predict
  data[data$subsector == "residential", "y_pred"] <- predict(model[[1]], data[data$subsector == "residential",])
  data[data$subsector == "commercial",  "y_pred"] <- predict(model[[2]], data[data$subsector == "commercial",])

  data <- data %>%
    mutate(`specific investment_pred` = .data[["y_pred"]] * .data[["gdppop"]],
           `relative PE savings_pred` = .data[["relative PE savings"]]) %>%
    gather("variable", "value",
           -any_of(c("region", "subsector", "renovation"))) %>%
    separate("variable", c("variable", "method"), "_") %>%
    mutate(method = replace_na(.data[["method"]], "data"),
           variable = gsub(" ", "", .data[["variable"]])) %>%
    spread("variable", "value")
  ggplot(data, aes_string("relativePEsavings", "specificinvestment", colour = "region")) +
    geom_line(aes(size = region, linetype = method)) +
    geom_point(aes(shape = renovation), size = 3) +
    facet_wrap("subsector", scales = "free_x") +
    scale_size_manual(values = ifelse(levels(data$region) == "EU28", 2, 1)) +
    theme_bw()

  
  # Unspecific regional factor -------------------------------------------------
  # specificRenovationCost_r * factor_r = beta1 + beta2 * relativePeSaving_r
  

  

}
