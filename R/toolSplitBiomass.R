#' Splits Biomass according to GDP per Capita
#'
#' This is adapted from EDGE-B Function "splitBiomass.R" by Antoine Levesque.
#'
#' Any value belonging to the carrier defined by "varName" will be split into
#' the default carrier names "biomod" and "biotrad" according to weights given
#' by "df_gdppop".
#'
#' @param df dataframe with carrier as 'variable'
#' @param df_gdppop dataframe containing the GDP per Capita
#' @param min lower threshold of GDP per Capita
#' @param max upper threshold of GDP per Capita
#' @param varName nomenclature of biomass
#' @param newvarName nomenclature of mod/trad biomass
#'
#' @returns factor.data.frame
#'
#' @author Hagen Tockhorn
#'
#' @export


toolSplitBiomass <- function(df,
                             df_gdppop,
                             min=10000,
                             max=15000,
                             varName="biomass") {

  # FUNCTIONS ------------------------------------------------------------------

  # Returns share of modern Biomass
  shareTrad <- function(x, min, max) {
    share <- pmin(x * 0 + 1, pmax(0 * x, (max - x) / (max - min)))
    return(share)
  }


  # PARAMETERS -----------------------------------------------------------------

  bioComponents <- "^bio(mod|trad).$"


  # PROCESS DATA ---------------------------------------------------------------

  keepCol = setdiff(colnames(df),c("value", "variable"))

  df_gdppop = df_gdppop %>% rename(gdppop = value) %>% select(-variable)

  # Split Biomass
  tmp = df %>%
    left_join(df_gdppop, by = c("region","period")) %>%
    spread(variable, value) %>%
    mutate(shareMo = shareTrad(.data[["gdppop"]],min,max),
           biomod2 = .data[[varName]] * shareMo,
           biotrad2 = .data[[varName]] * (1 - shareMo))


  # Check for already existing Entries and add them up if existing
  bioCols = grep(bioComponents,colnames(tmp),value = T)
  tmp[bioCols][is.na(tmp[bioCols])] <- 0

  biomod_names = grep("^biomod.$", bioCols, value = T)
  biotrad_names = grep("^biotrad.$", bioCols, value = T)
  if(length(biomod_names) > 1){
    tmp$biomod = rowSums(tmp[,biomod_names],na.rm = T)}
  else{ tmp$biomod = tmp[[biomod_names]]
  }

  if(length(biotrad_names) > 1){
    tmp$biotrad = rowSums(tmp[,biotrad_names],na.rm = T)
  }else{
    tmp$biotrad = tmp[[biotrad_names]]
  }


  # OUTPUT ---------------------------------------------------------------------

  tmp = tmp %>% select(- matches(bioComponents)) %>%
    select(-gdppop, -shareMo, -.data[[varName]]) %>%
    gather(variable, value, -one_of(keepCol)) %>%
    factor.data.frame()


  return(tmp[colnames(df)])

}
