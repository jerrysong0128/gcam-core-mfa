# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L238.iron_steel_trade
#'
#' Model input for regional and (globally) traded iron and steel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L238.Supplysector_tra},
#'   \code{L238.SectorUseTrialMarket_tra}, \code{L238.SubsectorAll_tra}, \code{L238.TechShrwt_tra},
#'   \code{L238.TechCost_tra}, \code{L238.TechCoef_tra}, \code{L238.Production_tra}, \code{L238.Supplysector_reg},
#'   \code{L238.SubsectorAll_reg}, \code{L238.TechShrwt_reg}, \code{L238.TechCoef_reg}, \code{L238.Production_reg_imp},
#'   \code{L238.Production_reg_dom}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by if_else left_join mutate rename select summarise ungroup
#' @importFrom tidyr complete nesting replace_na
#' @importFrom tibble tibble
#' @author Siddarth Durga July 2022
module_energy_L238.iron_steel_trade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_irnstl_RegionalSector",
             FILE = "energy/A_irnstl_RegionalSubsector",
             FILE = "energy/A_irnstl_RegionalTechnology",
             FILE = "energy/A_irnstl_TradedSector",
             FILE = "energy/A_irnstl_TradedSubsector",
             FILE = "energy/A_irnstl_TradedTechnology",
             FILE = "energy/A323.globaltech_shrwt",
             FILE = "energy/A_irnstl_base_shareweights",
             FILE = "energy/A_irnstl_tech_reference",
             "LB1092.Tradebalance_iron_steel_Mt_R_Y",
             "L2323.StubTechProd_iron_steel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L238.Supplysector_tra",
             "L238.SectorUseTrialMarket_tra",
             "L238.SubsectorAll_tra",
             "L238.TechShrwt_tra",
             "L238.TechCost_tra",
             "L238.TechCoef_tra",
             "L238.Production_tra",
             "L238.Supplysector_reg",
             "L238.SubsectorAll_reg",
             "L238.TechShrwt_reg",
             "L238.TechCoef_reg",
             "L238.Production_reg_imp",
             "L238.Production_reg_dom"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_commodity <- GrossExp_Mt <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_Mt <- Prod_Mt <- GCAM_region_ID <- GCAM_region <- NetExp_Mt <- Prod_bm3 <-
      NetExp_bm3 <- value <- metric <- flow <- GrossExp <- route.production <-
      route.share <- technology <- share.weight <- reference_tech <- ref_shwt <-
      share.weight.base <- gated.shwt <- shareweight <- NULL # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_irnstl_RegionalSector <- get_data(all_data, "energy/A_irnstl_RegionalSector", strip_attributes = TRUE)
    A_irnstl_RegionalSubsector <- get_data(all_data, "energy/A_irnstl_RegionalSubsector", strip_attributes = TRUE)
    A_irnstl_RegionalTechnology <- get_data(all_data, "energy/A_irnstl_RegionalTechnology", strip_attributes = TRUE)
    A_irnstl_TradedSector <- get_data(all_data, "energy/A_irnstl_TradedSector", strip_attributes = TRUE)
    A_irnstl_TradedSubsector <- get_data(all_data, "energy/A_irnstl_TradedSubsector", strip_attributes = TRUE)
    A_irnstl_TradedTechnology <- get_data(all_data, "energy/A_irnstl_TradedTechnology", strip_attributes = TRUE)
    A323.globaltech_shrwt <- get_data(all_data, "energy/A323.globaltech_shrwt", strip_attributes = TRUE)
    A_irnstl_base_shareweights <- get_data(all_data, "energy/A_irnstl_base_shareweights", strip_attributes = TRUE)
    A_irnstl_tech_reference <- get_data(all_data, "energy/A_irnstl_tech_reference", strip_attributes = TRUE)
    LB1092.Tradebalance_iron_steel_Mt_R_Y <- get_data(all_data, "LB1092.Tradebalance_iron_steel_Mt_R_Y")
    L2323.StubTechProd_iron_steel <- get_data(all_data, "L2323.StubTechProd_iron_steel", strip_attributes = TRUE)

    # The public trade interface remains a single regional and a single global
    # steel commodity.  Internally, split domestic supply and exports across
    # the 22 production-route sectors in proportion to calibrated production.
    steel_routes <- L2323.StubTechProd_iron_steel %>%
      distinct(minicam.energy.input = supplysector)

    steel_route_shares <- L2323.StubTechProd_iron_steel %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(region, year, supplysector) %>%
      summarise(route.production = sum(calOutputValue), .groups = "drop") %>%
      group_by(region, year) %>%
      mutate(route.share = if_else(rep(sum(route.production) > 0, n()),
                                   route.production / sum(route.production),
                                   rep(0, n()))) %>%
      ungroup() %>%
      rename(minicam.energy.input = supplysector)

    A_irnstl_TradedTechnology <- A_irnstl_TradedTechnology %>%
      select(-minicam.energy.input) %>%
      repeat_add_columns(steel_routes)

    A_irnstl_RegionalTechnology <- bind_rows(
      A_irnstl_RegionalTechnology %>% filter(grepl("import", subsector)),
      A_irnstl_RegionalTechnology %>%
        filter(grepl("domestic", subsector)) %>%
        select(-technology, -minicam.energy.input) %>%
        repeat_add_columns(steel_routes) %>%
        mutate(technology = paste("domestic", minicam.energy.input))
    )

    # Gate route-level share-weights so demand is not sent to production routes
    # that cannot supply in a given year (root cause of Supply=0/Demand>0 in 2025).
    # Route availability comes from A323.globaltech_shrwt; calibrated routes use
    # their region-specific base share-weight, and advanced routes get the route
    # availability (globaltech_shrwt) multiplied by their reference route's base.
    route_shrwt_global <- A323.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      select(technology, year, share.weight)

    # Region-specific base share-weights (2015), interpolated linearly to 1 in 2100
    route_shrwt_base <- A_irnstl_base_shareweights %>%
      select(region, technology, share.weight.base = shareweight) %>%
      mutate(year = 2015) %>%
      complete(nesting(region, technology), year = c(2015, MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight.base = if_else(year == 2100, 1, share.weight.base)) %>%
      group_by(region, technology) %>%
      mutate(share.weight.base = approx_fun(year, share.weight.base)) %>%
      ungroup

    # Combine: calibrated routes -> own base share-weight; advanced routes ->
    # route availability x reference route base share-weight
    route_shrwt_regional <- route_shrwt_global %>%
      left_join_error_no_match(A_irnstl_tech_reference, by = "technology") %>%
      repeat_add_columns(GCAM_region_names %>% select(region)) %>%
      left_join(route_shrwt_base %>% rename(ref_shwt = share.weight.base),
                by = c("reference_tech" = "technology", "region", "year")) %>%
      tidyr::replace_na(list(ref_shwt = 1)) %>%
      left_join(route_shrwt_base, by = c("technology", "region", "year")) %>%
      mutate(share.weight = if_else(!is.na(share.weight.base),
                                    share.weight.base, share.weight * ref_shwt)) %>%
      select(region, minicam.energy.input = technology, year, gated.shwt = share.weight)

    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L238.Supplysector_tra: generic supplysector info for traded iron and steel
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_irnstl_TradedSector$region <- gcam.USA_REGION

    # L238.Supplysector_tra: generic supplysector info for traded iron and steel
    L238.Supplysector_tra <- mutate(A_irnstl_TradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L238.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L238.SectorUseTrialMarket_tra <- select(A_irnstl_TradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L238.SubsectorAll_tra: generic subsector info for traded iron and steel
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L238.SubsectorAll_tra <- write_to_all_regions(A_irnstl_TradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names,
                                                  has_traded = TRUE)


    # Change traded iron and steel interpolation rule and to.value in countries listed in energy.IRON_STEEL.DOMESTIC_SW
    L238.SubsectorAll_tra$interpolation.function[which(L238.SubsectorAll_tra$subsector %in% energy.IRON_STEEL.TRADED_SW)] <- "s-curve"
    L238.SubsectorAll_tra$to.year[which(L238.SubsectorAll_tra$subsector %in% energy.IRON_STEEL.TRADED_SW)] <- 2300

    # Base technology-level table for several tables to be written out")
    A_irnstl_TradedTechnology_R_Y <- repeat_add_columns(A_irnstl_TradedTechnology,
                                                   tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = paste(subsector, minicam.energy.input),
             market.name = region,
             region = gcam.USA_REGION)

    # L238.TechShrwt_tra: Share-weights of traded technologies (gated by route availability)
    # Base-year share-weights are supplied by the calibration (Production) tables,
    # so only future-year share-weights are written here to avoid redundancy.
    L238.TechShrwt_tra <- A_irnstl_TradedTechnology_R_Y %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      left_join(route_shrwt_regional, by = c(market.name = "region", "minicam.energy.input", "year")) %>%
      mutate(share.weight = if_else(is.na(gated.shwt), share.weight, gated.shwt)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L238.TechCost_tra: Costs of traded technologies
    L238.TechCost_tra <- A_irnstl_TradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L238.TechCoef_tra: Coefficient and market name of traded technologies
    L238.TechCoef_tra <- select(A_irnstl_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])



    # L238.Production_tra: Output (gross exports) of traded technologies
    L238.GrossExports_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                             filter(metric=="exports_reval") %>%
                                                             rename(GrossExp_Mt=value,region=GCAM_region),
                                                           GCAM_region_names,
                                                           by = "region") %>%
      select(region, year, GrossExp_Mt)

    L238.Production_tra <- filter(A_irnstl_TradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L238.GrossExports_Mt_R_Y,
                               by = c(market.name = "region", "year")) %>%
      left_join_error_no_match(steel_route_shares,
                               by = c(market.name = "region", "year", "minicam.energy.input")) %>%
      mutate(calOutputValue = round(GrossExp_Mt * route.share, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L238.Supplysector_reg: generic supplysector info for iron and steel
    L238.Supplysector_reg <- mutate(A_irnstl_RegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L238.SubsectorAll_reg: generic subsector info for regional iron and steel (competing domestic prod vs intl imports)
    L238.SubsectorAll_reg <- write_to_all_regions(A_irnstl_RegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names)

    # Change iron and steel domestic supply interpolation rule and to.value in countries listed in energy.IRON_STEEL.DOMESTIC_SW
    L238.SubsectorAll_reg$to.value[which(L238.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L238.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- 1
    L238.SubsectorAll_reg$interpolation.function[which(L238.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L238.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- "s-curve"
    L238.SubsectorAll_reg$to.year[which(L238.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L238.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- 2105

    # Base technology-level table for several tables to be written out")
    A_irnstl_RegionalTechnology_R_Y <- repeat_add_columns(A_irnstl_RegionalTechnology,
                                                     tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L238.TechShrwt_reg: Share-weights of regional technologies (domestic routes gated by availability)
    # Base-year share-weights are supplied by the calibration (Production) tables,
    # so only future-year share-weights are written here to avoid redundancy.
    L238.TechShrwt_reg <- A_irnstl_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      left_join(route_shrwt_regional, by = c("region", "minicam.energy.input", "year")) %>%
      mutate(share.weight = if_else(is.na(gated.shwt), share.weight, gated.shwt)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L238.TechCoef_reg: Coefficient and market name of traded technologies
    L238.TechCoef_reg <- select(A_irnstl_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L238.Production_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in LB1092
    L238.GrossImports_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                           filter(metric=="imports_reval") %>%
                                                           mutate(supplysector = "traded iron and steel") %>%
                                                           rename(GrossImp_Mt=value,region=GCAM_region),
                                                           GCAM_region_names,
                                                           by = "region") %>%
      select(region, supplysector, year, GrossImp_Mt)

    L238.Production_reg_imp <- A_irnstl_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L238.GrossImports_Mt_R_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_Mt) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L238.Production_reg_dom: Output (flow) of domestic

    #### DOMESTIC TECHNOLOGY OUTPUT = iron and steel PRODUCTION - GROSS EXPORTS
    L238.DomSup_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                     filter(metric=="domestic_supply") %>%
                                                     rename(DomSup_Mt=value,region=GCAM_region),
                                                   GCAM_region_names,
                                                   by = "region") %>%
      select(region, year, DomSup_Mt)

    L238.Production_reg_dom <- A_irnstl_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L238.DomSup_Mt_R_Y,
                               by = c("region", "year")) %>%
      left_join_error_no_match(steel_route_shares,
                               by = c("region", "year", "minicam.energy.input")) %>%
      mutate(calOutputValue = round(DomSup_Mt * route.share, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Produce outputs
    L238.Supplysector_tra %>%
      add_title("Supplysector info for iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSector") ->
      L238.Supplysector_tra

    L238.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with iron and steel trade") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSector") ->
      L238.SectorUseTrialMarket_tra

    L238.SubsectorAll_tra %>%
      add_title("Subsector info for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSubsector") ->
      L238.SubsectorAll_tra

    L238.TechShrwt_tra %>%
      add_title("Technology share-weights for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions; route technologies gated by route availability") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology",
                     "energy/A323.globaltech_shrwt",
                     "energy/A_irnstl_base_shareweights",
                     "energy/A_irnstl_tech_reference") ->
      L238.TechShrwt_tra

    L238.TechCost_tra %>%
      add_title("Technology costs for traded iron and steel") %>%
      add_units("1975$/kg") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology") ->
      L238.TechCost_tra

    L238.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded iron and steel") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology") -> L238.TechCoef_tra

    L238.Production_tra %>%
      add_title("Technology calibration for traded iron and steel") %>%
      add_units("Mt") %>%
      add_comments("Regional exports of iron and steel that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") -> L238.Production_tra

    L238.Supplysector_reg %>%
      add_title("Supplysector info for regional iron and steel") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced iron and steel versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalSector") ->
      L238.Supplysector_reg

    L238.SubsectorAll_reg %>%
      add_title("Subsector info for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalSubsector") ->
      L238.SubsectorAll_reg

    L238.TechShrwt_reg %>%
      add_title("Technology share-weights for regional iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions; domestic route technologies gated by route availability") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalTechnology",
                     "energy/A323.globaltech_shrwt",
                     "energy/A_irnstl_base_shareweights",
                     "energy/A_irnstl_tech_reference") ->
      L238.TechShrwt_reg

    L238.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional iron and steel") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalTechnology") ->
      L238.TechCoef_reg

    L238.Production_reg_imp %>%
      add_title("Technology calibration for regional iron and steel commodities: imports") %>%
      add_units("Mt") %>%
      add_comments("Consumption of iron and steelthat are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L238.Production_reg_imp

    L238.Production_reg_dom %>%
      add_title("Technology calibration for regional iron and steel: consumption of domestic production") %>%
      add_units("Mt") %>%
      add_comments("Consumption of iron and steel produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L238.Production_reg_dom

    return_data(L238.Supplysector_tra,
                L238.SectorUseTrialMarket_tra,
                L238.SubsectorAll_tra,
                L238.TechShrwt_tra,
                L238.TechCost_tra,
                L238.TechCoef_tra,
                L238.Production_tra,
                L238.Supplysector_reg,
                L238.SubsectorAll_reg,
                L238.TechShrwt_reg,
                L238.TechCoef_reg,
                L238.Production_reg_imp,
                L238.Production_reg_dom)
  } else {
    stop("Unknown command")
  }
}
