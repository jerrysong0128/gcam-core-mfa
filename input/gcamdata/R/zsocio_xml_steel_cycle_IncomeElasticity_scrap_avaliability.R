# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_steel_cycle_IncomeElasticity_scrap_availability_xml
#'
#' Write the SSP income-elasticity files using the steel-cycle (MFA) steel
#' demand projection and write the corresponding regional scrap constraints.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated income-elasticity and scrap-availability XML files.
module_socio_steel_cycle_IncomeElasticity_scrap_availability_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  MODULE_INPUTS <- c(
    "L2323.MFA_p_iron_steel_subsector_total_Mt_Y",
    "L2323.MFA_scrap_iron_steel_subsector_total_Mt_Y",
    FILE = "common/GCAM_region_names",
    "L102.pcgdp_thous90USD_Scen_R_Y",
    "L101.Pop_thous_SSP_R_Yfut"
  )

  income_elasticity_xml <- paste0("iron_steel_incelas_mfa_ssp", SSP_NUMS, ".xml")
  scrap_availability_xml <- paste0("scrap_availability_ssp", SSP_NUMS, ".xml")

  MODULE_OUTPUTS <- setNames(
    c(income_elasticity_xml, scrap_availability_xml),
    rep("XML", length(income_elasticity_xml) + length(scrap_availability_xml))
  )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Follow mfa_tools.R: elasticity is the change in per-capita steel
    # production divided by the change in per-capita GDP. Calculate the lag
    # before filtering to GCAM future years so that the first model period can
    # use the preceding period when it is available.
    L2323.MFA_p_iron_steel_subsector_total_Mt_Y %>%
      inner_join(
        L102.pcgdp_thous90USD_Scen_R_Y %>%
          left_join_error_no_match(GCAM_region_names,
                                   by = "GCAM_region_ID") %>%
          transmute(region, scenario, year = as.integer(year), pcgdp = value),
        by = c("region", "scenario", "year")
      ) %>%
      inner_join(
        L101.Pop_thous_SSP_R_Yfut %>%
          left_join_error_no_match(GCAM_region_names,
                                   by = "GCAM_region_ID") %>%
          transmute(region, scenario, year = as.integer(year), population = value),
        by = c("region", "scenario", "year")
      ) %>%
      group_by(region, scenario) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(
        income.elasticity =
          (log(value / lag(value)) - log(population / lag(population))) /
          log(pcgdp / lag(pcgdp)),
        income.elasticity = pmax(-10, pmin(10, income.elasticity)),
        energy.final.demand = "regional iron and steel"
      ) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS, is.finite(income.elasticity)) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) ->
      income_elasticity

    # Quantities in the steel-cycle table are Mt of scrap. GCAM's scrap input
    # coefficient is 1.1 Mt scrap per Mt crude steel, so convert the available
    # material to the equivalent constraint on scrap-based steel production.
    scrap_conversion_factor <- 1.1
    scrap_constraint_years <- MODEL_FUTURE_YEARS

    # Loop through the SSPs and build both XML structures.

    for(ssp in SSP_NUMS) {

      ssp_name <- paste0("SSP", ssp)
      xmlfn <- paste0("iron_steel_incelas_mfa_ssp", ssp, '.xml')
      scrap_xmlfn <- paste0("scrap_availability_ssp", ssp, ".xml")

      create_xml(xmlfn) %>%
        add_xml_data(income_elasticity %>% filter(scenario == ssp_name),
                     "IncomeElasticity") %>%
        add_precursors(MODULE_INPUTS) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)

      L2323.MFA_scrap_iron_steel_subsector_total_Mt_Y %>%
        filter(scenario == ssp_name, year %in% scrap_constraint_years) %>%
        transmute(
          region,
          ghgpolicy = "scrap-constraint",
          market = region,
          constraint.year = year,
          constraint = value / scrap_conversion_factor
        ) -> scrap_constraint

      create_xml(scrap_xmlfn) %>%
        add_xml_data(scrap_constraint, "GHGConstr") %>%
        add_precursors("L2323.MFA_scrap_iron_steel_subsector_total_Mt_Y") ->
        scrap_xml_obj

      assign(scrap_xmlfn, scrap_xml_obj)
    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
