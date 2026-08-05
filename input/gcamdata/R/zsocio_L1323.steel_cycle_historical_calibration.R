# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L1323.steel_cycle_historical_calibration
#'
#' Calibrate historical regional iron and steel stocks by end-use subsector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on code{command}: declared inputs, declared outputs, or the
#'   generated historical stock-cycle tables.
#' @details Allocates historical steel consumption to transport (T), machinery
#'   (M), construction (C), and products (P), then applies an inflow-driven
#'   Weibull dynamic stock model. Historical population is extended to 1900 to
#'   calculate stock per capita. Lifetime pathways are also prepared for the
#'   downstream SSP stock-cycle chunk.
#' @importFrom dplyr arrange bind_rows filter group_by if_else left_join mutate
#'   rename select summarise transmute ungroup
#' @importFrom tidyr complete crossing nesting pivot_longer unnest
#' @importFrom tibble as_tibble tibble
#' @importFrom stats approx
#' @author Jerry
module_socio_L1323.steel_cycle_historical_calibration <- function(command, ...) {

  MODULE_INPUTS <- c(
    FILE = "common/GCAM_region_names",
    FILE = "material/historical_calibration/A1323.MFA_stock_subsector_baseyear_calibration_Y",
    FILE = "material/historical_calibration/A1323.MFA_SSP_subsector_lftm_parameter",
    "LB1092.Tradebalance_iron_steel_Mt_R_Y",
    "L101.Pop_thous_R_Yh"
  )

  MODULE_OUTPUTS <- c(
    "L1323.MFA_spc_iron_steel_subsector_tpc_Yh",
    "L1323.Pop_thous_R_Yh_longtail",
    "L1323.MFA_lifetime_subsector_lft_scale_Yall",
    "L1323.MFA_lifetime_subsector_lft_shape_Yall"
  )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- GCAM_region <- S_per <- i_value <- i_value_subsector <-
      lft_scale <- lft_shape <- metric <- population <- region <- scenario <-
      stock_value <- subsector <- value <- year <- . <- NULL

    all_data <- list(...)[[1]]
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Load the repository's required native-R ODYM implementation into this
    # MAKE environment, avoiding package-global side effects.
    sys.source(file.path("R", "odym_r.R"), envir = environment())

    fabrication_loss <- 0.1
    historical_years <- sort(unique(
      LB1092.Tradebalance_iron_steel_Mt_R_Y$year[
        LB1092.Tradebalance_iron_steel_Mt_R_Y$metric == "consumption_reval"
      ]
    ))
    base_year <- max(historical_years)
    improved_year <- 2060

    # Allocate historical finished-steel consumption to end-use subsectors.
    A1323.MFA_stock_subsector_baseyear_calibration_Y %>%
      rename(T = T_S_per, M = M_S_per, C = C_S_per, P = P_S_per) %>%
      select(region, T, M, C, P) %>%
      pivot_longer(cols = -region, names_to = "subsector", values_to = "S_per") ->
      A1323.MFA_stock_subsector_baseyear_calibration_Y_long

    LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      filter(metric == "consumption_reval", year %in% historical_years) %>%
      transmute(region = GCAM_region, year, i_value = value * (1 - fabrication_loss)) %>%
      left_join(A1323.MFA_stock_subsector_baseyear_calibration_Y_long,
                by = "region", relationship = "many-to-many") %>%
      transmute(region, year, subsector, i_value_subsector = S_per * i_value) ->
      L1323.MFA_i_subsector_iron_steel_tpc_Yh

    # Prepare scenario-specific lifetime pathways. Historical assumptions are
    # held through the base year and linearly converge to scenario assumptions
    # in 2060, matching the reference mode-2 workflow.
    A1323.MFA_SSP_subsector_lftm_parameter %>%
      rename(T = T_lft_scale, M = M_lft_scale, C = C_lft_scale, P = P_lft_scale) %>%
      select(region, scenario, T, M, C, P) %>%
      pivot_longer(cols = c(T, M, C, P), names_to = "subsector", values_to = "lft_scale") ->
      A1323.MFA_lifetime_subsector_lft_scale_long

    A1323.MFA_lifetime_subsector_lft_scale_long %>%
      filter(scenario == "history") %>%
      select(-scenario) %>%
      rename(history_lft_scale = lft_scale) %>%
      left_join(
        A1323.MFA_lifetime_subsector_lft_scale_long %>%
          filter(!scenario %in% c("history", "ghistory")),
        by = c("region", "subsector"), relationship = "many-to-many"
      ) %>%
      tidyr::crossing(year = seq(min(historical_years),
                                 max(improved_year, base_year), by = 1)) %>%
      mutate(lft_scale = if_else(
        year <= base_year,
        history_lft_scale,
        if_else(year < improved_year,
                history_lft_scale + (lft_scale - history_lft_scale) *
                  (year - base_year) / (improved_year - base_year),
                lft_scale)
      )) %>%
      select(region, scenario, subsector, year, lft_scale) %>%
      arrange(region, scenario, subsector, year) ->
      L1323.MFA_lifetime_subsector_lft_scale_Yall

    L1323.MFA_lifetime_subsector_lft_scale_Yall %>%
      mutate(lft_shape = lft_scale * 0.03) %>%
      select(region, scenario, subsector, year, lft_shape) ->
      L1323.MFA_lifetime_subsector_lft_shape_Yall

    # Run the historical inflow-driven DSM using the SSP2 lifetime path, as in
    # the reference calculation.
    L1323.MFA_s_iron_steel_subsector_Mt_Yh <- tibble()
    for(region_loop in unique(L1323.MFA_i_subsector_iron_steel_tpc_Yh$region)) {
      for(subsector_loop in unique(L1323.MFA_i_subsector_iron_steel_tpc_Yh$subsector)) {
        L1323.MFA_i_subsector_iron_steel_tpc_Yh %>%
          filter(region == region_loop, subsector == subsector_loop) %>%
          arrange(year) -> subsector_region_data

        L1323.MFA_lifetime_subsector_lft_scale_Yall %>%
          filter(region == region_loop, scenario == "SSP2",
                 subsector == subsector_loop,
                 year %in% subsector_region_data$year) %>%
          arrange(year) %>%
          select(lft_scale) %>%
          unlist(use.names = FALSE) %>%
          as.numeric() -> odym_scale

        L1323.MFA_lifetime_subsector_lft_shape_Yall %>%
          filter(region == region_loop, scenario == "SSP2",
                 subsector == subsector_loop,
                 year %in% subsector_region_data$year) %>%
          arrange(year) %>%
          select(lft_shape) %>%
          unlist(use.names = FALSE) %>%
          as.numeric() -> odym_shape

        DSM(t = subsector_region_data$year,
            i = subsector_region_data$i_value_subsector,
            lt = list(Type = "Weibull", Shape = odym_shape,
                      Scale = odym_scale)) -> DSMobject
        DSMobject$compute_s_c_inflow_driven()

        L1323.MFA_s_iron_steel_subsector_Mt_Yh %>%
          bind_rows(tibble(region = region_loop,
                           year = subsector_region_data$year,
                           subsector = subsector_loop,
                           value = DSMobject$compute_stock_total())) ->
          L1323.MFA_s_iron_steel_subsector_Mt_Yh
      }
    }

    # Extend historical population to 1900 by linear interpolation between the
    # earliest available observations and retain reported values thereafter.
    L101.Pop_thous_R_Yh %>%
      rename(population = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      complete(year = seq(min(historical_years), max(historical_years)),
               nesting(GCAM_region_ID, region)) %>%
      group_by(GCAM_region_ID, region) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(population = approx(year[!is.na(population)],
                                 population[!is.na(population)],
                                 year, rule = 2)[["y"]]) %>%
      ungroup() %>%
      select(GCAM_region_ID, region, year, population) ->
      L1323.Pop_thous_R_Yh_longtail

    L1323.MFA_s_iron_steel_subsector_Mt_Yh %>%
      mutate(stock_value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(
        select(L1323.Pop_thous_R_Yh_longtail, region, year, population),
        by = c("region", "year")
      ) %>%
      mutate(spc_value_subsector = stock_value / population * 1000,
             odym_type = "spc") %>%
      select(region, year, odym_type, subsector, spc_value_subsector,
             stock_value, population) ->
      L1323.MFA_spc_iron_steel_subsector_tpc_Yh

    L1323.MFA_spc_iron_steel_subsector_tpc_Yh %>%
      add_title("Historical iron and steel stock per capita by end-use subsector") %>%
      add_units("t steel per person") %>%
      add_comments("Calculated with an inflow-driven Weibull dynamic stock model") %>%
      add_precursors("common/GCAM_region_names",
                     "material/historical_calibration/A1323.MFA_stock_subsector_baseyear_calibration_Y",
                     "material/historical_calibration/A1323.MFA_SSP_subsector_lftm_parameter",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y",
                     "L101.Pop_thous_R_Yh") ->
      L1323.MFA_spc_iron_steel_subsector_tpc_Yh

    L1323.Pop_thous_R_Yh_longtail %>%
      add_title("Historical population extended annually to 1900") %>%
      add_units("thousand persons") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_R_Yh") ->
      L1323.Pop_thous_R_Yh_longtail

    L1323.MFA_lifetime_subsector_lft_scale_Yall %>%
      add_title("Iron and steel Weibull lifetime scale by SSP and end use") %>%
      add_units("years") %>%
      add_precursors("material/historical_calibration/A1323.MFA_SSP_subsector_lftm_parameter") ->
      L1323.MFA_lifetime_subsector_lft_scale_Yall

    L1323.MFA_lifetime_subsector_lft_shape_Yall %>%
      add_title("Iron and steel Weibull lifetime shape by SSP and end use") %>%
      add_units("Unitless") %>%
      add_precursors("L1323.MFA_lifetime_subsector_lft_scale_Yall") ->
      L1323.MFA_lifetime_subsector_lft_shape_Yall

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
