# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L2323.steel_cycle_SSP
#'
#' Project regional iron and steel stocks, flows, and scrap under the SSPs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on code{command}: declared inputs, declared outputs, or the
#'   generated SSP steel-cycle tables.
#' @details Uses mode-2 stock-saturation curves and a stock-driven Weibull
#'   dynamic stock model. The historical stock path is supplied by
#'   code{module_socio_L1323.steel_cycle_historical_calibration}.
#' @importFrom dplyr arrange bind_rows distinct filter group_by if_else
#'   inner_join lag left_join mutate recode rename select summarise
#'   transmute ungroup
#' @importFrom tidyr crossing fill pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @author Jerry
module_socio_L2323.steel_cycle_SSP <- function(command, ...) {

  MODULE_INPUTS <- c(
    FILE = "common/GCAM_region_names",
    FILE = "material/future_prediction/A2323.MFA_SSP_subsector_stock_S_curve_parameter",
    FILE = "material/future_prediction/A2323.MFA_SSP_subsector_recovery_parameter",
    "L101.Pop_thous_SSP_R_Yfut",
    "L102.pcgdp_thous90USD_Scen_R_Y",
    "L1323.MFA_spc_iron_steel_subsector_tpc_Yh",
    "L1323.Pop_thous_R_Yh_longtail",
    "L1323.MFA_lifetime_subsector_lft_scale_Yall",
    "L1323.MFA_lifetime_subsector_lft_shape_Yall"
  )

  MODULE_OUTPUTS <- c(
    "L2323.MFA_i_iron_steel_subsector_Mt_Y",
    "L2323.MFA_s_iron_steel_subsector_Mt_Y",
    "L2323.MFA_o_iron_steel_subsector_Mt_Y",
    "L2323.MFA_p_iron_steel_subsector_Mt_Y",
    "L2323.MFA_scrap_iron_steel_subsector_Mt_Y",
    "L2323.MFA_i_iron_steel_subsector_total_Mt_Y",
    "L2323.MFA_s_iron_steel_subsector_total_Mt_Y",
    "L2323.MFA_o_iron_steel_subsector_total_Mt_Y",
    "L2323.MFA_p_iron_steel_subsector_total_Mt_Y",
    "L2323.MFA_scrap_iron_steel_subsector_total_Mt_Y"
  )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- C <- C_a_sat <- C_b_sat <- C_S_sat <- C_spc <-
      M <- M_a_sat <- M_b_sat <- M_S_sat <- M_spc <- P <- P_a_sat <-
      P_b_sat <- P_S_sat <- P_spc <- T <- T_a_sat <- T_b_sat <-
      T_S_sat <- T_spc <- calibration_delta <-
      historical_spc_slope <- lft_scale <- lft_shape <-
      metric <- odym_type <- pcgdp_90thousUSD <- population <- r_rate <-
      region <- scenario <- spc_value <- spc_value_pred <- subsector <-
      value <- value_pc_reuse <- value_scrap <- year <- . <- NULL

    all_data <- list(...)[[1]]
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    sys.source(file.path("R", "odym_r.R"), envir = environment())

    fabrication_loss <- 0.1
    improved_year <- 2060
    slope_calibration_years <- 10
    base_year <- max(L1323.MFA_spc_iron_steel_subsector_tpc_Yh$year)

    L102.pcgdp_thous90USD_Scen_R_Y %>%
      dplyr::rename(pcgdp_90thousUSD = value) %>%
      dplyr::mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L102.pcgdp_thous90USD_Scen_R_Y

    L101.Pop_thous_SSP_R_Yfut %>%
      dplyr::rename(population = value) %>%
      dplyr::mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L101.Pop_thous_SSP_R_Yfut

    # Mode-2 stock-per-capita saturation curves.
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      dplyr::select(region, scenario, year, pcgdp_90thousUSD) %>%
      left_join_error_no_match(
        dplyr::select(A2323.MFA_SSP_subsector_stock_S_curve_parameter,
               region, scenario,
               T_S_sat, M_S_sat, C_S_sat, P_S_sat,
               T_a_sat, M_a_sat, C_a_sat, P_a_sat,
               T_b_sat, M_b_sat, C_b_sat, P_b_sat),
        by = c("region", "scenario")
      ) %>%
      dplyr::mutate(T_spc = T_S_sat / (1 + exp(T_a_sat - T_b_sat * pcgdp_90thousUSD * 1000)),
             M_spc = M_S_sat / (1 + exp(M_a_sat - M_b_sat * pcgdp_90thousUSD * 1000)),
             C_spc = C_S_sat / (1 + exp(C_a_sat - C_b_sat * pcgdp_90thousUSD * 1000)),
             P_spc = P_S_sat / (1 + exp(P_a_sat - P_b_sat * pcgdp_90thousUSD * 1000))) %>%
      dplyr::select(region, scenario, year, T_spc, M_spc, C_spc, P_spc) %>%
      tidyr::pivot_longer(cols = c(T_spc, M_spc, C_spc, P_spc),
                   names_to = "subsector", values_to = "spc_value_pred") %>%
      dplyr::mutate(subsector = dplyr::recode(subsector,
                                T_spc = "T", M_spc = "M",
                                C_spc = "C", P_spc = "P")) ->
      L2323.MFA_spc_iron_steel_tpc_YFu_before_calibration

    # Calibrate the level at the historical endpoint and smooth the first ten
    # future annual increments to the recent historical slope.
    L1323.MFA_spc_iron_steel_subsector_tpc_Yh %>%
      dplyr::filter(year %in% (base_year - 4):base_year) %>%
      dplyr::arrange(region, subsector, year) %>%
      dplyr::group_by(region, subsector) %>%
      dplyr::summarise(historical_spc_slope = mean(diff(spc_value_subsector), na.rm = TRUE),
                .groups = "drop") ->
      L2323.MFA_spc_iron_steel_slope_calibration

    L2323.MFA_spc_iron_steel_tpc_YFu_before_calibration %>%
      dplyr::left_join(
        L1323.MFA_spc_iron_steel_subsector_tpc_Yh %>%
          dplyr::filter(year == base_year) %>%
          dplyr::select(region, subsector, spc_value_subsector),
        by = c("region", "subsector")
      ) %>%
      dplyr::left_join(L2323.MFA_spc_iron_steel_slope_calibration,
                by = c("region", "subsector")) %>%
      dplyr::group_by(region, scenario, subsector) %>%
      dplyr::arrange(year, .by_group = TRUE) %>%
      dplyr::mutate(calibration_delta = spc_value_subsector -
               spc_value_pred[year == base_year][1],
             calibration_delta = dplyr::if_else(is.na(calibration_delta), 0,
                                         calibration_delta),
             future_initial_spc_slope = spc_value_pred[year > base_year][1] -
               spc_value_pred[year == base_year][1],
             slope_calibration_delta = historical_spc_slope -
               future_initial_spc_slope,
             slope_calibration_delta = dplyr::if_else(is.na(slope_calibration_delta),
                                               0, slope_calibration_delta),
             slope_calibration_step = pmax(year - base_year, 0),
             slope_calibration_weight = pmax(
               0, 1 - (slope_calibration_step - 1) /
                 (slope_calibration_years - 1)
             ),
             slope_calibration_increment = dplyr::if_else(
               year > base_year,
               slope_calibration_delta * slope_calibration_weight, 0
             ),
             slope_calibration_adjustment = cumsum(slope_calibration_increment),
             spc_value = spc_value_pred + calibration_delta +
               slope_calibration_adjustment) %>%
      dplyr::ungroup() %>%
      dplyr::select(region, scenario, year, subsector, spc_value) ->
      L2323.MFA_spc_iron_steel_tpc_YFu_after_calibration

    dplyr::bind_rows(
      L1323.MFA_spc_iron_steel_subsector_tpc_Yh %>%
        dplyr::select(region, year, subsector, spc_value = spc_value_subsector) %>%
        tidyr::crossing(scenario = dplyr::distinct(
          L2323.MFA_spc_iron_steel_tpc_YFu_after_calibration, scenario
        )$scenario),
      L2323.MFA_spc_iron_steel_tpc_YFu_after_calibration %>%
        dplyr::filter(year > base_year)
    ) %>%
      dplyr::mutate(odym_type = "spc") %>%
      dplyr::select(region, scenario, year, odym_type, subsector, spc_value) ->
      L2323.MFA_spc_iron_steel_subsector_tpc_Yall

    # Join historical and SSP population, then calculate total stock.
    L1323.Pop_thous_R_Yh_longtail %>%
      dplyr::select(GCAM_region_ID, region, year, population) %>%
      tidyr::crossing(scenario = dplyr::distinct(L101.Pop_thous_SSP_R_Yfut, scenario)$scenario) %>%
      dplyr::bind_rows(L101.Pop_thous_SSP_R_Yfut %>%
                  dplyr::select(GCAM_region_ID, region, scenario, year, population)) %>%
      dplyr::distinct(region, scenario, year, .keep_all = TRUE) ->
      L101_Pop_hist_and_fut

    L2323.MFA_spc_iron_steel_subsector_tpc_Yall %>%
      left_join_error_no_match(
        dplyr::select(L101_Pop_hist_and_fut, region, scenario, year, population),
        by = c("region", "scenario", "year")
      ) %>%
      dplyr::transmute(region, scenario, year, odym_type = "s", subsector,
                value = spc_value * population * 0.001) ->
      L2323.MFA_s_iron_steel_subsector_Mt_Y

    # Extend lifetime assumptions beyond 2060 by holding the theoretical limit.
    L2323.MFA_s_iron_steel_subsector_Mt_Y %>%
      dplyr::distinct(region, scenario, subsector, year) %>%
      dplyr::left_join(L1323.MFA_lifetime_subsector_lft_scale_Yall,
                by = c("region", "scenario", "subsector", "year")) %>%
      dplyr::left_join(L1323.MFA_lifetime_subsector_lft_shape_Yall,
                by = c("region", "scenario", "subsector", "year")) %>%
      dplyr::group_by(region, scenario, subsector) %>%
      dplyr::arrange(year, .by_group = TRUE) %>%
      tidyr::fill(lft_scale, lft_shape, .direction = "downup") %>%
      dplyr::ungroup() -> L2323.MFA_lifetime_Yall

    # Stock-driven DSM for each region, SSP, and end-use subsector.
    dsm_progress <- progress::progress_bar$new(
      format = paste0(
        "[:bar] :percent (:current/:total) ",
        "Time elapsed: :elapsed | ETA: :eta"
      ),
      total = length(unique(L2323.MFA_s_iron_steel_subsector_Mt_Y$region)) *
        length(unique(L2323.MFA_s_iron_steel_subsector_Mt_Y$scenario)) *
        length(unique(L2323.MFA_s_iron_steel_subsector_Mt_Y$subsector)),
      clear = FALSE,
      width = 100
    )

    L2323.MFA_i_iron_steel_subsector_Mt_Y <- tibble::tibble()
    L2323.MFA_o_iron_steel_subsector_Mt_Y <- tibble::tibble()
    for(region_loop in unique(L2323.MFA_s_iron_steel_subsector_Mt_Y$region)) {
      for(subsector_loop in unique(L2323.MFA_s_iron_steel_subsector_Mt_Y$subsector)) {
        for(scenario_loop in unique(L2323.MFA_s_iron_steel_subsector_Mt_Y$scenario)) {
          L2323.MFA_s_iron_steel_subsector_Mt_Y %>%
            dplyr::filter(region == region_loop, scenario == scenario_loop,
                   subsector == subsector_loop) %>%
            dplyr::arrange(year) -> stock_data

          L2323.MFA_lifetime_Yall %>%
            dplyr::filter(region == region_loop, scenario == scenario_loop,
                   subsector == subsector_loop,
                   year %in% stock_data$year) %>%
            dplyr::arrange(year) -> lifetime_data

          DSM(t = stock_data$year, s = stock_data$value,
              lt = list(Type = "Weibull",
                        Shape = lifetime_data$lft_shape,
                        Scale = lifetime_data$lft_scale)) -> DSMobject
          DSMobject$compute_stock_driven_model(NegativeInflowCorrect = TRUE) ->
            DSMoutputs

          L2323.MFA_i_iron_steel_subsector_Mt_Y %>%
            dplyr::bind_rows(tibble::tibble(region = region_loop, scenario = scenario_loop,
                             year = stock_data$year, subsector = subsector_loop,
                             value = DSMoutputs[[3]])) ->
            L2323.MFA_i_iron_steel_subsector_Mt_Y

          L2323.MFA_o_iron_steel_subsector_Mt_Y %>%
            dplyr::bind_rows(tibble::tibble(region = region_loop, scenario = scenario_loop,
                             year = stock_data$year, subsector = subsector_loop,
                             value = DSMobject$compute_outflow_total())) ->
            L2323.MFA_o_iron_steel_subsector_Mt_Y

          dsm_progress$tick()
        }
      }
    }

    L2323.MFA_i_iron_steel_subsector_Mt_Y %>%
      dplyr::mutate(value = round(value, energy.DIGITS_CALOUTPUT), odym_type = "i") %>%
      dplyr::select(region, scenario, year, odym_type, subsector, value) ->
      L2323.MFA_i_iron_steel_subsector_Mt_Y

    L2323.MFA_o_iron_steel_subsector_Mt_Y %>%
      dplyr::mutate(value = round(value, energy.DIGITS_CALOUTPUT), odym_type = "o") %>%
      dplyr::select(region, scenario, year, odym_type, subsector, value) ->
      L2323.MFA_o_iron_steel_subsector_Mt_Y

    # Recovery/reuse pathway and available scrap. In the reference workflow
    # the EoL recovery multiplier is one at its theoretical limit; r_rate then
    # divides recovered material between direct reuse and scrap.
    A2323.MFA_SSP_subsector_recovery_parameter %>%
      dplyr::rename(T = T_r_rate, M = M_r_rate, C = C_r_rate, P = P_r_rate) %>%
      dplyr::select(region, scenario, T, M, C, P) %>%
      tidyr::pivot_longer(cols = c(T, M, C, P), names_to = "subsector",
                   values_to = "r_rate") -> recovery_long

    recovery_long %>%
      dplyr::filter(scenario == "history") %>%
      dplyr::select(-scenario) %>%
      dplyr::rename(history_r_rate = r_rate) %>%
      dplyr::left_join(recovery_long %>% dplyr::filter(scenario != "history"),
                by = c("region", "subsector"),
                relationship = "many-to-many") %>%
      dplyr::left_join(
        dplyr::distinct(L2323.MFA_o_iron_steel_subsector_Mt_Y,
                 region, scenario, subsector, year),
        by = c("region", "scenario", "subsector"),
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(r_rate = dplyr::if_else(
        year <= base_year, history_r_rate,
        dplyr::if_else(year < improved_year,
                history_r_rate + (r_rate - history_r_rate) *
                  (year - base_year) / (improved_year - base_year), r_rate)
      )) %>%
      dplyr::select(region, scenario, subsector, year, r_rate) -> recovery_Yall

    recovery_Yall %>%
      left_join_error_no_match(L2323.MFA_o_iron_steel_subsector_Mt_Y,
                               by = c("region", "scenario", "subsector", "year")) %>%
      dplyr::mutate(value_pc_recovery = value,
             value_pc_reuse = value_pc_recovery * r_rate,
             value_pc_scrap = value_pc_recovery * (1 - r_rate)) %>%
      dplyr::select(region, scenario, subsector, year,
             value_pc_reuse, value_pc_scrap) -> post_consumer_material

    L2323.MFA_i_iron_steel_subsector_Mt_Y %>%
      left_join_error_no_match(post_consumer_material,
                               by = c("region", "scenario", "subsector", "year")) %>%
      dplyr::mutate(value_fbr_scrap = value * fabrication_loss * 0.8,
             value_scrap = value_pc_scrap + value_fbr_scrap) %>%
      dplyr::transmute(region, scenario, year, odym_type = "scrap", subsector,
                value = value_scrap) ->
      L2323.MFA_scrap_iron_steel_subsector_Mt_Y

    L2323.MFA_i_iron_steel_subsector_Mt_Y %>%
      left_join_error_no_match(dplyr::select(post_consumer_material,
                                      region, scenario, subsector, year,
                                      value_pc_reuse),
                               by = c("region", "scenario", "subsector", "year")) %>%
      dplyr::mutate(value = dplyr::if_else(value * (1 + fabrication_loss) - value_pc_reuse < 0,
                             value * (1 + fabrication_loss),
                             value * (1 + fabrication_loss) - value_pc_reuse),
             odym_type = "p") %>%
      dplyr::select(region, scenario, year, odym_type, subsector, value) ->
      L2323.MFA_p_iron_steel_subsector_Mt_Y

    # Aggregate end-use subsectors while retaining the standard long format.
    L2323.MFA_i_iron_steel_subsector_Mt_Y %>%
      dplyr::group_by(region, scenario, year) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(odym_type = "i") %>%
      dplyr::select(region, scenario, year, odym_type, value) ->
      L2323.MFA_i_iron_steel_subsector_total_Mt_Y

    L2323.MFA_s_iron_steel_subsector_Mt_Y %>%
      dplyr::group_by(region, scenario, year) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(odym_type = "s") %>%
      dplyr::select(region, scenario, year, odym_type, value) ->
      L2323.MFA_s_iron_steel_subsector_total_Mt_Y

    L2323.MFA_o_iron_steel_subsector_Mt_Y %>%
      dplyr::group_by(region, scenario, year) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(odym_type = "o") %>%
      dplyr::select(region, scenario, year, odym_type, value) ->
      L2323.MFA_o_iron_steel_subsector_total_Mt_Y

    L2323.MFA_p_iron_steel_subsector_Mt_Y %>%
      dplyr::group_by(region, scenario, year) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(odym_type = "p") %>%
      dplyr::select(region, scenario, year, odym_type, value) ->
      L2323.MFA_p_iron_steel_subsector_total_Mt_Y

    L2323.MFA_scrap_iron_steel_subsector_Mt_Y %>%
      dplyr::group_by(region, scenario, year) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(odym_type = "scrap") %>%
      dplyr::select(region, scenario, year, odym_type, value) ->
      L2323.MFA_scrap_iron_steel_subsector_total_Mt_Y

    output_precursors <- c(
      "common/GCAM_region_names",
      "material/future_prediction/A2323.MFA_SSP_subsector_stock_S_curve_parameter",
      "material/future_prediction/A2323.MFA_SSP_subsector_recovery_parameter",
      "L101.Pop_thous_SSP_R_Yfut",
      "L102.pcgdp_thous90USD_Scen_R_Y",
      "L1323.MFA_spc_iron_steel_subsector_tpc_Yh",
      "L1323.Pop_thous_R_Yh_longtail",
      "L1323.MFA_lifetime_subsector_lft_scale_Yall",
      "L1323.MFA_lifetime_subsector_lft_shape_Yall"
    )

    for(output_name in MODULE_OUTPUTS) {
      output_data <- get(output_name)
      output_data %>%
        add_title(gsub("_", " ", output_name)) %>%
        add_units("Mt") %>%
        add_comments("Mode-2 SSP iron and steel dynamic stock model output") %>%
        add_precursors(output_precursors) -> output_data
      assign(output_name, output_data)
    }

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
