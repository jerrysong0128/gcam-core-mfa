# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1323.iron_steel
#'
#' Sets up input, output, and IO coefficients for iron and steel and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1323.out_Mt_R_iron_steel_Yh}, \code{L1323.IO_GJkg_R_iron_steel_F_Yh}, \code{L1323.in_EJ_R_iron_steel_F_Y}, \code{L1323.in_EJ_R_indenergy_F_Yh}, \code{L1323.SubsectorInterp_iron_steel}. The corresponding file in the
#' original data system was \code{LA1323.iron_steel.R} (energy level1).
#' @details This chunk generates input, output, IO coefficients, and subsector shareweights for the iron and steel sector. It begins by downscaling Worrell regional data from 1994
#' to set up process emissions factors that are multiplied by country emissions from CDIAC to determine production. Limestone consumption is calculated from the same downscaling.
#' IEA fuelshares and heat and electricity are used to determine energy use by fuel. Energy inputs are then subtracted from industrial energy use and any resulting negative values
#' are dealt with by moving their accounting to the iron and steel sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019, Siddarth Durga April 2023
module_energy_L1323.iron_steel <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/steel_prod_process",
             FILE = "energy/steel_prod_process_Wuppertal",
             FILE = "energy/steel_intensity",
             FILE = "energy/WSA_direct_reduced_iron_2008_2019.csv",
             FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/A323.subsector_interp",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "LB1092.Tradebalance_iron_steel_Mt_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1323.out_Mt_R_iron_steel_Yh",
             "L1323.IO_GJkg_R_iron_steel_F_Yh",
             "L1323.in_EJ_R_iron_steel_F_Y",
             "L1323.in_EJ_R_indenergy_F_Yh",
             "L1323.SubsectorInterp_iron_steel"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
      share <- value <- iron_steel <- year <- value.y <- value.x <- iso <- unit_prod <-
      GCAM_region_ID <- fuel <- industry <- output <- energy_use <- scalar <- coefficient <-
      Unit <- technology <- '2008' <- '2009' <- '2010' <-'2011' <- '2012' <- '2013' <- '2014' <- '2015' <-
      '2016' <- '2017' <- '2018' <- '2019' <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    #All_steel <- get_data(all_data, "energy/steel_prod", strip_attributes = TRUE)
    All_steel <- get_data(all_data, "energy/steel_prod_process", strip_attributes = TRUE)
    All_steel_Wuppertal <- get_data(all_data, "energy/steel_prod_process_Wuppertal", strip_attributes = TRUE)
    DRI_stats <- get_data(all_data, "energy/WSA_direct_reduced_iron_2008_2019.csv", strip_attributes = TRUE)
    A323.subsector_interp <- get_data(all_data, "energy/A323.subsector_interp", strip_attributes = TRUE)
    steel_intensity <- get_data(all_data, "energy/steel_intensity", strip_attributes = TRUE)
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    LB1092.Tradebalance_iron_steel_Mt_R_Y <- get_data(all_data, "LB1092.Tradebalance_iron_steel_Mt_R_Y", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")

    # Keep the active GCAM 8.2 calibration horizon.  The WSA DRI series ends
    # before 2021, so carry its final observations forward rather than
    # reverting the model's historical-year definitions to BC3/GCAM 7.0.
    All_steel_years <- unique(All_steel$year)
    All_steel_max_year <- max(All_steel_years)
    if(All_steel_max_year < FINAL_HISTORICAL_YEAR) {
      All_steel_missing_years <- HISTORICAL_YEARS[HISTORICAL_YEARS > All_steel_max_year]
      All_steel <- All_steel %>%
        complete(nesting(country_name, unit_prod),
                 year = c(All_steel_years, All_steel_missing_years)) %>%
        group_by(country_name, unit_prod) %>%
        mutate(BLASTFUR = approx_fun(year, BLASTFUR, rule = 2),
               EAF = approx_fun(year, EAF, rule = 2)) %>%
        ungroup()
    }

    DRI_years <- suppressWarnings(as.integer(names(DRI_stats)))
    DRI_years <- DRI_years[!is.na(DRI_years)]
    DRI_max_year <- max(DRI_years)
    if(DRI_max_year < FINAL_HISTORICAL_YEAR) {
      DRI_missing_years <- HISTORICAL_YEARS[HISTORICAL_YEARS > DRI_max_year]
      DRI_stats <- copy_data_forward_wide(DRI_stats, DRI_max_year, DRI_missing_years)
    }

    #Estimate DRI (direct reduced iron) consumption from country-wise WSA DRI production, imports, and exports data
    DRI_stats %>%
      gather(year,value,-metric,-country_name)%>%
      spread(metric,value)%>%
      replace(is.na(.), 0) %>%
      mutate(year = as.numeric(year),
             DRI_consumption=`DRI production`- `DRI exports` + `DRI imports`,
             DRI_consumption=ifelse(DRI_consumption<0,0,DRI_consumption))%>%
      select(country_name,year,DRI_consumption)-> DRI_stats

    #Calculate EAF-Scrap and EAF-DRI based steel production using DRI consumption data
    All_steel %>%
      left_join(DRI_stats,by=c("country_name","year")) %>%
      replace(is.na(.), 0) %>%
      rename(`EAF with DRI`=DRI_consumption) %>%
      mutate(`EAF with scrap`=EAF-`EAF with DRI`,
             `EAF with DRI`=ifelse(`EAF with scrap`<=0,EAF,`EAF with DRI`),
             `EAF with scrap`=ifelse(`EAF with scrap`<0,0,`EAF with scrap`))%>%
      select(-EAF)%>%
      left_join(iso_GCAM_regID,by="country_name") -> All_steel

    # Replace base-year data with Wuppertal data
    All_steel_Wuppertal_R_tech <- All_steel_Wuppertal %>%
      tidyr::pivot_longer(cols = !c(subsector, technology), names_to = "country_name") %>%
      mutate(year = MODEL_FINAL_BASE_YEAR,
             country_name = case_when(
               country_name == "Russia" ~"Russian Federation",
               # since we are going to group by GCAM region, we can just assign all
               # EU data to one EU country
               # then split between EU-12 and EU-15 below
               country_name == "EU_27" ~ "Germany",
               country_name == "Iran" ~ "Iran, Islamic Republic of",
               country_name == "South_Africa" ~ "South Africa",
               country_name == "South_Korea" ~ "Korea, Republic of",
               country_name == "United_States" ~ "United States of America",
               TRUE ~ country_name
             )) %>%
      left_join(iso_GCAM_regID, by = "country_name")

    # By subsector, region
    All_steel_Wuppertal_R_subs <- All_steel_Wuppertal_R_tech %>%
      group_by(GCAM_region_ID, year, subsector) %>%
      summarise(value = sum(value)) %>%
      ungroup

    # Tech split in base year
    Wuppertal_R_tech_ratio <- All_steel_Wuppertal_R_tech %>%
      group_by(GCAM_region_ID, year, subsector, technology) %>%
      summarise(value = sum(value)) %>%
      group_by(GCAM_region_ID, year, subsector) %>%
      mutate(tech_ratio = value / sum(value)) %>%
      ungroup %>%
      select(-value) %>%
      # we set NAs to 1 only if in energy.CALIBRATED_STEEL_TECHS
      mutate(tech_ratio = if_else(is.na(tech_ratio) & technology %in% energy.CALIBRATED_STEEL_TECHS, 1,
             if_else(is.na(tech_ratio), 0, tech_ratio)))


    # ===================================================
    # 2. Perform computations
    # ===================================================
    # Recalculate steel production by technology across years to be consistent
    # with the iron and steel trade balance (consumption = production - exports + imports)
    # Change steel production to long format and aggregate to region level
    All_steel %>%
      #aggregate the production to regional level
      group_by(GCAM_region_ID, year) %>%
      summarise(BF_BOF =sum(BLASTFUR),EAF_scrap_fossil_NG_finish=sum(`EAF with scrap`),
                DRI_EAF_NG =sum(`EAF with DRI`)) %>%
      ungroup -> All_steel

    # Split Wuppertal Eu data into EU-15 and EU-12
    All_steel_EU_split <- All_steel %>%
      filter(year == MODEL_FINAL_BASE_YEAR,
             GCAM_region_ID %in% filter(GCAM_region_names, grepl("^EU", region))$GCAM_region_ID) %>%
      group_by(GCAM_region_ID) %>%
      summarise(total = sum(BF_BOF) + sum(EAF_scrap_fossil_NG_finish) + sum(DRI_EAF_NG)) %>%
      ungroup %>%
      mutate(EU_prop = total / sum(total))

    # Now add in EU split to Wuppertal data and prepare to replace default data
    All_steel_Wuppertal_R_EU_split <- All_steel_Wuppertal_R_subs %>%
      filter(GCAM_region_ID %in% All_steel_EU_split$GCAM_region_ID) %>%
      repeat_add_columns(All_steel_EU_split) %>%
      mutate(value = value * EU_prop) %>%
      select(GCAM_region_ID = GCAM_region_ID.y, year, subsector, value)

    All_steel_Wuppertal_R_EU  <- All_steel_Wuppertal_R_subs %>%
      filter(!GCAM_region_ID %in% All_steel_EU_split$GCAM_region_ID) %>%
      bind_rows(All_steel_Wuppertal_R_EU_split) %>%
      mutate(value = value * 1000) %>%
      tidyr::pivot_wider(names_from = subsector) %>%
      tidyr::replace_na(list(BF_BOF = 0,
                             DRI_EAF_coal = 0,
                             DRI_EAF_NG = 0,
                             EAF_scrap_fossil_NG_finish = 0))

    # Remove Wuppertal region and year from All_steel and then add in new data
    All_steel <- All_steel %>%
      anti_join(All_steel_Wuppertal_R_EU, by = c("GCAM_region_ID", "year")) %>%
      bind_rows(All_steel_Wuppertal_R_EU) %>%
      tidyr::replace_na(list(DRI_EAF_coal = 0))

      #Obtain the index of GCAM_regions and sub sectors that are calibrated to zero steel production in the base-year
      All_steel %>%
        filter(year==MODEL_FINAL_BASE_YEAR & (EAF_scrap_fossil_NG_finish ==0| BF_BOF ==0 | DRI_EAF_NG ==0)) %>%
        left_join(GCAM_region_names,by=c("GCAM_region_ID"))-> L1323.index

      #add a minimal steel production value (0.5% of the total) to technologies in the base-year where they are calibrated to zero
      # except DRI_EAF_coal
      All_steel %>%
        tidyr::pivot_longer(cols = c(BF_BOF, EAF_scrap_fossil_NG_finish, DRI_EAF_NG, DRI_EAF_coal), names_to = "technology") %>%
        group_by(GCAM_region_ID, year) %>%
        mutate(value = if_else(technology != "DRI_EAF_coal" & value == 0 & year == MODEL_FINAL_BASE_YEAR,
                               sum(value)* 0.005,
                               value),
               pct = value / sum(value)) %>%
        ungroup %>%
        #join the WSA total steel production data from LB1092.Tradebalance_iron_steel_Mt_R_Y
        left_join(GCAM_region_names,by="GCAM_region_ID") %>%
        left_join(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                    filter(metric == "production") %>%
                    rename(region=GCAM_region,production=value),by=c("region","year")) %>%
        #recalculate the steel production by technologies and regions
        #For example, steel produced from BF_BOF is equal to WSA total production multiplied by percent BF_BOF
        mutate(value = pct * (1/CONV_KT_MT) * production) %>%
        select(GCAM_region_ID , year, technology, value)%>%
        #convert unit from kt to mt
        mutate(value = value * CONV_KT_MT,
               subsector = technology)  -> L1323.out_Mt_R_iron_steel_Yh

   # L2323.SubsectorInterp_iron_steel: Subsector shareweight interpolation of iron and steel sector
      A323.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
        L1323.SubsectorInterp_iron_steel

      # Get steel energy use from IEA energy balances
      L1012.en_bal_EJ_R_Si_Fi_Yh %>%
        filter(grepl("steel", sector)) %>%
        group_by(GCAM_region_ID, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(sector = "iron and steel") ->
        en_steel

      # Map fuel in iron and steel sector
      en_steel %>%
        left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
        select(-fuel, fuel = industry) %>%
        na.omit() %>%
        group_by(GCAM_region_ID, year, sector, fuel) %>%
        summarise(value = sum(value)) %>%
        ungroup() ->
        en_steel

      # Calculate bottom-up energy consumption = production * intensity from literature
      L1323.out_Mt_R_iron_steel_Yh %>%
        rename(output = value) %>%
        left_join(steel_intensity  %>% select(-ratio),
                  by = c("subsector", "technology")) %>%
        mutate(value = value * CONV_GJ_EJ / CONV_T_MT,
               energy_use = output * value,
               unit = "EJ") ->
        Intensity_literature

      # Scaler: IEA's estimates of fuel consumption divided by bottom-up estimate of energy consumption
      Intensity_literature %>%
        group_by(GCAM_region_ID, year, fuel) %>%
        dplyr::summarise(energy_use = sum(energy_use)) %>%
        ungroup() %>%
        left_join(en_steel %>% select(GCAM_region_ID, year, fuel, value),by = c("GCAM_region_ID","fuel",  "year")) %>%
        mutate(value = replace_na(value,0), #replace NA IEA data with zero
               value= if_else(value == 0 & energy_use > 0, energy_use, value), #if bottom-up calculation is non-zero and IEA value is zero, then set IEA value = bottom-up value
               scalar = replace_na(value / energy_use, 1), #calculate scalar = IEA data/bottom-up data, if NA replace scaler = 1
               scalar = if_else(energy_use == 0 & value > 0, 1, scalar),  #if IEA data is non-zero, but bottom-up data is zero; set scaler = 1
               scalar = if_else(scalar>=6,1,scalar), #if IEA data is 6 times higher or lower than bottom-up calculation; then do not scale the results (i.e., scaler = 1)
               scalar = if_else(scalar<=0.16,1,scalar)) -> Scaler


      # Intensity scaled = Intensity from the literature times scaler.
      Intensity_literature %>%
        left_join(Scaler %>% select(GCAM_region_ID, year, fuel, scalar),by = c("GCAM_region_ID", "fuel", "year")) %>%
        mutate(coefficient = value * scalar) %>%
        select(GCAM_region_ID, year, subsector, technology, fuel, coefficient, Unit) ->
        Intensity_scaled

      IO_iron_steel_calculated <- steel_intensity %>%
        select(subsector, technology, fuel,ratio) %>%
        distinct() %>%
        repeat_add_columns(select(iso_GCAM_regID, GCAM_region_ID) %>% distinct(GCAM_region_ID)) %>%
        repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
        left_join(Intensity_scaled, by = c("GCAM_region_ID", "year", "subsector", "technology", "fuel")) %>%
        na.omit %>%
        mutate(coefficient=coefficient*ratio) %>%
        select(-ratio)

      IO_iron_steel_Wuppertal <- steel_intensity %>%
        mutate(value = value * CONV_GJ_EJ / CONV_T_MT) %>%
        select(subsector, technology, fuel, coefficient = value, Unit) %>%
        repeat_add_columns(select(iso_GCAM_regID, GCAM_region_ID) %>% distinct(GCAM_region_ID)) %>%
        repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS))

      # Decide whether to use IOs by scaling from IEA balance or just directly
      # use provided IOS
      if (energy.IRON_STEEL_CALCULATE_IO){
        IO_iron_steel <- IO_iron_steel_calculated
      } else {
        IO_iron_steel <- IO_iron_steel_Wuppertal

      }

     # Use IO to calculate energy input
    L1323.out_Mt_R_iron_steel_Yh %>%
      left_join(IO_iron_steel, by = c("subsector","technology","year","GCAM_region_ID")) %>%
      mutate(value = value * coefficient,
             supplysector = subsector) %>%
      select(GCAM_region_ID, supplysector, year, subsector, technology, fuel, "value") ->
      L1323.in_EJ_R_iron_steel_F_Y

	  IO_iron_steel %>%
	    mutate(supplysector = subsector) %>%
      select(GCAM_region_ID, year, supplysector, subsector, technology, fuel, coefficient) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

	 # FIRST ENERGY CALCULATION ---------------
	# Subtract iron and steel energy use from other industrial energy use
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value,0),
             value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh_tmp

    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      L1323.in_EJ_R_indenergy_F_Yh

    # FIRST IO ADJUSTMENT ---------------
    #Adjust negative energy use

    # Identify rows with negative energy use
    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      select(-sector) ->
      negative

    # revise IO coefficients
    if (energy.IRON_STEEL_CALCULATE_IO){
      # If using calcuated IOs, switch to zero
      L1323.IO_GJkg_R_iron_steel_F_Yh %>%
        left_join(negative,by = c("GCAM_region_ID", "year", "fuel")) %>%
        mutate(coefficient = if_else(replace_na(value, 0) < 0, 0, coefficient),value = NULL) ->
        L1323.IO_GJkg_R_iron_steel_F_Yh
    } else {
      # Otherwise switch to calculated IOs
      L1323.IO_GJkg_R_iron_steel_F_Yh %>%
        left_join(negative,by = c("GCAM_region_ID", "year", "fuel")) %>%
        left_join(IO_iron_steel_calculated,
                                 by = c("GCAM_region_ID", "year", "subsector", "technology", "fuel")) %>%
        mutate(coefficient = if_else(replace_na(value, 0) < 0 & !is.na(coefficient.y), coefficient.y, coefficient.x)) %>%
        select(-value, -coefficient.x, -coefficient.y) ->
        L1323.IO_GJkg_R_iron_steel_F_Yh
    }
    # SECOND ENERGY CALCULATION ---------------
    #Recalculate

    # Recalculate the input steel energy with revised IO coefficients
    L1323.out_Mt_R_iron_steel_Yh %>%
      left_join(L1323.IO_GJkg_R_iron_steel_F_Yh,
                by = c("subsector", "technology", "GCAM_region_ID", "year")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, year, subsector, technology, fuel, value) ->
      L1323.in_EJ_R_iron_steel_F_Y

    # Redo the iron and steel energy use and other industrial energy use subtraction
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      replace_na(list(value = 0)) %>%
      mutate(value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh_tmp

    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      L1323.in_EJ_R_indenergy_F_Yh

    # SECOND IO ADJUSTMENT ------------------
    #Adjust negative energy use
    # Identify rows with negative energy use
    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      select(-sector) ->
      negative

    # revise IO coefficients
      # set to zero, except in 2015, divide by 10
      L1323.IO_GJkg_R_iron_steel_F_Yh %>%
        left_join(negative,by = c("GCAM_region_ID", "year", "fuel")) %>%
        mutate(coefficient = if_else(replace_na(value, 0) < 0,
                                     0,
                                     coefficient)) %>%
        select(-value)->
        L1323.IO_GJkg_R_iron_steel_F_Yh

      # THIRD ENERGY CALCULATION ---------------
      #Recalculate

      # Recalculate the input steel energy with revised IO coefficients
      L1323.out_Mt_R_iron_steel_Yh %>%
        left_join(L1323.IO_GJkg_R_iron_steel_F_Yh,
                  by = c("subsector", "technology", "GCAM_region_ID", "year")) %>%
        mutate(value = value * coefficient) %>%
        select(GCAM_region_ID, year, subsector, technology, fuel, value) ->
        L1323.in_EJ_R_iron_steel_F_Y

      # Redo the iron and steel energy use and other industrial energy use subtraction
      L1322.in_EJ_R_indenergy_F_Yh %>%
        rename(raw = value) %>%
        left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                    group_by(GCAM_region_ID, year, fuel) %>%
                    summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
        replace_na(list(value = 0)) %>%
        mutate(value = raw - value , raw = NULL) ->
        L1323.in_EJ_R_indenergy_F_Yh

    # ===================================================
    # Produce outputs

    L1323.out_Mt_R_iron_steel_Yh %>%
      add_title("Historical steel outputs by region, fuel, and year") %>%
      add_units("Mt iron_steel") %>%
      add_comments("Outputs are collecting from World steel association and then aggregating to GCAM regions") %>%
      add_legacy_name("L1323.out_Mt_R_iron_steel_Yh") %>%
      add_precursors( "energy/steel_prod_process", "energy/WSA_direct_reduced_iron_2008_2019.csv","common/iso_GCAM_regID",
                      "LB1092.Tradebalance_iron_steel_Mt_R_Y","common/GCAM_region_names") ->
      L1323.out_Mt_R_iron_steel_Yh

    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      add_title("Input-output coefficients for steel production") %>%
      add_units("GJ/kg steel") %>%
      add_comments("IO coefficients for steel") %>%
      add_legacy_name("L1323.IO_GJkg_R_iron_steel_F_Yh") %>%
      add_precursors( "energy/steel_prod_process", "energy/steel_intensity", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    L1323.in_EJ_R_iron_steel_F_Y %>%
      add_title("Historical input energy use for the iron and steel sector") %>%
      add_units("Exajoules") %>%
      add_comments("Calculated by steel production and IO coefficients") %>%
      add_legacy_name("L1323.in_EJ_R_iron_steel_F_Y") %>%
      add_precursors("energy/steel_prod_process","energy/steel_intensity", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_iron_steel_F_Y

    L1323.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted iron and steel energy use from industrial energy use values in L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1323.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1322.in_EJ_R_indenergy_F_Yh", "energy/steel_prod_process", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_indenergy_F_Yh

    L1323.SubsectorInterp_iron_steel %>%
      add_title("Subsector shareweight interpolation of iron and steel sector") %>%
      add_units("NA") %>%
      add_comments("For iron and steel sector, the subsector shareweight interpolation function infromation from A323.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L1323.SubsectorInterp_iron_steel") %>%
      add_precursors("energy/A323.subsector_interp", "common/GCAM_region_names") ->
      L1323.SubsectorInterp_iron_steel



    return_data(L1323.SubsectorInterp_iron_steel,L1323.out_Mt_R_iron_steel_Yh, L1323.IO_GJkg_R_iron_steel_F_Yh, L1323.in_EJ_R_iron_steel_F_Y, L1323.in_EJ_R_indenergy_F_Yh)

  } else {
    stop("Unknown command")
  }
}
