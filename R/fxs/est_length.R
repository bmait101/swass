
# this is an updated version of wdnr.fmdb::estimate_lengths(), 
# but removes the expand counts and length bin to lengths parts
# which I have already done in separate steps


est_legnth <- 
  function(data, 
           lengthed_data = NULL, 
           grouping = c("county", "waterbody.name", 
                        "wbic", "survey.year", 
                        "survey.seq.no", "site.seq.no", 
                        "swims.station.id", "visit.fish.seq.no"), 
           add_yoy_grouping = TRUE, 
           min_sample_size = 30, 
           include_lake_class = FALSE, 
           ...
           ) 
  {
    group_chk <- grouping == c("county", "waterbody.name", "wbic", 
                               "survey.year", "survey.seq.no", "site.seq.no", 
                               "swims.station.id", "visit.fish.seq.no")
    
    if (add_yoy_grouping) {
      grouping <- c(grouping, "yoy")
      data <- data %>% mutate(yoy = case_when(yoy == "Y" ~ "Y", TRUE ~ "N"))
      
      if (!is.null(lengthed_data)) {
        lengthed_data <- lengthed_data %>% mutate(yoy = case_when(yoy == "Y" ~ "Y", TRUE ~ "N"))
        
      }
      
      min_sample_size <- min_sample_size/2
      
    }
    
    length_estimator <- function(nonlength_data, 
                                 lengthed_data, 
                                 grouping,
                                 min_samples = min_sample_size, 
                                 est_method = "effort"
                                 ) 
      {
      data_groups <- rlang::syms(grouping)
      data_colnames <- colnames(nonlength_data)
      length_counts <- lengthed_data %>% 
        mutate(rounded.length = round(length)) %>% 
        group_by(!!!data_groups, species, species.code) %>% 
        mutate(effort_n = n()) %>% 
        filter(effort_n >= min_samples) %>%
        ungroup()
      length_props <- length_counts %>% 
        group_by(!!!data_groups, species, species.code, rounded.length) %>% 
        summarize(prop = n()/unique(effort_n), .groups = "drop_last") %>% 
        nest() %>% 
        ungroup()
      estimated_lengths <- nonlength_data %>% 
        as_tibble() %>% 
        left_join(length_props, by = c(grouping, "species", "species.code")) %>% 
        mutate(
          length = map_dbl(data, 
                           function(x) {
                             if (is.null(x)) {return(NA)}
                             else {if (nrow(x) == 1) {length_out <- x$rounded.length}
                               else {length_jitter <- seq(-0.5, 0.4, by = 0.1)
                               length_out <- sample(x$rounded.length + sample(length_jitter, 1), size = 1, prob = x$prop)}
                               return(length_out)}
                             }
                           ), 
          length.est = ifelse(is.na(length), NA, est_method)) %>% 
        select(-data) %>% 
        relocate(length.est, .after = length.unit)
      return(estimated_lengths)}
    
    nfish <- sum(data$number.of.fish)
    check_fish_number <- function(out_data) {
      if (sum(out_data$number.of.fish) != nfish) {
        stop("Something went wrong. Please contact package maintainer.")
      }
    }

    length_fishraw <- data %>% filter(!is.na(length))
    nonlength_fishraw <- data %>% filter(is.na(length))
    
    if (!is.null(lengthed_data)) {
      expanded_length_data <- lengthed_data %>% 
        filter(!is.na(length))}
    if (!all(group_chk) || length(group_chk) == 0) {
      if (is.null(lengthed_data)) {
        stop("You must enter lengthed_data if grouping variables are altered") }
      else {est_lengths <- length_estimator(nonlength_fishraw, 
                                            expanded_length_data, 
                                            grouping = grouping, 
                                            est_method = paste(grouping, collapse = "*"))
        out <- length_fishraw %>% 
          mutate(length.est = "measured") %>% 
          relocate(length.est, .after = length.unit) %>% 
          rbind(est_lengths) %>% wdnr.fmdb::add_fmdb_class("fmdb_fishraw")
        check_fish_number(out)
        return(out)}}
    else {
      if (is.null(lengthed_data)) {
        effort_est_lengths <- nonlength_fishraw %>% length_estimator(length_fishraw, 
                                                                     grouping = grouping)
        eff_length_fishraw <- length_fishraw %>% dplyr::mutate(length.est = "measured") %>% 
          dplyr::relocate(length.est, .after = length.unit) %>% 
          rbind(effort_est_lengths, .)
        survey_nonlength_fishraw <- nonlength_fishraw %>% 
          dplyr::anti_join(eff_length_fishraw, by = c(grouping, 
                                                      "species"))
        survey_groups <- grouping[-grep("visit.fish.seq.no", 
                                        grouping)]
        survey_est_lengths <- survey_nonlength_fishraw %>% 
          length_estimator(length_fishraw, grouping = survey_groups, 
                           est_method = "survey") %>% rbind(eff_length_fishraw, 
                                                            .)
        out <- survey_est_lengths %>% dplyr::relocate(length.est, 
                                                      .after = length.unit) %>% wdnr.fmdb::add_fmdb_class("fmdb_fishraw")
        check_fish_number(out)
        return(out)
      }
      else {
        if (include_lake_class) {
          nonlength_fishraw <- add_lake_class(nonlength_fishraw)
          expanded_length_data <- add_lake_class(expanded_length_data)
          grouping <- c(grouping, "lake_class")
        }
        wb_year_groups <- grouping[-grep("survey.seq.no|visit.fish.seq.no", 
                                         grouping)]
        wb_year_est_lengths <- length_estimator(nonlength_fishraw, 
                                                expanded_length_data, grouping = wb_year_groups, 
                                                est_method = "waterbody*year") %>% rbind(length_fishraw %>% 
                                                                                           dplyr::mutate(length.est = "measured"), .)
        wb_groups <- wb_year_groups[-grep("survey.year", 
                                          wb_year_groups)]
        wb_est_lengths <- nonlength_fishraw %>% dplyr::anti_join(wb_year_est_lengths, 
                                                                 by = grouping) %>% length_estimator(expanded_length_data, 
                                                                                                     grouping = wb_groups, est_method = "waterbody") %>% 
          rbind(wb_year_est_lengths, .)
        county_groups <- wb_groups[-grep("wbic|waterbody.name", 
                                         wb_groups)]
        county_est_lengths <- nonlength_fishraw %>% dplyr::anti_join(wb_est_lengths, 
                                                                     by = grouping) %>% length_estimator(expanded_length_data, 
                                                                                                         grouping = county_groups, est_method = "county") %>% 
          rbind(wb_est_lengths, .)
        if (include_lake_class) {
          lakeclass_groups <- county_groups[-grep("county", 
                                                  county_groups)]
          lakeclass_est_lengths <- nonlength_fishraw %>% 
            dplyr::anti_join(county_est_lengths, by = grouping) %>% 
            length_estimator(expanded_length_data, grouping = lakeclass_groups, 
                             est_method = "lake_class") %>% rbind(county_est_lengths, 
                                                                  .)
        }
        state_groups <- NULL
        if (include_lake_class) {
          state_est_lengths <- nonlength_fishraw %>% dplyr::anti_join(lakeclass_est_lengths, 
                                                                      by = grouping) %>% length_estimator(expanded_length_data, 
                                                                                                          grouping = state_groups, est_method = "state") %>% 
            rbind(lakeclass_est_length, .)
        }
        else {
          state_est_lengths <- nonlength_fishraw %>% dplyr::anti_join(county_est_lengths, 
                                                                      by = grouping) %>% length_estimator(expanded_length_data, 
                                                                                                          grouping = state_groups, est_method = "state") %>% 
            rbind(county_est_lengths, .)
        }
        out <- state_est_lengths %>% dplyr::relocate(length.est, 
                                                     .after = length.unit) %>% wdnr.fmdb::add_fmdb_class("fmdb_fishraw")
        check_fish_number(out)
        return(out)
      }
    }
  }
