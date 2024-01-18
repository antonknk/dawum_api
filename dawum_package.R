call_dawum_api <- function(cache_data = T, use_cached = F) {
  pacman::p_load("httr", "xml2", "readr", "here", "dplyr", "tidyr", "purrr")
  
  if (file.exists(here("dawum_api", "cache.txt"))){
    cache <- readr::read_file(here("dawum_api", "cache.txt"))
  } else {
    cache <- "Cache Datei fehlt"
  }
  
  message(paste0("Date Cache: \t", cache, "\n Date API: \t", readr::read_file("https://api.dawum.de/last_update.txt")))
  
  if (use_cached) {
    if (file.exists(here("dawum_api", "dawum.rds"))){
      df_long <- read_rds(here("dawum_api", "dawum.rds"))
    } else {
      message("No cached data.")
      break
    }
  } else {
    
    dawum_api <- GET("https://api.dawum.de")
    dawum_json <- content(dawum_api)
    
    as_df_tibble <- function(x) tibble(as.data.frame(x))
    
    df <- map_dfr(dawum_json$Surveys, as_df_tibble)
    
    df_parties <- map_dfr(dawum_json$Parties, as_df_tibble)
    df_parties$id <- names(dawum_json$Parties)
    
    df_parliaments <- map_dfr(dawum_json$Parliaments, as_df_tibble)
    df_parliaments$id <- names(dawum_json$Parliaments)
    
    df_taskers <- map_dfr(dawum_json$Taskers, as_df_tibble) %>%
      rename("tasker" = "Name")
    df_taskers$id <- names(dawum_json$Taskers)
    
    df_institutes <- map_dfr(dawum_json$Institutes, as_df_tibble) %>%
      rename("institute" = "Name")
    df_institutes$id <- names(dawum_json$Institutes)
    
    df_long <- df %>%
      pivot_longer(cols = starts_with("Results."), names_to = "partycode", names_prefix = "Results\\.", values_to = "value") %>%
      left_join(df_parties, by = c("partycode" = "id")) %>%
      left_join(df_parliaments, by = c("Parliament_ID" = "id"), suffix = c("_party", "_parliament")) %>%
      left_join(df_taskers, by = c("Tasker_ID" = "id")) %>%
      left_join(df_institutes, by = c("Institute_ID" = "id")) %>%
      mutate(field_end = as.Date(Survey_Period.Date_End),
             field_start = as.Date(Survey_Period.Date_Start),
             date = as.Date(Date),
             value = value/100, 
             Date = NULL,
             Survey_Period.Date_Start = NULL,
             Survey_Period.Date_End = NULL,
             Surveyed_Persons = as.numeric(Surveyed_Persons)) %>%
      rename("parliament" = "Shortcut_parliament",
             "n_surveyed" = "Surveyed_Persons",
             "party" = "Shortcut_party")  %>% 
      janitor::clean_names() %>% 
      filter(!is.na(value)) %>% 
      select(date, field_start, field_end, parliament, party, value, institute, tasker, n_surveyed, method_id)
    
    if (cache_data) {
      write_file(readr::read_file("https://api.dawum.de/last_update.txt"), here("dawum_api","cache.txt"))
      write_rds(df_long, here("dawum_api", "dawum.rds"))
    }
  }
  
  return(df_long)
}

select_parties <- function(df, parties = c("SPD", "CDU/CSU", "CDU", "CSU", "Grüne", "AfD", "FDP", "Linke", "Freie Wähler")) {
  
  tidyr::pivot_wider(filter(df, party %in% parties),
                     names_from = party, values_from = value) %>% 
    mutate(Sonstige = 1 - rowSums(.[9:ncol(.)], na.rm = T)) %>% 
    tidyr::pivot_longer(cols = -c(date:method_id), names_to = "party", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    select(date:parliament, party, value, institute:method_id) %>% 
    return()
}

subset_parliament <- function(df, parliament = "Bundestag") return(filter(df, parliament == parliament))
subset_latest <- function(df) filter(filter(df, date == max(date)), n_surveyed == max(n_surveyed))
subset_date_range <- function(df, min_date = "2017-01-18", max_date = Sys.Date()) arrange(filter(df, date >= as.Date(min_date) & date <= as.Date(max_date)), date) 

calc_cis <- function(df, alpha = 0.05) {
  mutate(df, std.err = sqrt((value * (1 - value)) / n_surveyed),
         conf.low = value - qnorm(1 - alpha / 2) * std.err,
         conf.high = value + qnorm(1 - alpha / 2) * std.err) %>% 
    select(date:value, std.err, conf.high, conf.low, everything()) %>% 
    return()
}

plot_single_poll <- function(df, colors) {
  require(ggplot2)
  
  df %>% 
    mutate(party = factor(ifelse(party %in% c("CDU", "CSU"), "CDU/CSU", party),
                          levels = c("SPD", "CDU/CSU", "Grüne", "AfD", "FDP", "Linke", "Freie Wähler", "Sonstige")),
           title = paste0(institute, "-Umfrage (", lubridate::day(field_start),".", lubridate::month(field_start),". - ", lubridate::day(field_end),".", lubridate::month(field_end),".",lubridate::year(field_end),").")) %>% 
    ggplot(aes(party, value, fill = party)) +
    scale_y_continuous("Anteil Sonntagsfrage", labels = scales::label_percent(1), limits = c(0,0.4)) +
    scale_x_discrete(NULL) +
    scale_fill_manual(values = colors) +
    geom_col() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "darkgrey", width = 0.5) +
    facet_wrap(~title)
}

plot_polling_average <- function(df, k = 5, colors) {
  
  df %>%
    mutate(party = factor(ifelse(party %in% c("CDU", "CSU"), "CDU/CSU", party),
                          levels = c("SPD", "CDU/CSU", "Grüne", "AfD", "FDP", "Linke", "Freie Wähler", "Sonstige"))) %>% 
    mutate(value = zoo::rollmeanr(value, k = k, fill = NA), .by = party) %>%
    ggplot(aes(date, value, color = party)) +
    scale_color_manual("Party", values = colors) +
    geom_hline(yintercept = 0.05, color = "black", linetype = "longdash") +
    geom_line() 
    
}



