#' @param df The input dataframe from the CSVs.

clean_conmed = function(df) {
  
  ################################
  # Manual review.
  if (F) {
    df = files$concomitant_medications
    dim(df)
    names(df)
    str(df)
    
    # Is there duplication at the patient level? Check # of records per patient id.
    dupes = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Review duplication by patient (grouped):
    table(dupes$pat_dupes)
    # Unique patients:
    cat("Unique patients:", nrow(dupes),
        "Duplication %",  round((1 - nrow(dupes) / nrow(df))*100, 2), "\n")
    # Append dupes to dataframe.
    dupes = df %>% group_by(patno) %>% mutate(pat_dupes=n()) %>% arrange(patno)
    # Review duplication by observation:
    table(dupes %>% ungroup() %>% select(pat_dupes))
    # Review records with duplication, looking at key administrative fields.
    subset(dupes, pat_dupes > 1, c(patno, pat_dupes, event_id, f_status,
                                   orig_entry, last_update, site_aprv))
    
    # Visually review full data that has duplication.
    View(subset(dupes, pat_dupes > 1))
  }
  
  
  
  
  ################################
  # Ensure only one observation per patient.
  
  # Keep the first (earliest) record for each patient.
  #df = df %>% group_by(patno) %>% arrange(rec_id) %>% filter(row_number() == 1)
  
  # This subsets to the first row for each patient, which may not be the best
  # row choice. However this ensures that we don't join multiple results for a
  # patient to our main data frame. Ideally we would figure out which row is
  # best for a given patient.
  # df = df %>% distinct(patno, .keep_all = T)
  
  ################################
  # Remove fields that we don't want to keep.
  df = subset(df, select = c(patno, dismed, ledd, totddose))
  
  # Make ledd score based on derived variables: 
  # "To find the LEDD for COMT inhibitors, first find the 
  # total dose of Levodopa only, and then multiply that value 
  # by either 0.5 or 0.33 as instructed."
  df$ledd[grep("x", df$ledd)] = df$totddose[grep("x", df$ledd)] * 0.33
  df$ledd[df$ledd == ""] = NA
  df$ledd = as.numeric(df$ledd)
  
  # Proportion of meds that are for PD
  df1 = df %>% group_by(patno) %>% summarise(prop_PDmed = mean(dismed)) 
  
  # Mean ledd score
  df2 = df %>% group_by(patno) %>% summarise(ledd = mean(ledd, na.rm = T))
  
  df = inner_join(df1, df2, by = "patno")
  df$ledd[is.nan(df$ledd)] = NA
  
  ################################
  # Return the cleaned result.
  df
}