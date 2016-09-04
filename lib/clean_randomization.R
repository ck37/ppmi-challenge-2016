#' Clean up the randomization table.
#' @param rand The randomization table as a dataframe.
clean_randomization = function(df) {

  # Manual review.
  if (F) {
    df = files$randomization_table
    dim(df)
    colnames(df)

    # Is there duplication at the patient level? Check # of records per patient id.
    dupes = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Unique patients:
    cat("Unique patients:", nrow(dupes),
        "Duplication %",  round((1 - nrow(dupes) / nrow(df))*100, 2), "\n")

    ############
    # More review

    # V = data has been verified.
    table(df$f_status, useNA="ifany")

    # Review enrollment date field.
    class(df$enrolldt)
    head(df$enrolldt)

    # There are no NAs:
    table(is.na(df$enrolldt), useNA="ifany")
    # And just one element with a blank value:
    table(df$enrolldt == "", useNA="ifany")

    # Is there duplication at the patient level? Check # of records per patient id.
    review_duplicates = df %>% group_by(patno) %>% summarize(pat_dupes=n())
    # Duplication: none
    table(review_duplicates$pat_dupes)
  }

  # Remove subjects with missing enrollment date.
  # This removes a single observation.
  df = subset(df, enrolldt != "")

  # Convert enroll date string into a true date.
  # Field only has month & year so we impute the day to the 15th of the month.
  # Otherwise R will not be be able to convert the string to a date.
  df$enrolldt = as.Date(paste0(df$enrolldt, "/15"), "%m/%Y/%d")
  head(df$enrolldt)
  # Everyone has a valid enrollment date:
  table(is.na(df$enrolldt))

  # Convert birth date string into a true date.
  df$birthdt = as.Date(paste0(df$birthdt, "/15"), "%m/%Y/%d")
  head(df$birthdt)
  # Everyone has a valid birthdate:
  table(is.na(df$birthdt))

  # Calculate an age variable based on enroll date - birth date.
  # This corresponds to age at enrollment.
  df$age = as.numeric(floor((df$enrolldt - df$birthdt) / 365.25))
  table(df$age)
  hist(df$age)
  summary(df$age)

  # Clean up gender.
  table(df$gender)
  # Convert gender to a binary indicator for female.
  df$gender = as.numeric(df$gender %in% c(0, 1))

  # TODO: clean up other fields from randomization_table

  # Remove fields that we don't want to keep.
  names(df)
  df = subset(df, select = -c(rec_id, event_id, pag_name, infodt, f_status,
                                  orig_entry, last_update, query, site_aprv))
  names(df)

  # Return our resulting dataframe.
  df
}