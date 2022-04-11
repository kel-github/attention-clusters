#### functions to analyse the behavioural data from the sequence
#### comarisons as the 1st component of the STRIAVISE WP1
#### written by K. Garner, 2020 +

#---------------------------------------------------------------------------------------------------------------------------
# data reading/wrangling 
#---------------------------------------------------------------------------------------------------------------------------
get_fnames <- function(i, j, fn, ses_type){
  # get the filenames for given subject i, and sessions in j (can be plural)
  # key args
  # -- i = subject number
  # -- j = session number(s)
  # -- fn = file name pattern (string)
  if (i < 10){
    sub_str = "sub-00%d"
  } else if (i > 9 & i < 100) {
    sub_str = "sub-0%d"
  } else {
    sub_str = "sub-%d"
  }
  fn <- paste(sub_str, fn, sep="")
  get_session_strings <- function(x) dir(sprintf(paste(data_path, sub_str, "ses-0%d", ses_type, sep = "/"), i, x), pattern=sprintf(fn, i, x), full.names = TRUE)
  do.call(cbind, lapply(j, get_session_strings))
}


get_dat <- function(subjects, sessions, data_path, ses_type, fn, sep = "\t"){
  # this function takes the filenames produced by get_fnames, and 
  # reads in the data from each, outputting a longform dataframe
  # key args
  # -- subjects: list of subjects from which you wish to get data
  # -- sessions: session numbers, should be one list, same across all subjects
  # -- ses_type = "behav" or "func"
  # -- fn = filename pattern, to be linked to subject str
  # -- sep = separator pattern in the data file
  files <- do.call(rbind, lapply(subjects, get_fnames, j=sessions, fn=fn, ses_type=ses_type))
  rownames(files) <- subjects
  colnames(files) <- sessions
  resplog <- function(i, j) {
    tmp = read.table(files[as.character(i),as.character(j)], sep = sep, header = TRUE)
    tmp$sub = i
    tmp$sess = j
    tmp
  }
  
  d <- do.call(rbind, lapply(subjects, function(i) do.call(rbind, lapply(sessions, function (j) resplog(i, j)))))
  d
}


get_participant_data <- function(subjects, sessions, data_path, ses_type = "beh") {
  # this function loads each participant's data
  # and concatenates into a longform dataset
  # key args
  # -- subjects = list of subject numbers (integers)
  # -- sessions = list of session numbers (same across subjects)
  # -- data_path = path to data
  # -- ses_type = "behav" or "func"
  fn = "_ses-01_task-learnAttExp_events.tsv"
  d = do.call(rbind, lapply(subjects, get_dat, sessions=sessions, data_path=data_path, ses_type=ses_type, fn=fn))
  
  # get trials to getthe reward conditions 
  fn <- '_ses-0%d_task-learnAttExp_trls.csv'
  t = do.call(rbind, lapply(subjects, get_dat, sessions=sessions, data_path=data_path, ses_type=ses_type, fn=fn, sep=","))
  names(t)[names(t) == "trial_num"] = "t"

  d <- inner_join(d, t, by=c("sub", "sess", "t"))
  d <- allocate_conditions_on_d(d)
  d
}


allocate_conditions_on_d <- function(d){
  # allocate factors to the dataset d (output by either get_dat or get_fmri_dat)
  # ALLOCATE CUE CONDITIONS

  d$cert <- NA
  d$cert[ d$probability == 1 & d$position == 0 ] = ".8"
  d$cert[ d$probability == 1 & d$position == 1 ] = ".2"
  d$cert[ d$probability == 2 & d$position == 1 ] = ".8"
  d$cert[ d$probability == 2 & d$position == 0 ] = ".2"
  d$cert[ d$probability == 3 ] = ".5"
  d$cert <- as.factor(d$cert)
  
  d$reward_type <- as.factor(d$reward_type)
  levels(d$reward_type) <- c("htgt/hdst", "htgt/ldst", "ltgt/ldst", "ltgt/hdst")
  
  d
}

#---------------------------------------------------------------------------------------------------------------------------
# data cleaning
#---------------------------------------------------------------------------------------------------------------------------

clean_sub_data <- function(subject, data, sd_reject=2.5, RT_min=0.1){
  # this function takes the subject index (defined in the variable subject)
  # and all the raw data
  # it filters the data to get the individual subject's data, then trims to
  # get the correct RTs that are > .1 s, and are < 3 * sd from the median
  # for each certainty and reward condition
  # key args
  # -- subject = subject number
  # -- data = dataframe
  data %>% filter(sub == subject) %>% 
           filter(rt > RT_min) %>%
           filter(resp == 1) %>% 
           group_by(sess, cert, reward_type) %>%
           mutate(mu = median(rt), sigma = sd(rt), N = length(rt)) %>%
           filter(rt < (mu + (sd_reject*sigma))) %>%
           ungroup()
}

#---------------------------------------------------------------------------------------------------------------------------
# data computations
#---------------------------------------------------------------------------------------------------------------------------

sub.model <- function(data, csub){
  # use this function to model each participants correct RT data
  # function returns summary of the anova
  # key args
  # -- data: dataframe to model
  # -- csub: subject to model (integer)
  data = data %>% filter(sub == csub)
  mod <- with(data, lm( rt ~ cert*reward_type*t)) 
  Anova(mod)
} 