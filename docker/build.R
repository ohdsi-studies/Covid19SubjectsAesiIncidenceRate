# NOTE: before running this script be sure to set the 
# working directory to the location of this file

bashString <- "cp ../renv.lock ."
system(bashString)

bashstring <- "docker build -t covid19_subjects_aesi_incidence_rate:1.2.4 ."
system(bashstring)

