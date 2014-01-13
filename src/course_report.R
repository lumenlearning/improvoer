################################################################################
# ImprovOER Source Code
# Copyright (C) 2013-2014 Lumen LLC. 
# 
# ImprovOER is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# ImprovOER is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with ImprovOER. If not, see <http://www.gnu.org/licenses/>.
################################################################################

################################################################################
# File: course_report.R
# Author: Aaron Johnson
#
# Desc: Functions required to create the instructional design report
################################################################################

# get.assessment.data returns a data.frame consisting of the data that will be
# used to populate the table portion of the report template.
#
# Arguments
#   * config: A configuration list object
#   * course.id: The ID of the course for which we want to obtain data
get.assessment.data <- function(config, course.id) {
  # Get raw API data
  assessments <- get.course.assessments.cache(
    config=config,
    course.id=course.id)
  submissions <- get.course.submissions.cache(
    config=config,
    course.id=course.id)
  
  
  #######################################################################
  ### Get an "assessment ease" parameter estimate for each assessment
  #######################################################################
  
  # Convert total_seconds_on_page and score_as_percent to percentiles
  submissions$total_seconds_on_page[submissions$total_seconds_on_page == 0] <- NA
  submissions$total_time_perc <- rank(x=submissions$total_seconds_on_page, ties.method="average", na.last="keep") / sum(!is.na(submissions$total_seconds_on_page))
  submissions$score_perc <- rank(x=submissions$score_as_percent, ties.method="average", na.last="keep") / sum(!is.na(submissions$score_as_percent))

  # Write out the input file for the parameter estimation program
  param.est.inputs <- na.omit(submissions[, c("user_id", "assignment_id", "total_time_perc", "score_perc")])
  input.file.path <- sprintf("%s/est-params-%s.csv", config[["params.input.dir"]], course.id)
  write.table(
    x=param.est.inputs,
    sep=",",
    col.names=FALSE,
    row.names=FALSE,
    file=input.file.path)
  
  # Run the parameter estimation program and capture the standard output
  params.txt <- paste(
    system(
      command=sprintf(
        fmt="java -cp \"%s:%s\" com.lumenlearning.apps.estimate_params.estimate_params %s %s %s %s",
        config[["params.clojure.path"]],
        config[["params.estimator.path"]],
        input.file.path,
        config[["params.eta"]],
        config[["params.max.iters"]],
        config[["params.precision"]]),
      intern=TRUE),
    collapse="\n")
  
  # Convert the text output to a data.frame object
  params.con <- textConnection(params.txt)
  params <- read.csv(file=params.con, header=FALSE)
  colnames(params) <- c("id", "param")
  close(params.con)
  
  # Associate each assessment with its assessment multiplier
  assessments <- merge(
    x=assessments,
    y=params,
    by="id",
    all.x=TRUE)



  
  ####################################################################
  ### Calculate various descriptive statistics for each assessment ###
  ####################################################################

  # Calculate median submission time for each assessment
  median.submissions <- aggregate(formula=submitted_at_num ~ assignment_id, data=submissions, FUN=median, na.rm=T)
  median.submissions$submitted_at_median <- as.POSIXct(x=median.submissions$submitted_at_num, origin=as.POSIXct("1970-01-01 00:00:00"))
  assessments <- merge(x=assessments, y=median.submissions[, c("assignment_id", "submitted_at_median")], by.x="id", by.y="assignment_id", all.x=TRUE)
  
  # Calculate median total_seconds_on_page
  # and median total_pageviews for each assessment
  tmp <- aggregate(
    x=submissions[, c("score", "score_as_percent", "total_seconds_on_page", "total_pageviews", "seconds_per_pageview")],
    by=list(submissions$assignment_id),
    FUN=median.na)
  colnames(tmp) <- c("assignment_id", "median_points", "median_percent", "median_seconds_on_page", "median_pageviews", "median_seconds_per_pageview")
  assessments <- merge(
    x=assessments,
    y=tmp,
    by.x="id",
    by.y="assignment_id",
    all.x=TRUE)
  
  # Calculate percent submitting, percent late, percent zeros, percent perfect
  tmp <- aggregate(
    x=submissions[, c("attempted", "late", "zero", "perfect")],
    by=list(submissions$assignment_id),
    FUN=percent.na)
  colnames(tmp) <- c("assignment_id", "percent_submitted", "percent_late", "percent_zero", "percent_perfect")
  assessments <- merge(
    x=assessments,
    y=tmp,
    by.x="id",
    by.y="assignment_id",
    all.x=TRUE)
  
  # Calculate the mean number of attempts for each assignment
  tmp <- aggregate(
    x=submissions[, c("attempt")],
    by=list(submissions$assignment_id),
    FUN=mean.na)
  colnames(tmp) <- c("assignment_id", "mean_attempts")
  assessments <- merge(
    x=assessments,
    y=tmp,
    by.x="id",
    by.y="assignment_id",
    all.x=TRUE)
  
  # Calculate median minutes on page
  assessments$median_minutes_on_page <- assessments$median_seconds_on_page / 60
  assessments$median_minutes_per_pageview <- assessments$median_seconds_per_pageview / 60
  

  # select various columns and rename them in preparation for use in the code
  # that generates the row HTML.
  asmt.data <- rnm.cols(
    sel.cols(
      assessments,
      c("id", "name", "html_url", "due_at", "points_possible", "submitted_at_median", "param", "median_minutes_on_page", "median_pageviews", "median_minutes_per_pageview", "median_points", "median_percent", "percent_submitted", "percent_late", "percent_zero", "percent_perfect", "mean_attempts")),
    c("asmt.id", "title", "url", "due.date", "pts.poss", "med.submit.time", "asmt.ease", "median.prep.minutes", "median.prep.pageviews", "median.minutes.per.page", "median.points", "median.percent", "perc.sub", "perc.late", "perc.zero", "perc.perfect", "mean.subs"))
  
  return(asmt.data)
}


# get.course.data returns a data.frame consisting of the data that will be
# used to populate the course portion of the report template.
#
# Arguments
#   * config: a configuration list object
#   * course.id: the ID of the course for which we want to obtain data
get.course.data <- function(config, course.id) {
  # get raw course data from the API
  courses <- get.course.info.cache(config=config, course.id=course.id)

  # convert timestamps to POSIXct values
  courses$start_at <- import.canvas.time(courses$start_at)
  courses$end_at <- import.canvas.time(courses$end_at)

  # select various columns and rename them for use with the template.
  crs.data <- rnm.cols(
    sel.cols(
      courses,
      c("id", "course_code", "name", "start_at", "end_at")),
    c("crs.id", "crs.code", "title", "st.dt", "end.dt"))

  # course URL
  crs.data$url <- sprintf("https://%s/courses/%s", config[["canvas.host"]], course.id)

  # student count
  crs.data$std.ct <- get.course.student.count(
    config=config,
    course.id=course.id)

  # teacher count
  crs.data$tch.ct <- get.course.teacher.count(
    config=config,
    course.id=course.id)

  # assessment count
  crs.data$asmt.ct <- nrow(
    get.course.assessments.cache(
      config=config,
      course.id=course.id))

  # submission count
  crs.data$sbms.ct <- nrow(
    get.course.submissions.cache(
      config=config,
      course.id=course.id))
  
  return(crs.data)
}


# get.template.data gives a list object containing the data necessary to
# populate the instructional design report.
#
# Arguments
#   * config: a configuration list object
#   * course.id: the ID of the course for which we want to create a report
get.template.data <- function(config, course.id) {
  # Get all the course and assessment data required for populating the report template.
  crs.data <- get.course.data(config=config, course.id=course.id)
  asmt.data <- get.assessment.data(config=config, course.id=course.id)
  
  # Create the named vector that we'll send to the gsub.multi function. Each
  # name is a search string, each value is its name's respective replacement.
  reps <- list(
    `{COURSE_NAME}`=crs.data$title[1],
    `{COURSE_ID}`=crs.data$crs.id[1],
    `{COURSE_URL}`=crs.data$url[1],
    `{COURSE_CODE}`=crs.data$crs.code[1],
    `{COURSE_START}`=crs.data$st.dt[1],
    `{COURSE_END}`=crs.data$end.dt[1],
    `{NUM_STUDENTS}`=crs.data$std.ct[1],
    `{NUM_TEACHERS}`=crs.data$tch.ct[1],
    `{ASMT_COUNT}`=crs.data$asmt.ct[1],
    `{SBMS_COUNT}`=crs.data$sbms.ct[1],

    # run each row in the asmt.data frame through build.row to generate report
    # HTML for that assessment. Collapse into one large HTML string to fill the
    # table in the instructional design report.
    `{TABLE_ROWS}`=
      paste(
        sapply(
          X=1:nrow(asmt.data),
          FUN=function(i) { build.row(as.list(asmt.data[i, ])) }),
        collapse=""))
  
  return(reps)
}

# get.rpt.dir.path generates a directory name for a course report given the
# application base directory, the course name, and the course ID.
#
# Arguments
#   * base.dir: the base directory, intended to contain multiple course
#     report directories.
#   * course.name: a string containing the name of the course
#   * course.id: the course ID
get.rpt.dir.path <- function(base.dir, course.name, course.id) {
  sprintf("%s/%s-%s", base.dir, make.path.friendly(course.name), as.character(course.id))
}
