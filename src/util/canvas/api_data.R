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
# File: api_data.R
# Author: Aaron Johnson
#
# Desc: A collection of function definitions for getting data from the
#   Canvas API.
################################################################################

library(memoise)

global.debug <- TRUE

# get.course.info(.nocache/.cache) gets information for a course
#
# Arguments
#   * config: a onfiguration list object
#   * course.id: the course for which we want to obtain data
get.course.info.nocache <- function(config, course.id) {
  api <- get.api(config=config)
  api$call(sprintf("courses/%s", course.id))
}
get.course.info.cache <- memoize(f=get.course.info.nocache)

# get.course.assessments(.nocache/.cache) gets assessment data for a course.
#
# Arguments
#   * config: a onfiguration list object
#   * course.id: the course for which we want to obtain data
get.course.assessments.nocache <- function(config, course.id) {
  api <- get.api(config=config)
  
  assessments <- api$call(sprintf("courses/%s/assignments", course.id))
  assessments$automatic_peer_reviews <- import.canvas.true.false(assessments$automatic_peer_reviews)
  assessments$due_at <- import.canvas.time(assessments$due_at)
  assessments$due_at_num <- as.numeric(assessments$due_at)
  assessments$peer_reviews <- import.canvas.true.false(assessments$peer_reviews)
  assessments$muted <- import.canvas.true.false(assessments$muted)
  assessments$locked_for_user <- import.canvas.true.false(assessments$locked_for_user)
  
  return(assessments)
}
get.course.assessments.cache <- memoize(f=get.course.assessments.nocache)

# get.course.submissions(.nocache/.cache) gets submission data for a course.
#
# Arguments
#   * config: a onfiguration list object
#   * course.id: the course for which we want to obtain data
get.course.submissions.nocache <- function(config, course.id) {
  api <- get.api(config=config)
  
  # Get submissions data from Canvas API
  if (global.debug) {
    cat("get.course.submissions.nocache: Getting submissions data from API ...\n")
  }
  submissions <- api$call(sprintf("courses/%s/students/submissions?student_ids=all", course.id))
  
  # Import data into R format (T/F, timestamps)
  if (global.debug) { cat("get.course.submissions.nocache: Importing data ...\n") }
  submissions$grade_matches_current_submission <-
    import.canvas.true.false(submissions$grade_matches_current_submission)
  submissions$graded <- submissions$graded_at != "" && !is.na(submissions$graded_at)
  submissions$graded_at <- import.canvas.time(submissions$graded_at)
  submissions$graded_at_num <- as.numeric(submissions$graded_at)
  submissions$submitted_at <- import.canvas.time(submissions$submitted_at)
  submissions$submitted_at_num <- as.numeric(submissions$submitted_at)
  submissions$late <- import.canvas.true.false(submissions$late)
  
  # Normalize scores by percentage
  # We need points possible from the assessments in order to do this
  if (global.debug) { cat("get.course.submissions.nocache: Normalizing scores ...\n") }
  assessments <- get.course.assessments.cache(config=config, course.id=course.id)
  submissions <- merge(
    x=submissions,
    y=assessments[, c("id", "points_possible", "due_at", "due_at_num")],
    by.x="assignment_id",
    by.y="id",
    all.x=TRUE)
  submissions$score_as_percent <- submissions$score / submissions$points_possible
  submissions$score_as_percent[!is.finite(submissions$score_as_percent)] <- NA
  submissions$zero <- submissions$score_as_percent == 0
  submissions$perfect <- submissions$score_as_percent >= 1.0
  
  # Determine if this student even attempted the assessment
  if (global.debug) { cat("get.course.submissions.nocache: Determining attempted status ...\n") }
  submissions$attempted <- (function() {
    n <- nrow(submissions)
    attempted <- rep(FALSE, n)
    for (i in 1:n) {
      if (submissions$submission_type[i] %in% c("[\"none\"]", "")) {
        if (submissions$graded[i]) {
          if (!is.na(submissions$score[i])) {
            if (submissions$score[i] > 0) {
              attempted[i] <- TRUE
            }
          }
        } else {
          attempted[i] <- NA
        }
      } else {
        if (!is.na(submissions$attempt[i]) & submissions$attempt[i] > 0) {
          attempted[i] <- TRUE
        }
      }
    }
    
    return(attempted)
  })()
  
  ### Get the time on page and pageview count for each submission
  if (global.debug) { cat("get.course.submissions.nocache: Retrieving pageviews ...\n") }
  pageviews <- get.course.pageviews.cache(config=config, course.id=course.id)
  
  # If they didn't submit it, use the due date of the assignment instead.
  submissions$submitted_at_num[is.na(submissions$submitted_at_num)] <- submissions$due_at_num[is.na(submissions$submitted_at_num)]
  
  # For efficiency of the algorithm, sort by user_id and submitted_at_num
  submissions <- submissions[order(submissions$user_id, submissions$submitted_at_num), ]
  
  # For each submission, find out total time spent on page and total pageviews
  # between the previous submission and this one.
  if (global.debug) { cat("get.course.submissions.nocache: Calculating total seconds on page ...\n") }
  submissions$total_seconds_on_page <- as.numeric(NA)
  submissions$total_pageviews <- as.numeric(NA)
  for (u in unique(submissions$user_id)) {
    u.rows <- which(submissions$user_id == u)
    u.pv <- subset(pageviews, user_id == u)
    if (nrow(u.pv) > 0){
      # Handle the first submission, which doesn't have a previous submission boundary.
      applicable.views <- which(u.pv$created_at_unix <= submissions$submitted_at_num[u.rows[1]])
      submissions$total_seconds_on_page[u.rows[1]] <- sum.na(u.pv$seconds_on_page[applicable.views])
      submissions$total_pageviews[u.rows[1]] <- length(applicable.views)
      
      # Handle remaining submissions, which do have a previous submission boundary.
      for (i in 2:length(u.rows)) {
        applicable.views <- which(u.pv$created_at_unix <= submissions$submitted_at_num[u.rows[i]] & u.pv$created_at_unix > submissions$submitted_at_num[u.rows[i-1]])
        submissions$total_seconds_on_page[u.rows[i]] <- sum.na(u.pv$seconds_on_page[applicable.views])
        submissions$total_pageviews[u.rows[i]] <- length(applicable.views)
      }
    }
  }
  
  submissions$seconds_per_pageview <- submissions$total_seconds_on_page / submissions$total_pageviews
  
  return(submissions)
}
get.course.submissions.cache <- memoize(f=get.course.submissions.nocache)

# get.course.enrollments(.nocache/.cache) gets enrollment data for a course.
#
# Arguments
#   * config: a onfiguration list object
#   * course.id: the course for which we want to obtain data
get.course.enrollments.nocache <- function(config, course.id) {
  api <- get.api(config=config)
  return(api$call(sprintf("courses/%s/enrollments", course.id)))
}
get.course.enrollments.cache <- memoize(f=get.course.enrollments.nocache)

# get.course.student.count returns the number of students enrolled in the course
#
# Arguments
#   * config: a onfiguration list object
#   * course.id: the course for which we want to obtain data
get.course.student.count <- function(config, course.id) {
  enrollments <- get.course.enrollments.cache(config=config, course.id=course.id)
  return(nrow(enrollments[enrollments$type == "StudentEnrollment", ]))
}

# get.course.teacher.count returns the number of teachers enrolled in the course
#
# Arguments
#   * config: a onfiguration list object
#   * course.id: the course for which we want to obtain data
get.course.teacher.count <- function(config, course.id) {
  enrollments <- get.course.enrollments.cache(config=config, course.id=course.id)
  return(nrow(enrollments[enrollments$type == "TeacherEnrollment", ]))
}
