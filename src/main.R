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
# File: main.R
# Author: Aaron Johnson
#
# Desc: File containing the main function (create.crs.rpt) that creates a
#   course instructional design report.
################################################################################

# Load all other supporting functions
source("src/util/canvas/api_data.R")
source("src/util/canvas/api_utils.R")
source("src/util/canvas/pageview_data.R")
source("src/util/data_utils.R")
source("src/util/functional_utils.R")
source("src/util/io_utils.R")
source("src/util/mysql_utils.R")
source("src/util/shell_utils.R")
source("src/util/template_utils.R")
source("src/util/text_utils.R")

# Load course report generation functions
source("src/course_report.R")

# Load configuration file
source("config/config.R")

# create.crs.rpt generates an instructional design report for a given course
# and writes the HTML report out to a file.
#
# Arguments
#   * config: a configuration list object, created by the config.R script.
#   * course.name: a string containing the name of the course
#   * course.id: the ID of the course
create.crs.rpt <- function(config, course.name, course.id) {
  # Create directory for the report if it doesn't exist already
  rpt.dir <- get.rpt.dir.path(
    base.dir=config[["base.dir"]],
    course.name=course.name,
    course.id=course.id)
  dir.create(rpt.dir, recursive=TRUE)
  
  # Write out the report
  rpt.path <- sprintf("%s/###ID_Report-%s.html", rpt.dir, course.id)
  cat(
    file=rpt.path,
    gsub.multi(
      text=rd.txt(config[["template.path"]]),
      replacements=get.template.data(config, course.id),
      fixed=TRUE))
}
