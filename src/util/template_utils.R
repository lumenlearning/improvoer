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
# File: template_utils.R
# Author: Aaron Johnson
#
# Desc: A collection of variable and function definitions that facilitate
#   templating tasks.
################################################################################

# tab and nl are convenience variables to make templating code more readable
tab <- "\t"
nl <- "\n"

# line is a wrapper function for paste that places a newline character at the
# end of a string. Use arguments exactly as you would for R's paste function.
line <- function(..., sep=" ", collapse=NULL) {
  return(paste(paste(..., sep=sep, collapse=collapse), nl, sep=""))
}

# td returns a string of the form "<td>{x}</td>", where {x} is the input string.
# 
# Arguments
#   * x: a string to be wrapped with <td></td> HTML tags.
td <- function(x) {
  return(paste("<td>", x, "</td>", sep=""))
}

# format.decimal transforms a number into a string with the specified number of
# decimal places behind the decimal point. The number will be rounded at the
# last decimal place.
#
# Arguments
#   * x: a number (i.e., 5.372, not "5.372")
#   * decimals: the desired number of decimal places
# Example:
#   format.decimal(1.2345, 3) returns "1.235"
format.decimal <- function(x, decimals=0) {
  return(sprintf(sprintf("%%.%df", decimals), x))
}

# format.percent transforms a number into a string formatted as a percent. The
# number will be rounded at the last decimal place
#
# Arguments
#   * x: a number (i.e., 0.738, not "0.738")
#   * decimals: the desired number of decimal places
# Example:
#   format.percent(0.76382, 1) returns "76.4%"
format.percent <- function(x, decimals=0) {
  return(sprintf("%s%%", format.decimal(x=(x * 100), decimals=decimals)))
}

# build.row returns the HTML-formatted data for a single assessment, to be used
# in the table section of course instructional design report template.
#
# Arguments
#   * d: a list object containing the descriptive data for a single assignment.
build.row <- function(d) {
  paste(
    line("<tr>"),
    line(tab, td(g(d, "asmt.id"))),
    line(tab, td(g(d, "due.date"))),
    line(tab, td(g(d, "med.submit.time"))),
    line(tab, td(sprintf("<a href=\"%s\">%s</a>", g(d, "url"), g(d, "title")))),
    line(tab, td(format.decimal(g(d, "asmt.ease"), 2))),
    line(tab, td(format.percent(g(d, "median.percent")))),
    line(tab, td(format.decimal(g(d, "median.points")))),
    line(tab, td(g(d, "pts.poss"))),
    line(tab, td(format.decimal(g(d, "median.prep.minutes"), 2))),
    line(tab, td(format.decimal(g(d, "median.prep.pageviews"), 2))),
    line(tab, td(format.decimal(g(d, "median.minutes.per.page"), 2))),
    line(tab, td(format.percent(g(d, "perc.sub")))),
    line(tab, td(format.percent(g(d, "perc.late")))),
    line(tab, td(format.percent(g(d, "perc.zero")))),
    line(tab, td(format.percent(g(d, "perc.perfect")))),
    line(tab, td(format.decimal(g(d, "mean.subs"), 2))),
    line("</tr>"),
    sep="")
}
