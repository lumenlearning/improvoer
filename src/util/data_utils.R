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
# File: data_utils.R
# Author: Aaron Johnson
#
# Desc: A collection of function definitions that facilitate working with data.
################################################################################

# read.csv.string attempts to interpret a string as CSV data, returning a
# data.frame object containing that data on success. If x is NULL, NA, an
# empty string (""), or only contains whitespace, then read.csv.string
# returns NA. Extra arguments get passed on to read.csv.
#
# Arguments
#   * x: a string containing CSV data
#   * ...: arguments to be passed on to to read.csv
read.csv.string <- function(x, ...){
  # Handle the empty case
  if (identical(NULL, x)){ return(NA) }
  if (identical(NA, x))  { return(NA) }
  if (identical("", x))  { return(NA) }
  if (length(grep(pattern="[^ \r\n\t]", x=x)) == 0) { return(NA) }
  
  # Read CSV from x
  x.text.con <- textConnection(x)
  dframe <- read.csv(x.text.con, ...)
  close(x.text.con)
  
  return(dframe)
}


# sum.na returns the sum of a vector of numbers after stripping out NA values.
# It is the same as calling sum with na.rm=TRUE, but if x is NULL or if x
# contains only NA values, then sum.na will return NA.
#
# Arguments
#   * x: A numeric vector, possibly with NA values.
sum.na <- function(x) {
  if (is.null(x)) {
    return(as.numeric(NA))
  }
  if (all(is.na(x))) {
    return(as.numeric(NA))
  }
  return(sum(x, na.rm=T))
}

# mean.na is analogous to sum.na, but for the mean function.
mean.na <- function(x) {
  if (is.null(x)) {
    return(as.numeric(NA))
  }
  if (all(is.na(x))) {
    return(as.numeric(NA))
  }
  return(mean(x, na.rm=T))
}

# median.na is analogous to sum.na, but for the median function.
median.na <- function(x) {
  if (is.null(x)) {
    return(as.numeric(NA))
  }
  if (all(is.na(x))) {
    return(as.numeric(NA))
  }
  return(median(x, na.rm=T))
}

# percent.na strips a logical (TRUE/FALSE) vector of NAs and returns the
# percentage of those values that are TRUE. If x is NULL, or if all values in x
# are NA, precent.na returns NA.
#
# Arguments
#   * x: a logical vector, possibly with NA values.
percent.na <- function(x) {
  if (is.null(x)) {
    return(as.numeric(NA))
  }
  if (all(is.na(x))) {
    return(as.numeric(NA))
  }
  not.na <- as.numeric(na.omit(x))
  return(sum(not.na) / length(not.na))
}
