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
# File: functional_utils.R
# Author: Aaron Johnson
# 
# Desc: A collection of function definitions that facilitate functional
#   programming in R.  This is usually achieved by creating proper functions to
#   use in place of R's extensive syntactic sugar.
################################################################################

# sel.cols returns a data.frame consisting of the specified columns from the
# given data.frame object, in the order that they were specified. Can be used
# in place of R's [, column_list] syntax.
#
# Arguments
#   * df: the data.frame from which to select columns
#   * cols: a character vector (e.g., c("col_name1", "col_name2", "that_col"))
#     specifying the columns to select
sel.cols <- function(df, cols) {
  return(df[, cols])
}

# rnm.cols changes the names of the columns in a data.frame object. Can be used
# in place of the colnames function.
#
# Arguments
#   * df: the data.frame whose columns we wish to rename
#   * new.names: a character vector containing the new column names, in order
#     of the columns.
rnm.cols <- function(df, new.names) {
  colnames(df) <- new.names
  return(df)
}

# g is a functional way of retrieving a named value from an object such as a
# list.  It is a functional replacement of the coll[[x]] syntactic sugar.  If
# the requested named value does not exist in the collection, g returns a
# default value (by default, NA).
#
# Arguments
#   * coll: some collection object with named values, typically a list.
#   * x: the name of the value to retrieve.
#   * default: the default value to return if x is not found in coll.
g <- function(coll, x, default=NA) {
  val <- coll[[x]]
  if (is.null(val)) {
    val <- default
  }
  return (val)
}
