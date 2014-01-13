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
# File: io_utils.R
# Author: Aaron Johnson
#
# Desc: A collection of function definitions that facilitate various
#   I/O operations.
################################################################################

# rd.txt is a convenience function that reads the file located at path and
# returns its contents as a string.
#
# Arguments
#   * path: the path of the file to read
rd.txt <- function(path) {
  return(readChar(path, file.info(path)$size))
}

# make.path.friendly makes a string "path-friendly" by replacing with a single
# underscore (_) each sequence of characters not belonging to the following
# character classes:
#   * a-z (lowercase)
#   * A-Z (uppercase)
#   * 0-9 (digits)
# String-inital and string-final underscores are removed. This function is
# useful for making user input strings into suitable path names.
# 
# Arguments
#   * x: the string to make friendly
make.path.friendly <- function(x) {
  return(
    trim(
      x=gsub(
        pattern="[^a-zA-Z0-9]+",
        replacement="_",
        x=trim(x),
        perl=TRUE),
      chars="_"))
}
