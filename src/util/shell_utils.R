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
# File: shell_utils.R
# Author: Aaron Johnson
#
# Desc: A collection of function definitions that facilitate shell operations
################################################################################

# quote.string adds double quotes to the front and back of a string.
#
# Arguments
#   * s: the string to be quoted
#   * q: the quote character to use (" by default)
quote.string <- function(s, q="\"") {
  return(paste(q, s, q, sep=""))
}

# get.stdout runs a command in the shell and returns its standard output as
# a string.
#
# Arguments
#   * cmd: the command to run in the shell
get.stdout <- function(cmd) {
  return(
    paste(
      system(command=cmd, intern=TRUE, ignore.stderr=TRUE),
      collapse="\n"))
}
