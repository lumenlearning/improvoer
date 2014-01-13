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
# File:   text_utils.R
# Author: Aaron Johnson
#
# Desc:   A collection of function definitions that facilitate manipulation
#         of text.
################################################################################

# gsub.multi makes multiple gsub replacements on a single string value.
#
# Arguments
#   * text: the string to be modified
#   * replacements: a list object where each name is the search string and each
#     value is the replacement string.
#   * ...: extra arguments are passed on to R's gsub function.
gsub.multi <- function(text, replacements, ...) {
  # for each search pattern
  for (pattern in names(replacements)) {
    # get the corresponding replacement string
    rep <- replacements[[pattern]]
    # skip this one if either the pattern or the replacement is NA
    if (is.na(rep) || is.na(pattern)) {
      next
    }
    # make the replacement in the text
    text <- gsub(pattern=pattern, replacement=rep, x=text, ...)
  }

  return(text)
}

# rmv.pattern searches a string for a pattern and deletes it.
#
# Arguments
#   * x: the string to be modified
#   * pattern: the pattern to be deleted
#   * ...: extra arguments are passed to R's gsub function
rmv.pattern <- function(x, pattern, ...) {
  # Use gsub to remove the pattern from x
  return(gsub(pattern=pattern, replacement="", x=x, ...))
}

# left.trim greedily removes the specified characters from the front of a
# string. Removes whitespace by default.
#
# Arguments
#   * x: the string to be trimmed
#   * chars: a string containing the characters to look for and remove
#     (whitespace by default---space, tab, carriage return and newline)
#   * ...: extra arguments are eventually passed through to R's gsub function.
left.trim <- function(x, chars=" \t\r\n", ...) {
  return(rmv.pattern(x=x, pattern=sprintf("^[%s]+", chars), ...))
}

# right.trim greedily removes the specified characters from the back of
# a string. Removes whitespace by default.
#
# Arguments
#   * x: the string to be trimmed
#   * chars: a string containing the characters to look for and remove
#     (whitespace by default---space, tab, carriage return and newline)
#   * ...: extra arguments are eventually passed through to R's gsub function.
right.trim <- function(x, chars=" \t\r\n", ...) {
  return(rmv.pattern(x=x, pattern=sprintf("[%s]+$", chars), ...))
}

# trim greedily removes the specified characters from the front and back of
# a string. Removes whitespace by default.
#
# Arguments
#   * x: the string to be trimmed
#   * chars: a string containing the characters to look for and remove
#     (whitespace by default---space, tab, carriage return and newline)
#   * ...: extra arguments are eventually passed through to R's gsub function.
trim <- function(x, chars=" \t\r\n", ...) {
  return(right.trim(left.trim(x, chars), chars, ...))
}
