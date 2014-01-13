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
# File: api_utils.R
# Author: Aaron Johnson
#
# Desc: A collection of function definitions that facilitate working with the
#   Canvas API.
################################################################################

# get.api returns an object to be used for calling the Canvas API.
#
# Arguments
#   * python.cmd: The name of the Python 3 interpreter to use (e.g., "python",
#     "python3", "python3.2", etc.)
#   * auth.token.path: The path to the Canvas authentication token to use for
#     the API calls.
#   * canvas.host: A string giving the hostname for the server running the
#     instance of Canvas to which you'd like to connect
#     (e.g., "canvas.instructure.com")
#   * config: A configuration list object.  If this argument is given, the
#     list values supercede all other arguments given.
# External Dependencies:
#   * Python 3 interpreter installed
#   * canvaslms python package installed
#     (https://github.com/lumenlearning/python3-canvaslms-api)
#   * PYTHON_PATH environment variable set properly
get.api <- function(python.cmd, auth.token.path, canvas.host, config) {
  # Create an object (technically a list) with functions that will allow us
  #   communicate with the Canvas LMS via the RESTful API.
  
  # If a config list was passed in, then override all other arguments with
  # its values.
  if (!is.null(config)) {
    python.cmd <- config[["python.cmd"]]
    auth.token.path <- config[["auth.token.path"]]
    canvas.host <- config[["canvas.host"]]
  }

  api <- list()
  api$python.cmd <- python.cmd
  api$auth.token.path <- auth.token.path
  api$canvas.host <- canvas.host

  # Construct the API command prefix to be used when calling the
  # Python interpreter.
  api$cmd.prfx <- paste(
    api$python.cmd, "-m canvaslms.callapi.call_api_csv", 
    quote.string(api$auth.token.path),
    quote.string(api$canvas.host),
    sep=" ")

  # This function returns a string giving the full command to be called from
  # the shell.
  #
  # The cmd argument is the API call to make (minus "/api/V1"). For example:
  # "courses/123456/users?enrollment_type=student"
  api$construct.call <- function(cmd) {
    paste(api$cmd.prfx, quote.string(cmd), sep=" ")}

  # Call the API and return a data.frame with the results.
  api$call <- function(cmd) {
    read.csv.string(get.stdout(api$construct.call(cmd)))}
  
  return(api)
}

# import.canvas.time converts a vector of Canvas timestamp strings to an
# equivalent vector of POSIXct values.
#
# Arguments
#   * x: a vector of timestamp strings
import.canvas.time <- function(x) {
  return(as.POSIXct(x=x, format="%Y-%m-%dT%H:%M:%SZ"))
}

# import.canvas.true.false converts a vector of Canvas True/False string values
# to an equivalent vector of logical values.
import.canvas.true.false <- function(x) {
  sapply(X=x, FUN=function(x) {
    if (is.na(x)) {
      return(as.logical(NA))
    } else if (x == "True") {
      return(TRUE)
    } else if (x == "False") {
      return(FALSE)
    } else {
      # Catch-all for any other values that might arise
      return(as.logical(NA))
    }
  })  
}
