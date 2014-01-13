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
# File: mysql_utils.R
# Author: Aaron Johnson
#
# Desc: A collection of functions for working with MySQL databases.
################################################################################

library(RJDBC)

# get.mysql.jdbc.drv returns a MySQL driver for JDBC database connectivity.
#
# Arguments
#   * jdbc.jar.path: The path to the JAR file containing the JDBC driver class
#   * jdbc.driver.class: The name of the JDBC driver class
#   * mysql.identifier: The character to use for delimiting identifiers in
#     SQL queries.
get.mysql.jdbc.drv <- function(jdbc.jar.path, jdbc.driver.class, mysql.identifier.quote) {
  return(
    JDBC(
      classPath=jdbc.jar.path, 
      driverClass=jdbc.driver.class,
      identifier.quote=mysql.identifier.quote))
}

# build.mysql.array creates an array of values for use in an SQL query.
#
# Arguments
#   * values: the vector of values to put into an SQL array
build.mysql.array <- function(values) {
  return(paste("('", paste(as.character(values), collapse="','"), "')", sep=""))
}

# get.mysql.conn returns a JDBC connection object for a MySQL database
#
# Arguments
#   * driver: the driver object for MySQL (obtained from get.mysql.jdbc.drv)
#   * host: the host name of the server running MySQL
#   * port: the port MySQL is running on (usuall 3306)
#   * schema: the name of the schema to connect to
#   * username: log in as username
#   * password: use password
get.mysql.conn <- function(driver, host, port, schema, username, password){
  conn.str <- sprintf("jdbc:mysql://%s:%s/%s", host, as.character(port), schema)
  return(dbConnect(driver, conn.str, username, password))
}
