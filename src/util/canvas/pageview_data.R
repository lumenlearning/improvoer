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
# File: pageview_data.R
# Author: Aaron Johnson
#
# Desc: A collection of functions for working with Canvas pageview data.
################################################################################

library(memoise)

# get.course.pageviews(.nocache/.cache) returns the pageview data for a
# specific course. Prefer passing in a config object to sending
# individual arguments.
#
# Arguments
#   * jdbc.jar.path: The path to the JAR file containing the MySQL driver class
#   * host: the server running MySQL
#   * port: the port on which MySQL is running (usually 3306)
#   * schema: the name of the schema
#   * username: log in as username
#   * password: use password
#   * course.id: the ID of the course for which to get pageview data
#   * jdbc.driver.class: The name of the MySQL driver class
#     (usually "com.mysql.jdbc.Driver")
#   * mysql.identifier.quote: The character to use for delimiting names in
#     SQL queries (usually `).
#   * config: a list object containing all the necessary information to connect
#     to the database, etc.  If this argument is given, then it will supercede
#     the information in all other arguments.
get.course.pageviews.nocache <- function(
  jdbc.jar.path,
  host, port, schema, username, password,
  course.id,
  jdbc.driver.class="com.mysql.jdbc.Driver",
  mysql.identifier.quote="`",
  config) {
  
  # If a configuration list is passed in then unpack it and assign its values to
  # the other arguments. This will mask up any previously defined variables with
  # conflicting names.
  if (!is.null(config)) {
    jdbc.jar.path <- config[["jdbc.jar.path"]]
    host <- config[["pageview.db.host"]]
    port <- config[["pageview.db.port"]]
    schema <- config[["pageview.db.schema"]]
    username <- config[["pageview.db.username"]]
    password <- config[["pageview.db.password"]]
    jdbc.driver.class <- config[["jdbc.driver.class"]]
    mysql.identifier.quote <- config[["mysql.identifier.quote"]]
  }
  
  # Open a connection to the pageview database
  conn <- get.mysql.conn(
    driver=get.mysql.jdbc.drv(
      jdbc.jar.path=jdbc.jar.path, jdbc.driver.class=jdbc.driver.class,
      mysql.identifier.quote=mysql.identifier.quote),
    host=host, port=port, schema=schema, username=username, password=password)
  
  # Get the pageviews for course.id
  pageviews <- dbGetQuery(
    conn=conn,
    statement=sprintf(
      "SELECT * 
       FROM pageviews
       WHERE
         context_type = \"Course\"
         AND context_id = %s;", course.id))
  
  # Close the connection
  dbDisconnect(conn)

  if (nrow(pageviews) > 0) {
    # Estimate seconds on page for each pageview
    pageviews <- calculate.seconds.on.page(pageviews)
    # Convert each pageview URL to its canonical representation
    pageviews <- clean.urls(pageviews)
  }
  
  return(pageviews)
}
get.course.pageviews.cache <- memoize(f=get.course.pageviews.nocache)

# clean.urls converts each pageview URL to its canonical representation
#
# Arguments
#   * pageviews: a data.frame object obtained from
#     get.course.pageviews(.nocache/.cache)
clean.urls <- function(pageviews) {
  # Normalize URLs in pageviews$url to enable consistent matching and lookup
  pageviews$base_url <- gsub(pattern="(/|\\?module_item_id=[0-9]+)$", perl=T, replacement="", x=pageviews$url)
  pageviews$api_url <- gsub(pattern="/courses", perl=T, replacement="/api/v1/courses", x=pageviews$base_url)
  pageviews$api_url <- gsub(pattern="/wiki", perl=T, replacement="/pages", x=pageviews$api_url)
  
  return(pageviews)
}

# calculate.seconds.on.page estimates an amount of time spent on the page for
# each pageview.
#
# Arguments
#   * pageviews: a data.frame object obtained from
#     get.course.pageviews(.nocache/.cache)
calculate.seconds.on.page <- function(pageviews) {
  # Put the pageviews in order of session_id and timestamp
  pageviews <- pageviews[order(pageviews$session_id, pageviews$created_at_unix), ]
  
  # seconds_on_page(i) = timestamp(i+1) - timestamp(i).
  timestamp_i_1 <- c(pageviews$created_at_unix[2:nrow(pageviews)], as.numeric(NA))
  timestamp_i <- pageviews$created_at_unix
  pageviews$seconds_on_page <- timestamp_i_1 - timestamp_i
  
  # Handle the edge cases --- session-final pageviews have no "next timestamp"
  # from which to be subtracted.  Session-final pageviews therefore get NA
  # for seconds_on_page.
  session.counts <- aggregate(x=pageviews$session_id, by=list(pageviews$session_id), FUN=length)
  colnames(session.counts) <- c("session_id", "count")
  pageviews$seconds_on_page[cumsum(session.counts$count)] <- as.numeric(NA)
  
  # If it was greater than an hour, it probably isn't a valid
  # seconds_on_page measurement.
  pageviews$seconds_on_page[pageviews$seconds_on_page > 3600] <- NA
  
  return(pageviews)
}

test.calculate.seconds.on.page <- function() {
  test.data <- data.frame(
    session_id=c(4, 4, 4, 4, 1, 3, 3, 3, 2, 2),
    created_at_unix=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  
  test.data <- calculate.seconds.on.page(test.data)

  return(identical(test.data$seconds_on_page, c(1, 1, 1, NA, NA, 1, 1, NA, 1, NA)))
}
