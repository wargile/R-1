# Cassandra R interface
# http://cran.r-project.org/web/packages/RCassandra/RCassandra.pdf

package.install("RCassandra")
library(RCassandra)

conn <- RC.connect(host="127.0.0.1", port=9160L)

RC.describe.keyspaces(conn)
RC.use(conn, "Weather_data", cache.def=T)
#RC.get(conn, "temperature", "1234ABCD", c.names="2013-04-03 07:01:00")
y <- RC.read.table(conn, "temperature_by_day")
y <- y[order(as.integer(row.names(y))), ]
y
RC.close(conn)
