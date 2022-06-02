library(here)
library(glue)

comms <- 49:84

# 48_att fails

for (i in comms) {
  edgelist_url <- glue("https://web.archive.org/web/20200706020922/http://moreno.ss.uci.edu/comm{i}.dat")
  node_data_url <- glue("https://web.archive.org/web/20200706020922/http://moreno.ss.uci.edu/comm{i}_att.dat")

  download.file(edgelist_url, here("data-raw", "addhealth", glue("edgelist{i}.dat")))
  download.file(node_data_url, here("data-raw", "addhealth", glue("node_data{i}.dat")))
}
