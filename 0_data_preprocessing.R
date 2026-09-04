##
id = "Your ID"
pw = "UR PW"

## install.package
rm(list = ls()[ !(ls() %in% c("id", "pw")) ])
pkg = c("data.table", "tidyverse", "RPostgres", "DBI", 'glue', "frenchdata", 
        "nanoparquet", "dplyr", "readxl", 'writexl')

for(i in 1:length(pkg)){
  
  if(!require(pkg[i], character.only = T)){
    install.packages(pkg[i], dependencies = TRUE)
    require(pkg[i], character.only = T)
  }
}; rm(i, pkg)

## DB connection
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = id,
  password = pw
)

## Parameter Setting
table_name_crsp <- "crsp.msf_v2"
s_date <- "1959-01-01"
e_date <- "1978-12-31"

##--------------------------
## CRSP
##--------------------------

## Function define
get_data <- function(conn, s_date, e_date, table_name,
                     exchanges = c("N"), 
                     share_types = c("NS")) {
  
  tbl_identifier <- DBI::SQL(table_name)
  
  query <- glue::glue_sql("
    SELECT 
      a.permno, 
      a.Mthcaldt AS date,
      a.Mthprc,
      a.Mthret,
      a.Mthcap::numeric * 1000 AS Mthcap,
      a.Mthprevcap * 1000 AS Mthprevcap,
      a.shrout::numeric * 1000 AS shrout
    FROM {tbl_identifier} AS a
    WHERE a.Mthcaldt BETWEEN {s_date} AND {e_date}
      AND a.sharetype IN ({share_types*})
      AND a.securitytype = 'EQTY'
      AND a.securitysubtype = 'COM'
      AND a.usincflg = 'Y'
      AND a.issuertype IN ('ACOR', 'CORP')
      AND a.primaryexch IN ({exchanges*})
      AND a.conditionaltype IN ('RW', 'NW')
      AND a.tradingstatusflg = 'A'
  ", .con = conn)
  
  DBI::dbGetQuery(conn, query)
}

## call DB via function
crsp_data <- get_data(conn = wrds, 
                      s_date = s_date, 
                      e_date = e_date, 
                      table_name = table_name_crsp)

EWR = crsp_data %>% group_by(date) %>% summarise(EWR = mean(mthret, na.rm = T))
VWR <- crsp_data %>% 
  group_by(date) %>% 
  summarise(
    VWR = sum(mthret * mthprevcap, na.rm = TRUE) /
      sum(mthprevcap[!is.na(mthret)], na.rm = TRUE)
  )

res = left_join(EWR, VWR, by = 'date')
rm(crsp_data, EWR, VWR,wrds)

if(!dir.exists("data")){
  dir.create("Data")
}
writexl::write_xlsx(res, "Data/ND/EWR_VWR.xlsx")
writexl::write_xlsx(res, "Data/NDS/EWR_VWR.xlsx")