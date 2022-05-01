# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(rvest)
library(httr)


# Variables ----------------------------------------------------------------------------------------
link <- "https://cardano-mainnet.blockfrost.io/api/v0/assets/"
policy_id <- "d4e087164acf8314f1203f0b0996f14908e2a199a296d065f14b8b09"
time_now <- as_datetime(now())
RAR <- readRDS("data/RAR.rds")


# Functions ----------------------------------------------------------------------------------------
extract_num <- function(x) as.numeric(gsub("[^0-9\\-]+","",as.character(x)))

loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# Extract information from jpg.store ---------------------------------------------------------------
# jpg.store/api/policy - all supported policies
# jpg.store/api/policy/[id]/listings - listings for a given policy
# jpg.store/api/policy/[id]/sales - sales for a given policy
JPG_list <- list()
p <- 1
while (TRUE) {
  api_link <- sprintf("https://server.jpgstoreapis.com/policy/%s/listings?page=%d", policy_id, p)
  X <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
  if (nrow(X) == 0) break
  JPG_list[[p]] <- X
  X[, page := p]
  p <- p + 1
}

JPG <- rbindlist(JPG_list)

JPG[, link   := paste0("https://www.jpg.store/asset/", asset_id)]
JPG[, asset  := display_name]
JPG[, price  := price_lovelace/10**6]
JPG[, sc     := "yes"]
JPG[, market := "jpg.store"]
JPG[, type   := "listing"]

setDT(JPG); setDT(RAR)
loj(JPG, RAR, "asset_id")



# JPG sales ----------------------------------------------------------------------------------------
JPGS_list <- lapply(1:13, function(p) {
  api_link <- sprintf("https://server.jpgstoreapis.com/policy/%s/sales?page=%d", policy_id, p)
  X <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
  return(X)
})

JPGS <- rbindlist(JPGS_list)

JPGS[, asset          := display_name]
JPGS[, price          := price_lovelace/10**6]
JPGS[, market         := "jpg.store"]
JPGS[, sold_at        := as_datetime(confirmed_at)]
JPGS[, sold_at_hours  := difftime(time_now, sold_at, units = "hours")]
JPGS[, sold_at_days   := difftime(time_now, sold_at, units = "days")]
JPGS[, type           := "listing"]

setDT(JPGS); setDT(RAR)
loj(JPGS, RAR, "asset_id")

JPGS <- JPGS[sold_at_hours <= 72]


# Merge markets data -------------------------------------------------------------------------------
# Listings
DT <- JPG

# Sales
DTS <- JPGS

# Add data collection timestamp
DT[, data_date := time_now]
DTS[, data_date := time_now]

# Fix bug with listing call returning many times the same asset (JPG)
DT <- DT[!duplicated(asset)]
DTS <- DTS[!duplicated(asset)]


# Price rank ---------------------------------------------------------------------------------------
DT[, price_rank := as.numeric(as.integer(factor(price)))]
DT[, price_rank := (price_rank-min(price_rank))/(max(price_rank)-min(price_rank))]
DT[, slvd       := factor(slvd, levels = LETTERS)]
DT[, cabin_size := factor(cabin_size, levels = c("Chateau", "Estate", "Cottage"))]

DTS[, price_rank := as.numeric(as.integer(factor(price)))]
DTS[, price_rank := (price_rank-min(price_rank))/(max(price_rank)-min(price_rank))]
DTS[, slvd       := factor(slvd, levels = LETTERS)]
DTS[, cabin_size := factor(cabin_size, levels = c("Chateau", "Estate", "Cottage"))]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTS, file = "data/DTS.rds")


# Database evolution -------------------------------------------------------------------------------
DTE <- copy(DT)
.file_name <- "data/DTE.rds"
if (file.exists(.file_name)) {
  cat("File data/DTE exists:", file.exists(.file_name), "\n")
  DTE_old <- readRDS(.file_name)
  DTE <- rbindlist(list(DTE, DTE_old), use.names = TRUE)
  DTE <- DTE[difftime(time_now, data_date, units = "hours") <= 24] # Only retain last 24 hours
}
saveRDS(DTE, file = .file_name)
