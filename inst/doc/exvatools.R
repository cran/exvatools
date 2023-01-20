## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval=FALSE, echo=TRUE-------------------------------------------
#  install.packages("exvatools")

## ----library------------------------------------------------------------------
library(exvatools)

## ----make_wio, eval=FALSE, echo=TRUE------------------------------------------
#  wio <- make_wio("icio2021", year = 2018,
#                  src_dir = "C:/Users/Username/Documents/R")

## ----make_wio_test, message=FALSE---------------------------------------------
wio <- make_wio("iciotest")

## ----make_custom_wio, eval=FALSE----------------------------------------------
#  wio <- make_custom_wio(df, g_names = c("C01", "C02", "C03"))

## ----summary_wio--------------------------------------------------------------
summary(wio)

## ----BY-----------------------------------------------------------------------
BY <- wio$B %*% wio$Y

## ----sum_WBY------------------------------------------------------------------
BY <- rsums(BY, "BY")
print(cbind(head(BY, 10), head(wio$X, 10)))

## ----WBY----------------------------------------------------------------------
# To calculate all value added induced by demand:
VBY <- dmult(wio$W, wio$B) %*% wio$Y
VBY

## ----meld---------------------------------------------------------------------
VBY <- meld(VBY)
VBY

## ----bkoffd-------------------------------------------------------------------
bkoffd(VBY)

## ----make_exvadec ESP---------------------------------------------------------
exvabm <- make_exvadec(wio, exporter = "ESP", method = "bm_src")

## ----bkdown_esp_srvwc_usa-----------------------------------------------------
get_exvadec_bkdown(exvabm, exporter = "ESP", 
                   sector = "SRVWC", importer = "USA")

## ----make_exvadec_wwz---------------------------------------------------------
exvawwz <- make_exvadec(wio, exporter = "all", method = "wwz",
                        output = "terms", quiet = TRUE)

## ----bkdown_usa_manuf_chn-----------------------------------------------------
get_exvadec_bkdown(exvawwz, exporter = "USA", 
                sector = "MANUF", importer = "CHN")

## ----make_exvadir, results='hide'---------------------------------------------
exvadir <- make_exvadir(wio, exporter = "ESP", va_type = "FC", 
                        flow_type = "EXGR")

## ----summary exvadir----------------------------------------------------------
summary(exvadir)

## ----get_data_power-----------------------------------------------------------
get_data(exvadir, exporter = c("WLD", "EU27", "FRA",
                               "NONEU27", "USA"),
         sector = c("TOTAL", "GOODSWU", "SRVWC"),
         importer = c("WLD", "EU27", "NONEU27"))

## ----define_aukus-------------------------------------------------------------
LATAM <- c("ESP", "MEX")

## ----get_data_vax_latam-------------------------------------------------------
get_data(exvawwz, "DVA_INT", exporter = "LATAM", 
         sector = c("TOTAL", "MANUF", "SRVWC"), 
         importer = "USA", custom = TRUE)

## ----get_data_vax_latam_intrarreg---------------------------------------------
get_data(exvawwz, "EXGR", exporter = "NAFTA", 
         sector = c("TOTAL", "TOTALxSRVWC", "SRVWC"), 
         importer = c("WLD", "NAFTA", "WLDxNAFTA"), custom = TRUE)

## ----get_va_exgr--------------------------------------------------------------
get_va_exgr(wio,geo_orig = "USA", sec_orig = "SRVWC",
            geo_export = "ESP", sec_export = "MANUF")

## ----get_va_exgry-------------------------------------------------------------
get_va_exgry(wio, geo_orig = "USA", geo_export = "CHN",
             sec_export = "MANUF", geo_fd = "USA")

## ----get_va_fd----------------------------------------------------------------
get_va_fd(wio, geo_orig = "CHN", sec_orig = "TOTAL",
          geo_fd = "USA", sec_fd = "MANUF")

## ----make_exvadec_oecd_tiva, results='hide'-----------------------------------
exvativa <- make_exvadec(wio, exporter = "all", method = "oecd",
                         output = "tiva", quiet = TRUE)

## ----tiva_esp-----------------------------------------------------------------
get_exvadec_bkdown(exvativa, exporter = "ESP")

## ----info_sec-----------------------------------------------------------------
info_sec("iciotest")

## ----info_geo-----------------------------------------------------------------
info_geo("iciotest")

## ----info_sec_2021------------------------------------------------------------
info_sec("icio2021")

## ----get_geo_codes_wiod2016---------------------------------------------------
get_geo_codes("EU27", wiotype = "wiod2016")

## ----get_sec_codes_icio2021---------------------------------------------------
get_sec_codes("BIZSV", wiotype = "icio2021")

