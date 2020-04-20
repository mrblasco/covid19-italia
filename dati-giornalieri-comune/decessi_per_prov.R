library(reshape2)
library(dplyr)
count_na <- function(x, ...) mean(is.na(x))


ds <- readr::read_csv("comune_giorno.csv",na=c("","9999"))

# keep full names in a separate df
ds.names <- distinct(select(ds, starts_with("NOME"),COD_PROVCOM))

COL=c(rep(gray(.75),5),"red")

# function to extract and aggregate data per comune
extract_comune <- function(ds, provcom) {
  filter(ds, COD_PROVCOM ==provcom) %>%
  select(COD_PROVCOM, GE, CL_ETA, starts_with("TOT")) %>%
  mutate(month = substr(GE,1,2)) %>% 
  group_by(month) %>%
  select(-CL_ETA) %>% 
  summarize_if(is.integer,  list("Sum" = sum, "Miss"= count_na), na.rm = T) 
}

# plot top provinces
par(mfrow=c(3,3), oma = c(4,0,4,1), mar = c(4,4,3,1))
top_prov_com <- names(tail(sort(xtabs(~COD_PROVCOM,data=ds)),9))
for(p in top_prov_com) {
  by_ge <- extract_comune(ds, p)
  matplot(x=by_ge$month, y=select(by_ge, ends_with("Sum")), type="l", col = COL, lwd=2, axes = F, ann=F)
  x.lab <- paste(c("Gen","Feb","Mar","Apr"), paste0(round(100 * (by_ge$TOTALE_20_Miss)),"%"), sep = "\n")
  axis(1, at=1:4, x.lab, padj = .5)
  y.ticks <- pretty(axTicks(2))
  mtext(side = 2, text = y.ticks, at = y.ticks, las = 2, cex = 0.5)
  grid(nx = NA, ny = NULL)
  title(main=filter(ds.names, COD_PROVCOM==p)$NOME_COMUNE
    , ylab="decessi", xlab = "(% dato mancante 2020)")
}
title(main = "Decessi mensili nell'anno 2020 (rosso) e negli anni 2015-2019 (grigio)", outer = T)
mtext(side = 1, line = 2, adj = 0.1, text = "Fonte: https://www.istat.it/it/archivio/6789", outer = T, cex = 0.5)


