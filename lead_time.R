## Forecast skill vs lead time plot

d2 <- data_date_model()

## get target date & site
date = "2021-06-26"
site = "HARV"
targ <- targets() %>% filter(siteID == site)
hor <- list()

## for each model
models = unique(d2$team)
for(m in seq_along(models)){

  ## organize data by horizon
  hor[[models[m]]] = d2 %>% filter(team == models[m],time == date, siteID == site) %>%
    pivot_wider(names_from="statistic",values_from="gcc_90") #%>%

}

yrng = c(min(sapply(hor,function(x){min(x$lower95)})),
        max(sapply(hor,function(x){max(x$upper95)})))
xrng = as.Date(range(sapply(hor,function(x){range(as.Date(x$forecast_start_time))})),origin = "1970-01-01")

plot(xrng,yrng,xlim=xrng,ylim=yrng,type='n',
     xlab="Lead Time",ylab="gcc90")
for(m in seq_along(models)){
  leadTime = as.Date(hor[[m]]$forecast_start_time)
  ecoforecastR::ciEnvelope(leadTime,
                           hor[[m]]$lower95,
                           hor[[m]]$upper95,
                           col=ecoforecastR::col.alpha(m,0.5))
  lines(x=leadTime,y=hor[[m]]$mean,col=m)
}
points(targ$time,targ$gcc_90)



