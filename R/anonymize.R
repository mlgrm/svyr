anonymize <- function(df,
                      private.fields=NULL,
                      gps.fields=c(
                        "latitude",
                        "longitude",
                        "altitude"
                      )){
  df[private.fields] <- lapply(df[private.fields],function(x){
    rep("##--hidden--##",length(x))
  })
  df[gps.fields] <- lapply(df[gps.fields],jitter)
  df
}
