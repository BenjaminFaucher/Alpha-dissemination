# Computing first date of submission and collection date associated

countries = c("United Kingdom","Denmark","Australia","Gibraltar","Italy","Netherlands","Singapore","Hong Kong","Israel","Ireland","Japan",
              "France","Spain","Portugal","Norway","Finland","Sweden","South Korea","Canada","India","Germany","Switzerland","United States of America",
              "Brazil","New Zealand","Luxembourg","Pakistan","Belgium","Slovakia","Mexico","Sri Lanka","Romania","Ecuador","Turkey","Hungary","Peru","Iran",
              "Poland","Czech Republic","Austria","Latvia","United Arab Emirates","Trinidad and Tobago","Nigeria","Jordan","Malaysia","Bangladesh",            
              "Thailand","South Africa","Gambia","Ghana","Macedonia","Bosnia and Herzegovina","Chinese Taipei","Russian Federation","Georgia","Rwanda",
              "Lithuania","Croatia","Indonesia","China","Chile","Tunisia","Belarus","Cote d'Ivoire","Palestine","Colombia","Bahrain","Malta","Burkina Faso",           
              "Faroe Islands","Uganda","El Salvador","Madagascar","Papua New Guinea")

GISAID = read.csv('./Processing Data/GISAID.csv')
GISAID <- GISAID[GISAID$alpha == 1,] 
#GISAID$date.sub <- as.Date(GISAID$date.sub)
#GISAID <- GISAID[GISAID$country %in% countries,] %>% group_by(country) %>% summarise(date.submission = min(date.sub),date.collection = date.col[which(date.sub == min(date.sub))], date.collection.lo = date.col.min[which(date.sub == min(date.sub))], date.collection.up = date.col.max[which(date.sub == min(date.sub))], col.imputed = imputed[which(date.sub == min(date.sub))], date.1st.sub = min(date.sub))

idx <- GISAID %>% group_by(country) %>% slice(which.min(as.Date(date.sub)))
idx <- idx[idx$country %in% countries,]
first.VOC = data.frame(country=idx$country,date.collection=idx$date.col,date.submission = idx$date.sub,col.imputed = idx$imputed, date.1st.sub=idx$date.sub , date.collection.lo = idx$date.col.min, date.collection.up = idx$date.col.max)
df <- data.frame(country=c("El Salvador","Madagascar","Papua New Guinea"),date.collection=rep(NA,3),date.submission = rep(NA,3),col.imputed = rep(NA,3), date.1st.sub=rep(NA,3) , date.collection.lo = rep(NA,3), date.collection.up = rep(NA,3))
df <- rbind(df,first.VOC)
rownames(df) = df$country


df$date.collection <- as.Date(df$date.collection)
df$date.submission <- as.Date(df$date.submission)
df$date.1st.sub <- as.Date(df$date.1st.sub)
df$date.collection.lo <- as.Date(df$date.collection.lo)
df$date.collection.up <- as.Date(df$date.collection.up)
