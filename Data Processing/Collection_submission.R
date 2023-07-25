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
GISAID <- GISAID[GISAID$country %in% countries,] %>% group_by(country) %>% summarise(date.submission = min(date.sub),date.collection = date.col[which(date.sub == min(date.sub))])
