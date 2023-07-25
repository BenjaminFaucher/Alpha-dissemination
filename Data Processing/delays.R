library(dplyr)
library(reshape2)
############################

## Matrices of delays #####

############################
#Import sequences of UK variants outside of UK

GISAID = read.csv('./Processing Data/GISAID.csv')
GISAID <- GISAID[GISAID$alpha == 1,] 
df = GISAID[GISAID$country != "United Kingdom",]
df$date.col = as.Date(df$date.col)
df$date.sub = as.Date(df$date.sub)
############################

#Compute the matrice M where M[i,j] = n(i,j)/N(i) where N(i) is the number of collected sequences between day i-1 and i+1
#and n(i,j) is the number of these sequences that are submitted day j.

df <- df %>%
  arrange(date.col, date.sub)

df$date.col <- as.Date(df$date.col, format = "%m-%d-%Y")

first_date <- as.Date("2020-01-01", format = "%Y-%m-%d")
df$col1 <- as.numeric(df$date.col - first_date) + 1
df$col2 <- df$col1 + 1
df$col3 <- df$col1 - 1

df$sub1 <- df$col1 + df$delai
df$sub2 <- df$col2 + df$delai
df$sub3 <- df$col3 + df$delai

col <- c(df$col1, df$col2, df$col3)
sub <- c(df$sub1, df$sub2, df$sub3)
dd <- data.frame(x = col, y = sub)

count_df <- dd %>%
  group_by(x, y) %>%
  summarise(count = n())

total_df <- dd %>%
  group_by(x) %>%
  summarise(total = n())

merged_df <- left_join(count_df, total_df, by = "x")

size <- 519
M <- matrix(0, nrow = size, ncol = size)

for (i in 1:nrow(merged_df)) {
  x <- merged_df$x[i]
  y <- merged_df$y[i]
  count <- merged_df$count[i]
  total <- merged_df$total[i]
  M[x, y] <- count / total
}
rownames(M) <- paste0("d.", 1:nrow(M))
colnames(M) <- paste0("d.", 1:ncol(M))

############################
#Store the dataframein a list for each country

nn <- c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium",
        "Belize", "Bermuda", "Bolivia", "Bonaire", "Bosnia and Herzegovina", "Botswana", "Brazil", "British Virgin Islands",
        "Brunei", "Bulgaria","Burkina Faso", "Cambodia", "Cameroon", "Canada", "Cayman Islands", "Central African Republic", "Chile", "China", "Chinese Taipei", "Colombia", "Comoros", "Costa Rica", "Cote d\'Ivoire", "Crimea", "Croatia",
        "Cuba", "Curacao", "Cyprus", "Czech Republic", "Democratic Republic of the Congo", "Denmark", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Estonia", "Eswatini", "Ethiopia", "Faroe Islands", "Finland", 
        "France", "French Guiana", "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guinea Bissau", "Guinea", "Guyana", "Haiti", "Honduras", "Hong Kong", "Hungary", 
        "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kosovo", "Kuwait", "Latvia", "Lebanon", "Lesotho", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", 
        "Malawi", "Malaysia", "Malta", "Martinique", "Mauritius", "Mayotte", "Mexico", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", "New Zealand", "Niger", "Nigeria", "Northern Mariana Islands",
        "Norway", "Oman", "Pakistan", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Republic of the Congo", "Reunion", "Romania", "Russian Federation", "Rwanda","Saint Lucia", "Saint Vincent and Grenadines",
        "Saudi Arabia", "Senegal", "Serbia", "Sierra Leone", "Singapore", "Sint Eustatius", "Sint Maarten", "Slovakia", "Slovenia", "Somalia", "South Africa", "South Korea", "Spain", "Sri Lanka", "Suriname", "Sweden", "Switzerland", "Thailand", "Togo", "Trinidad and Tobago",
        "Tunisia", "Turkey", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Uzbekistan", "Venezuela", "Vietnam", "Zambia", "Zimbabwe")


delaysafter <- list()

# Boucle pour créer les copies de dataframe avec les noms souhaités et les stocker dans la liste
for (i in seq_along(nn)) {
  delaysafter[[i]] <- data.frame(M)  # Créer une copie de dataframe (pour éviter les liens)
  names(delaysafter)[i] <- nn[i]       # Attribuer le nom du vecteur 'v' à chaque dataframe dans la liste
}
