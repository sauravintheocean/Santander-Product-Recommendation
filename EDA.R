---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}
library(dplyr)
library(lubridate)
data <- read.csv("/Users/prashansachaudhary/Desktop/Prashansa-academic/DPA/Project/santander-product-recommendation/train_ver2.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",", na.strings = c("", "NA"))
print(data)
```
#### ggplot2 Theme Trick
```{r message=FALSE, warning=FALSE}
library(ggplot2)
my_theme <- theme_bw() +
  theme(axis.title=element_text(size=20),
        plot.title=element_text(size=32),
        axis.text =element_text(size=12))

my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=20),
        plot.title=element_text(size=32),
        axis.text =element_text(size=12))

```
 
```{r}
# Get the column Names
colnames(data)
```

# Check data types of each column
```{r}
str(data)
```

# Get the dimension of the dataframe
```{r}
dim(data)
```

# Converting dates to YYYY-MM-DD format
```{r}
library(lubridate)
data$fecha_dato <- ymd(data$fecha_dato)
data$fecha_alta <- ymd(data$fecha_alta)
data$ult_fec_cli_1t <- ymd(data$ult_fec_cli_1t)
```

# Converting antiguedad from string to num
```{r}
data$antiguedad <- as.numeric(as.character(data$antiguedad))
```

# Converting age from string to num
```{r}
data$age <- as.numeric(as.character(data$age))
```

# Converting antiguedad from string to num
```{r}
data$antiguedad <- as.numeric(as.character(data$antiguedad))
```

# Creating month column to analyze when products are bought the most
```{r}
data$month <- format(data$fecha_dato, "%m")
print(data)
```
# Check the null values for each column
```{r}
colSums(is.na(data))
# Following columns have null values:
# ind_empleado, pais_residencia, sexo, fecha_alta, ind_nuevo, indrel, ult_fec_cli_1t
# indrel_1mes, tiprel_1mes, indresi, indext, conyuemp, canal_entrada, indfall
# tipodom, cod_prov, nomprov, ind_actividad_cliente, renta, segmento, ind_nomina_ult1
# ind_nom_pens_ult1
```

########################### Checking NA in each column #####################


############### Missing values treatment for each feature   ################
#------------------------ind_empleado-------------------------#
```{r}
library(dplyr)
data %>% 
  count(ind_empleado)


# A 2492
# B 3566
# F 2523
# N 13610977
# S 17
# NA 27734

# Since most prominent category is N, we can either replace NA with N or create a new category.
# It makes more sense to create a new category. Hence labelling missing values as "UNKNOWN".

data$ind_empleado <- as.character(data$ind_empleado)
data$ind_empleado[is.na(data$ind_empleado)]       <- "UNKNOWN"
data$ind_empleado <- as.factor(data$ind_empleado)
print(data$ind_empleado)

```

#------------------------pais_residencia-------------------------#
```{r}
data %>% 
  count(pais_residencia)

# Checking count of NAs
sum(is.na(data$pais_residencia))

# There are 118 different categories, the most prominent being ES with a count 13553710.
# There are 27734 NA values, we can either impute NAs with most prominent category ES or create a new caregory.
# It makes more sense to create a new category. Hence labelling missing values as "UNKNOWN".

data$pais_residencia <- as.character(data$pais_residencia)
data$pais_residencia[is.na(data$pais_residencia)]       <- "UNKNOWN"
data$pais_residencia <- as.factor(data$pais_residencia)

```

```{r}
#------------------------sexo-------------------------#
data %>% 
  count(sexo)
# H 6195253
# V 7424252
# NA  27804

sum(is.na(data$sexo))

# Number of NAs is 27804. Also the count of H and V are almost same. Hence we cannot impute 
# NAs with any of the categories. It makes more sense here to create a new category.
# Hence, imputing missing values with a new category "UNKNOWN"

data$sexo <- as.character(data$sexo)
data$sexo[is.na(data$sexo)]       <- "UNKNOWN"
data$sexo <- as.factor(data$sexo)
print(data$sexo)
```

#------------------------age-------------------------#
```{r}
library(pastecs)
library(tidyr)
#library(data.table)
library(ggplot2)

data %>% 
  count(age)

age_summary <- stat.desc(data$age)
age_summary


ggplot(data=data,aes(x=age)) + 
  geom_bar(alpha=0.50,fill="lightblue",color="black") +
  ggtitle("Distribution of Age") 

# age distribution has two peaks one at 23(students) and another at 43(middle-age).
# There are customers with age < 18. Let's replace these outliers with mean of closest normal distribution.
data$age[(data$age < 18)]  <- mean(data$age[(data$age >= 18) & (data$age <=30)],na.rm=TRUE)

# There are customers with age > 100. Let's replace these outliers with mean of closest normal distribution.
data$age[(data$age > 100)] <- mean(data$age[(data$age >= 30) & (data$age <=100)],na.rm=TRUE)

# Let's impute the NA with median age value.
data$age[is.na(data$age)]  <- median(data$age,na.rm=TRUE)

data$age  <- round(data$age)
```


#------------------------fecha_alta-------------------------#
```{r}
data %>% 
  count(fecha_alta)

# Check count of missing values
sum(is.na(data$fecha_alta))

# There are 6257 different dates. 
# In this case it is difficult to impute the  NAs with a single date
# Hence, we choose to remove these 27734 records.
# We can drop this column as we have antiguedad

data$fecha_alta[is.na(data$fecha_alta)] <- median(data$fecha_alta,na.rm=TRUE)
```

#------------------------ind_nuevo-------------------------#
```{r}
data %>% 
  count(ind_nuevo)


# 0 12808368
# 1   811207
# NA   27734

# Filter all the records with NA in ind_nuevo and group by ncodpers.
df1 <- data %>% filter(is.na(ind_nuevo))
agg_tbl <- df1 %>% group_by(ncodpers) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
df2 <- agg_tbl %>% as.data.frame()
# Get the maximum count
max(df2$total_count)

# Since maximum number of records for a customer with missing ind_neuvo is 6
# we can impute NA with 1

data$ind_nuevo[is.na(data$ind_nuevo)] <- 1 

# Let's check whether NAs have been imputed.
sum(is.na(data$ind_nuevo))
print(data$ind_nuevo)
```


#------------------------antiguedad-------------------------#
```{r}
sum(is.na(data$antiguedad))
# There are 27734 missing values which is exactly same as ind_nuevo column.
# It might be possible that values for both columns are missing simultaneously.
# Let's verify it.

summary(data[is.na(data$antiguedad),]%>%select(ind_nuevo))
# Since ind_nuevo = 1 for all records where antiguedad is missing. It suggests that these are 
# new customers. So lets assign minimum seniority to these customers.

data$antiguedad[is.na(data$antiguedad)] <- min(data$antiguedad,na.rm=TRUE)
# Seniority cannot be negative. Hence round these negative values to 0.

data$antiguedad[data$antiguedad<0]      <- 0

# Let's check whether NAs have been imputed.
sum(is.na(data$antiguedad))
print(data)

```
#--------------------------indrel-------------------------#

```{r}

data %>% 
  count(indrel)

# 1     13594782
# 99       24793
# NA       27734

# Impute NAs with most common value.
data$indrel[is.na(data$indrel)] <- 1
print(data$indrel)

```

#---------------------ult_fec_cli_1t----------------------#
```{r}
data %>% 
  count(ult_fec_cli_1t)

# Percentage of missing values in this column.
sum(is.na(data$ult_fec_cli_1t))/nrow(data) * 100

# There are 223 different categories and the percentage of NA is 99.
# It is safe to drop this column.
data <- data %>% select(-ult_fec_cli_1t)
print(data)

```

#----------------------indrel_1mes------------------------#

```{r}
data %>% 
  count(indrel_1mes)

# Replacing 1.0 by 1, 2.0 by 2, 3.0 by 3, and 4.0 by 4 
data$indrel_1mes[data$indrel_1mes == '1.0'] <- '1'
data$indrel_1mes[data$indrel_1mes == '2.0'] <- '2'
data$indrel_1mes[data$indrel_1mes == '3.0'] <- '3'
data$indrel_1mes[data$indrel_1mes == '4.0'] <- '4'

# Since most of the observations belong to category 1 we can replace NA with 1
data$indrel_1mes[is.na(data$indrel_1mes)] <- "1"
print(data$indrel_1mes)
```

#----------------------tiprel_1mes------------------------#

```{r}
data %>% 
  count(tiprel_1mes)

# Since the number of records belonging class A and I are almost same. It makes more sense
# to create a new category. Hence, creating a new category "UNKNOWN".

data$tiprel_1mes <- as.character(data$tiprel_1mes)
data$tiprel_1mes[is.na(data$tiprel_1mes)]       <- "UNKNOWN"
data$tiprel_1mes <- as.factor(data$tiprel_1mes)
print(data$tiprel_1mes)
```

#------------------------indresi-------------------------#
```{r}
data %>% 
  count(indresi)

# Number of NAs are 27734. Let's create a new categoy "UNKNOWN" to impute these NAs.
data$indresi <- as.character(data$indresi)
data$indresi[is.na(data$indresi)]       <- "UNKNOWN"
data$indresi <- as.factor(data$indresi)
print(data$indresi)
```

#------------------------indext-------------------------#
```{r}
data %>% 
  count(indext)

# Number of NAs are 27734. Let's create a new categoy "UNKNOWN" to impute these NAs.
data$indext <- as.character(data$indext)
data$indext[is.na(data$indext)]       <- "UNKNOWN"
data$indext <- as.factor(data$indext)
print(data$indext)
```
#------------------------conyuemp-------------------------#
```{r}
data %>% 
  count(conyuemp)

# Percentage of missing values in this column.
sum(is.na(data$conyuemp))/nrow(data) * 100

# Since percentage of NA values are very high. We could either drop this column or create a new 
# category to impute NA values.

data$conyuemp <- as.character(data$conyuemp)
data$conyuemp[is.na(data$conyuemp)]       <- "UNKNOWN"
data$conyuemp <- as.factor(data$conyuemp)
print(data$conyuemp)
```
#------------------------canal_entrada-------------------------#
```{r}
data %>% 
  count(canal_entrada)

# There are 186126 NA values and 162 categories. It is difficult to assign NA to any one category.
# Hence, create a new category to impute NAs.
data$canal_entrada <- as.character(data$canal_entrada)
data$canal_entrada[is.na(data$canal_entrada)]       <- "UNKNOWN"
data$canal_entrada <- as.factor(data$canal_entrada)
data$canal_entrada
```
#------------------------indfall-------------------------#

```{r}
data %>% 
  count(indfall)

# There are 27734 missing values. We can impute NAs with N or create a new category.
# Here, we are choosing to impute with highest occuring category.

data$indfall[is.na(data$indfall)] <- "N"
print(data$indfall)
```
#------------------------tipodom-------------------------#
```{r}
data %>% 
  count(tipodom)
# Since this column contains just one value, there is no variance in the data.
# Hence it is safe to drop it.

data <- data %>% select(-tipodom)
```
#------------------------cod_prov-------------------------#
```{r}
# This column represents province code and the next column nomprov represents province name.
# Since one of these are enough. We can do away with this column and keep the next.
data <- data %>% select(-cod_prov)

```

#------------------------nomprov-------------------------#
```{r}
# Check the count of missing values
sum(is.na(data$nomprov))


data %>% 
  count(nomprov)

# There are 93591 NAs and there are 52 categories. In this case it is relevant to create a 
# new category. 

data$nomprov <- as.character(data$nomprov)
data$nomprov[is.na(data$nomprov)]       <- "UNKNOWN"
data$nomprov <- as.factor(data$nomprov)
```

#---------------------ind_actividad_cliente---------------#
```{r}
data %>% 
  count(ind_actividad_cliente)


# Impute NAs with 0 
data$ind_actividad_cliente[is.na(data$ind_actividad_cliente)] <- median(data$ind_actividad_cliente,na.rm=TRUE)
print(data$ind_actividad_cliente)
```

#--------------------------renta---------------------------#
```{r}
# Check the percentage of missing values
sum(is.na(data$renta))/nrow(data) * 100

min(data$renta, na.rm=T)
max(data$renta, na.rm=T)

# Since around 20 percent data is missing and the range of values are very high.
# It is not wise to impute NAs with a single value.
# Let's club renta with nomprov to the see the median income for each province.

df_renta_grouped_by_nomprov <- data %>% filter(!is.na(renta)) %>% group_by(nomprov) %>% summarise(median_income = median(renta)) %>% arrange(median_income) %>% mutate(city=factor(nomprov,levels=nomprov))
ggplot(data=df_renta_grouped_by_nomprov,aes(x=city, y=median_income)) + 
  geom_point(color="#00FF00") +
  guides(color="none") + 
  xlab("Province Name") +
  ylab("Median Income") +  
  theme(axis.text.x=element_blank(), axis.ticks = element_blank()) + 
  geom_text(aes(x=city,y=median_income,label=nomprov),angle=90,hjust=-.25) +
  theme(plot.background=element_rect(fill="#FFFFFF"),
        panel.background=element_rect(fill="#d3d3d3"),
        panel.grid =element_blank(),
        axis.title =element_text(color="#000000"),
        axis.text  =element_text(color="#000000"),
        plot.title =element_text(color="#000000",size=32)) +
  ylim(c(50000,200000)) +
  ggtitle("Distribution of Median Income by Province")


df_new <- data %>% select(nomprov) %>% merge(data %>% group_by(nomprov) %>% summarise(med_income=median(renta,na.rm=TRUE)),by="nomprov") %>% select(nomprov,med_income) %>% arrange(nomprov)

df_test <- merge(data %>% select(nomprov), data %>% group_by(nomprov) %>% summarise(med_income=median(renta,na.rm=TRUE)), by="nomprov") %>% select(nomprov,med_income) %>% arrange(nomprov)

data <- arrange(data,nomprov)
data$renta[is.na(data$renta)] <- df_test$med_income[is.na(data$renta)]
data$renta[is.na(data$renta)] <- median(data$renta,na.rm=TRUE)

data <- arrange(data,fecha_dato)



data %>% group_by(nomprov) %>% summarise(median_income = median(renta, na.rm=TRUE)) 
```

#------------------------segmento-------------------------#
```{r}
data %>% 
  count(segmento)


data$segmento <- as.character(data$segmento)
data$segmento[is.na(data$segmento)]       <- "UNKNOWN"
data$segmento <- as.factor(data$segmento)
print(data$segmento)
print(data)
```

#----------------------ind_nomina_ult1---------------------#
```{r}
data %>% 
  count(ind_nomina_ult1)

# Impute NA with 0
data$ind_nomina_ult1[is.na(data$ind_nomina_ult1)]       <- 0
print(data$ind_nomina_ult1)
```

#----------------------ind_nom_pens_ult1---------------------#
```{r}
data %>% 
  count(ind_nom_pens_ult1)

# Impute NA with 0
data$ind_nom_pens_ult1[is.na(data$ind_nom_pens_ult1)]       <- 0
print(data$ind_nom_pens_ult1)
```
####################### Engineering Target Variable ##################
Convert all the features to numeric dummy indicators
```{r}

str(data)

print(features)
data[is.na(data)] <- 0

#data$ind_empleado <- as.character(data$ind_empleado)
#data$pais_residencia <- as.character(data$pais_residencia)
#data$sexo <- as.character(data$sexo)
#data$indrel_1mes <- as.character(data$indrel_1mes)
#data$tiprel_1mes <- as.character(data$tiprel_1mes)
#data$indresi <- as.character(data$indresi)
#data$indext <- as.character(data$indext)
#data$conyuemp <- as.character(data$conyuemp)
#data$canal_entrada <- as.character(data$canal_entrada)
#data$indfall <- as.character(data$indfall)
#data$nomprov <- as.character(data$nomprov)
#data$segmento <- as.character(data$segmento)
#data$ind_nomina_ult1 <- as.numeric(data$ind_nomina_ult1)
#data$ind_nom_pens_ult1 <- as.numeric(data$ind_nom_pens_ult1)
#data$month <- as.numeric(data$month)

str(data)
```

```{r}
is.na(data)
colSums(is.na(data))
```

```{r}
char.cols <- names(data)[sapply(data,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(data[[name]]))
  cat('\n')
  }
```

```{r}
data$indfall[data$indfall==""]                 <- "N"
data$tiprel_1mes[data$tiprel_1mes==""]         <- "A"
data$indrel_1mes[data$indrel_1mes==""]         <- "1"
data$indrel_1mes[data$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers
data$indrel_1mes                             <- as.factor(as.integer(data$indrel_1mes))
data$pais_residencia[data$pais_residencia==""] <- "UNKNOWN"
data$sexo[data$sexo==""]                       <- "UNKNOWN"
data$ind_empleado[data$ind_empleado==""]       <- "UNKNOWN"
data$indext[data$indext==""]                   <- "UNKNOWN"
data$indresi[data$indresi==""]                 <- "UNKNOWN"
data$conyuemp[data$conyuemp==""]               <- "UNKNOWN"
data$segmento[data$segmento==""]               <- "UNKNOWN"
```

```{r}
features            <- grepl("ind_+.*ult.*",names(data))
data[,features]     <- lapply(data[,features],function(x)as.integer(round(x)))
data$total.services <- rowSums(data[,features],na.rm=TRUE)
print(data)

```
```{r}
print(data)
```


# To study trends in customers adding or removing services, I will create a label for each product and month that indicates whether a customer added, dropped or maintained that service in that billing cycle. I will do this by assigning a numeric id to each unique time stamp, and then matching each entry with the one from the previous month. The difference in the indicator value for each product then gives the desired value.
```{r}
data              <- data %>% arrange(fecha_dato)
data$month.id      <- as.numeric(factor((data$fecha_dato)))
data$month.next.id <- data$month.id + 1
```


# I’ll build a function that will convert differences month to month into a meaningful label. Each month, a customer can either maintain their current status with a particular product, add it, or drop it.
```{r}
status.change <- function(x){
  if ( length(x) == 1 ) { # if only one entry exists, I'll assume they are a new customer and therefore are adding services
    label = ifelse(x==1,"Added","Maintained")
  } else {
    diffs <- diff(x) # difference month-by-month
    diffs <- c(0,diffs) # first occurrence will be considered Maintained, which is a little lazy. A better way would be to check if the earliest date was the same as the earliest we have in the dataset and consider those separately. Entries with earliest dates later than that have joined and should be labeled as "Added"
    label <- rep("Maintained", length(x))
    label <- ifelse(diffs==1,"Added",
                    ifelse(diffs==-1,"Dropped",
                           "Maintained"))
  }
  label
}

```



# Now we can actually apply this function to each feature using lapply and ave
```{r}
data[,features] <- lapply(data[,features], function(x) return(ave(x,data$ncodpers, FUN=status.change)))
```

#I’m only interested in seeing what influences people adding or removing services, so I’ll trim away any instances of “Maintained”. Since big melting/casting operations can be slow, I’ll take the time to check for rows that should be completely removed, then melt the remainder and remove the others. 


```{r}
interesting <- rowSums(data[,features]!="Maintained")
data          <- data[interesting>0,]
data          <- data %>%
                gather(key=feature,
                value=status,
                ind_ahor_fin_ult1:ind_recibo_ult1)
data          <- filter(data,status!="Maintained")
head(data)
```

```{r}
print(data)
```


```{r}
str(data)
colSums(is.na(data))
write.csv(data,"/Users/prashansachaudhary/Desktop/Prashansa-academic/DPA/Project/santander-product-recommendation/cleandata.csv")
```

############################################# EDA ##################################################

#the first day of the week the customer signed a contract with the bank,Number of cutomers that became first holder by day of week
```{r}
library(lubridate)
library(ipred)

selectrow <- c("fecha_dato", "fecha_alta", "pais_residencia") # Rows that I want to analize
df1 <- data.table::fread("/Users/prashansachaudhary/Desktop/Prashansa-academic/DPA/Project/santander-product-recommendation/cleandata.csv", select = selectrow, showProgress =F)


df1$fecha_dato <- ymd(df1$fecha_dato) # Format date
df1$fecha_alta <- ymd(df1$fecha_alta)
df1$year_dato <- year(df1$fecha_dato) # Extract year
df1$year_alta <- year(df1$fecha_alta)
df1$month_dato <- lubridate::month(df1$fecha_dato, label = T) # Extract month
df1$month_alta <- lubridate::month(df1$fecha_alta, label = T)
df1$weekday_alta <- lubridate::wday(df1$fecha_alta, label = T)
#data<-as.data.table(data)

ggplot(df1[,.N,by=weekday_alta],aes(x=weekday_alta, y=N, fill=weekday_alta)) +
  geom_bar(stat="identity", fill = "lightblue") +
  geom_text(aes(x = weekday_alta, y = 0.05, label = paste0("(",N, ")",sep="")),
            hjust=0.5, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Weekday',
       y = 'Freq',
       title = "Number of customers that became 'first holder' by day of week") +
  theme_minimal()

```

# Year & Month (year and month the customer first signed a contract with the bank)
```{r}
ggplot(df1[year_alta > 2009,.N,by =.(month_alta,year_alta)], aes(x = month_alta,y=N,fill=month_alta)) +
  geom_bar(stat="identity", fill = "Lightblue") + 
  ggtitle("Number of customers that became 'first holder' by month and year") + geom_col() +
  facet_wrap(~year_alta) + theme(axis.text= element_text(size=5),
        legend.text = element_text(size=10),
        legend.title= element_text(size=10),
        strip.text  = element_text(face="bold")) + scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 20))


```

```{r}
print(data)
data$ind_empleado <- as.character(data$ind_empleado)
data$pais_residencia <- as.character(data$pais_residencia)
data$sexo <- as.character(data$sexo)
data$indrel_1mes <- as.character(data$indrel_1mes)
data$tiprel_1mes <- as.character(data$tiprel_1mes)
data$indresi <- as.character(data$indresi)
data$indext <- as.character(data$indext)
data$conyuemp <- as.character(data$conyuemp)
data$canal_entrada <- as.character(data$canal_entrada)
data$indfall <- as.character(data$indfall)
data$nomprov <- as.character(data$nomprov)
data$segmento <- as.character(data$segmento)
data$month <- as.numeric(data$month)

```



#Service Changes by Month
```{r}
library(dplyr)
library(ggplot2)
library(tidyverse)
totals.by.feature <- data %>%
  group_by(month,feature) %>%
  summarise(counts=n())

data %>% 
  group_by(month,feature,status) %>%
  summarise(counts=n())%>%
  ungroup() %>%
  inner_join(totals.by.feature,by=c("month","feature")) %>%

  mutate(counts=counts.x/counts.y) %>%
  ggplot(aes(y=counts,x=factor(month.abb[month],levels=month.abb[seq(12,1,-1)]))) +
  geom_bar(aes(fill=status), stat="identity") +
  facet_wrap(facets=~feature,ncol = 6) +
  coord_flip() +
  my_theme_dark +
  ylab("Count") +
  xlab("") + 
  ylim(limits=c(0,1)) +
  ggtitle("Relative Service \nChanges by Month") +
  theme(axis.text   = element_text(size=2.6),
        axis.text.x = element_text(size = 5),
        legend.text = element_text(size=10),
        legend.title= element_text(size=10),
        axis.title = element_text(size=8),
        strip.text  = element_text(face="bold"),legend.position = "top", strip.background=element_rect(colour = "black")) + scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 20)) +
   scale_fill_brewer(palette="Set1")
```

```{r}
print(data)
```

# Average Service Changes by Month
```{r}
month.counts              <- table(unique(data$month.id)%%12)
cur.names                 <- names(month.counts)
cur.names[cur.names=="0"] <- "12"
names(month.counts) <- cur.names
month.counts              <- data.frame(month.counts) %>%
  rename(month=Var1,month.count=Freq) %>% mutate(month=as.numeric(month))

data %>% 
  group_by(month,feature,status) %>%
  summarise(counts=n())%>%
  ungroup() %>%
  inner_join(month.counts,by="month") %>%

  mutate(counts=counts/month.count) %>%
  ggplot(aes(y=counts,x=factor(month.abb[month],levels=month.abb[seq(12,1,-1)]))) +
  geom_bar(aes(fill=status), stat="identity") +
  facet_wrap(facets=~feature,ncol = 6) +
  coord_flip() + my_theme_dark +
  ylab("Count") +
  xlab("") + 
  ggtitle("Average Service \nChanges by Month") +
  theme(axis.text   = element_text(size=2.6),
        axis.text.x = element_text(size = 5),
        legend.text = element_text(size=10),
        legend.title= element_text(size=10),
        axis.title = element_text(size=8),
        strip.text  = element_text(face="bold"),legend.position = "top", strip.background=element_rect(colour = "black")) + scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 20)) +
   scale_fill_brewer(palette="Set1")
```


# Service Changes by Gender
```{r}
library(ggplot2)

data %>%
  filter(sexo!="UNKNOWN") %>%
  ggplot(aes(x=sexo)) +
  geom_bar(aes(fill=status)) +
  facet_wrap(facets=~feature,ncol = 6) +
  ylab("Count") +
  xlab("") +
  ggtitle("Service Changes by Gender") +
  theme(axis.text   = element_text(size=2.6),
        axis.text.x = element_text(size = 5),
        legend.text = element_text(size=10),
        legend.title= element_text(size=10),
        axis.title = element_text(size=8),
        strip.text  = element_text(face="bold"),legend.position = "top", strip.background=element_rect(colour = "black")) + scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 20)) +
   scale_fill_brewer(palette="Set1")

```

# Seniority Vs Age
```{r}
data %>%
  group_by(ncodpers) %>%
  summarise(age=max(age),seniority=max(antiguedad)) %>%
  select(age,seniority) %>%
  ggplot(aes(x=age,y=seniority)) + 
  my_theme +
  geom_point(alpha=0.4) +
  ggtitle("Seniority vs. Age")
```
#Changes in service according to the city
```{r}
data %>%
  group_by(nomprov,status) %>%
  summarise(y=mean(total.services)) %>%
  ggplot(aes(x=factor(nomprov,levels=sort(unique(nomprov),decreasing=TRUE)),y=y)) +
  geom_bar(stat="identity",aes(fill=status)) +
  geom_text(aes(label=nomprov),
            y=0.2,
            hjust=0,
            angle=0,
            size=2,
            color="#222222") +
  coord_flip() + theme_minimal() +
  xlab("City") +
  ylab("Total # Changes") + 
  ggtitle("Service Changes\n by City") +
  theme(axis.text    = element_blank(),
        legend.text  = element_text(size=2),
        legend.title = element_text(size=2)) +
  scale_fill_manual(values=c("orange","lightpink"))
```

#Income Vs Age
```{r}
data %>%
  ggplot(aes(x=age,y=log(renta))) +
  geom_point(alpha=0.5,aes(color=status)) +
  xlab("Age") +
  ylab("Income (log scale)") + 
  ggtitle("Income vs. Age") +
  theme(legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_color_manual(values=c("cyan","blue"))
```



################################################################################
```{r}
# Since there are 27734 or more missing values in columns: ind_empleado, pais_residencia,
# sexo, fecha_alta, ind_nuevo, indrel, indresi, indext, indfall, tipodom, ind_actividad_cliente
# Lets checks if these values are missing simultaneously
#nrow(data[is.na(data$ind_empleado) & is.na(data$pais_residencia) & is.na(data$sexo) & is.na(data$sexo) ,])
# ind_empleado:  Number of records of each category
#is.na.data.frame(data)
#print(data)
```


#Service changes by seniority

```{r}
data %>%
  group_by(antiguedad,status) %>%
  summarise(counts=n()) %>%
  ggplot(aes(x=factor(antiguedad),y=log(counts))) +
  geom_point(alpha=0.6,aes(color=status)) +
  xlab("Seniority (Months)") +
  ylab("Total # Changes") + 
  ggtitle("Service Changes \n by Seniority") +
  theme(axis.text    = element_blank(),
        legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_color_manual(values=c("cyan","blue"))

```

# Histogram reveals that younger group belongs to college graduates and older group belongs to individual group of customers.

```{r}
data$age<-as.numeric(as.character(data$age))
library(ggthemes)
library(ggplot2)
data$segmento <- factor(data$segmento, labels = c("Other", "VIP", "Individuals", "Graduates"))
ggplot(data, aes(x=age, fill=factor(segmento))) +
  geom_bar() +
  facet_grid(".~segmento") + 
  scale_fill_manual("Customer Segmentation", values=c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")) +
  theme_classic() +
  theme(legend.position="bottom") +
  scale_y_continuous("Frequency") 
```

#Plotting the distribution of total serivces according to year.
```{r}
library(ggplot2)
boxplot(data$total.services~fecha_dato,
data=data,
main="Different boxplots for total services on the basis of years",
xlab="year duration",
ylab="total services",
col="orange",
border="black"
)
```


# gender VS segmento
```{r}
ggplot(data, aes(x=sexo,  fill = factor(sexo))) +
  geom_bar() +
  facet_grid(".~segmento") + 
  scale_fill_discrete(name="sexo") +
  scale_y_continuous("Frequency")
```


# Normalized service changes by New status
```{r}
tot.new     <- sum(data$ind_nuevo==1)
tot.not.new <- sum(data$ind_nuevo!=1)
tmp.df3      <- data %>%
  group_by(ind_nuevo,status) %>%
  summarise(counts=n())
tmp.df3$counts[tmp.df3$ind_nuevo==1] = tmp.df3$counts[tmp.df3$ind_nuevo==1] / tot.new
tmp.df3$counts[tmp.df3$ind_nuevo!=1] = tmp.df3$counts[tmp.df3$ind_nuevo!=1] / tot.not.new
tmp.df3 %>%
  ggplot(aes(x=factor(feature),y=counts)) +
  geom_bar(aes(fill=status,factor(ind_nuevo)),stat='identity') +
  coord_flip() +
  ylab("Count") +
  xlab("") +
  ggtitle("Normalized Service \n Changes by New Status") +
  theme(axis.text    = element_text(size=10),
        legend.text  = element_text(size=14),
        legend.title = element_blank()      ,
        strip.text   = element_text(face="bold")) +
  scale_fill_manual(values=c("lightblue","darkblue"))
```

```{r}
#cordata<-cor(data[sapply(data,is.numeric)])
#corrplot(cordata)

```













Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file). 

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

