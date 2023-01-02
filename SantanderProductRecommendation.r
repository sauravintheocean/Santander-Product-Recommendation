# Load libraries
library(data.table)
#library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(fasttime)
#library(xgboost)
#library(caret)
#library(pROC)

setwd("/Users/sauravkantkumar/Desktop/IITC/Fall2022/CSP571/Project/data")
set.seed(1234)

train_df <- read.csv("train_ver2.csv", header =TRUE, stringsAsFactors = TRUE, sep = ",", na.strings = c("", "NA"))
test_df <- read.csv("test_ver2.csv", header =TRUE, stringsAsFactors = TRUE, sep = ",", na.strings = c("", "NA"))

features <- names(train_df)[grepl("ind_+.*ult.*",names(train_df))]

train_df                     <- train_df %>% arrange(fecha_dato) %>% as.data.table()
train_df$month_id            <- as.numeric(factor((train_df$fecha_dato)))
train_df$previous_month_id   <- train_df$month_id - 1
test_df$month_id          <- max(train_df$month_id) + 1
test_df$previous_month_id <- max(train_df$month_id)

test_df <- merge(test_df,train_df[,names(train_df) %in% c(features,"ncodpers","month_id"),with=FALSE],by.x=c("ncodpers","previous_month_id"),by.y=c("ncodpers","month_id"),all.x=TRUE)
train_df <- rbind(train_df,test_df)

rm(test_df)

# Converting dates to YYYY-MM-DD format
train_df$fecha_dato <- ymd(train_df$fecha_dato)
train_df$fecha_alta <- ymd(train_df$fecha_alta)
train_df$ult_fec_cli_1t <- ymd(train_df$ult_fec_cli_1t)
unique(train_df$fecha_dato)

# Converting antiguedad from string to num
train_df$antiguedad <- as.numeric(as.character(train_df$antiguedad))

# Converting age from string to num
train_df$age <- as.numeric(as.character(train_df$age))

# Creating month column to analyze when products are bought the most
train_df$month <- month(train_df$fecha_dato)

# Check the count of null values for each column
colSums(is.na(train_df))

############################ Data Cleaning #######################
#----- ind_empleado: Employee index: A active, B ex employed, F filial, N not employee, P pasive
train_df %>% 
  dplyr::count(ind_empleado)

# A 2492
# B 3566
# F 2523
# N 13610977
# S 17
# NA 27734

# Since most prominent category is N, we can either replace NA with N or create a new category.
# It makes more sense to create a new category. Hence labelling missing values as "UNKNOWN".

sum(is.na(train_df$ind_empleado))

train_df$ind_empleado <- as.character(train_df$ind_empleado)
train_df$ind_empleado[is.na(train_df$ind_empleado)]       <- "N"
train_df$ind_empleado <- as.factor(train_df$ind_empleado)

#--------- pais_residencia:  Customer's Country residence --------------------------------------------------#
train_df %>% 
  dplyr::count(pais_residencia)

# Checking count of NAs
sum(is.na(train_df$pais_residencia))

# There are 118 different categories, the most prominent being ES with a count 13553710.
# There are 27734 NA values, we can either impute NAs with most prominent category ES or create a new caregory.
# It makes more sense to create a new category. Hence labelling missing values as "UNKNOWN".

train_df$pais_residencia <- as.character(train_df$pais_residencia)
train_df$pais_residencia[is.na(train_df$pais_residencia)]       <- "ES"
train_df$pais_residencia <- as.factor(train_df$pais_residencia)

#-------sexo:  Customer's sex            -------------------------#
train_df %>% 
  dplyr::count(sexo)

# H 6195253
# V 7424252
# NA  27804

sum(is.na(train_df$sexo))

# Number of NAs is 27804. Also the count of H and V are almost same. Hence we cannot impute 
# NAs with any of the categories. It makes more sense here to create a new category.
# Hence, imputing missing values with a new category "UNKNOWN"

train_df$sexo <- as.character(train_df$sexo)
train_df$sexo[is.na(train_df$sexo)]       <- "V"
train_df$sexo <- as.factor(train_df$sexo)

#-----------------------------age--------------------------------#
min(train_df$age, na.rm=T)
max(train_df$age, na.rm=T)

# Minimum value of age is 2 and maximum value is 164

ggplot(data=train_df,aes(x=age)) + 
  geom_bar(alpha=0.50,fill="lightblue",color="black") +
  ggtitle("Distribution of Age") 


# antiguedad is not properly captured for first 6 months
train_df[train_df$ncodpers==1375586]$antiguedad
train_df[train_df$ncodpers==1050992]$antiguedad


# Derive month of birth for each customer to correct the age
age_change  <- train_df[month_id>6,.(age,month,month_id,age_diff=c(0,diff(age))),by="ncodpers"]
age_change  <- age_change[age_diff==1]
age_change  <- age_change[!duplicated(age_change$ncodpers)]
setkey(train_df,ncodpers)
train_df <- merge(train_df,age_change[,.(ncodpers,birthday_month=month)],by=c("ncodpers"),all.x=TRUE,sort=FALSE)
train_df$birthday_month[is.na(train_df$birthday_month)] <- 7 # July is the only month we don't get to check for increment so if there is no update then use it
train_df$age[train_df$birthday_month <= 7 & train_df$month_id<train_df$birthday_month] <- train_df$age[train_df$birthday_month <= 7 & train_df$month_id<train_df$birthday_month]  - 1 # correct ages in the first 6 months
train_df$age[is.na(train_df$age)] <- median(train_df$age,na.rm=TRUE)
train_df$age <- round(train_df$age)
train_df <- as.data.frame(train_df)

rm(age_change)

#--------------------ind_nuevo: New customer Index. 1 if the customer registered in the last 6 months.----------------------#
# Check how many months of history these customers have

# Filter all the records with NA in ind_nuevo and group by ncodpers.
df1 <- train_df %>% filter(is.na(ind_nuevo))
agg_tbl <- df1 %>% group_by(ncodpers) %>% 
  dplyr::summarise(total_count=n(),
            .groups = 'drop')

agg_tbl <- df1 %>% group_by(ncodpers) %>% 
  dplyr::summarise(total_count=n(),
            .groups = 'drop')

df2 <- agg_tbl %>% as.data.frame()

# Get the maximum count
max(df2$total_count)

# Since maximum number of records for a customer with missing ind_neuvo is 6
# we can impute NA with 1

train_df$ind_nuevo[is.na(train_df$ind_nuevo)] <- 1 

# Let's check whether NAs have been imputed.
sum(is.na(train_df$ind_nuevo))

rm(df1)
rm(df2)
rm(agg_tbl)

#---------------antiguedad: Customer seniority (in months)-----------------------------------------------#
sum(is.na(train_df$antiguedad))
# There are 27734 missing values which is exactly same as ind_nuevo column.
# It might be possible that values for both columns are missing simultaneously.
# Let's verify it.

summary(train_df[is.na(train_df$antiguedad),]%>%select(ind_nuevo))

# Since ind_nuevo = 1 for all records where antiguedad is missing. It suggests that these are 
# new customers. So lets assign minimum seniority to these customers.
#train_df$antiguedad[is.na(train_df$antiguedad)] <- min(train_df$antiguedad,na.rm=TRUE)

# Since first 6 months value remain constant. Let's correct it.
new_antiguedad <- train_df %>% dplyr::select(ncodpers,month_id,antiguedad) %>% dplyr::group_by(ncodpers) %>%
                  dplyr::mutate(antiguedad=min(antiguedad,na.rm=T) + month_id - 6) %>% ungroup() %>% 
                  dplyr::arrange(ncodpers) %>% dplyr::select(antiguedad)

train_df <- train_df %>% arrange(ncodpers)
train_df$antiguedad <- new_antiguedad$antiguedad
train_df$antiguedad[train_df$antiguedad<0] <- -1
elapsed_months <- function(end_date, start_date) {
  12 * (year(end_date) - year(start_date)) + (month(end_date) - month(start_date))
}
recalculated_antiguedad <- elapsed_months(train_df$fecha_dato,train_df$fecha_alta)
train_df$antiguedad[!is.na(train_df$fecha_alta)] <- recalculated_antiguedad[!is.na(train_df$fecha_alta)]

rm(new_antiguedad)
rm(recalculated_antiguedad)

#-------------------fecha_alta: The date in which the customer became as the first holder of a contract in the bank----------------------------------------#
# Check count of missing values
sum(is.na(train_df$fecha_alta))

# Let's impute the missing dates with some middle date
train_df$fecha_alta[is.na(train_df$fecha_alta)] <- median(train_df$fecha_alta,na.rm=TRUE)


#------indrel: 1 (First/Primary), 99 (Primary customer during the month but not at the end of the month) --#
train_df %>% 
  dplyr::count(indrel)

# 1     13594782
# 99       24793
# NA       27734

# Impute NAs with most common value.
train_df$indrel[is.na(train_df$indrel)] <- 1

#-----ult_fec_cli_1t: Last date as primary customer (if he isn't at the end of the month) ----#
train_df %>% 
  dplyr::count(ult_fec_cli_1t)

# Percentage of missing values in this column.
sum(is.na(train_df$ult_fec_cli_1t))/nrow(train_df) * 100

# There are 223 different categories and the percentage of NA is 99.
# It is safe to drop this column.
train_df <- train_df %>% select(-ult_fec_cli_1t)
#----------------------indrel_1mes------------------------#
train_df %>% 
  dplyr::count(indrel_1mes)

# Replacing 1.0 by 1, 2.0 by 2, 3.0 by 3, and 4.0 by 4 
train_df$indrel_1mes[train_df$indrel_1mes == '1.0'] <- '1'
train_df$indrel_1mes[train_df$indrel_1mes == '2.0'] <- '2'
train_df$indrel_1mes[train_df$indrel_1mes == '3.0'] <- '3'
train_df$indrel_1mes[train_df$indrel_1mes == '4.0'] <- '4'

# Since most of the observations belong to category 1 we can replace NA with 1
train_df$indrel_1mes[is.na(train_df$indrel_1mes)] <- "1"

#---tiprel_1mes: Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)----#

train_df %>% 
  dplyr::count(tiprel_1mes)

# Since the number of records belonging class A and I are almost same. It makes more sense
# to create a new category. Hence, creating a new category "UNKNOWN".

train_df$tiprel_1mes <- as.character(train_df$tiprel_1mes)
train_df$tiprel_1mes[is.na(train_df$tiprel_1mes)]       <- "I"
train_df$tiprel_1mes <- as.factor(train_df$tiprel_1mes)

#----indresi: Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
train_df %>% 
  dplyr::count(indresi)

# Number of NAs are 27734. Let's create a new categoy "UNKNOWN" to impute these NAs.
train_df$indresi <- as.character(train_df$indresi)
train_df$indresi[is.na(train_df$indresi)]       <- "S"
train_df$indresi <- as.factor(train_df$indresi)


#------indext: Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
train_df %>% 
  dplyr::count(indext)

# Number of NAs are 27734. Let's create a new categoy "UNKNOWN" to impute these NAs.
train_df$indext <- as.character(train_df$indext)
train_df$indext[is.na(train_df$indext)]       <- "N"
train_df$indext <- as.factor(train_df$indext)

#--- conyuep: Spouse index. 1 if the customer is spouse of an employee 
train_df %>% 
  dplyr::count(conyuemp)

# Percentage of missing values in this column.
sum(is.na(train_df$conyuemp))/nrow(train_df) * 100

# Since percentage of NA values are very high. We could either drop this column or create a new 
# category to impute NA values.

train_df$conyuemp <- as.character(train_df$conyuemp)
train_df$conyuemp[is.na(train_df$conyuemp)]       <- "UNKNOWN"
train_df$conyuemp <- as.factor(train_df$conyuemp)

#-----------canal_entrada: channel used by the customer to join-----------------#
train_df %>% 
  dplyr::count(canal_entrada)

# There are 186126 NA values and 162 categories. It is difficult to assign NA to any one category.
# Hence, create a new category to impute NAs.
train_df$canal_entrada <- as.character(train_df$canal_entrada)
train_df$canal_entrada[is.na(train_df$canal_entrada)]       <- "UNKNOWN"
train_df$canal_entrada <- as.factor(train_df$canal_entrada)


#-----------indfall: Deceased index. N/S ---------------------------------------#
train_df %>% 
  dplyr::count(indfall)

# There are 27734 missing values. We can impute NAs with N or create a new category.
# Here, we are choosing to impute with highest occuring category.

train_df$indfall[is.na(train_df$indfall)] <- "N"


#------tipodom: Addres type. 1, primary address-------------------------#
train_df %>% 
  dplyr::count(tipodom)

# Since this column contains just one value, there is no variance in the data.
# Hence it is safe to drop it.

train_df <- train_df %>% select(-tipodom)

#-------cod_prov: Province code (customer's address)-------------------------#
# This column represents province code and the next column nomprov represents province name.
# Since one of these are enough. We can do away with this column and keep the next.

train_df <- train_df %>% select(-cod_prov)

#-----nomprov: Province name --------------------------------------------------#
# Check the count of missing values
sum(is.na(train_df$nomprov))


train_df %>% 
  dplyr::count(nomprov)

# There are 93591 NAs and there are 52 categories. In this case it is relevant to create a 
# new category. 

train_df$nomprov <- as.character(train_df$nomprov)
train_df$nomprov[is.na(train_df$nomprov)]       <- "UNKNOWN"
train_df$nomprov <- as.factor(train_df$nomprov)


#-------ind_actividad_cliente: Activity index (1, active customer; 0, inactive customer)-------#
train_df %>% 
  dplyr::count(ind_actividad_cliente)


# Impute NAs with 0 
train_df$ind_actividad_cliente[is.na(train_df$ind_actividad_cliente)] <- median(train_df$ind_actividad_cliente,na.rm=TRUE)

#------renta: Gross income of the household------------------------------------#
# Check the percentage of missing values
sum(is.na(train_df$renta))/nrow(train_df) * 100

#min(train_df$renta, na.rm=T)
#max(train_df$renta, na.rm=T)

train_df$renta <- as.numeric(as.character(train_df$renta))
# Since around 20 percent data is missing and the range of values are very high.
# It is not wise to impute NAs with a single value.
# Let's club renta with nomprov to the see the median income for each province.

df_renta_grouped_by_nomprov <- train_df %>% filter(!is.na(renta)) %>% group_by(nomprov) %>% summarise(median_income = median(renta)) %>% arrange(median_income) %>% dplyr::mutate(city=factor(nomprov,levels=nomprov))
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

new_incomes <- merge(train_df %>% select(nomprov), train_df %>% group_by(nomprov) %>% summarise(med_income=median(renta,na.rm=TRUE)), by="nomprov") %>% select(nomprov,med_income) %>% arrange(nomprov)
train_df <- arrange(train_df, nomprov)
train_df$renta[is.na(train_df$renta)] <- new_incomes$med_income[is.na(train_df$renta)]

train_df$renta[is.na(train_df$renta)] <- median(train_df$renta, na.rm=TRUE)

rm(df_renta_grouped_by_nomprov)
rm(new_incomes)
#----------------------ind_nomina_ult1---------------------#
train_df %>% 
  dplyr::count(ind_nomina_ult1)

# Impute NA with 0
train_df$ind_nomina_ult1[is.na(train_df$ind_nomina_ult1)]       <- 0


#---- segmento: segmentation: 01 - VIP, 02 - Individuals 03 - college graduated  ---------#
train_df %>% 
  dplyr::count(segmento)


train_df$segmento <- as.character(train_df$segmento)
train_df$segmento[is.na(train_df$segmento)]       <- "UNKNOWN"
train_df$segmento <- as.factor(train_df$segmento)

#----------ind_nom_pens_ult1: Pensions---------------------#
train_df %>% 
  dplyr::count(ind_nom_pens_ult1)

# Impute NA with 0
train_df$ind_nom_pens_ult1[is.na(train_df$ind_nom_pens_ult1)]       <- 0


sum(is.na(train_df))


# Convert target variables to dummy
features <- grepl("ind_+.*ult.*",names(train_df))
train_df[,features] <- lapply(train_df[,features],function(x)as.integer(round(x)))

############################################## Cleaning Complete ########################################################

#------------------------- Create Lag Features --------------------------------#
# Lag feature refer to lagged product ownership(Whether or not the product was owned 1,2,3,4,5 etc months ago).
# For each product, it is beneficial to consider not only it's value
# for the current month but also the value for previous months.

# Function to create Lagged Fatures: The idea is to join the data by customer id, ncodpers, 
# and to match the month with the lag month. For example, to add a 2-month lag feature to an observation in month 5,
# we want to extract the value of feature_name at month 3.

create_lag_feature <- function(df,                            # Should be data.table
                               feature_name,                  # Name of the feature to lag
                               months_to_lag=1,               # vector of integers indicating how many months to lag
                               by=c("ncodpers","month_id"),   # # keys to join data.tables by
                               na.fill = NA) 
{
  df_subset <- df[,mget(c(by,feature_name))]
  names(df_subset)[names(df_subset) == feature_name] <- "original_feature"
  original_month_id <- df_subset$month_id
  added_names <- c()
  for (month_ago in months_to_lag){
    print(paste("Collecting information on",feature_name,month_ago,"month(s) ago"))
    colname <- paste("lagged_",feature_name,".",month_ago,"months_ago",sep="")
    added_names <- c(colname,added_names)
    df_subset <- merge(df_subset,
                       df_subset[,.(ncodpers,
                              month_id=month_ago+original_month_id,
                              lagged_feature=original_feature)],
                    by=by,
                    all.x=TRUE,
                    sort=FALSE)
    names(df_subset)[names(df_subset)=="lagged_feature"] <- colname

  }
  df <- merge(df,
              df_subset[,c(by,added_names),with=FALSE],
              by=by,
              all.x=TRUE,
              sort=FALSE)
  df[is.na(df)] <- na.fill
  return(df)
}


# Create Lagged Feature
train_df <- as.data.table(train_df)
train_df <- create_lag_feature(train_df,'ind_actividad_cliente',1:11,na.fill=0)
train_df <- as.data.table(train_df, TRUE)

train_df <- as.data.frame(train_df)

colnames(train_df)

features <- names(train_df)[grepl("ind_+.*ult.*",names(train_df))]

test <- train_df %>%
  filter(month_id==max(train_df$month_id))

train <- train_df %>%
  filter(month_id < max(train_df$month_id))

write.csv(train,"cleaned_train.csv",row.names=FALSE)
write.csv(test,"cleaned_test.csv",row.names=FALSE)

################################################################################
#----------------------  Create Purchased Column ------------------------------#
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month_id","previous_month_id",labels)
df     <- df[,names(df) %in% cols,with=FALSE]

# connect each month to the previous one
df     <- merge(df,df,by.x=c("ncodpers","previous_month_id"),by.y=c("ncodpers","month_id"),all.x=TRUE)

# entries that don't have a corresponding row for the previous month will be NA and
# I will treat these as if that product was owned 
df[is.na(df)] <- 0

# for each product, the difference between the current month on the left and the
# previous month on the right indicates whether a product was added (+1), dropped (-1),
# or unchanged (0)

products <- rep("",nrow(df))

for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(get(colx)-get(coly))]
  products[diffs>0] <- paste0(products[diffs>0],label,sep=" ")
}


df <- df[,.(ncodpers,month_id,products)]
write.csv(df,"purchased-products.csv",row.names=FALSE)

######################## Get purchase and transaction information #######################
df <- fread("purchased-products.csv") 
full <- fread("cleaned_train.csv")
labels <- grepl("\\_ult1",names(full))
full$total_products <- rowSums(full[,labels,with=FALSE])
df[["counts"]] <- sapply(strsplit(df$products," "),length)
df <- merge(df,full[,.(month_id,ncodpers,total_products)],by=c("month_id","ncodpers"),all.x=TRUE,sort=FALSE)
purchase_count <- rbind(data.table(ncodpers=df$ncodpers,month_id=df$month_id),
                        data.table(ncodpers=unique(df$ncodpers),month_id=18))
original_month_id <- df$month_id
for (month_ago in 1:4){
  print(paste("Collecting number of purchases",month_ago,"months ago"))
  colname <- paste("num_purchases_",month_ago,"_months_ago",sep="")
  df[,month_id:=original_month_id + month_ago]
  tmp <- merge(purchase_count,df[,.(ncodpers,month_id,counts)],by=c("ncodpers","month_id"),sort=FALSE,all.x=TRUE)
  purchase_count[[colname]] <- tmp$counts
  
}

for (month_ago in 1:5){
  print(paste("Counting total products",month_ago,"months ago"))
  
  colname <- paste("total_products_",month_ago,"_months_ago",sep="")
  df[,month_id:=original_month_id + month_ago]
  tmp <- merge(purchase_count,df[,.(ncodpers,month_id,total_products)],by=c("ncodpers","month_id"),sort=FALSE,all.x=TRUE)
  purchase_count[[colname]] <- tmp$total_products
}

purchase_count[is.na(purchase_count)] <- 0
write.csv(purchase_count,"purchase-count.csv",row.names=FALSE)

############################################################################################################
# Create purchase frequency feature

df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month_id","previous_month_id",labels)
df     <- df[,names(df) %in% cols,with=FALSE]
df     <- merge(df,df,by.x=c("ncodpers","previous_month_id"),by.y=c("ncodpers","month_id"),all.x=TRUE)

df[is.na(df)] <- 0
products <- rep("",nrow(df))
num_transactions <- rep(0,nrow(df))
purchase_frequencies <- data.frame(ncodpers=df$ncodpers, month_id=(df$previous_month_id + 2))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(ncodpers,month_id,change=get(colx)-get(coly))]
  num_transactions <- num_transactions + as.integer(diffs$change!=0)
  diffs[diffs<0] <- 0
  setkey(diffs,ncodpers)
  d <- diffs[,.(frequency = cumsum(change)),by=ncodpers]
  purchase_frequencies[[paste(label,"_purchase_count",sep="")]] <- d$frequency
}
purchase_frequencies$num_transactions <- num_transactions
purchase_frequencies <- purchase_frequencies %>%
  dplyr::group_by(ncodpers) %>%
  dplyr::mutate(num_transactions = cumsum(num_transactions))
write.csv(purchase_frequencies,"purchase_frequencies.csv",row.names=FALSE)

##############################################################################################################
# Engineer the rest of the features and do some minor cleaning before modeling
# source("project/Santander/lib/engineer-features.R")

library(plyr)
library(dplyr)
library(caret)
library(pROC)

# Train on month 5 and 11 and validate on 17 for CV data then
# train on month 6 and 12 and predict on test. The second months are separated
# into a separate variable so I can turn on/off using them
val_train_month <- 5
val_test_month  <- 17
train_month     <- 6
extra_train_months_val <- c(11)
extra_train_months_test <- c(12)

months_to_keep  <- c(val_train_month,val_test_month,train_month,extra_train_months_val,extra_train_months_test)
df   <- fread("cleaned_train.csv")
test <- fread("cleaned_test.csv")


# add activity index previous month
recent_activity_index <- merge(rbind(df[,.(ncodpers,month_id,ind_actividad_cliente,
                                           segmento)],
                                     test[,.(ncodpers,month_id,ind_actividad_cliente,
                                             segmento)]),
                               df[,.(ncodpers,month_id=month_id+1,
                                     old_ind_actividad_cliente=ind_actividad_cliente,
                                     old_segmento=segmento)],
                               by=c("ncodpers","month_id"),
                               sort=FALSE)

recent_activity_index[,activity_index_change:=ind_actividad_cliente-old_ind_actividad_cliente]
recent_activity_index[,segmento_change:=as.integer(segmento!=old_segmento)]
df   <- merge(df,recent_activity_index[,.(ncodpers,
                                          month_id,
                                          old_ind_actividad_cliente,
                                          activity_index_change,
                                          old_segmento,
                                          segmento_change)],
              by=c("ncodpers","month_id"),all.x=TRUE)

test <- merge(test,recent_activity_index[,.(ncodpers,
                                            month_id,
                                            old_ind_actividad_cliente,
                                            activity_index_change,
                                            old_segmento,
                                            segmento_change)],
              by=c("ncodpers","month_id"),all.x=TRUE)

df$old_segmento[is.na(df$old_segmento)] <- df$segmento[is.na(df$old_segmento)] 
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- df$old_ind_actividad_cliente[is.na(df$ind_actividad_cliente)] 

df[is.na(df)] <- 0
products <- names(df)[grepl("ind_+.*_+ult",names(df))]

# create a data frame with just the product ownership variables so we can create lag ownership features
products_owned <- df %>%
  select(ncodpers,month_id,one_of(products)) %>%
  as.data.table()

df   <- as.data.table(df)
test <- as.data.table(test)
original_month_id <- products_owned$month_id
df <- df[month_id %in% months_to_keep,]

test <- test[,!names(test) %in% products,with=FALSE] # Removing product ownership because it is about to be re added month by month

# create features indicating whether or not a product was owned in each of the past
# X months. for each lag, match the month with the earlier one and through some name manipulation
# extract whether the product was owned or not
for (month_ago in 1:11){
  print(paste("Collecting data on product ownership",month_ago,"months ago..."))
  products_owned[,month_id:=original_month_id+month_ago]
  df <- merge(df,products_owned,by=c("ncodpers","month_id"),all.x=TRUE)
  change_names <- names(df)[grepl("\\.y",names(df))]
  new_names <- gsub("\\.y",paste("_",month_ago,"month_ago",sep=""),change_names)
  names(df)[grepl("\\.y",names(df))] <- new_names
  
  
  change_names <- names(df)[grepl("\\.x",names(df))]
  new_names <- gsub("\\.x","",change_names)
  names(df)[grepl("\\.x",names(df))] <- new_names
  
  
  test <- merge(test,products_owned,by=c("ncodpers","month_id"),all.x=TRUE)
  
  change_names <- names(test)[grepl("\\.y",names(test))]
  new_names <- gsub("\\.y",paste("_",month_ago,"month_ago",sep=""),change_names)
  names(test)[grepl("\\.y",names(test))] <- new_names
  
  change_names <- names(test)[grepl("\\.x",names(test))]
  new_names <- gsub("\\.x","",change_names)
  names(test)[grepl("\\.x",names(test))] <- new_names
  
}

names(test)[names(test) %in% products] <- paste(names(test)[names(test) %in% products],"_1month_ago",sep="")

# there will be NA values where there isn't a match to the left side since we used all.x=TRUE, assume those correspond
# to products that were not owned
df[is.na(df)] <- 0
test[is.na(test)] <- 0


# Function to calculate the number of months since owned
months_since_owned<- function(dt,products,months_to_search,default_value = 999){
  
  for (product in products){
    print(paste("Finding months since owning",product))
    colname <- paste(product,"_last_owned",sep="")
    dt[[colname]] <- default_value
    for (month_ago in seq(months_to_search,1,-1)){
      cur_colname <- paste(product,"_",month_ago,"month_ago",sep="")
      dt[[colname]][dt[[cur_colname]] == 1] <- month_ago
    }
  }
  return(dt)
}

# get the number of months since each product was owned
df <- months_since_owned(df,products,12)
test <- months_since_owned(test,products,12)
df <- as.data.frame(df)
test <- as.data.frame(test)


# compute total number of products owned previous month
df$total_products <- rowSums(df[,names(df) %in% names(df)[grepl("ind.*1month\\_ago",names(df))]],na.rm=TRUE)
test$total_products <- rowSums(test[,names(test) %in% names(test)[grepl("ind.*1month\\_ago",names(test))]],na.rm=TRUE)

# save the month id for use creating window ownership features
products_owned$month_id <- original_month_id

# windows of product ownership. For each window size look back at previous months and see if the product was 
# ever owned. I do this by adding the value of the ownership variable X months ago for X = 1:window_size
# then converting to a binary indicator if the value is positive (meaning it was owned at least once)
for (product in products){
  for (window_size in 2:6){
    print(paste("Getting ownership for",product,"within last",window_size,"months"))
    colname <- paste(product,"_owned_within_",window_size,"months",sep="")
    df[[colname]]   <- 0
    test[[colname]] <- 0
    for (month_ago in 1:window_size){
      current_col     <- paste(product,"_",month_ago,"month_ago",sep="")
      df[[colname]]   <- df[[colname]]  + df[[current_col]]
      test[[colname]] <- test[[colname]]  + test[[current_col]]
    }
    df[[colname]]   <- as.integer(df[[colname]] > 0)
    test[[colname]] <- as.integer(test[[colname]] > 0)
  }
}

# add in purchase frequency feature for each product
purchase_frequencies <- fread("purchase_frequencies.csv")

df   <- merge(df,purchase_frequencies,by=c("month_id","ncodpers"),all.x = TRUE)
test <- merge(test,purchase_frequencies,by=c("month_id","ncodpers"), all.x=TRUE)
df[is.na(df)] <- 0
test[is.na(test)] <- 0

# fix some rare value that was causing an error
#df$sexo[df$sexo=="UNKNOWN"] <- "V"
#test$sexo[test$sexo=="UNKNOWN"] <- "V"

# append "_target" so I can keep straight which are the target variables and which indicate ownership as a feature
new_names <- names(df)
new_names[new_names %in% products] <- paste(new_names[new_names %in% products],"_target",sep="")
names(df) <- new_names

labels <- names(df)[grepl(".*_target",names(df))]
purchase_w <- names(df)[grepl(".*.count",names(df))]
# products <- names(df)[grepl("ind_+.*_+ult",names(df)) & !grepl(".*_target|.count|month\\_ago",names(df))]
ownership_names <- names(df)[grepl("month\\_ago",names(df))]


test$ind_empleado[test$ind_empleado=="S"] <- "N" # Some rare value that was causing errors with factors later
char_cols <- names(test)[sapply(test,is.character)]
test[,char_cols] <- lapply(test[,char_cols], as.factor)

df$ind_empleado[df$ind_empleado=="S"] <- "N"
char_cols <- names(df)[sapply(df,is.character)]
df[,char_cols] <- lapply(df[,char_cols], as.factor)

# force the factor levels to be the same 
factor_cols <- names(test)[sapply(test,is.factor)]
for (col in factor_cols){
  df[[col]] <- factor(df[[col]],levels=levels(test[[col]]))
}
#df$ult_fec_cli_1t[is.na(df$ult_fec_cli_1t)] <- "UNKNOWN"

# only keep entries where customers purchased products and the month matches one of our sets
purchased <- as.data.frame(fread("purchased-products.csv"))
ids_val_train   <- purchased$ncodpers[purchased$month_id %in% val_train_month & (purchased$products!="")]
ids_val_test    <- purchased$ncodpers[purchased$month_id %in% val_test_month & (purchased$products!="")]
ids_train       <- purchased$ncodpers[purchased$month_id %in% train_month & (purchased$products!="")]

extra_train_ids_val <- purchased$ncodpers[purchased$month_id %in% extra_train_months_val & (purchased$products!="")]
extra_train_ids_test <- purchased$ncodpers[purchased$month_id %in% extra_train_months_test & (purchased$products!="")]

# convert the birthday month feature to a named factor
df$birthday_month   <- factor(month.abb[df$birthday_month],levels=month.abb)
test$birthday_month <- factor(month.abb[test$birthday_month],levels=month.abb)

df$month   <- factor(month.abb[df$month],levels=month.abb)
test$month <- factor(month.abb[test$month],levels=month.abb)

# discard some columns that are no longer useful
df <- select(df,-fecha_alta,-fecha_dato,-previous_month_id)
df$ind_empleado[is.na(df$ind_empleado)]       <- "N"
df$pais_residencia[is.na(df$pais_residencia)]       <- "ES"
df$indresi[is.na(df$indresi)]       <- "S"
df$indext[is.na(df$indext)]       <- "N"

# separate the data into the various parts
extra_train_val <- df %>% 
  filter(ncodpers %in% extra_train_ids_val & month_id %in% extra_train_months_val)

extra_train_test <- df %>% 
  filter(ncodpers %in% extra_train_ids_test & month_id %in% extra_train_months_test)

val_train <- df %>% 
  filter(ncodpers %in% ids_val_train & month_id %in% val_train_month)

val_test <- df %>% 
  filter(ncodpers %in% ids_val_test & month_id %in% val_test_month) 

df <- df %>% 
  filter(ncodpers %in% ids_train & month_id %in% train_month) 

test <- test %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-previous_month_id) 

# save as binary for faster loading
save(df,test,val_train,val_test,extra_train_val,extra_train_test,file="data_prepped.RData")
#######################################################################################
make_hybrid <- function(x,df,product){
  # combine the product ownership and probability into a hybrid column
  hybrid <- paste(as.character(x),
                  as.character(df[[paste(product,"_pred",sep="")]]))
  return(hybrid)
}


paste.strings <- function(products){
  paste(products,collapse=" ")
}

get_recommendations <- function(df,products,n_recs = 7){
  for (product in products){
    df[[product]] <- make_hybrid(df[[product]],df,product)
  }
  
  # the predicted probability columns are no longer needed
  df <- df[,!grepl("_pred",names(df)),with=FALSE]
  
  # melt the data frame 
  df <- as.data.frame(melt(df,
                           id_vars      = c("ncodpers","month.id"),
                           measure_vars = products,
                           variable_name= "product",
                           value_name   = "score"))
  df <- df %>%
    filter(grepl("0\\ ",score)) # only keep products that have potential to be added
  df <- df %>%
    mutate(score=as.numeric(gsub("0\\ ","",score))) # re-extract the probability
  
  # arrange in descending order and produce the recommendations
  df <- df %>%
    group_by(ncodpers,month_id) %>%
    arrange(desc(score)) %>%
    dplyr::slice(1:n_recs) %>%
    dplyr::summarise(added_products=paste.strings(product)) %>% 
    dplyr::select(ncodpers,added_products)
  
  return(df)
}
############################################# Modelling ################################################
library(data.table)
library(plyr)
library(dplyr)
library(xgboost)
library(caret)

use_resampling_weights <- FALSE
use_many_seeds         <- FALSE
if (use_many_seeds){
  rand_seeds <- 1:10
} else{
  rand_seeds <- 1
}

# read data
load("data_prepped.RData")
use_extra_train_FLAG = TRUE
if (use_extra_train_FLAG){
  val_train <- rbind(val_train,extra_train_val)
  df       <- rbind(df,extra_train_test)
}
labels  <- names(df)[grepl(".*_target",names(df)) & !grepl("ahor|aval",names(df))] # target values

#purchase_count <- read.csv("purchase-count.csv", header =TRUE, stringsAsFactors = TRUE, sep = ",", na.strings = c("", "NA"))
purchase_count <- fread("purchase-count.csv")
df   <- merge(df,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
test <- merge(test,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
val_train   <- merge(val_train,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
val_test <- merge(val_test,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)

#purchased <- as.data.frame(read.csv("purchase-count.csv", header =TRUE, stringsAsFactors = TRUE, sep = ",", na.strings = c("", "NA"))) 
purchased <- as.data.frame(fread("purchased-products.csv"))

products_df <- df %>%
  select(ncodpers,month_id) %>%
  merge(purchased,by=c("ncodpers","month_id"),sort=FALSE) %>%
  filter(products!="")


products_df$products <- sapply(strsplit(products_df$products, " "), function(x) sample(x,1))
products_df$products <- factor(products_df$products,levels=gsub("\\_target","",labels))
product_names_save<-gsub("\\_target","",labels)
products_df$products <- as.integer(products_df$products)-1

products_val <- val_train %>%
  select(ncodpers,month_id) %>%
  merge(purchased,by=c("ncodpers","month_id"),sort=FALSE) %>%
  filter(products!="")

products_val$products <- sapply(strsplit(products_val$products, " "), function(x) sample(x,1))
products_val$products <- factor(products_val$products,levels=gsub("\\_target","",labels))
products_val$products <- as.integer(products_val$products)-1


train_labels <- list()
train_labels[["products"]] <- as(data.matrix(products_df[["products"]]),'dgCMatrix')

train_labels_val <- list()
train_labels_val[["products"]] <- as(data.matrix(products_val[["products"]]),'dgCMatrix')

june_fractions  <- table(products_df$products[products_df$month_id==6])
june_fractions  <- june_fractions / sum(june_fractions)
total_fractions <- table(products_df$products)
total_fractions <- total_fractions / sum(total_fractions)
prod_weights_df     <- (june_fractions / total_fractions)



may_fractions   <- table(products_val$products[products_val$month_id==5])
may_fractions   <- may_fractions / sum(may_fractions)
total_fractions <- table(products_val$products)
total_fractions <- total_fractions / sum(total_fractions)
prod_weights_val     <- (may_fractions / total_fractions)


if (use_resampling_weights){
  df_weights  <- prod_weights_df[products_df$products+1]
  val_weights <- prod_weights_val[products_val$products+1]
} else {
  df_weights <- rep(1,nrow(df))
  val_weights <- rep(1,nrow(val_train))
  
}

# make sure the factor levels agree
factor_cols <- names(test)[sapply(test,is.factor)]
for (col in factor_cols){
  df[[col]] <- factor(df[[col]],levels=union(levels(df[[col]]),levels(test[[col]])))
  val_train[[col]] <- factor(val_train[[col]],levels=union(levels(val_train[[col]]),levels(val_test[[col]])))
}

# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight
labels               <- names(df)[grepl(".*_target",names(df)) & !grepl("ahor|aval",names(df))] # target values
purchase_w           <- names(df)[grepl(".*.count",names(df))] # number of times a product has been bought in the past 5 months
ownership_names      <- names(df)[grepl("month\\_ago",names(df)) & !grepl("month\\_previous",names(df))] # various features indicating whether or not a product was owned X months ago
drop_names           <- names(df)[grepl("dropped",names(df))] # various features indicating whether or not a product was owned X months ago
add_names            <- names(df)[grepl("added",names(df))] # various features indicating whether or not a product was owned X months ago
num_added_names      <- names(df)[grepl("num\\_added",names(df))]  # total number of products added X months ago
num_purchases_names  <- names(df)[grepl("num\\_purchases",names(df))]  # total number of products added X months ago
total_products_names <- names(df)[grepl("total\\_products",names(df))]  # total number of products owned X months ago
owned_within_names   <- names(df)[grepl("owned\\_within",names(df))]  # whether or not each product was owned with X months
# numeric features to use
numeric_cols <- c("age",
                  "renta",
                  "antiguedad",
                  purchase_w,
                  "total_products",
                  "num_transactions",
                  num_purchases_names)

categorical_cols <- c("sexo",
                      "ind_nuevo",
                      "ind_empleado",
                      "segmento",
                      "nomprov",
                      "indext",
                      "indresi",
                      "indrel",
                      "tiprel_1mes",
                      ownership_names,
                      owned_within_names,
                      "segmento_change",
                      "activity_index_change",
                      "ind_actividad_cliente",
                      "month",
                      "birthday_month")

# one-hot encode the categorical features
ohe <- dummyVars(~.,data = df[,names(df) %in% categorical_cols])
ohe <- as(data.matrix(predict(ohe,df[,names(df) %in% categorical_cols])), "dgCMatrix")
ohe_test <- dummyVars(~.,data = test[,names(test) %in% categorical_cols])
ohe_test <- as(data.matrix(predict(ohe_test,test[,names(test) %in% categorical_cols])), "dgCMatrix")
ohe_val_train <- dummyVars(~.,data = val_train[,names(val_train) %in% categorical_cols])
ohe_val_train <- as(data.matrix(predict(ohe_val_train,val_train[,names(val_train) %in% categorical_cols])), "dgCMatrix")
ohe_val_test <- dummyVars(~.,data = val_test[,names(val_test) %in% categorical_cols])
ohe_val_test <- as(data.matrix(predict(ohe_val_test,val_test[,names(val_test) %in% categorical_cols])), "dgCMatrix")

# remember the id's for people and months for later since all that actually goes
# into xgboost is the raw feature data
save_id       <- df$ncodpers
save_month_id <- df$month_id
save_month    <- df$month
save_id_test       <- test$ncodpers
save_month_id_test <- test$month_id
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric_cols]))
test       <- cbind(ohe_test,data.matrix(test[,names(test) %in% numeric_cols]))

save_id_val       <- val_train$ncodpers
save_month_id_val <- val_train$month_id
save_id_test_val       <- val_test$ncodpers
save_month_id_test_val <- val_test$month_id
save_month_val    <- val_train$month
val_train         <- cbind(ohe_val_train,data.matrix(val_train[,names(val_train) %in% numeric_cols]))
val_test       <- cbind(ohe_val_test,data.matrix(val_test[,names(val_test) %in% numeric_cols]))

# use a 75/25 train/test split so we can compute MAP@7 locally. The test set
# is predicted using a model trained on all of the training data
train_ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

# tuning hyperparameters to optimize MAP@7 must be done manually. I previously did 
# a grid search and these parameters were okay so I commented it out for now. You just 
# simply scan parameters and save the ones that gave you the best local MAP@7 on the validation data

test_save <- test
best_map <- 0
# for (depth in c(3,5,7,9,11,15)){
# for (eta in c(0.01,0.025, 0.05,0.1,0.25,0.5)){
depth <- 7
eta <- 0.05
test <- test_save
predictions         <- list()
predictions_val     <- list()
predictions_val_future     <- list()

################################################################################
make_hybrid <- function(x,df,product){
  # combine the product ownership and probability into a hybrid column
  hybrid <- paste(as.character(x),
                  as.character(df[[paste(product,"_pred",sep="")]]))
  return(hybrid)
}


paste.strings <- function(products){
  paste(products,collapse=" ")
}

get_recommendations <- function(df,products,n_recs = 7){
  for (product in products){
    df[[product]] <- make_hybrid(df[[product]],df,product)
  }
  
  # the predicted probability columns are no longer needed
  df <- df[,!grepl("_pred",names(df)),with=FALSE]
  
  # melt the data frame 
  df <- as.data.frame(melt(df,
                           id.vars      = c("ncodpers","month_id"),
                           measure.vars = products,
                           variable.name= "product",
                           value.name   = "score"))
  
  df <- df %>%
    filter(grepl("0\\ ",score)) # only keep products that have potential to be added
  df <- df %>%
    mutate(score=as.numeric(gsub("0\\ ","",score))) # re-extract the probability
  
  # arrange in descending order and produce the recommendations
  df <- df %>%
    group_by(ncodpers,month_id) %>%
    arrange(desc(score)) %>%
    dplyr::slice(1:n_recs) %>%
    dplyr::summarise(added_products=paste.strings(product)) %>% 
    dplyr::select(ncodpers,added_products)
  
  return(df)
}
apk <- function(k, actual, predicted)
{
  if (length(actual)==0){return(0.0)}
  score <- 0.0
  cnt <- 0.0
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt   <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  if (is.na(score)){
    debug<-0
  }
  return(score)
}
mapk <- function (k, actual, predicted)
{
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}
################################################################################

build_predictions_xgboost <- function(df, test, label, label_name,depth,eta, weights, rand_seeds=0){
  #library(xgboost)
  # df:         training data
  # test:       the data to predict on
  # label:      vector containing the target label
  # label_name: name of the label
  # depth:      XGBoost max tree depth
  # eta:        XGBoost learning rate
  for (rand_seed_num in 1:length(rand_seeds)){
    print(paste("Building model with random seed ", rand_seeds[rand_seed_num]))
    set.seed(rand_seeds[rand_seed_num])
    dtrain <- xgb.DMatrix(data = df, label=label, weight=weights)
    model <- xgboost(data = dtrain,
                     max.depth = depth, 
                     eta = eta, nthread = 4,
                     nround = 175, 
                     objective = "multi:softprob", 
                     num_class=22, #hardcoded!
                     verbose =1 ,
                     print.every.n = 10)
    imp <- xgb.importance(feature_names = colnames(df),model=model)
    save(imp,file=paste("IMPORTANCE_multiclass_",gsub("\\_target","",label_name),".RData",sep=""))
    print(imp)
    if (rand_seed_num == 1) {# initialize predictions on first time
      preds <- predict(model,test)
    } else {
      preds <- predict(model,test) + preds
    }
  }
  predictions        <- list(preds / length(rand_seeds))
  names(predictions) <- paste(gsub("_target","",label_name),"_pred",sep="")
  return(predictions)
}


# loop over the labels and create predictions of the validation data and training data
# for each
label_count <- 1
for (label in c("products")){
  # the syntax for indexing train.labels is messy but functional
  # predictions_val <- c(predictions_val,build.predictions.xgboost(df[train.ind,],df[-train.ind,],train.labels[[label]][train.ind,1,drop=F],label,depth,eta) )
  # accuracy <- mean(train.labels[[label]][-train.ind,1]==round(predictions_val[[label.count]]))
  # print(sprintf("Accuracy for label %s = %f",label,accuracy)) # accuracy not super useful for this task
  # if (accuracy < 1){ # perfect accuracy causes some error with pROC
  # print(pROC::auc(roc(train.labels[[label]][-train.ind,1],predictions_val[[label.count]])))
  # } else {
  # print("auc perfect")
  # }
  
  # now predict on the testing data
  downweight_factor <- 1
  # predictions <- c(predictions,build.predictions.xgboost(df,test,train.labels[[label]],label,depth,eta,ifelse(save.month=="Jun",1,downweight.factor)) )
  # predictions_val_future <- c(predictions_val_future,build.predictions.xgboost(val.train,val.test,train.labels.val[[label]],label,depth,eta,ifelse(save.month.val=="May",1,downweight.factor)) )
  
  predictions <- c(predictions,build_predictions_xgboost(df,test,train_labels[[label]],label,depth,eta,weights=df_weights,rand_seeds))
  predictions_val_future <- c(predictions_val_future,build_predictions_xgboost(val_train,val_test,train_labels_val[[label]],label,depth,eta,weights=val_weights,rand_seeds))
  label_count <- label_count + 1
}

predictions[[1]]     <- matrix(predictions[[1]],nrow=nrow(test),byrow = TRUE)
predictions_val_future[[1]] <- matrix(predictions_val_future[[1]],nrow=(nrow(val_test)),byrow = TRUE)
colnames(predictions[[1]]) <- product_names_save
colnames(predictions_val_future[[1]]) <- product_names_save

# collect the results
predictions <- as.data.table(predictions[[1]])
# predictions_val <- as.data.table(predictions_val)
predictions_val_future <- as.data.table(predictions_val_future[[1]])

names_to_change <- names(predictions)
names_to_change <- gsub("products\\_pred\\.","",names_to_change)
names_to_change <- paste(names_to_change,"_pred",sep="")
names(predictions) <- names_to_change

names_to_change <- names(predictions_val_future)
names_to_change <- gsub("products\\_pred\\.","",names_to_change)
names_to_change <- paste(names_to_change,"_pred",sep="")
names(predictions_val_future) <- names_to_change


test        <- as.data.table(cbind(data.frame(data.matrix(test)),predictions))
val_future        <- as.data.table(cbind(data.frame(data.matrix(val_test)),predictions_val_future))

# can drop some of the data at this point and put back the id's
test <- test[,grepl("ind_+.*_+ult",names(test)),with=FALSE]
test$ncodpers <- save_id_test
test$month_id <- save_month_id_test

val_future <- val_future[,grepl("ind_+.*_+ult",names(val_future)),with=FALSE]
val_future$ncodpers <- save_id_test_val
val_future$month_id <- save_month_id_test_val

products <- gsub("_target","",labels)


# the features containing "1month_ago" will tell us whether or not a product is a new purchase in our predictions
owned_products <- names(test)[grepl("1month\\_ago",names(test)) & !(grepl("_pred",names(test)))]

# save the products for use in the recommendation script
save(products,file="./products.Rdata")

# put the predictions in the right format 
test <- test %>%
  select(ncodpers,month_id,contains("_pred"),contains("1month"))
names(test)[grepl("1month",names(test))] <- gsub("\\_1month\\_ago","",names(test)[grepl("1month",names(test))])

val_future <- val_future %>%
  select(ncodpers,month_id,contains("_pred"),contains("1month"))
names(val_future)[grepl("1month",names(val_future))] <- gsub("\\_1month\\_ago","",names(val_future)[grepl("1month",names(val_future))])
# save the results


val_recs_future  <- get_recommendations(as.data.table(val_future),products)
val_future$added_products <- val_recs_future$added_products

purchased <- as.data.frame(fread("purchased-products.csv"))
val_future <- val_future %>%
  merge(purchased,by=c("ncodpers","month_id"))
MAP <- mapk(k=7,strsplit(val_future$products, " "),strsplit(val_future$added_products," "))
print(paste("Validation future MAP@7 = ",MAP))

write.csv(test,"xgboost_preds_test_multiclass.csv",row.names = FALSE)
write.csv(val_future,"xgboost_preds_val_future_multiclass.csv",row.names = FALSE)


###########################Model 2 #############################################
library(tidyr)
library(xgboost)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(pROC)
library(lubridate)

set.seed(1)
use_many_seeds   <- FALSE 
if (use_many_seeds){
  rand_seeds <- 101:110
} else{
  rand_seeds <- 1
}

# read data
load("data_prepped.RData")
use_extra_train_FLAG = TRUE
if (use_extra_train_FLAG){
  val_train <- rbind(val_train,extra_train_val)
  df       <- rbind(df,extra_train_test)
}


purchase_count <- fread("purchase-count.csv")
df   <- merge(df,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
test <- merge(test,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
val_train   <- merge(val_train,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
val_test <- merge(val_test,purchase_count,by=c("ncodpers","month_id"),sort=FALSE)
rm(purchase_count)

# make sure the factor levels agree
factor_cols <- names(test)[sapply(test,is.factor)]
for (col in factor_cols){
  df[[col]] <- factor(df[[col]],levels=union(levels(df[[col]]),levels(test[[col]])))
  val_train[[col]] <- factor(val_train[[col]],levels=union(levels(val_train[[col]]),levels(val_test[[col]])))
}

# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight
labels               <- names(df)[grepl(".*_target",names(df)) & !grepl("ahor|aval",names(df))] # target values
purchase_w           <- names(df)[grepl(".*.count",names(df))] # number of times a product has been bought in the past 5 months
ownership_names      <- names(df)[grepl("month\\_ago",names(df)) & !grepl("month\\.previous",names(df))] # various features indicating whether or not a product was owned X months ago
drop_names           <- names(df)[grepl("dropped",names(df))] # various features indicating whether or not a product was owned X months ago
add_names            <- names(df)[grepl("added",names(df))] # various features indicating whether or not a product was owned X months ago
num_added_names      <- names(df)[grepl("num\\.added",names(df))]  # total number of products added X months ago
num_purchases_names  <- names(df)[grepl("num\\.purchases",names(df))]  # total number of products added X months ago
total_products_names <- names(df)[grepl("total\\.products",names(df))]  # total number of products owned X months ago
owned_within_names   <- names(df)[grepl("owned\\.within",names(df))]  # whether or not each product was owned with X months
# numeric features to use
numeric_cols <- c("age",
                  "renta",
                  "antiguedad",
                  purchase_w,
                  "total_products",
                  "num_transactions",
                  num_purchases_names)

categorical_cols <- c("sexo",
                      "ind_nuevo",
                      "ind_empleado",
                      "segmento",
                      "nomprov",
                      "indext",
                      "indresi",
                      "indrel",
                      "tiprel_1mes",
                      ownership_names,
                      owned_within_names,
                      "segmento_change",
                      "activity_index_change",
                      "ind_actividad_cliente",
                      "month",
                      "birthday_month")

# one-hot encode the categorical features
ohe <- dummyVars(~.,data = df[,names(df) %in% categorical_cols])
ohe <- as(data.matrix(predict(ohe,df[,names(df) %in% categorical_cols])), "dgCMatrix")
ohe_test <- dummyVars(~.,data = test[,names(test) %in% categorical_cols])
ohe_test <- as(data.matrix(predict(ohe_test,test[,names(test) %in% categorical_cols])), "dgCMatrix")
ohe_val_train <- dummyVars(~.,data = val_train[,names(val_train) %in% categorical_cols])
ohe_val_train <- as(data.matrix(predict(ohe_val_train,val_train[,names(val_train) %in% categorical_cols])), "dgCMatrix")
ohe_val_test  <- dummyVars(~.,data = val_test[,names(val_test) %in% categorical_cols])
ohe_val_test  <- as(data.matrix(predict(ohe_val_test,val_test[,names(val_test) %in% categorical_cols])), "dgCMatrix")

train_labels        <- list()
train_labels_val        <- list()

# convert labels into XGBoost's sparse matrix representation
for (label in labels){
  train_labels[[label]]     <- as(data.matrix(df[[label]]),'dgCMatrix')
  train_labels_val[[label]] <- as(data.matrix(val_train[[label]]),'dgCMatrix')
}

# remember the id's for people and months for later since all that actually goes
# into xgboost is the raw feature data
save_id       <- df$ncodpers
save_month_id <- df$month_id
save_month    <- df$month
save_id_test       <- test$ncodpers
save_month_id_test <- test$month_id
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric_cols]))
test       <- cbind(ohe_test,data.matrix(test[,names(test) %in% numeric_cols]))

save_id_val       <- val_train$ncodpers
save_month_id_val <- val_train$month_id
save_id_test_val       <- val_test$ncodpers
save_month_id_test_val <- val_test$month_id
save_month_val    <- val_train$month
val_train         <- cbind(ohe_val_train,data.matrix(val_train[,names(val_train) %in% numeric_cols]))
val_test          <- cbind(ohe_val_test,data.matrix(val_test[,names(val_test) %in% numeric_cols]))
set.seed(1)

# use a 75/25 train/test split so we can compute MAP@7 locally. The test set
# is predicted using a model trained on all of the training data
train_ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

# tuning hyperparameters to optimize MAP@7 must be done manually. I previously did 
# a grid search and these parameters were okay so I commented it out for now. You just 
# simply scan parameters and save the ones that gave you the best local MAP@7 on the validation data

test_save <- test
best_map <- 0
# for (depth in c(3,5,7,9,11,15)){
# for (eta in c(0.01,0.025, 0.05,0.1,0.25,0.5)){
depth <- 7
eta <- 0.05
test <- test_save
predictions         <- list()
predictions_val     <- list()
predictions_val_future     <- list()
# this function takes in training/testing data and returns predicted probabilities
build_predictions_xgboost <- function(df, test, label, label_name,depth,eta,weights,rand_seeds=0){
  for (rand_seed_num in 1:length(rand_seeds)){
    set.seed(rand_seeds[rand_seed_num])
    library(xgboost)
    # df:         training data
    # test:       the data to predict on
    # label:      vector containing the target label
    # label.name: name of the label
    # depth:      XGBoost max tree depth
    # eta:        XGBoost learning rate
    dtrain <- xgb.DMatrix(data = df, label=label,weight=weights)
    # model <- xgb.cv(data = dtrain,
    # max.depth = depth, 
    # eta = eta, nthread = 4,
    # nround = 100, 
    # objective = "binary:logistic", 
    # verbose =1 ,
    # print.every.n = 10,
    # nfold=5)
    model <- xgboost(data = dtrain,
                     max.depth = depth,
                     eta = eta, nthread = 4,
                     nround = 80, 
                     subsample=0.75,
                     # colsample_bytree=0.5,
                     objective = "binary:logistic", 
                     verbose =1 ,
                     print_every_n = 10)
    if (rand_seed_num == 1 ) { # initialize predictions on first time
      preds <- predict(model,test)
    } else {
      preds <- predict(model,test) + preds
    }
  }
  imp <- xgb.importance(feature_names = colnames(df),model=model)
  save(imp,file=paste("IMPORTANCE_",gsub("\\_target","",label_name),".RData",sep=""))
  print(imp)
  predictions        <- list(preds / length(rand_seeds))
  names(predictions) <- paste(gsub("_target","",label_name),"_pred",sep="")
  return(predictions)
}

# loop over the labels and create predictions of the validation data and training data
# for each
label_count <- 1
for (label in labels){
  # the syntax for indexing train.labels is messy but functional
  # predictions_val <- c(predictions_val,build.predictions.xgboost(df[train.ind,],df[-train.ind,],train.labels[[label]][train.ind,1,drop=F],label,depth,eta) )
  # accuracy <- mean(train.labels[[label]][-train.ind,1]==round(predictions_val[[label.count]]))
  # print(sprintf("Accuracy for label %s = %f",label,accuracy)) # accuracy not super useful for this task
  # if (accuracy < 1){ # perfect accuracy causes some error with pROC
  # print(pROC::auc(roc(train.labels[[label]][-train.ind,1],predictions_val[[label.count]])))
  # } else {
  # print("auc perfect")
  # }
  
  # now predict on the testing data
  downweight_factor <- 2
  predictions <- c(predictions,build_predictions_xgboost(df,test,train_labels[[label]],label,depth,eta,ifelse(save_month=="Jun",1,downweight_factor),rand_seeds) )
  predictions_val_future <- c(predictions_val_future,build_predictions_xgboost(val_train,val_test,train_labels_val[[label]],label,depth,eta,ifelse(save_month_val=="May",1,downweight_factor),rand_seeds) )
  label_count <- label_count + 1
  
}

# collect the results
predictions <- as.data.table(predictions)
predictions_val_future <- as.data.table(predictions_val_future)
test        <- as.data.table(cbind(data.frame(data.matrix(test)),predictions))
val_future        <- as.data.table(cbind(data.frame(data.matrix(val_test)),predictions_val_future))

# can drop some of the data at this point and put back the id's
test <- test[,grepl("ind_+.*_+ult",names(test)),with=FALSE]
test$ncodpers <- save_id_test
test$month_id <- save_month_id_test

val_future <- val_future[,grepl("ind_+.*_+ult",names(val_future)),with=FALSE]
val_future$ncodpers <- save_id_test_val
val_future$month_id <- save_month_id_test_val

# get product names
products <- gsub("_target","",labels)

# the features containing "1month_ago" will tell us whether or not a product is a new purchase in our predictions
owned_products <- names(test)[grepl("1month\\_ago",names(test)) & !(grepl("_pred",names(test)))]

# save the products for laer use 
save(products,file="./products.Rdata")

# put the predictions in the right format 
test <- test %>%
  select(ncodpers,month_id,contains("_pred"),contains("1month"))
names(test)[grepl("1month",names(test))] <- gsub("\\_1month\\_ago","",names(test)[grepl("1month",names(test))])

val_future <- val_future %>%
  select(ncodpers,month_id,contains("_pred"),contains("1month"))
names(val_future)[grepl("1month",names(val_future))] <- gsub("\\_1month\\_ago","",names(val_future)[grepl("1month",names(val_future))])

# get the local MAP@7 CV score
val_recs_future  <- get_recommendations(as.data.table(val_future),products)
val_future$added_products <- val_recs_future$added_products

purchased <- as.data.frame(fread("purchased-products.csv"))
val_future <- val_future %>%
  merge(purchased,by=c("ncodpers","month_id"))
MAP <- mapk(k=7,strsplit(val_future$products, " "),strsplit(val_future$added_products," "))
print(paste("Validation future MAP@7 = ",MAP))

write.csv(test,"xgboost_preds_test_singleclass.csv",row.names = FALSE)
write.csv(val_future,"xgboost_preds_val_future_singleclass.csv",row.names = FALSE)

####################### Combine Prediction from both models #####################
source("project/Santander/lib/dataframe-correlation.R")

weights <- list()
weights[['single']] <- 1
weights[['multi']] <- .1


test_xgboost <- as.data.frame(fread("xgboost_preds_test_singleclass.csv"))
val_xgboost  <- as.data.frame(fread("xgboost_preds_val_future_singleclass.csv"))

test_xgboost_multi <- as.data.frame(fread("xgboost_preds_test_multiclass.csv"))
val_xgboost_multi  <- as.data.frame(fread("xgboost_preds_val_future_multiclass.csv"))

pred_names <-names(test_xgboost[grepl("pred",names(test_xgboost))])
product_names <- gsub("\\_pred","",pred_names)
test_xgboost <- test_xgboost %>% 
  select(ncodpers,month_id,one_of(pred_names),one_of(product_names))
test_xgboost_multi <- test_xgboost_multi %>% 
  select(ncodpers,month_id,one_of(pred_names),one_of(product_names))
val_xgboost <- val_xgboost %>% 
  dplyr::select(ncodpers,month_id,one_of(pred_names),one_of(product_names))
val_xgboost_multi <- val_xgboost_multi %>% 
  select(ncodpers,month_id,one_of(pred_names),one_of(product_names))

scale_factor_test <- mean(data.matrix(test_xgboost[,names(test_xgboost) %in% pred_names])) /  mean(data.matrix(test_xgboost_multi[,names(test_xgboost_multi) %in% pred_names]))
test_xgboost_multi[,names(test_xgboost_multi) %in% pred_names] <- test_xgboost_multi[,names(test_xgboost_multi) %in% pred_names] *scale_factor_test
scale_factor_val <- mean(data.matrix(val_xgboost[,names(val_xgboost) %in% pred_names])) /  mean(data.matrix(val_xgboost_multi[,names(val_xgboost_multi) %in% pred_names]))
val_xgboost_multi[,names(val_xgboost_multi) %in% pred_names] <- val_xgboost_multi[,names(val_xgboost_multi) %in% pred_names] *scale_factor_val


pred_names <- names(test_xgboost)[grepl("\\_pred",names(test_xgboost))]


preds_val <- val_xgboost
preds_val[,names(preds_val)%in% pred_names] <- (weights[["single"]]  * val_xgboost[,names(val_xgboost)%in% pred_names] +
                                                  weights[["multi"]] *   val_xgboost_multi[,names(val_xgboost_multi)%in% pred_names]) / (weights[['multi']]+weights[['single']])

write.csv(preds_val,"combined_preds_val.csv")

preds_test <- test_xgboost
preds_test[,names(preds_test)%in% pred_names] <- (weights[["single"]]  * test_xgboost[,names(test_xgboost)%in% pred_names] +
                                                    weights[["multi"]] *   test_xgboost_multi[,names(test_xgboost_multi)%in% pred_names]) /  (weights[['multi']]+weights[['single']])
write.csv(preds_test,"combined_preds_test.csv")

########################## Generate Recommendations ############################
# Combine information about product ownership in the previous month
# and predicted probability of owning a product in the current month to produce
# recommendations of currently unowned products

val  <- fread("xgboost_preds_val_future_singleclass.csv")
val$products <- val$products.x
purchased <- as.data.frame(fread("purchased-products.csv"))


load("products.Rdata")

# get the recommendations
val_recs  <- get_recommendations(val,products)
val$added_products <- val_recs$added_products

val <- val %>%
  merge(purchased,by=c("ncodpers","month_id"))

# compute MAP@7 on the validation set
MAP <- mapk(k=7,strsplit(val$products, " "),strsplit(val$added_products," "))
print(paste("Validation MAP@7 = ",MAP))

# now predict on test 
test <- fread("xgboost_preds_test_singleclass.csv")
# test <- fread("combined_preds_test.csv") # filename for ensemble version
test_recs <- get_recommendations(test,products)

# write out final submission
write.csv(test_recs,"recommendations_xgboost.csv",row.names = FALSE)

