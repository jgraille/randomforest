#################################
# titanic.R
#
# Create a model to predict which passengers survived the titanic shipwreck. 
# https://www.kaggle.com/c/titanic
#
# Started 08/03/2020 by Jean-Baptiste Graille
#################################

rm(list = ls(all.names = TRUE))

# Getting Packages
required_packages <- c(
  "plyr",
  "dplyr",
  "data.table",
  "randomForest",
  "here"
)
# Check for installed packages and install missing ones
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
# Load necessary packages
lapply(required_packages, require, character.only = TRUE)

# The `dummies`` packages has been removed from the cran
# Still availabe here https://cran.r-project.org/src/contrib/Archive/dummies/
#install.packages(here("packages/dummies_1.5.6.tar.gz"), repos = NULL, type="source")
require(dummies)


# Outliers function
options(stringsAsFactors = FALSE)
detect_outlier<-function(datatable,n,features){
  # need library("plyr","data.table")
  outlier_indices<-c()
  dt_outlier_indices<-data.table()
  datatable$index<-rownames(datatable)
  for (val in features){
    Q1<-quantile(datatable[!is.na(datatable[[val]])][[val]],.25)
    Q3<-quantile(datatable[!is.na(datatable[[val]])][[val]],.75)
    # interquantile range
    IQR<-Q3-Q1
    # outlier step
    outlier_step<-1.5*IQR
    if(empty(datatable[datatable[[val]]<Q1-outlier_step | datatable[[val]]>Q3+outlier_step,c("index")])){
      outlier_list_col<-0
    } else {
      outlier_list_col<-datatable[datatable[[val]]<Q1-outlier_step | datatable[[val]]>Q3+outlier_step,c("index")]
    }
    outlier_indices<-c(outlier_indices,outlier_list_col)
    
  }
  dt_outlier_indices<-data.table(as.numeric(unlist(outlier_indices)))
  dt_outlier_indices<-dt_outlier_indices[,.N,by=V1][N>n] # choose to keep outliers in more than n features
  datatable[as.numeric(rownames(datatable)) %in% dt_outlier_indices$V1]
}
# Filling with NA
check_na_blank<-function(datatable){
  for(j in seq_along(datatable)){
    if(!empty(datatable[datatable[[j]]=="" | datatable[[j]]==" " | is.na(datatable[[j]]), ])){
      data.table::set(datatable, i=which(datatable[[j]]==""), j=j, value=NA)
      data.table::set(datatable, i=which(datatable[[j]]==" "), j=j, value=NA)
      print(paste("Replacing ' '/ '' to NA in",colnames(datatable)[j]))
    }
  }
}

# loading train dataset
df_train <- read.csv(here('titanicdata/train.csv'),stringsAsFactors = F)
# convert to data.table
setDT(df_train)
# catching outliers
outliers <- detect_outlier(df_train,2,c("Age","SibSp","Parch","Fare"))
# Removing outliers. Todo -> keep them and apply winsorization
df_train <- df_train[!PassengerId %in% outliers[, PassengerId], ]
# loading test dataset
df_test <- read.csv(here("titanicdata/test.csv"), stringsAsFactors = F)
setDT(df_test)
outCols <- c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked")
df_test[,Survived := as.integer(NA)]
full_data <- base::rbind(df_train[,..outCols],df_test[,..outCols])
# Cleaning
check_na_blank(full_data)

# Fix errors
# https://www.kaggle.com/c/titanic/discussion/39787
# Abott family
full_data[grepl("Abbott, Mrs. Stanton", Name), `:=`(SibSp = 0, Parch = 2)]
full_data[grepl("Abbott, Master. Eugene Joseph", Name), `:=`(SibSp = 1, Parch = 1)]

# Ford family
#full_data$Age[full_data$PassengerId==1737] <- 55
# Ford, Mr. William Neal
full_data[grepl("Ford, Mr. William Neal",Name),`:=`(SibSp = 3, Parch = 1)]
# Ford, Miss. Robina Maggie "Ruby"
full_data[grepl("Ford, Miss. Robina Maggie",Name),`:=`(SibSp = 3, Parch = 1)]
# Ford, Miss. Doolina Margaret "Daisy"
full_data[grepl("Ford, Miss. Doolina Margaret",Name), `:=`(SibSp = 3, Parch = 1)]
# Ford, Mrs. Edward (Margaret Ann Watson)
full_data[grepl("Ford, Mrs. Edward",Name), `:=`(SibSp = 0, Parch = 4, Age = 55)]
# Ford, Mr. Edward Watson
full_data[grepl("Ford, Mr. Edward Watson",Name), `:=`(SibSp = 3, Parch = 1)]

# Ford, Mr. Arthur PassengerId==1181 does not belong to the Ford's family
# https://www.encyclopedia-titanica.org/titanic-victim/margaret-ann-watson-ford.html

# Cavendish, Mrs. Tyrell William (Julia Florence Siegel)
full_data[grepl("Cavendish, Mrs. Tyrell",Name), Age := 26]

# Samaan family
# https://www.encyclopedia-titanica.org/titanic-victim/hanna-elias-samaan.html
# Ages are filled from the public record manuscript
# Samaan, Mr. Hanna male (the father)
full_data[grepl("Samaan, Mr. Hanna", Name), `:=`(Age = 40, SibSp = 0, Parch = 2)]

# Samaan, Mr. Elias
full_data[grepl("Samaan, Mr. Elias", Name), `:=`(Age = 18, SibSp = 1, Parch = 1)]
# Samaan, Mr. Youssef
full_data[grepl("Samaan, Mr. Youssef", Name), `:=`(Age = 16, SibSp = 1, Parch = 1)]


#################################################################### Titles #####################################################
# Catching the title 
full_data[,Titre := gsub('(.*, )|(\\..*)', '',Name)]
df_titles <- full_data[, .N, by = "Titre"]

# Reassembling the small groups into the big ones.
full_data[Titre %in% df_titles[N < 10,Titre] & Sex == "female", Titre := "Mrs"]
full_data[Titre %in% df_titles[N < 10,Titre] & Sex == "male", Titre := "Mrs"]
# Converting to dummies (1,0) variables the Titre column. It will return Integers columns
full_data <- cbind(full_data, dummy(full_data[, Titre], sep = "_Titre_"))
# Converting character Sex col to integer Sex col
full_data[,Sex := ifelse(Sex == "male",1,0)]
# Creating Family size col
full_data[, Family_Size := SibSp + Parch + 1]
# Creating Single,Small,Medium,Large granularity for Family
full_data[,`:=`(
  Single = ifelse(Family_Size == 1, 1,0),
  Small = ifelse(Family_Size == 2, 1,0),
  Medium = ifelse(Family_Size %in% c(3,4), 1,0),
  Large = ifelse(Family_Size > 4, 1,0)
)]
#################################################################### Ticket #####################################################
Extract_Ticket<-c()
for ( i in full_data[,Ticket]){
  if (is.na(as.numeric(i))){
    i<-gsub(" .*$","",i)
    i<-gsub(pattern='((?<![0-9])\\.)|(\\.(?![0-9]))','',i, perl=TRUE)
    i<-gsub("/","",i)
    print(i)
    Extract_Ticket<-c(Extract_Ticket,gsub("/","",i))
  }
  else {
    Extract_Ticket<-c(Extract_Ticket,"X")
  }
  Extract_Ticket
}
# Extracting the tikets and converting to dummies
full_data <- cbind(full_data, Extract_Ticket)
full_data[Extract_Ticket == "SCParis", Extract_Ticket := "SCPARIS"]
full_data <- cbind(full_data, dummy(full_data[, Extract_Ticket], sep = "_Ext_"))

# Creating Cabin_New col
full_data[, Cabin_New := factor(sapply(full_data[, Cabin], function(x) strsplit(x, NULL)[[1]][1]))]
full_data[is.na(Cabin_New),Cabin_New := "X"]
unique(full_data[, Cabin_New])
full_data <- cbind(full_data, dummy(full_data[, Cabin_New], sep = "_Cabin_"))
full_data[grep("NA",full_data$Cabin_New),Cabin_New]
#################################################################### Missing values : Embarked ########################################
full_data[is.na(Fare), Fare :=  median(full_data[Pclass == '3' & Embarked == 'S', Fare], na.rm = TRUE)]
colSums(is.na(full_data))

# https://www.encyclopedia-titanica.org/titanic-survivor/martha-evelyn-stone.html
# The tow persons board in Southampton
full_data[is.na(Embarked), Embarked := "S"]

full_data[Embarked == "S", Embarked := 1]
full_data[Embarked == "C", Embarked := 2]
full_data[Embarked == "Q", Embarked := 3]


full_data <- cbind(full_data, dummy(full_data[, Embarked], sep = "_Embarked_"))

################################################################### Missing values : Age ###############################################
#full_data<-cbind(as.integer(rownames(full_data)),full_data)

for (i in full_data[is.na(Age),PassengerId]){
     full_data[PassengerId == i, Age := median(
       full_data[
        SibSp == full_data[PassengerId == i, SibSp] & 
        Parch == full_data[PassengerId == i, Parch] & 
        Pclass == full_data[PassengerId == i, Pclass] & 
        !is.na(full_data[, Age]), Age]
     )]
}

# Missing Age
# https://www.encyclopedia-titanica.org/titanic-survivor/hannah-riordan.html
full_data[grepl("Riordan, Miss. Johanna", Name), Age := 21]
# https://www.encyclopedia-titanica.org/titanic-victim/hannah-naughton.html
full_data[grepl("Naughton, Miss. Hannah", Name), Age := 21]
# Others
full_data[grepl("Spector, Mr. Woolf", Name), Age := 23]
full_data[grepl("Ware, Mr. Frederick", Name), Age := 35]
full_data[grepl("Peter, Master. Michael J", Name), Age := 4]
full_data[grepl("Sage, Mr. John George",Name), Age := 44]
full_data[grepl("Sage, Mrs. John",Name), Age := 44]

full_data[, Pclass := as.integer(Pclass)]
full_data[, Sex := as.integer(Sex)]
full_data[, Embarked := as.integer(Embarked)]
full_data[, Cabin_New := as.factor(Cabin_New)]
colSums(is.na(full_data))


# Reenconding
noFactorsCols <- c(
  "PassengerId","Name", "Age", "SibSp", "Parch", "Ticket",
  "Fare", "Cabin", "Extract_Ticket", "Family_Size",
  "Single", "Small", "Medium", "Large"
)
factorCols <- setdiff(names(full_data), noFactorsCols)
full_data[,(factorCols) := lapply(.SD,as.factor),.SDcols = factorCols]

train <- full_data[1:880,]
test <- full_data[881:1298,]

str(train)

# Embarked has been commented because the error rate performs better
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare 
                           + Family_Size +
                           + Titre
                           + full_data_Titre_Master + full_data_Titre_Miss + full_data_Titre_Mr + full_data_Titre_Mrs
                           + Single + Small + Medium + Large
                           #+ Embarked
                           #+ full_data_Embarked_1 + full_data_Embarked_2 + full_data_Embarked_3 
                           + full_data_Cabin_A + full_data_Cabin_B + full_data_Cabin_C + full_data_Cabin_D
                           + full_data_Cabin_E + full_data_Cabin_F + full_data_Cabin_G + full_data_Cabin_T
                           + full_data_Cabin_X
                           + full_data_Ext_A + full_data_Ext_A4 + full_data_Ext_A5 + full_data_Ext_AQ3 + full_data_Ext_AQ4
                           + full_data_Ext_AS + full_data_Ext_C + full_data_Ext_CA + full_data_Ext_CASOTON
                           + full_data_Ext_Fa + full_data_Ext_FC + full_data_Ext_FCC + full_data_Ext_LINE 
                           + full_data_Ext_LP + full_data_Ext_PC + full_data_Ext_PP + full_data_Ext_PPP
                           + full_data_Ext_SC + full_data_Ext_SCA3 + full_data_Ext_SCA4 + full_data_Ext_SCAH + full_data_Ext_SCOW 
                           + full_data_Ext_SCPARIS + full_data_Ext_SOC + full_data_Ext_SOP + full_data_Ext_SOPP
                           + full_data_Ext_SOTONO2 + full_data_Ext_SOTONOQ + full_data_Ext_SP + full_data_Ext_STONO + full_data_Ext_STONO2
                           + full_data_Ext_STONOQ + full_data_Ext_SWPP + full_data_Ext_WC + full_data_Ext_WEP + full_data_Ext_X
                           ,data=train)
                           
print(rf_model)                            
print("Survived:0 = No, 1 = Yes")
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test[, PassengerId], Survived = prediction)
write.csv(solution, file = here("titanicdata/rf_mod_Solution_1.csv"), row.names = F)

quit(save = 'no')




