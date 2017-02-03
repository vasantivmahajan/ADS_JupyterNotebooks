
mydata = read.csv("C:\\Users\\Vasanti\\Desktop\\NEUdocs\\Studymaterial\\ADS\\Inclass_excercise\\DataSheet.csv")
mydata

#1) Min, Max, Median, Avg for GPA and YearofWorkExp
min_gpa <- min(mydata[,2])
min_gpa

max_gpa <- max(mydata[,2])
max_gpa

median_gpa <- median(mydata[,2])
median_gpa

avg_gpa <- mean(mydata[,2]) 
avg_gpa

min_years_of_exp <- min(mydata[,3])
min_years_of_exp

max_years_of_exp <- max(mydata[,3])
max_years_of_exp

median_years_of_exp <- median(mydata[,3])
median_years_of_exp

avg_years_of_exp <- mean(mydata[,3]) 
avg_years_of_exp

#2) Find the mode of the Salary
# mode for expected salary
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
  
mode_exp_salary <- getmode(mydata[,9])
print(mode_exp_salary)

# mode for latest salary

mode_latest_salary <- getmode(mydata[,4])
print(mode_latest_salary)

#3) % of students having Co/op and not having Co/op
#install.packages("plyr", repos='http://cran.us.r-project.org')
library(plyr)
# since yes is written in two forms "y" and "Y", it would consider them different
count(mydata, "Coops.Internships..YN.")

# to convert "y" into "Y"
mydata$Coops.Internships..YN.<- as.character(mydata$Coops.Internships..YN.)
mydata$Coops.Internships..YN.[mydata$Coops.Internships..YN. == "y"] <- "Y"
# it is now tranformed and we would get the count if "Y" and "N"
dataframe <- count(mydata, "Coops.Internships..YN.")
#calculate the total number of instances 
length <- length(mydata$Coops.Internships..YN.)
length
#calcualte the percentage
dataframe$percentage <- with(dataframe,freq/length * 100)
dataframe


#4) No of students with more than 500 LinkedIn contacts

number_of_students_gretaer_than_500_conatcts <-nrow(filter(mydata, mydata$Number.of.contacts.on.Linkedin > 500))
number_of_students_gretaer_than_500_conatcts

#5) Find the Inter Quartile Range for the Expected Salalry Range?
inter_quartile_range = mydata$Expected.Salary.after.graduation
iqr<-IQR(inter_quartile_range)
iqr


