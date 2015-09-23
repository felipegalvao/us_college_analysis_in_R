# Set the working directory to where the datasets are stored
setwd("C:/Data_Science/US College/CollegeScorecard_Raw_Data")

# Load the required packages
library(readr)
library(ggplot2)
library(plyr)
library(reshape2)
library(R2HTML)

# Read the data. 2005 is the latest year where 
# earnings information is available
college_data <- read_csv("MERGED2005_PP.csv")

# Remove duplicates on the dataframe based on the opeid6 column, since
# earnings and debt informations is the same
college_data <- college_data[!duplicated(college_data[,"opeid6"]),]

# Change NULL and PrivacySuppresed values on column to R NA, then convert
# everything to numeric
college_data$GRAD_DEBT_MDN[college_data$GRAD_DEBT_MDN == "PrivacySuppressed"] <- NA
college_data$GRAD_DEBT_MDN[college_data$GRAD_DEBT_MDN == "NULL"] <- NA
college_data$GRAD_DEBT_MDN <- as.numeric(college_data$GRAD_DEBT_MDN)

# Do the same cleaning for the earnings in 6 years column
college_data$md_earn_wne_p6[college_data$md_earn_wne_p6 == "PrivacySuppressed"] <- NA
college_data$md_earn_wne_p6[college_data$md_earn_wne_p6 == "NULL"] <- NA
college_data$md_earn_wne_p6 <- as.numeric(college_data$md_earn_wne_p6)

# Cleaning for tuition fees
college_data$TUITIONFEE_IN[college_data$TUITIONFEE_IN == "PrivacySuppressed"] <- NA
college_data$TUITIONFEE_IN[college_data$TUITIONFEE_IN == "NULL"] <- NA
college_data$TUITIONFEE_IN <- as.numeric(college_data$TUITIONFEE_IN)

# Create a column on the DF for Earnings to Debt ratio (earnings / debt)
college_data$earnings_debt_ratio <- college_data$md_earn_wne_p6 / college_data$GRAD_DEBT_MDN

# Create column that calculates the number of months necessary to pay the
# average debt with the median earnings, considering that one person
# uses 10% of that earnings to pay the debt
college_data$months_to_pay <- college_data$GRAD_DEBT_MDN / (college_data$md_earn_wne_p6 / 10 / 12)

# Set constant variables about the US average per capita income
# and average student loan debt (debt for 2005, income for 2011 (6 years after entry))
us_per_capita_income <- 49781.4
us_avg_stdnt_loan_debt <- 17233

# Filter the colleges where the earnings average is above 
# and the average debt is lesser than the US average
selected_colleges <- college_data[which(college_data$md_earn_wne_p6 > us_per_capita_income & college_data$GRAD_DEBT_MDN < us_avg_stdnt_loan_debt),]

# Sort the data frame by the ratio column
top_10_selected_by_ratio <- selected_colleges[with(selected_colleges, order(-earnings_debt_ratio)),]
top_10_selected_by_ratio <- top_10_selected_by_ratio[1:10,]
top_10_selected_by_ratio$Institution <- strtrim(top_10_selected_by_ratio$INSTNM, 40)
top_10_selected_by_ratio$Institution <- factor(top_10_selected_by_ratio$Institution, levels=top_10_selected_by_ratio$Institution)

# Plot bar graph with the top 10 colleges by Earnings to Debt Ratio
ggplot(data=top_10_selected_by_ratio, aes(x=Institution, y=earnings_debt_ratio)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# top_10_selected_by_ratio %>%
# ggvis(~Institution, ~earnings_debt_ratio) %>%
#   layer_bars() %>%
#   add_axis("x", title="Institution", title_offset = 180, properties = axis_props(    
# labels = list(angle = 45, align = "left", fontSize = 10)
#   )) %>%
#   add_axis("y", title = "Earnings to debt ratio")

# Export table to HTML
HTMLDir <- "C:/Data_Science/US College/CollegeScorecard_Raw_Data/HTMLData"
HTMLStart(outdir=HTMLDir, file="college_analysis_by_ratio", extension="html", echo=FALSE, HTMLframe=TRUE)
HTML.title("Analysis of US College Data", HR=1)
print(top_10_selected_by_ratio[,c("Institution","md_earn_wne_p6","GRAD_DEBT_MDN","earnings_debt_ratio", "months_to_pay")])
HTMLStop()

# Now select the top ones by earnings
top_10_selected_by_earnings <- selected_colleges[with(selected_colleges, order(-md_earn_wne_p6)),]
top_10_selected_by_earnings <- top_10_selected_by_earnings[1:10,]
top_10_selected_by_earnings$Institution <- strtrim(top_10_selected_by_earnings$INSTNM, 40)
top_10_selected_by_earnings$Institution <- factor(top_10_selected_by_earnings$Institution, levels=top_10_selected_by_earnings$Institution)

# Plot bar graph with the top 10 colleges by earnings
ggplot(data=top_10_selected_by_earnings, aes(x=Institution, y=md_earn_wne_p6)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# top_10_selected_by_earnings %>%
# ggvis(~Institution, ~md_earn_wne_p6) %>%
#   layer_bars() %>%
#   add_axis("x", title="Institution", title_offset = 180, properties = axis_props(    
# labels = list(angle = 45, align = "left", fontSize = 10)
#   )) %>%
#   add_axis("y", title = "Earnings", title_offset = 75)


# Export table to HTML
HTMLDir <- "C:/Data_Science/US College/CollegeScorecard_Raw_Data/HTMLData"
HTMLStart(outdir=HTMLDir, file="college_analysis_by_earnings", extension="html", echo=FALSE, HTMLframe=TRUE)
HTML.title("Analysis of US College Data", HR=1)
print(top_10_selected_by_earnings[,c("Institution","md_earn_wne_p6","GRAD_DEBT_MDN","earnings_debt_ratio", "months_to_pay")])
HTMLStop()

# Plot scatterplot with the correlation between Tuition and Earnings
ggplot(college_data, aes(x=TUITIONFEE_IN, y=md_earn_wne_p6)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)

# college_data %>%
# ggvis(~TUITIONFEE_IN, ~md_earn_wne_p6) %>%
#   layer_points() %>%
#   layer_model_predictions(model = "lm", se = TRUE)

cor_tuition_earnings <- cor(college_data$TUITIONFEE_IN, college_data$md_earn_wne_p6, use="complete.obs")
print(cor_tuition_earnings)