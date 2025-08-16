install.packages("dplyr")
library("dplyr")
df<-read.csv("C:\\Users\\HP\\Documents\\final project\\teen_addiction.csv")
View(df)

# t test
#  Compare Sleep Hours on Weekdays vs Weekends
t.test(df$Sleep_Hours, df$Weekend_Usage_Hours, paired = FALSE)

#check 
if (p_value < 0.05) {
  print("Variances are significantly different. Use Welch's t-test.")
} else {
  print("Variances are not significantly different. Use Student's t-test (var.equal = TRUE).")
}

# f test

# Academic Performance by Social Media Usage Group
df$SM_Group <- ifelse(df$Time_on_Social_Media > 2, "High", "Low")
df$SM_Group <- factor(df$SM_Group)

f_test3 <- var.test(Academic_Performance ~ SM_Group, data = df)
print(f_test3)


if (p_value < 0.05) {
  print(paste("Variance difference is significant. F =", round(f_value, 3)))
} else {
  print(paste("No significant variance difference. F =", round(f_value, 3)))
}

#Chi-Squre
# Is more educational screen time associated with better academic performance?
df <- df %>%
  mutate(EduTime_Group = case_when(
    Time_on_Education == 0 ~ "None",
    Time_on_Education <= 2 ~ "Low",
    TRUE ~ "High"
  ))
df <- df %>%
  mutate(Performance_Group = case_when(
    Academic_Performance >= 8 ~ "High",
    Academic_Performance >= 5 ~ "Medium",
    TRUE ~ "Low"
  ))
table9 <- table(df$EduTime_Group, df$Performance_Group)
chisq.test(table9)

# Use in an if-statement
if (chi_result$p.value < 0.05) {
  print("Significant association between Education Time and Academic Performance.")
} else {
  print("No significant association.")
}


# z test
library(BSDA)
install.packages("BSDA")
#Is the average Anxiety Level different between Gamers and Non-Gamers?
df$Is_Gamer <- ifelse(df$Time_on_Gaming > 0, "Gamer", "Non-Gamer")
group1 <- df$Anxiety_Level[df$Is_Gamer == "Gamer"]
group2 <- df$Anxiety_Level[df$Is_Gamer == "Non-Gamer"]
z.test(x = group1, y = group2, sigma.x = 2, sigma.y = 2, conf.level = 0.95)

#Anova
#Is Depression Level different across Phone Usage Purpose?
df$Phone_Usage_Purpose <- as.factor(df$Phone_Usage_Purpose)
anova4 <- aov(Depression_Level ~ Phone_Usage_Purpose, data = df)
summary(anova4)