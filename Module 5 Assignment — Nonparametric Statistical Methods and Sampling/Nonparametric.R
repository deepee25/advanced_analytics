#=========================
#Section 13-2
#=========================
#=========================
# Game Attendance
#=========================
#Hypothesis
#Null Hypothesis H0 : median = 3000 (Claim); Median number of paid attendees at 20 local football games is 3000 (CLAIM)
#Alternative Hypothesis H1: median != 3000; Median number of paid attendees at 20 local football games is not equal to 3000
alpha_q6 <- 0.05
median_q6 <- 3000
game_attendees <- c(6210, 3150, 2700, 3012, 4875, 
                    3540, 6127, 2581, 2642, 2573, 
                    2792, 2800, 2500, 3700, 6030, 
                    5437, 2758, 3490, 2851, 2720)
diff_q6 <- game_attendees - median_q6
positives_q6 <- length(diff_q6[diff_q6>0])
positives_q6
negatives_q6 <- length(diff_q6[diff_q6<0])
negatives_q6
res_q6 <- binom.test(x = c(positives_q6, negatives_q6), alternative = "two.sided")
res_q6
ifelse(res_q6$p.value > alpha_q6, "Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")
#Conclusion: We do not have enough evidence to conclude that the median number of paid attendees at 20 football games is not 3000.
# As the claim is accurate I would use this figure as a guide to print the programs for the games.

#==========================
# Lottery Ticket Sales
#==========================
#Hypothesis
#Null Hypothesis H0 : median = 200 (Claim); Median number of lottery tickets sold = 200 (CLAIM)
#Alternative Hypothesis H1: median < 200; Median number of lottery tickets sold is below 200
alpha_q10 <- 0.05
median_q10 <- 200
positives_q10 <- 25
negatives_q10 <- 15
res_q10 <- binom.test(x = c(positives_q10, negatives_q10), alternative = "less")
res_q10
ifelse(res_q10$p.value > alpha_q10, "Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")
#Conclusion: We do not have enough evidence to conclude that the median number of lottery tickets sold per day is below 200
# Therefore, we cannot reject the claim of selling 200 lottery tickets a day made by the outlet owner.

#============================
#Section 13-3
#============================
#============================
# 4. Lengths of Prison Sentences
#============================
#Hypothesis

alpha_q4 = 0.05

length_of_prison_males_q4 = c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
length_of_prison_females_q4 = c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

res_q4 <- wilcox.test(x = length_of_prison_males_q4, y = length_of_prison_females_q4, alternative = "two.sided", correct = FALSE)
res_q4

pValue_q4 = res_q4$p.value
ifelse ( pValue_q4 > alpha_q4 ,"Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")

#====================================
# 8.Winning Baseball Games
#====================================
#Hypothessis:
#H0 Null Hypothesis: There is no difference in the no. of games won by NL and AL eastern Divisions (Claim)
#H1 Alternative Hypothesis: There is a difference in the no. of games won by NL and AL eastern Divisions
alpha_q8= 0.05
NL_Wins_East = c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL_Wins_East = c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

res_q8 = wilcox.test(x = NL_Wins_East, y = AL_Wins_East, alternative = "two.sided", correct = FALSE)
res_q8

pValue_q8 = res_q8$p.value
ifelse ( pValue_q8 > alpha_q8 ,"Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")
#Conclusion: We do not have enough evidence to conclude that there is a difference in the no. of games 
#won by NL (National League) and AL (American League) eastern division.

#============================
#Section 13-4
#============================
# • ws = 13, n = 15, α = 0.01, two-tailed
ws_q5 <- 13
critical_value_q5_table <- 16 # From Table K
criticalValue_q5_qsignrankFunc <- qsignrank(1 - (0.01/2), 15, lower.tail = FALSE)
criticalValue_q5_qsignrankFunc
ifelse ( ws_q5 > criticalValue_q5_qsignrankFunc ,"Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")

# • ws = 32, n = 28, α = 0.025, one-tailed
ws_q6 <- 32
critical_value_q6_table <- 117 # From Table K
criticalValue_q6_qsignrankFunc <- qsignrank(0.025, 28, lower.tail = TRUE)
criticalValue_q6_qsignrankFunc
ifelse ( ws_q6 > criticalValue_q6_qsignrankFunc ,"Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")

# • ws = 65, n = 20, α = 0.05, one-tailed
ws_q7 <- 65
critical_value_q7_table <- 60 # From Table K
criticalValue_q7_qsignrankFunc <- qsignrank(0.05, 20, lower.tail = TRUE)
criticalValue_q7_qsignrankFunc
ifelse ( ws_q7 > criticalValue_q7_qsignrankFunc ,"Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")

# • ws = 22, n = 14, α = 0.10, two-tailed
ws_q8 <- 22
critical_value_q8_table <- 26 # From Table K
criticalValue_q8_qsignrankFunc <- qsignrank(1 - (0.10/2), 14, lower.tail = FALSE)
criticalValue_q8_qsignrankFunc
ifelse ( ws_q8 > criticalValue_q8_qsignrankFunc ,"Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")

#============================
#Section 13-5
#============================
#============================
# 2. Mathematics Literacy Scores
#============================
#Hypothesis
#Null Hypothesis H0 : There is no difference in the means of mathematics literacy scores between the 3 regions 
#Alternative Hypothesis H1: There is a difference in the means of mathematics literacy scores between the 3 regions 

alpha_q2 <- 0.05
west_Hemisphere_q2 = data.frame(score = c(527, 406, 474, 381, 411), region = rep("Western Hemisphere",5))
euro_q2 = data.frame(score = c(520, 510, 513, 548, 496), region = rep("Europe",5))
east_asia_q2 = data.frame(score = c(523, 547, 547, 391, 549), region = rep("Eastern Asia",5))

data_q2 = rbind(west_Hemisphere_q2, euro_q2, east_asia_q2)

res_q2 <- kruskal.test(score ~ region, data = data_q2)
res_q2

res_q2$p.value

ifelse(res_q2$p.value > alpha_q2, "Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")
#Conclusion: We do not have enough evidence to conclude that there is a difference in the means of mathematics 
#literacy score between the three regions Western Hemisphere, Europe, and Eastern Asia

#============================
#Section 13-6
#============================
#=====================================
# Subway and Commuter Rail Passengers
#=====================================
#Hypothesis
#Null Hypothesis H0 : There is no correlation between the no. of daily passenger for commuter rail and subway service
#Alternative Hypothesis H1: There is a correlation between the no. of daily passenger for commuter rail and subway service
alpha_qSC <- 0.05
cit_vec <- c(1, 2, 3, 4, 5, 6)
subwa_vec <- c(845, 494, 425, 313, 108, 41)
com_rail_vec <- c(39, 291, 142, 103, 33, 38)

data <- data.frame(City = cit_vec, Subway_services = subwa_vec, Commuter_rail_services = com_rail_vec)

res_SC_q13_6 <- cor.test(data$Subway_services, data$Commuter_rail_services, method = "spearman")
res_SC_q13_6

res_SC_q13_6$p.value
res_SC_q13_6$estimate # correlation Coefficient
ifelse(res_SC_q13_6$p.value > alpha_qSC, "Failed to Reject H0 (Null Hypothesis)", "Reject H0 (Null Hypothesis)")


# Based on the Spearman's rank correlation coefficient test results, with a p-value of 0.2417, which is greater 
# than the significance level of 0.05, we fail to reject the null hypothesis. Therefore, we do not have enough 
# evidence to conclude that there is a correlation between the number of daily passenger trips for subways and 
# commuter rail service.
# 
# One reason why the transportation authority might use the results of this study is to understand the relationship 
# between subway and commuter rail services in terms of passenger demand. This information could be used to optimize 
# resources, schedule services more efficiently, or allocate resources based on the specific needs of each mode of 
# transportation.

#============================
#Section 14-3 -1
#============================
set.seed(20353)

roll_until_all_faces_appear_q14_3_1 <- function() {
  facesSeen <- numeric(6)
  numRolls <- 0
  while (sum(facesSeen < 1) > 0) {
    face <- sample(1:6, 1)
    facesSeen[face] <- 1
    numRolls <- numRolls + 1
  }
  return(numRolls)
}

numSimulations_q14_3_1 <- 1000
res_q14_3_1 <- replicate(numSimulations_q14_3_1, roll_until_all_faces_appear_q14_3_1())

experimentalAverage_q14_3_1 <- mean(res_q14_3_1)
cat("Experimental -> Average Number of Tosses =", round(experimentalAverage_q14_3_1, 1), "\n")

theoreticalAverage_q14_3_1 <- 6*(1 + 1/2 + 1/3 + 1/4 + 1/5 + 1/6)
cat("Theoretical -> Average Number of Tosses:", round(theoreticalAverage_q14_3_1, 1), "\n")

#============================
#Section 14-3 -2
#============================
set.seed(20353)
numSimulations_q14_3_2 <- 5000
shoot_q14_3_2 <- function() {
  while(TRUE){
    
    if (runif(1) < 0.6) {
      totalShots_q14_3 <- totalShots_q14_3 + 1
      return("Alice")
    }
    if (runif(1) < 0.8) {
      totalShots_q14_3 <- totalShots_q14_3 + 2
      return("Bob")
    }
  }
}

res_q14_3_2 <- replicate(numSimulations_q14_3_2, shoot_q14_3_2())

aliceWins_q14_3_2 <- length(res_q14_3_2[res_q14_3_2 == "Alice"])
bobWins_q14_3_2 <- length(res_q14_3_2[res_q14_3_2 == "Bob"])

expProbability_aliceWin_q14_3_2 <- round(aliceWins_q14_3_2 / numSimulations_q14_3_2 * 100, 2)
expProbability_bobWin_q14_3_2 <- round(bobWins_q14_3_2 / numSimulations_q14_3_2 * 100, 2)

theoreticalProbability_aliceWin_q14_3_2 <- 0.6
theoreticalProbability_bobWin_q14_3_2 <- 0.4 * 0.8

cat("Experimental Probability --> Alice Wins: ", expProbability_aliceWin_q14_3_2, "%\n")
cat("Experimental Probability --> Bob Wins: ", expProbability_bobWin_q14_3_2, "%\n")

cat("Theoretical Probability --> Alice Wins: ", theoreticalProbability_aliceWin_q14_3_2 * 100, "%\n")
cat("Theoretical Probability --> Bob Wins: ", theoreticalProbability_bobWin_q14_3_2 * 100, "%")


#Task2
set.seed(20353)
shoot_q14_3 <- function() {
  while(TRUE){
    if (runif(1) < 0.6) {
      return(1) 
    }
    if (runif(1) < 0.8) {
      return(2)
    } 
  }
}

totalShots_q14_3 <- 0
for (i in 1:numSimulations_q14_3_2) {
  totalShots_q14_3 <- totalShots_q14_3 + shoot_q14_3()
}
averageShots_q14_3 <- round(totalShots_q14_3 / numSimulations_q14_3_2, 2)
cat("Avg Number of Shots Fired:", averageShots_q14_3)