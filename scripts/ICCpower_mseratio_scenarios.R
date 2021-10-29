#check results Wieneke with results extracted from R and later in app?

library(dplyr)
library(Agree)

# MSE / app ratio scenarios.
ref <- simoutput %>% filter(method == "agreement" & k == 3 & n == 20 & deviation == 1 & cor == 0.7) %>% summarise(mse= mean(mse_icc)) %>% unlist()

goal <- simoutput %>% filter(method == "agreement" & k == 3 & n == 50 & deviation == 1 & cor == 0.7) %>% summarise(mse= mean(mse_icc)) %>% unlist()

ref/goal #2.5 times r raters; 8 raters.

#scenario 2: correlation is about 0.8, Currently we have 2 raters for 50 patients,; 3 raters are recommended, how many additional raters do I need?
ref <- simoutput %>% filter(method == "oneway" & k == 5 & n == 10 & deviation == 2 & cor == 0.6 & variance == 1) %>% summarise(mse= mean(mse_icc)) %>% unlist()

goal <- simoutput %>% filter(method == "oneway" & k == 6 & n == 10 & deviation == 2 & cor == 0.6 & variance == 1) %>% summarise(mse= mean(mse_icc)) %>% unlist()

ref
goal
ref/goal #2 times n patients; 100 patients.




#https://www.value-at-risk.net/bias/
#Irrespective of the value of σ, the standard error decreases with the square root of the sample size m.
# Mean squared error (MSE) combines the notions of bias and standard error. It is defined as
#Since we have already determined the bias and standard error of estimator [4.4], calculating its mean squared error is easy: MSE = SE^2 + bias^2 > if bias = 0 MSE = SE^2 ; so SE = sqrt(MSE)
  #Faced with alternative estimators for a given parameter, it is generally reasonable to use the one with the smallest MSE.
#In an analogy to standard deviation, taking the square root of MSE yields the root-mean-square error or root-mean-square deviation (RMSE or RMSD), which has the same units as the quantity being estimated; for an unbiased estimator, the RMSE is the square root of the variance, known as the standard error.
# SE = sd/sqrt(n)


# scenario 3: icc agreement is about 0.7, currently we have 30 patients rated by 4 raters, how close are we to the 3 raters; n = 50 situation - will we need more raters or patients?
ref <- simoutput %>% filter(method == "agreement" & k == 4 & n == 30 & deviation == 1 & cor == 0.7) %>% summarise(icc_e = mean(icc),
                                                                                                                  icc = 0,
                                                                                                                  mse= mean(mse_icc),
                                                                                                                  ciwidth = mean(width_icc)) %>%
  mutate(scenario = "reference")

goal <- simoutput %>% filter(method == "agreement" & k == 3 & n == 50 & deviation == 1 & cor == 0.7) %>%  summarise(icc_e = mean(icc),
                                                                                                                    icc = 0,
                                                                                                                 mse= mean(mse_icc),
                                                                                                                 ciwidth = mean(width_icc)) %>%
  mutate(scenario = "goal")

scenario <- bind_rows(data.frame(scenario = "reference", mse = ref),
                      data.frame(scenario = "goal", mse = goal)) %>%
  mutate(icc = 0,
         SE = sqrt(mse),
         lower = icc - (1.96 * SE),
         upper = icc + (1.96 * SE),
         scenario = factor(scenario, levels = c("reference", "goal")),
         mseratio = ref/goal)

ggplot(scenario, aes(x = scenario, y = icc))+
 # geom_point() +
  geom_line(data = scenario, aes(x = scenario, y = lower, group = 1), lty = "dashed") +
  geom_line(data = scenario, aes(x = scenario, y = upper, group = 1), lty = "dashed") +
  geom_errorbar(aes( x = scenario, ymin = lower, ymax = upper), width = 0.2)+
  ylim(-0.5, 0.5) + ylab("Confidence interval for ICC") +
  annotate(geom= "text", label= paste("MSE ratio = ", round(scenario$mseratio[1],2)), x = 2.2, y = 0.45)


ref/goal # 1 times n patients - ref is about equal to goal.


ggplot(simoutput, aes(cor, icc, group = method, color = method))+ geom_point()
ggplot(simoutput, aes(cor, mse_icc, group = method, color = method))+ geom_point()


ggplot(simoutput, aes(n, mse_icc, group = method, color = method))+ geom_point()
ggplot(simoutput, aes(n, width_icc, group = method, color = method))+ geom_point()


ggplot(data = simoutput, aes(width_icc, mse_icc, color = factor(deviation))) + geom_point() +
  geom_vline(xintercept = 0.2)

ggplot(data = simoutput, aes(width_icc, mse_icc, color = factor(cor))) + geom_point() +
  geom_vline(xintercept = 0.2)

ggplot(data = simoutput, aes(width_icc, mse_icc, color = factor(method))) + geom_point() +
  geom_vline(xintercept = 0.2)

width0.2 <-  simoutput %>% filter(width_icc <0.2)
with(width0.2, table(k,n))

simoutput %>% filter(width_icc <0.2 & k == 3 & n == 50) # only correlation of 0.8

simoutput %>% filter(width_icc <0.2 & k == 4 & n == 50) # only correlation of 0.8

width0.2 <-  simoutput %>% filter(width_icc <0.25)
with(width0.2, table(k,n))



