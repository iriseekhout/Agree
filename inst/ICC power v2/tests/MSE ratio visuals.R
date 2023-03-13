ref <- Agree::simoutput %>%
  filter(method %in% "agreement" &
           k %in% 3 &
           n %in% c(50) &
           deviation %in% 0 &
           cor %in% 0.7 &
           variance %in% 1) %>%
  summarise(
    icc_e = mean(icc),
    icc = 0,
    mse = mean(mse_icc),
    ciwidth = mean(width_icc),
    sem_e = mean(sem),
    sem = 0,
    mse_sem = mean(mse_sem),
    variance = mean(variance)
  ) %>%
  mutate(scenario = "adapted",
         k = 3,
         n = 50)
#target design
goal <-
  Agree::simoutput %>%
  filter(method %in% "agreement" &
           k %in% 3 &
           n %in% 100 &
           deviation %in% 0 &
           cor %in% 0.7 &
           variance %in% 1) %>%
  summarise(
    icc_e = mean(icc),
    icc = 0,
    mse = mean(mse_icc),
    ciwidth = mean(width_icc),
    sem_e = mean(sem),
    sem = 0,
    mse_sem = mean(mse_sem),
    variance = mean(variance)
  ) %>%
  mutate(scenario = "target",
         k = 3,
         n = 100)


scenario_icc <- bind_rows(ref, goal) %>%
  mutate(SE = sqrt(mse),
         lower = icc - (1.96 * SE),
         upper = icc + (1.96 * SE),
         scenario = factor(scenario, levels = c("adapted", "target")),
         mseratio = ref$mse/goal$mse,
         statistic = "ICC",
         yfact = 1)
#added for sem
scenario_sem <- bind_rows(ref, goal) %>%
  mutate(SE = sqrt(mse_sem),
         lower = sem - (1.96 * SE),
         upper = sem + (1.96 * SE),
         scenario = factor(scenario, levels = c("adapted", "target")),
         mseratio = ref$mse_sem/goal$mse_sem,
         statistic = "SEM",
         yfact = sqrt(variance))


##plot for the rater target versus adapted
targetk <- data.frame(target = rep(1, pull(scenario_icc[scenario_icc$scenario == "target","k"])))
adaptedk <- data.frame(adapted =rep(1,pull(scenario_icc[scenario_icc$scenario == "adapted","k"])))
colors <- c("recommended" = "#34495E", "current" = "#73C6B6")

ggplot()+
  geom_dotplot(data= targetk, aes(target, fill = "recommended"), binwidth = 1, dotsize = 2/6,  method = "histodot", color = "black")+
  geom_dotplot(data= adaptedk, aes(adapted, fill ="current"), binwidth = 1, dotsize = 2/6,  method = "histodot", color = "black")+
  scale_fill_manual("",values = colors)+
#  ylim(0,12)+
  coord_flip()+
  theme_void()


##plot for sample size target versus adaptied
targetn <- data.frame(target = rep(1, pull(scenario_icc[scenario_icc$scenario == "target","n"])))
adaptedn <- data.frame(adapted =rep(1,pull(scenario_icc[scenario_icc$scenario == "adapted","n"])))

ggplot()+
  geom_bar(data= targetn, aes(target), stat = "count", fill = "#34495E")+
  geom_bar(data= adaptedn, aes(adapted), stat = "count", fill ="#73C6B6")+
  coord_flip()+
  theme_classic()+
  ylim(0,200)+
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title = element_blank())


scenario <- scenario_icc
ggplot(scenario_icc, aes(x = scenario, y = icc))+
 # geom_line(data = scenario_icc, aes(x = scenario, y = lower, group = 1), lty = "dashed") +
#  geom_line(data = scenario_icc, aes(x = scenario, y = upper, group = 1), lty = "dashed") +
  geom_errorbar(aes( x = scenario, ymin = lower, ymax = upper), width = 0.1)+
  ylim(-(0.6*scenario$yfact[1]), (0.6*scenario$yfact[1])) +
  ylab(paste("Confidence interval for", scenario$statistic[1], sep = " ")) + xlab("")+
  coord_flip()+
  annotate(geom = "text", label = paste("width = ", round(scenario$ciwidth[1],2)), y = 0, x = 1.1)+
  annotate(geom = "text", label = paste("width = ", round(scenario$ciwidth[2],2)), y = 0, x = 2.1)+
  annotate(geom= "text", label= paste("MSE ratio = ", round(scenario$mseratio[1],2)), y = 0.45, x = 1.5)
