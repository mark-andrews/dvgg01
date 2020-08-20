library(tidyverse)

# make a small data frame
data_df <- tibble(var1 = c(1, 3, 7, 11),
                  var2 = c(5, 7, 10, 12),
                  var3 = c('a', 'b', 'a', 'b')
)

# A set of points
ggplot(data_df,
       mapping = aes(x = var1, y = var2)
) + geom_point()

# A line plot
ggplot(data_df,
       mapping = aes(x = var1, y = var2)
) + geom_line()

# A line plot with points on top
ggplot(data_df,
       mapping = aes(x = var1, y = var2)
) + geom_line() + geom_point()

# A set of points, with a colour mapping
ggplot(data_df,
       mapping = aes(x = var1, y = var2, colour = var3)
) + geom_point()

# A set of points, with a shape mapping
ggplot(data_df,
       mapping = aes(x = var1, y = var2, shape = var3)
) + geom_point()

# A set of points, with a shape mapping
# and change the size
ggplot(data_df,
       mapping = aes(x = var1, y = var2, shape = var3)
) + geom_point(size = 2)

# A more succint version of the above
ggplot(data_df,
       aes(var1, var2, shape = var3)
) + geom_point(size = 2)


# Alternative using the "pipe" (%>%)
data_df %>% ggplot(
  aes(var1, var2, shape = var3)
) + geom_point(size = 2)

# Another variant is this
ggplot(data_df) + 
  geom_point(mapping = aes(x = var1, y = var2)) 

ggplot(data_df,
       mapping = aes(x = var1, y = var2, colour = var3)
) + geom_point(mapping = aes(colour = var4))


# Load up the weight.csv 
weight_df <- read_csv('data/weight.csv')


# Histogram of height
ggplot(weight_df,
       mapping = aes(x = height)
) + geom_histogram(binwidth = 2)



# Histogram of height by gender (stacked)
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_histogram(binwidth = 2, colour = 'white')

# Histogram of height by gender (filled)
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_histogram(binwidth = 2, colour = 'white', position = 'fill')


# Histogram of height by gender (dodge)
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_histogram(binwidth = 2, colour = 'white', position = 'dodge')


# Histogram of height by gender (identity)
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_histogram(binwidth = 2, colour = 'white', position = 'identity')

# Histogram of height by gender (identity)
ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_histogram(binwidth = 2, fill = NA, position = 'identity')


# Histogram of height by gender (identity)
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_histogram(binwidth = 2, colour = 'white', position = 'identity', alpha = 0.5)



# Barplots ----------------------------------------------------------------

# we will use Titanic

titanic_df <- read_csv('data/TitanicSurvival.csv')

# bar plot of passenger class frequency
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)
) + geom_bar(position = 'stack')

# bar plot of passenger class frequency
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)
) + geom_bar(position = 'fill')

# bar plot of passenger class frequency
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)
) + geom_bar(position = 'dodge')

# bar plot of passenger class frequency
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)
) + geom_bar(position = 'identity')

ggplot(titanic_df,
       mapping = aes(x = passengerClass, colour = survived, fill = survived)
) + geom_bar(position = 'identity', alpha = 0.75)

# Make bar plot with predefined heights
carprices_df <- read_csv("data/carprice.csv")

carprices_avg <- group_by(carprices_df, Type) %>% 
  summarize(price = mean(Price))

ggplot(carprices_avg,
       mapping = aes(x = Type, y = price)
) + geom_bar(stat = 'identity')

ggplot(carprices_avg,
       mapping = aes(x = Type, y = price)
) + geom_col()



# Barplot (stat = identity) with another variable
fatrats_df <- read_csv('data/FatRats.csv')

# stacked barplot by default
ggplot(fatrats_df,
       aes(x = Source, y = Gain, fill = Protein)
) + geom_col()

# stacked barplot; position dodge
ggplot(fatrats_df,
       aes(x = Source, y = Gain, fill = Protein)
) + geom_col(position = 'dodge')

# stacked barplot; position identity
ggplot(fatrats_df,
       aes(x = Source, y = Gain, fill = Protein)
) + geom_col(position = 'identity', alpha = 0.5)


# Boxplots 

swiss_df <- swiss %>% rownames_to_column('province') %>%
  mutate(is_catholic = Catholic > 50)

# Tukey boxplot showing distribution of Fertility
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1)

# Tukey boxplot showing distribution of Fertility (flipped)
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1) 
  coord_flip()


# Tukey boxplot showing distribution of Fertility with points
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1) +
  geom_point() +
  coord_flip()


# Tukey boxplot showing distribution of Fertility with jittered points
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1) +
  geom_jitter(width = 0.025) +
  coord_flip()

# Tukey boxplot showing distribution of Fertility with jittered points
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.025) +
  coord_flip()

# Tukey boxplot showing distribution of Fertility with rug plot
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_rug(sides = 'l') +
  coord_flip()


# Tukey boxplot showing distribution of Fertility with rug plot
ggplot(swiss_df,
       mapping = aes(x = '', y = Fertility)
) + geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_rug(sides = 'l') +
  geom_jitter(width = 0.025) +
  coord_flip()


# Using another variable
# Tukey boxplot showing distribution of Fertility 
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility)
) + geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.025, size = 0.5)


# Using another variable
# Tukey boxplot showing distribution of Fertility, coloured boxes
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility, colour = is_catholic)
) + geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.025, size = 0.5)


# Using another variable
# Tukey boxplot showing distribution of Fertility, coloured boxes, coloured points
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility, fill = is_catholic)
) + geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(mapping = aes(colour = is_catholic),
              width = 0.025, size = 0.5)

# width scaled by sample size
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility, fill = is_catholic)
) + geom_boxplot(width = 0.1, outlier.shape = NA, varwidth = T) +
  geom_jitter(width = 0.025, size = 0.5)


# width scaled by sample size and add notch
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility, fill = is_catholic)
) + geom_boxplot(width = 0.1, outlier.shape = NA, varwidth = T, notch = T) +
  geom_jitter(width = 0.025, size = 0.5)

# boxplot with grouping variables
ToothGrowth %>% 
  mutate(dose = factor(dose)) %>% 
  ggplot(aes(x = dose, y = len, colour = supp)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(), size = 0.75)



# Scatterplots ------------------------------------------------------------

ggplot(weight_df,
       mapping = aes(x = height, y = weight)
) + geom_point(size = 0.5, colour = 'red')


# scatterplot by gender
ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)
) + geom_point(alpha = 0.6)

# scatterplot by gender with rug
ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)
) + geom_point(alpha = 0.6) +
  geom_rug(alpha = 0.35, size = 0.1)

# To do marginal plots, we use ggExtra
library(ggExtra)

p1 <- ggplot(weight_df,
             mapping = aes(x = height, y = weight, colour = gender)
) + geom_point(alpha = 0.6)

# a basic marginal scatterplot
ggMarginal(p1)

p2 <- ggplot(weight_df,
             mapping = aes(x = height, y = weight, colour = gender)
) + geom_point(alpha = 0.6) +
  theme_classic() + 
  theme(legend.position = 'bottom')

# margins with histograms
ggMarginal(p2, 
           type = 'histogram',
           groupColour = T,
           groupFill = T,
           position = 'identity',
           alpha = 0.5,
           bins = 50)

# margins with boxplots
ggMarginal(p2, 
           type = 'boxplot',
           groupColour = T,
           groupFill = T,
           position = 'identity',
           alpha = 0.5)


# Smoothing
ggplot(swiss_df,
       aes(x = Examination, y = Fertility)
) + geom_point() +
  stat_smooth(method = 'loess', formula = y ~ x, level = 0.5)

# Smoothing
ggplot(swiss_df,
       aes(x = Examination, y = Fertility)
) + geom_point() +
  stat_smooth(method = 'loess', formula = y ~ x, level = 0.5, n = 5)

# Line of best fit
ggplot(swiss_df,
       aes(x = Examination, y = Fertility)
) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ x, se = F)

# Line of best fit
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic)
) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ x, se = F)


# loees of best fit per group
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic)
) + geom_point() +
  stat_smooth(method = 'loess', formula = y ~ x, se = F)

# Line of best fit per group
# with fullrange
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic)
) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ x, se = F, fullrange = T)



# Line of best fit; one line for all
ggplot(swiss_df,
       aes(x = Examination, y = Fertility)
) + geom_point(aes(colour = is_catholic)) +
  stat_smooth(method = 'lm', formula = y ~ x, se = F, fullrange = T)


# Labels on points
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic, label = province)
) + geom_point() +
  geom_text()

# Labels without points
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic, label = province)
) + geom_text()


# Labels without points
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic, label = province)
) + geom_point(alpha = 0.5, size = 0.5) + geom_text()

# Using ggrepel
library(ggrepel)
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic, label = province)
) + geom_point(alpha = 0.5, size = 0.5) +
  geom_text_repel(size = 5,
                  segment.alpha = 0.5,
                  segment.size = 1)


library(ggrepel)
ggplot(swiss_df,
       aes(x = Examination, y = Fertility, colour = is_catholic, label = province)
) + geom_point(alpha = 0.5, size = 0.5) +
  geom_label_repel(size = 5,
                  segment.alpha = 0.5,
                  segment.size = 1)


# Bubble plot
ggplot(midwest,
       aes(x = percollege, y = percprof, size = poptotal)) + geom_point(alpha = 0.5)


# Bubble plot
ggplot(midwest,
       aes(x = percollege, y = percprof, size = popdensity)) + geom_point(alpha = 0.5)

# Facet plot
sleepstudy_df <- read_csv('data/sleepstudy.csv')

ggplot(sleepstudy_df,
       aes(x = Days, y = Reaction)
) + geom_point()


ggplot(sleepstudy_df,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + stat_smooth(method = 'lm', se = F)

ggplot(sleepstudy_df,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + stat_smooth(method = 'lm', se = F) +
  facet_wrap(~Subject, nrow = 3)

ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)
) + geom_point(alpha = 0.6) + stat_smooth(method = 'lm', se = F) +
  facet_wrap(~gender)


weight_df %>% 
  filter(race <= 3) %>% 
  ggplot(mapping = aes(x = height, y = weight)
  ) + geom_point(alpha = 0.6) + stat_smooth(method = 'lm', se = F) +
  facet_grid(race ~ gender)

