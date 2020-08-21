library(tidyverse)

# Load up the weight.csv 
weight_df <- read_csv('data/weight.csv')

# Freq polygon of height
ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_freqpoly(binwidth = 5)

# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(binwidth = 5, stat = 'bin')


# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(binwidth = 5, stat = 'bin', position = 'fill')

# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(binwidth = 5, stat = 'bin', position = 'identity', alpha = 0.5)

# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(binwidth = 5, stat = 'bin', position = 'dodge', alpha = 0.5)


# Density plots -----------------------------------------------------------

# area plot of height
ggplot(weight_df,
       mapping = aes(x = height)
) + geom_density(adjust = 2)

# area plot of height
ggplot(weight_df,
       mapping = aes(x = height)
) + geom_histogram(bins = 100, colour = 'white')

# area plot of height
ggplot(weight_df,
       mapping = aes(x = height)
) + geom_density()


# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, colour = gender)
) + geom_density(adjust = 2)



# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_density(adjust = 2)


# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_density(adjust = 2, position = 'identity', alpha = 0.5)



# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender, colour = gender)
) + geom_density(adjust = 2, position = 'fill')



# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_density(adjust = 2, position = 'fill', colour = NA)


# line plots --------------------------------------------------------------


library(timetk)
library(lubridate)
library(tidyverse)
nottingham_temp <- tk_tbl(nottem) %>%
        mutate(year = year(index),
               month = month(index)
        )

nottingham_df <- read_csv('data/nottingham_temp.csv')


ggplot(nottingham_df,
       mapping = aes(x = month, y = value)
) + geom_point()

# this line plots will not work
ggplot(nottingham_df,
       mapping = aes(x = month, y = value)
) + geom_point() + geom_line()

# we need to use `group = year`
ggplot(nottingham_df,
       mapping = aes(x = month, y = value, group = year)
) + geom_point() + geom_line()

# we need to use `group = year`, and colour 
ggplot(nottingham_df,
       mapping = aes(x = month, y = value, group = year, colour = factor(year))
) + geom_point() + geom_line()

ggplot(nottingham_df,
       mapping = aes(x = month, y = value, group = year, linetype = factor(year))
) + geom_point() + geom_line()


# Visualizing uncertainty -------------------------------------------------

M <- lm(value ~ poly(month, 2), data = nottingham_df)

df_new <- tibble(month = seq(1, 12, by = 0.1))

predicted_temps <- predict(M, newdata = df_new, interval = 'predict')

predicted_temps_df <- bind_cols(df_new, 
                                as_tibble(predicted_temps))

# point plot of predicted mean
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_point()

# pointrange plot of predicted mean with 95% CI
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_pointrange()


# pointrange plot of predicted mean with 95% CI
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_pointrange() + geom_line()

# linerange plot of predicted mean with 95% CI
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_linerange() + geom_line()

# ribbon plot of predicted mean with 95% CI
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_ribbon(alpha = 0.2) + geom_line()

# multiple line plot of predicted mean with 95% CI
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_line() +
    geom_line(aes(y = lwr), linetype = 3) +
    geom_line(aes(y = upr), linetype = 3)

# multiple line plot of predicted mean with 95% CI
ggplot(predicted_temps_df,
       aes(x = month, y = fit, ymin = lwr, ymax = upr)
) + geom_ribbon(alpha = 0.05) + 
        geom_line() +
        geom_line(aes(y = lwr), linetype = 3) +
        geom_line(aes(y = upr), linetype = 3)


# Heatmaps ----------------------------------------------------------------

ggplot(nottingham_df,
       mapping = aes(x = year, y = month, fill =value)
) + geom_tile() +
        scale_fill_gradient(low = 'yellow', high = 'red')

# Visualize a intercorrelation matrix
bfi_df <- read_csv('data/bfi.csv')
bfi_df <- bfi_df %>% select(A1:O5)
bfi_df %<>% select(A1:O5)

bfi_df %>%
        as.matrix() %>% 
        cor(use = 'complete.obs') %>% 
        as.data.frame() %>%
        rownames_to_column(var = 'item') %>% 
        pivot_longer(cols = -item, names_to = 'item2', values_to = 'r') %>% 
        ggplot(aes(x = item, y = item2, fill = r)) +
        geom_tile() +
        scale_fill_gradient(low = 'white', high = 'steelblue')
        

# Spatial maps ------------------------------------------------------------

mapdata <- read_csv('data/local_authority_map_data.csv.bz2')


ggplot(mapdata,
       mapping = aes(x = long, y = lat, group = group)
) + geom_polygon(colour = 'white', size = 0.1, fill = 'grey50') +
        coord_equal()

referendum_df <- read_csv('data/EU-referendum-result-data.csv')

referendum_mapdata <- inner_join(mapdata, referendum_df, by = c('id' = 'Area_Code'))

ggplot(referendum_mapdata,
       mapping = aes(x = long, y = lat, group = group, fill = Pct_Leave)
) + geom_polygon(colour = 'white', size = 0.1) +
        coord_equal() + 
        theme_classic()


# Fine tuning -------------------------------------------------------------

# changing themes

ggplot(weight_df,
       aes(x = height, y = weight, colour = gender)
) + geom_point() + 
        theme_classic()


library(ggthemes)
ggplot(weight_df,
       aes(x = height, y = weight, colour = gender)
) + geom_point() +
        labs(x = "Height (cm)",
             y = "Weight (cm)") +
        ggtitle("A scatterplot of weight on height by gender")


library(latex2exp)
ggplot(weight_df,
       aes(x = height, y = weight, colour = gender)
) + geom_point() +
        labs(x = TeX("$\\hat{\\beta}}$"),
             y = "Weight (cm)") +
        ggtitle("A scatterplot of weight on height by gender")


ggplot(weight_df,
       aes(x = height, y = weight, colour = gender)
) + geom_point() +
        labs(x = "Height (cm)",
             y = "Weight (cm)") +
        scale_color_manual(values = c("blue", "green"))


               
# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(binwidth = 5, stat = 'bin')  +
        scale_fill_manual(values = c("blue", "green"))



library(ggthemes)
# area plot of height
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)
) + geom_area(binwidth = 5, stat = 'bin')  +
    theme(axis.title.x = element_text(size = 30, colour = 'red'),
          legend.position='bottom',
          legend.title = element_text(size = 3),
          legend.text = element_text(size = 6),
          axis.text = element_blank(),
          axis.ticks = element_blank())

