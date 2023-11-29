[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/VsFGPggi)
library(tidyverse)
library(vegan)

load('sPlotOpen.RData')

str(DT2.oa)
glimpse(CWM_CWV.oa)

dt_sr <- DT2.oa %>% 
  distinct(PlotObservationID, Species) %>% 
  group_by(PlotObservationID) %>% 
  summarise(sr=n())

dt_sr$sr %>% 
  hist()

header_sr <- left_join(header.oa, dt_sr) %>% 
  filter(!is.na(Elevation)) %>% 
  filter(Releve_area < 2500)

str(header_sr)



#costruire il modello

fit1 <- glm(formula = sr~Elevation, data = header_sr, family = poisson)

fit1 %>% 
  summary

library(ggeffects)

fit1 %>% 
  ggeffect() %>% 
  plot


#split sample 

set.seed(1234)   #va messo prima di ogni funzione che randomizza per dirgli che la frazione subcampionata deve essere sempre la stessa. assicura la riproducibilità del codice
sr_training <- sample_frac(header_sr, size= 0.8 )   #campiona una frazione rispetto al totale del dataset

sr_test <- anti_join(header_sr, sr_training)   #prendo l'altro 20%

#identical per validare se i due metodi sono uguali

#se però faccio rigirare il codice, la frazione campionata è diversa. set.seed

#ora cslcolare l rmse

fit2 <- glm(formula = sr~Elevation, data = sr_training)

fit2 <- glm(formula = sr~Elevation, data = sr_training)

sr_pred <- predict(fit2, sr_test)  #la ricchezza è in forma log

min(predict(fit2, sr_test))

library(Metrics)

rmse(sr_test$sr, sr_pred)



####creare un primo modello da zero######

library(palmerpenguins)

data(penguins)

#formulare l'ipotesi. Andrebbe fatto prima di raccogliere il dato ma a volte capita di farlo su dataset già esistenti

#species ~ bill_length
#bill_length ~ body_mass 

hist(penguins$body_mass_g)

hist(penguins$bill_length_mm, breaks = 40)


penguins <- penguins %>% 
  drop_na(bill_length_mm, body_mass_g)

#validiamo il modello con lo split sample

  #va messo prima di ogni funzione che randomizza per dirgli che la frazione subcampionata deve essere sempre la stessa. assicura la riproducibilità del codice

map(1:100, function(x) {

bill_training <- sample_frac(penguins, size= 0.8 )   #campiona una frazione rispetto al totale del dataset

bill_test <- anti_join(penguins, bill_training)

#ora calcolare l rmse

fit_pen <- glm(formula = penguins$bill_length_mm~penguins$body_mass_g, data = bill_training)

summary(fit_pen)

bill_pred <- predict(fit_pen, bill_test)  #la ricchezza è in forma log


library(Metrics)

billrmse <- rmse(bill_test$bill_length_mm, bill_pred)

})

mod <- glm(formula = bill_length_mm ~ body_mass_g, data = bill_training)

mod %>% 
  ggpredict() %>% 
  plot()

pred_sp <- ggpredict(mod, terms = 'body_mass_g')

ggplot(pred_sp, aes(x, predicted))+
  geom_line()+
  geom_point(data = bill_training, aes(x= body_mass_g, y= bill_length_mm))+
  xlab ('body mass (g)')+
  ylab('bill length (mm)')+
  theme_bw()



library(gam)

mod2 <- gam(formula = bill_length_mm~s(body_mass_g, 3) , data = bill_training)

mod2 %>% 
  ggpredict() %>% 
  plot()

mod1 <- glm(formula = bill_length_mm ~ body_mass_g*species, data = bill_training)

mod1 %>% 
  summary()


mod_mix <- lmer(bill_length_mm ~ body_mass_g + (1|species))


#unire il dataset di esplot con extract con un bioclim mondiale a 10 minuti. 

library(raster)
library(sp)

bioclim_world <- getData("worldclim",var="bio",res=10)

plot(bioclim_world)

#ottenere un dataset con bio 1 12 sr y x e elevation

bioclim_global <- stack(bioclim_world)

coord_plot <- header.oa[,9:8]

#sarebbe meglio creare un vettore spaziale con st as sf così posso riscrivere il sistema di riferimento


values <- raster:: extract(bioclim_global, coord_plot)

values <- as.data.frame(values)

(is.na(values))

dataset <- cbind(coord_plot, values)

dataset <- cbind(header_sr, values)

header_sr <- left_join(header.oa, dt_sr) 

dataset <- cbind(header_sr, values)

dataset_completo <- dataset %>% 
  dplyr::select(Latitude, Longitude, bio1, bio12,Elevation, sr)


#fare matrice di correlazione e scaricare il pacchetto ggally



training <- sample_frac(dataset, size= 0.8 )   #campiona una frazione rispetto al totale del dataset

test <- anti_join(dataset,training)

#ora calcolare l rmse

fit_splot <- glm(sr~bio1+bio12, data = training)   #multiglm

summary(fit_splot)

splot_pred <- predict(fit_splot, test) 

library(Metrics)

splot_rmse <- rmse(test$sr, splot_pred)

library(modEvA)




mod_splot <- glm(sr~bio1+bio12, data = dataset)

mod_splot %>% 
  ggpredict() %>% 
  plot()

splot_pred_global <- predict( bioclim_global, fit_splot) 

plot(splot_pred_global)


#modelli differenti, mixed o usando i fattori come covariata









