# plots en español

# ------- 1 - PARADOJA DE SIMPSON -  ADMISIONES DE BERKELEY Y GÉNERO--------------
# Cargar datos
# Cargar paquete y datos
library(openintro)
data(ucb_admit)

# Inspeccionar el dataset
head(ucb_admit)

# Agregar datos por departamento
library(tidyverse)
ucb_summary <-
  ucb_admit %>%
  group_by(dept,gender,admit) |>
  summarise(n = n()) |>
    pivot_wider(id_cols = c(dept,gender),values_from = n,names_from=admit) |>
  mutate(
    applicants = admitted + rejected ,
    rate = 100*admitted / applicants) |>
  group_by(dept) |>
    mutate(total_applicants = sum(applicants),
           total_admitted = sum(admitted),
           total_rate = 100*total_admitted / total_applicants,
           prop_gender = 100*applicants / total_applicants) |>
  select(-c(admitted,rejected,applicants)) |>
  pivot_wider(names_from = gender,values_from = c(rate,prop_gender))


# Graficar
figura = ucb_summary |>
  ggplot(aes(x =prop_gender_female,y=rate_female))+
  geom_point(aes(size = total_applicants), color = "purple", shape = 1, stroke = 1) +  # círculos huecos
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_size_area(max_size = 10) +
  labs(
    x = "Porcentaje de postulantes mujeres",
    y = "Tasa de admisión (ambos géneros)"
  ) +
  theme_minimal()

ggsave(figura,file="simpson.pdf")







### -- GIF ANIMACIONES ---------
library(tidyverse)
library(gganimate)

#- corUnifn10-1.gif

all_df<-data.frame()
for(sim in 1:10){
  North_pole <- runif(10,1,10)
  South_pole <- runif(10,1,10)
  t_df<-data.frame(simulation=rep(sim,10),
                   North_pole,
                   South_pole)
  all_df<-rbind(all_df,t_df)
}


gif=ggplot(all_df,aes(x=North_pole,y=South_pole))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  ggtitle("Pseudo correlaciones de dos variables aleatorias")+
  labs(x="Polo norte",y="Polo sur")+
  transition_states(
    simulation,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save("imgs/gifs/corUnifn10-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))



## - corUnifFourNs-1_es.gif

all_df<-data.frame()
for(sim in 1:10){
  for(n in c(10,50,100,1000)){
    North_pole <- runif(n,1,10)
    South_pole <- runif(n,1,10)
    t_df<-data.frame(nsize=rep(n,n),
                     simulation=rep(sim,n),
                     North_pole,
                     South_pole)
    all_df<-rbind(all_df,t_df)
  }
}

gif1 = ggplot(all_df,aes(x=North_pole,y=South_pole))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  facet_wrap(~nsize)+
  labs(x="Polo norte",y="Polo sur")+
  transition_states(
    simulation,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')


anim_save("imgs/gifs/corUnifFourNs-1_es.gif", animation = animate(gif1, width = 480, height = 480, fps = 10))

# - corReal

library(MASS)
r<-.7

proportional_permute<-function(x,prop){
  indices<-seq(1:length(x))
  s_indices<-sample(indices)
  n_shuffle<-round(length(x)*prop)
  switch<-sample(indices)
  x[s_indices[1:n_shuffle]]<-x[switch[1:n_shuffle]]
  return(x)
}

all_df<-data.frame()
for(sim in 1:10){
  for(samples in c(10,50,100,1000)){
    #data <- mvrnorm(n=samples, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
    #North_pole <- data[, 1]  # standard normal (mu=0, sd=1)
    #South_pole <- data[, 2]

    North_pole <- runif(samples,1,10)
    South_pole <- proportional_permute(North_pole,.5)+runif(samples,-5,5)

    t_df<-data.frame(nsize=rep(samples,samples),
                     simulation=rep(sim,samples),
                     North_pole,
                     South_pole)
    all_df<-rbind(all_df,t_df)
  }
}

gif = ggplot(all_df,aes(x=North_pole,y=South_pole))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  facet_wrap(~nsize)+
  labs(x="Polo norte",y="Polo sur")+
  transition_states(
    simulation,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save("imgs/gifs/corRealgif-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))


# ----corNormFourNsgif -------------


all_df<-data.frame()
for(sim in 1:10){
  for(n in c(10,50,100,1000)){
    North_pole <- rnorm(n,0,1)
    South_pole <- rnorm(n,0,1)
    t_df<-data.frame(nsize=rep(n,n),
                     simulation=rep(sim,n),
                     North_pole,
                     South_pole)
    all_df<-rbind(all_df,t_df)
  }
}


gif = ggplot(all_df,aes(x=North_pole,y=South_pole))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  facet_wrap(~nsize)+
  transition_states(
    simulation,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save("imgs/gifs/corNormFourNs-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))


#---fig-4sample20unif---------

# Este bloque genera 10 muestras de tamaño 20 tomadas de una distribución uniforme (números del 1 al 10)
a <- round(runif(20 * 10, 1, 10))
df <- data.frame(a, sample = rep(1:10, each = 20))

# Calculamos la media de cada muestra
df2 <- aggregate(a ~ sample, df, mean)

# Añadimos una columna con la media correspondiente a cada observación
df <- cbind(df, mean_loc = rep(df2$a, each = 20))

library(gganimate)

# Creamos una animación con un histograma por muestra y una línea roja indicando la media
gif = ggplot(df, aes(x = a, group = sample, frame = sample)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean_loc, frame = sample), color = "red") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_classic() +
  transition_states(
    sample,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink()

anim_save("imgs/gifs/sampleHistUnif-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))

