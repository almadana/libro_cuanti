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







# -- GIF ANIMACIONES ---------
library(tidyverse)
library(gganimate)



# cap 3 --------

##- corUnifn10-1.gif

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

## - corReal

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


## ----corNormFourNsgif -------------


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


##---fig-4sample20unif---------

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




# capitulo 4 -------------
##fig-4normalMeanShift-----

some_means<-c(0,1,2,3,4,5,4,3,2,1)
all_df<-data.frame()
for(i in 1:10){
  dnorm_vec <- dnorm(seq(-10,10,.1),mean=some_means[i],sd=1)
  x_range   <- seq(-10,10,.1)
  means <- rep(some_means[i], length(x_range))
  sims <- rep(i, length(x_range))
  t_df<-data.frame(sims,means,x_range,dnorm_vec)
  all_df<-rbind(all_df,t_df)
}

gif = ggplot(all_df, aes(x=x_range,y=dnorm_vec))+
  geom_line()+
  theme_classic()+
  ylab("Densidad de probabilidad")+
  xlab("Valor")+
  ggtitle("Distribución normal con media cambiante")+
  transition_states(
    sims,
    transition_length = 1,
    state_length = 1
  )

anim_save("imgs/gifs/normalMovingMean-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))

#enter_fade() +
#exit_shrink() +
#ease_aes('sine-in-out')

## fig-4normalSDShift ---------

some_sds<-seq(0.5,5,.5)
all_df<-data.frame()
for(i in 1:10){
  dnorm_vec <- dnorm(seq(-10,10,.1),mean=0,sd=some_sds[i])
  x_range   <- seq(-10,10,.1)
  sds <- rep(some_sds[i], length(x_range))
  sims <- rep(i, length(x_range))
  t_df<-data.frame(sims,sds,x_range,dnorm_vec)
  all_df<-rbind(all_df,t_df)
}

labs_df<-data.frame(sims=1:10,
                    sds=as.character(seq(0.5,5,.5)))

gif = ggplot(all_df, aes(x=x_range,y=dnorm_vec, frame=sims))+
  geom_line()+
  theme_classic()+
  ylab("Densidad de probabilidad")+
  xlab("Valor")+
  ggtitle("Distribución normal con desvío cambiante")+
  geom_label(data = labs_df, aes(x = 5, y = .5, label = sds))+
  transition_states(
    sims,
    transition_length = 2,
    state_length = 1
  )+
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save("imgs/gifs/normalMovingSD-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))



## -----fig-4sample20unif------------

a<-round(runif(20*10,1,10))
df<-data.frame(a,sample=rep(1:10,each=20))
df2<-aggregate(a~sample,df,mean)
df<-cbind(df,mean_loc=rep(df2$a,each=20))

library(gganimate)

gif = ggplot(df,aes(x=a, group=sample,frame=sample)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean_loc,frame = sample),color="red")+
  scale_x_continuous(breaks=seq(1,10,1))+
  theme_classic()+
  transition_states(
    sample,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')


anim_save("imgs/gifs/sampleHistUnif-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))


##  fig-4samplingmean ----


get_sampling_means<-function(m,sd,s_size){
  save_means<-length(s_size)
  for(i in 1:s_size){
    save_means[i]<-mean(rnorm(s_size,m,sd))
  }
  return(save_means)
}

all_df<-data.frame()
for(sims in 1:10){
  for(n in c(10,50,100,1000)){
    sample<-rnorm(n,0,1)
    sample_means<-get_sampling_means(0,1,n)
    t_df<-data.frame(sims=rep(sims,n),
                     sample,
                     sample_means,
                     sample_size=rep(n,n),
                     sample_mean=rep(mean(sample),n),
                     sampling_mean=rep(mean(sample_means),n)
    )
    all_df<-rbind(all_df,t_df)
  }
}


gif = ggplot(all_df, aes(x=sample))+
  geom_histogram(aes(y=(..density..)/max(..density..)^.8),color="white",fill="grey")+
  geom_histogram(aes(x=sample_means,y=(..density..)/max(..density..)),fill="blue",color="white",alpha=.5)+
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = 1),
                lwd = .75,
                col = 'red')+
  geom_vline(aes(xintercept=sample_mean,frame=sims),color="red")+
  geom_vline(aes(xintercept=sampling_mean,frame=sims),color="blue")+
  facet_wrap(~sample_size)+xlim(-3,3)+
  theme_classic()+ggtitle("Distribución de la población (rojo), de las muestras (gris), \n y distribución muestral de la media (azul)")+
  ylab("Verosimilitud aproximada")+
  xlab("valor")+
  transition_states(
    sims,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')


anim_save("imgs/gifs/sampleDistNormal-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))

# cap 5 ---------
## fig-5expectedUnif-----
a<-round(runif(20*10,1,10))
df<-data.frame(a,sample=rep(1:10,each=20))



gif = ggplot(df,aes(x=a))+
  geom_histogram(bins=10, color="white")+
  theme_classic()+
  scale_x_continuous(breaks=seq(1,10,1))+
  geom_hline(yintercept=2)+
  ggtitle("Muestras pequeñas (N=20) de una distribución uniforme")+
  labs(y="frecuencia",x="valor")+
  transition_states(
    sample,
    transition_length = 2,
    state_length = 1
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save("imgs/gifs/sampleUnifExpected-1_es.gif", animation = animate(gif, width = 480, height = 480, fps = 10))




## randomizationTestgif -----
study<-round(runif(10,80,100))
no_study<-round(runif(10,40,90))

study_df<-data.frame(student=seq(1:10),study,no_study)
mean_original<-data.frame(IV=c("studied","didnt_study"),
                          means=c(mean(study),mean(no_study)))
t_df<-data.frame(sims=rep(1,20),
                 IV=rep(c("studied","didnt_study"),each=10),
                 values=c(study,no_study),
                 rand_order=rep(c(0,1),each=10))

raw_df<-t_df
for(i in 2:10){
  new_index<-sample(1:20)
  t_df$values<-t_df$values[new_index]
  t_df$rand_order<-t_df$rand_order[new_index]
  t_df$sims<-rep(i,20)
  raw_df<-rbind(raw_df,t_df)
}

raw_df$rand_order<-as.factor(raw_df$rand_order)
rand_df<-aggregate(values~sims*IV,raw_df,mean)
names(rand_df)<-c("sims","IV","means")


a<-ggplot(raw_df,aes(x=IV,y=values,color=rand_order,size=3))+
  geom_point(stat="identity",alpha=.5)+
  geom_point(data=mean_original,aes(x=IV,y=means),stat="identity",shape=21,size=6,color="black",fill="mediumorchid2")+
  geom_point(data=rand_df,aes(x=IV,y=means),stat="identity",shape=21,size=6,color="black",fill="gold")+
  theme_classic(base_size = 15)+
  coord_cartesian(ylim=c(40, 100))+
  theme(legend.position="none") +
  ggtitle("Randomization test: Original Means (purple),
          \n Randomized means (yellow)
          \n Original scores (red,greenish)")+
  transition_states(
    sims,
    transition_length = 1,
    state_length = 2
  )+enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save("imgs/gifs/randomizationTest-1_es.gif", animation = animate(a,nframes = 100, fps = 5))


# cap - chi cuadrado

# distribucion_chi2_animada.gif
# Crear los datos para múltiples distribuciones chi cuadrado
x_vals <- seq(0, 30, length.out = 500)
df_vals <- 1:10

# Generar densidades para cada df
chi_data <- expand.grid(x = x_vals, df = df_vals) |>
  mutate(densidad = dchisq(x, df = df))

# Crear el gráfico animado
p <- ggplot(chi_data, aes(x = x, y = densidad)) +
  geom_line(size = 1.2, color = "steelblue") +
  labs(title = "Distribución χ² con df = {closest_state}",
       x = "Valor χ²", y = "Densidad") +
  theme_minimal(base_size = 14) +
  transition_states(df, transition_length = 1, state_length = 1, wrap = FALSE) +
  ease_aes("linear")

# Guardar como .gif
anim_save("imgs/gifs/distribucion_chi2_animada.gif", p, duration = 10, fps = 5, width = 480, height = 480)


