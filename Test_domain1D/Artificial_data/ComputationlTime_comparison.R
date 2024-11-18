## analizing computational time
## TODO: add a comparison with pure R function

rm(list=ls())
graphics.off()
cat("\014")

#change here 
dir_w = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series"

load(paste0(dir_w,"/Test_domain1D/Artificial_data/results/ComputationalTime_comparison/times_KO_NoCV.Rdata"))
#just because does not change
times_NoCV[6,2:4] = times_NoCV[6,1]
df = times_NoCV


# Plot of grid dim vs time
df_long <- melt(df,id.vars = NULL, variable.name = "Colonna", value.name = "Valore")
df_long$Riga <- as.numeric(rep(rownames(df), times = ncol(df)))

# Creare una lista di plot, uno per ogni colonna
plots_colonne <- lapply(unique(df_long$Colonna), function(colonna) {
  ggplot(df_long[df_long$Colonna == colonna, ], aes(x = as.factor(Riga), y = Valore)) +
    geom_bar(stat = "identity") +
    ggtitle(paste("Time instants: ", colonna)) +
    labs(x = "Grid dimension", y = "Time [s]") +   # Imposta il nuovo titolo per l'asse x
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = as.character(sort(unique(df_long$Riga))))
})

# Combinare i grafici in una singola immagine
m=grid.arrange(grobs = plots_colonne, ncol = 2)


title = "grid_dim_vs_time"
ggsave(filename = paste0(title,".pdf"),
       plot = m,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/ComputationalTime_comparison",
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)



# Plot of time vs grid dim
df = as.data.frame(t(times_NoCV))
df_long <- melt(df,id.vars = NULL, variable.name = "Colonna", value.name = "Valore")
df_long$Riga <- as.numeric(rep(rownames(df), times = ncol(df)))

# Creare una lista di plot, uno per ogni colonna
plots_colonne <- lapply(unique(df_long$Colonna), function(colonna) {
  ggplot(df_long[df_long$Colonna == colonna, ], aes(x = as.factor(Riga), y = Valore)) +
    geom_bar(stat = "identity") +
    ggtitle(paste("Grid dimension: ", colonna)) +
    labs(x = "Time instants", y = "Time [s]") +   # Imposta il nuovo titolo per l'asse x
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = as.character(sort(unique(df_long$Riga))))
})

# Combinare i grafici in una singola immagine
m=grid.arrange(grobs = plots_colonne, ncol = 2)

title = "time_vs_grid_dim"
ggsave(filename = paste0(title,".pdf"),
       plot = m,
       device = NULL,
       path = "/Users/andreafranzoni/Documents/Politecnico/Magistrale/Tesi/Functional_time_series/Test_domain1D/Artificial_data/results/ComputationalTime_comparison",
       scale = 1,
       width = 14,
       height = 10,
       dpi = 300)
