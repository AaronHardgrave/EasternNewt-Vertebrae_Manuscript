
#Trunk 1 PCA

T1.ggp<-ggplot(T1.df, aes(x = T1.scores[,1], y = T1.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (38.57%)") +
  ylab("PC2 (10.64%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="T1")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) + 
  theme(legend.position="none")

T1.ggp

ggsave("Figure4_T1.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Trunk 4 PCA

T4.ggp<-ggplot(T4.df, aes(x = T4.scores[,1], y = T4.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (45.48%)") +
  ylab("PC2 (9.87%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="T4")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

T4.ggp

ggsave("Figure4_T4.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Trunk 7 PCA

T7.ggp<-ggplot(T7.df, aes(x = T7.scores[,1], y = T7.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (42.37%)") +
  ylab("PC2 (13.31%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="T7")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

T7.ggp

ggsave("Figure4_T7.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Trunk 10 PCA

T10.ggp<-ggplot(T10.df, aes(x = T10.scores[,1], y = T10.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (39.05%)") +
  ylab("PC2 (18.38%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="T10")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

T10.ggp

ggsave("Figure4_T10.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Trunk 13 PCA

T13.ggp<-ggplot(T13.df, aes(x = T13.scores[,1], y = T13.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (36.61%)") +
  ylab("PC2 (19.34%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="T13")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

T13.ggp

ggsave("Figure4_T13.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Sacral PCA

Sacral.ggp<-ggplot(Sacral.df, aes(x = Sacral.scores[,1], y = Sacral.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (37.76%)") +
  ylab("PC2 (20.11%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="Sacral")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

Sacral.ggp

ggsave("Figure5_Sacral.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)

#Caud1 PCA

Caud1.ggp<-ggplot(Caud1.df, aes(x = Caud1.scores[,1], y = Caud1.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (39.68%)") +
  ylab("PC2 (17.42%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="Caud1")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

Caud1.ggp

ggsave("Figure6_Caud1.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Caud2 PCA

Caud2.ggp<-ggplot(Caud2.df, aes(x = Caud2.scores[,1], y = Caud2.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (40.60%)") +
  ylab("PC2 (14.82%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="Caud2")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

Caud2.ggp

ggsave("Figure6_Caud2.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


#Caud3 PCA

Caud3.ggp<-ggplot(Caud3.df, aes(x = Caud3.scores[,1], y = Caud3.scores[,2], color = Group, show.legend = FALSE)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom="polygon", aes(fill=Group), alpha=0.2, lwd=1, show.legend = FALSE) +
  xlab("PC1 (40.57%)") +
  ylab("PC2 (22.99%)") + 
  scale_color_manual(values = c(Adult = "chartreuse3", "Eft" = "darkorange2",
                                "Juvenile" = "deepskyblue", "Paedomorph" = "darkmagenta")) +
  scale_fill_manual(values = c(Adult = "chartreuse2", "Eft" = "darkorange1",
                               "Juvenile" = "deepskyblue2", "Paedomorph" = "magenta")) +
  theme_light()+
  theme(panel.grid=element_blank(), axis.title.x = element_text(face="bold", size=13), axis.title.y = element_text(face="bold", size=13))+
  labs(title="Caud3")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size =15)) +
  theme(legend.position="none")

Caud3.ggp

ggsave("Figure6_Caud3.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)

