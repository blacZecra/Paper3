
#--- set colors for plots
cl = c(brewer.pal(9,"Blues")[7], brewer.pal(9,"Blues")[5],brewer.pal(9,"Greens")[7],brewer.pal(9,"Greens")[5])
colRG <- colorRampPalette(c("#A50F15", "#DE2D26", "#FB6A4A", "#FCAE91", "#FEE5D9", 
                            "#FFFFFF", 
                            "#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C"))

#--- Plotting functions
# cirlce plot to display ecosystem function and economic proxies delivery of the 4 cropping systems 
Plot.circle = function(indicators){ 
  
  Cdata <- subset(as.data.frame(summary), system==indicators)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(Cdata$services), ncol(Cdata)) )
  colnames(to_add) <- colnames(Cdata)
  to_add$services <- rep(levels(Cdata$services), each=empty_bar)
  Cdata <- rbind(Cdata, to_add)
  Cdata <- Cdata %>% arrange(services)
  Cdata$id <- seq(1, nrow(Cdata))
  
  # Get the name and the y position of each label
  label_data <- Cdata
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.25) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- Cdata %>% 
    group_by(services) %>% 
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data[1,2] <- max(Cdata$id)
  
  p <- ggplot(Cdata, aes(x=as.factor(id), y=mean, fill=services)) +
    geom_bar(aes(x=as.factor(id), y=mean, fill=services), stat="identity", alpha=0.2) +
    geom_errorbar(aes(ymin=mean, ymax=mean+ci), width=0.05) +
    scale_fill_manual(values=c("PaleGreen2","#C8B9DCFF","#E69F00","#619CFF")) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.05, y = 1.00, xend = 0.2, yend = 1.00), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.1, y = 0.75, xend = 0.2, yend = 0.75), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.2, y = 0.50, xend = 0.2, yend = 0.50), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.3, y = 0.25, xend = 0.2, yend = 0.25), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.4, y = 0.00, xend = 0.15, yend = 0.00), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    #geom_segment(data=grid_data, aes(x = end, y = 0.00, xend = start, yend = 0.00), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = 0, y = c(0.05,0.3, 0.55, 0.8, 1.05), label = c("0.00","0.25", "0.50", "0.75", "1.00"), size=2.5 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=mean, fill=services),colour="black", stat="identity", alpha=0.5) +
    ylim(-0.6,1.5) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=1.1, label=variable, hjust=hjust), color="black", alpha=0.8, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start-1, y = -0.05, xend = end+1, yend = -0.05), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE ) +  
    geom_text(data=base_data, aes(x = 0, y = -0.6, label=indicators), colour = "black", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE) +
    geom_text(data=base_data, aes(x = title + c(-1,1,-1,1), y = c(-0.3,-0.3,-0.3,-0.3), label=c("SUPP","REGU","PROV","ECON")), hjust=c(0,0,1,0.8), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)
  
  p + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
}

# cirlce plot to display ecosystem function and economic proxies delivery of the 4 cropping systems 
Plot.circleCc = function(indicators){ 
  
  Cdata <- subset(as.data.frame(summary), Cc==indicators)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(Cdata$services), ncol(Cdata)) )
  colnames(to_add) <- colnames(Cdata)
  to_add$services <- rep(levels(Cdata$services), each=empty_bar)
  Cdata <- rbind(Cdata, to_add)
  Cdata <- Cdata %>% arrange(services)
  Cdata$id <- seq(1, nrow(Cdata))
  
  # Get the name and the y position of each label
  label_data <- Cdata
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.25) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- Cdata %>% 
    group_by(services) %>% 
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data[1,2] <- max(Cdata$id)
  
  p <- ggplot(Cdata, aes(x=as.factor(id), y=mean, fill=services)) +
    geom_bar(aes(x=as.factor(id), y=mean, fill=services), stat="identity", alpha=0.2) +
    geom_errorbar(aes(ymin=mean, ymax=mean+ci), width=0.05) +
    scale_fill_manual(values=c("PaleGreen2","#C8B9DCFF","#E69F00","#619CFF")) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.05, y = 1.00, xend = 0.2, yend = 1.00), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.1, y = 0.75, xend = 0.2, yend = 0.75), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.2, y = 0.50, xend = 0.2, yend = 0.50), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.3, y = 0.25, xend = 0.2, yend = 0.25), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.4, y = 0.00, xend = 0.15, yend = 0.00), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    #geom_segment(data=grid_data, aes(x = end, y = 0.00, xend = start, yend = 0.00), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = 0, y = c(0.05,0.3, 0.55, 0.8, 1.05), label = c("0.00","0.25", "0.50", "0.75", "1.00"), size=2.5 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=mean, fill=services),colour="black", stat="identity", alpha=0.5) +
    ylim(-0.6,1.5) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=1.1, label=variable, hjust=hjust), color="black", alpha=0.8, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start-1, y = -0.05, xend = end+1, yend = -0.05), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE ) +  
    geom_text(data=base_data, aes(x = 0, y = -0.6, label=indicators), colour = "black", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE) +
    geom_text(data=base_data, aes(x = title + c(-1,1,-1,1), y = c(-0.3,-0.3,-0.3,-0.3), label=c("cluster1","cluster2","cluster3","cluster4")), hjust=c(0,0,1,0.8), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)
  
  p + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
}

# cirlce plot to display ecosystem function and economic proxies delivery of the 4 cropping systems 
Plot.circleCs = function(indicators){ 
  
  Cdata <- subset(as.data.frame(summary), Cs==indicators)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(Cdata$services), ncol(Cdata)) )
  colnames(to_add) <- colnames(Cdata)
  to_add$services <- rep(levels(Cdata$services), each=empty_bar)
  Cdata <- rbind(Cdata, to_add)
  Cdata <- Cdata %>% arrange(services)
  Cdata$id <- seq(1, nrow(Cdata))
  
  # Get the name and the y position of each label
  label_data <- Cdata
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.25) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- Cdata %>% 
    group_by(services) %>% 
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data[1,2] <- max(Cdata$id)
  
  p <- ggplot(Cdata, aes(x=as.factor(id), y=mean, fill=services)) +
    geom_bar(aes(x=as.factor(id), y=mean, fill=services), stat="identity", alpha=0.2) +
    geom_errorbar(aes(ymin=mean, ymax=mean+ci), width=0.05) +
    scale_fill_manual(values=c("PaleGreen2","#C8B9DCFF","#E69F00","#619CFF")) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.05, y = 1.00, xend = 0.2, yend = 1.00), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.1, y = 0.75, xend = 0.2, yend = 0.75), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.2, y = 0.50, xend = 0.2, yend = 0.50), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.3, y = 0.25, xend = 0.2, yend = 0.25), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = max(Cdata$id)-0.4, y = 0.00, xend = 0.15, yend = 0.00), colour = "grey70", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    #geom_segment(data=grid_data, aes(x = end, y = 0.00, xend = start, yend = 0.00), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = 0, y = c(0.05,0.3, 0.55, 0.8, 1.05), label = c("0.00","0.25", "0.50", "0.75", "1.00"), size=2.5 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=mean, fill=services),colour="black", stat="identity", alpha=0.5) +
    ylim(-0.6,1.5) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=1.1, label=variable, hjust=hjust), color="black", alpha=0.8, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start-1, y = -0.05, xend = end+1, yend = -0.05), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE ) +  
    geom_text(data=base_data, aes(x = 0, y = -0.6, label=indicators), colour = "black", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE) +
    geom_text(data=base_data, aes(x = title + c(-1,1,-1,1), y = c(-0.3,-0.3,-0.3,-0.3), label=c("cluster1","cluster2","cluster3","cluster4")), hjust=c(0,0,1,0.8), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)
  
  p + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
}

# boxplot for Proxies, Goods, Categories and EMF indexes (scaled 0-1)
Plot.box.EMF = function(indicators, ylim){ 
  for (i in indicators){
   
      plot(Zdata$system, Zdata[,i], xaxt="n", yaxt="n", border="grey30", col=cl, ylim=ylim, xlab="", ylab="")
      axis(side=1, at=1:4, labels=levels(Zdata$system), tck=0, padj=-0.5, cex.axis=1.5)
      axis(side=2, tck=0.02, padj=1, cex.axis=1.5)
      mtext(side=2,i, padj=-2, cex=1.25)
      
      S = Zdata$system
      y = Zdata[,i]
      m = lm(y ~ S)
      Fval = anova(m)["S", "F value"]
      Fval = sprintf("%.2f", Fval)
      Pval = anova(m)["S", "Pr(>F)"]
      Pval = ifelse(Pval < 0.001, "< 0.001", paste(" = ", sprintf("%.2f", Pval)) )
      DF = anova(m)["Residuals", "Df"]
      mtext(bquote(paste(italic("F")[paste("3,", .(DF))], " = ", .(Fval), italic(", P "), .(Pval))), cex=0.8)
      
      l = cld(emmeans(m, ~ S),Letters=letters, sort=F, adjust="n")[,".group"]
      l = gsub(" " , "", l)
      
      r  = max(Zdata[,i], na.rm=T) - min(Zdata[,i], na.rm=T)
      yi = aggregate(y, list(S), quantile, na.rm=T)[,2][,"75%"]
      text(1:4+0.3, yi+0.1, l, font=2, cex=1.25)
      
  }
}

# boxplot for Proxies, Goods, Categories and EMF indexes (scaled 0-1)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Barplot to display regression estimates (correlation coefficients) between selected variables, proxies and goods
Para.corr = function(var,para){
  LCL=UCL=COR=PVL=NULL
  for (i in para){
    Test = cor.test(Zdata[,var], Zdata[,i])
    LCL = c(LCL, Test$conf[1])
    UCL = c(UCL, Test$conf[2])
    COR = c(COR, Test$estimate) # for r2 add: ^2
    PVL = c(PVL, Test$p.value)
  }
barplot <- barplot(sort(COR, decreasing=T), border=NA, tck=0.02 ,mgp=c(1.5, 0.1,0), cex.axis=1, ylim= c(ifelse(min(COR)>0,-0.2,round(min(COR),digits=1)-0.1),round(max(COR),digits=1)+0.2), # c(round(min(COR),digits=1)-0.1,round(max(COR),digits=1)+0.2)
                   names="", ylab="regression estimate",main=var, cex.lab=1.1)
           text(x = barplot, -0.02, para[order(COR, decreasing=T)],cex=1.1, srt=90, adj = 1)
           text(x = barplot, COR[order(COR, decreasing=T)]+0.1, ifelse(PVL[order(COR, decreasing=T)]<0.05,"*",""), font=2, cex=2)
}

# Scatter plot with linear regression between selected variables, proxies and goods
Plot.reg.EMF = function(x, y){
  
  for (i in y){
    
    Zdata$y = Zdata[,i]	
    
    for (z in x){
      Zdata$x = Zdata[,z]
      
      plot(Zdata$x, Zdata$y, ylab="", xlab="", tck=0.02, mgp=c(1,0.1,0), cex=2, cex.axis=1.1, pch=16, col=cl[as.numeric(Zdata$system)])
      mtext(side=2,i, padj=-2, font=2, cex=1.2)
      mtext(side=1,z, padj=2, font=2, cex=1.2)
      
      m = lm(y~x, data=Zdata)
      
      if (anova(m)["x", "Pr(>F)"] < 0.05){
        xi = seq(min(Zdata$x, na.rm=T), max(Zdata$x, na.rm=T), l=30)
        yi = predict(m, data.frame(x=xi))
        se = predict(m, data.frame(x=xi), se.fit=T)$se.fit
        points(xi, yi, type="l", lwd=2, col="grey30")
        points(xi, yi-1.96*se, type="l", lwd=1, col="grey30", lty=2)
        points(xi, yi+1.96*se, type="l", lwd=1, col="grey30", lty=2)
        
        r2 = sprintf("%.3f",summary(m)$r.squared)
        p = sprintf("%.3f",summary(m)$coef["x", 4])
        mtext(bquote(paste(italic("R")^2, " = ", .(r2), italic(", P = "), .(p) )), cex=0.7)
      }
      
      if (anova(m)["x", "Pr(>F)"] > 0.05){
        r2 = sprintf("%.3f",summary(m)$r.squared)
        p = sprintf("%.3f",summary(m)$coef["x", 4])
        mtext(bquote(paste(italic("R")^2, " = ", .(r2), italic(", P = "), .(p) )), cex=0.7)
        
      }
    }
  }
}

# Plot number of goods performed above continuous threshold
thresplotCI = function(df, sys, col){
  
 df %>% filter(.,system %in% c("C-IT", sys)) %>%
  ggplot(aes(x=percent, y=mean, colour=system, fill=system))+ theme_bw(base_size=22) +   
  geom_ribbon(aes(x=percent, ymin=mean- 1.96*se, ymax=mean+1.96*se))+
  geom_point(size=1) +
  labs(x=expression("Threshold value (%)"), y=expression("Number of Goods" >= Threshold)) +
  scale_colour_manual(values = c(brewer.pal(9,"Blues")[8], col)) + 
  scale_fill_manual(values = alpha(c(brewer.pal(9,"Blues")[8], col), 0.1)) +
  scale_linetype_manual(values = c("solid","dashed","solid","longdash")) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.8,length(fctIdx)+1), breaks = seq(-1000,2000,1)) +
  theme(panel.grid = element_blank(),
        axis.title.x=element_text(face="italic",size=12, vjust=-0.5),
        axis.title.y=element_text(face="plain",size=12, vjust=2),
        axis.text = element_text(size=12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=10), legend.key = element_blank(),
        legend.background = element_blank(),legend.direction="vertical",
        legend.key.width = unit(0.5, "cm"),
        legend.spacing.x = unit(0.3, "cm"),
        legend.position = c(0.1,0.2)) 
}

# Plot number of goods performed above continuous threshold
thresplotCs = function(df, col){
  
  df %>% filter(.,Cs %in% c("intercropping","monoculture")) %>%
    ggplot(aes(x=percent, y=mean, colour=Cs, fill=Cs))+ theme_bw(base_size=22) +   
    geom_ribbon(aes(x=percent, ymin=mean- 1.96*se, ymax=mean+1.96*se))+
    geom_point(size=1) +
    labs(x=expression("Threshold value (%)"), y=expression("Number of Functions" >= Threshold)) +
    scale_colour_manual(values = c(brewer.pal(9,"Blues")[8], col)) + 
    scale_fill_manual(values = alpha(c(brewer.pal(9,"Blues")[8], col), 0.1)) +
    scale_linetype_manual(values = c("solid","dashed","solid","longdash")) +
    scale_y_continuous(expand=c(0,0), limits = c(-0.8,length(fctIdx)+1), breaks = seq(-1000,2000,1)) +
    theme(panel.grid = element_blank(),
          axis.title.x=element_text(face="italic",size=12, vjust=-0.5),
          axis.title.y=element_text(face="plain",size=12, vjust=2),
          axis.text = element_text(size=12))+
    theme(legend.title=element_blank(),legend.text=element_text(size=10), legend.key = element_blank(),
          legend.background = element_blank(),legend.direction="vertical",
          legend.key.width = unit(0.5, "cm"),
          legend.spacing.x = unit(0.3, "cm"),
          legend.position = c(0.8,0.8))
}

thresplotCs = function(df, col){
  
  df %>% filter(.,Cs %in% c("intercropping","monoculture")) %>%
    ggplot(aes(x=percent, y=mean, colour=Cs, fill=Cs))+ theme_bw(base_size=22) +   
    geom_ribbon(aes(x=percent, ymin=mean- 1.96*se, ymax=mean+1.96*se))+
    geom_point(size=1) +
    labs(x=expression("Threshold value (%)"), y=expression("Number of Functions" >= Threshold)) +
    scale_colour_manual(values = c(brewer.pal(9,"Blues")[8], col)) + 
    scale_fill_manual(values = alpha(c(brewer.pal(9,"Blues")[8], col), 0.1)) +
    scale_linetype_manual(values = c("solid","dashed","solid","longdash")) +
    scale_y_continuous(expand=c(0,0), limits = c(-0.8,length(fctIdx)+1), breaks = seq(-1000,2000,1)) +
    theme(panel.grid = element_blank(),
          axis.title.x=element_text(face="italic",size=12, vjust=-0.5),
          axis.title.y=element_text(face="plain",size=12, vjust=2),
          axis.text = element_text(size=12))+
    theme(legend.title=element_blank(),legend.text=element_text(size=10), legend.key = element_blank(),
          legend.background = element_blank(),legend.direction="vertical",
          legend.key.width = unit(0.5, "cm"),
          legend.spacing.x = unit(0.3, "cm"),
          legend.position = c(0.8,0.8))
}

# Plot number of goods performed above continuous threshold
thresplotChangeLegend = function(df, col, legend1, legend2){
  
  df %>% filter(.,Cs %in% c(legend1,legend2)) %>%
    ggplot(aes(x=percent, y=mean, colour=Cs, fill=Cs))+ theme_bw(base_size=22) +   
    geom_ribbon(aes(x=percent, ymin=mean- 1.96*se, ymax=mean+1.96*se))+
    geom_point(size=1) +
    labs(x=expression("Threshold value (%)"), y=expression("Number of Functions" >= Threshold)) +
    scale_colour_manual(values = c(brewer.pal(9,"Blues")[8], col)) + 
    scale_fill_manual(values = alpha(c(brewer.pal(9,"Blues")[8], col), 0.1)) +
    scale_linetype_manual(values = c("solid","dashed","solid","longdash")) +
    scale_y_continuous(expand=c(0,0), limits = c(-0.8,length(fctIdx)+1), breaks = seq(-1000,2000,1)) +
    theme(panel.grid = element_blank(),
          axis.title.x=element_text(face="italic",size=12, vjust=-0.5),
          axis.title.y=element_text(face="plain",size=12, vjust=2),
          axis.text = element_text(size=12))+
    theme(legend.title=element_blank(),legend.text=element_text(size=10), legend.key = element_blank(),
          legend.background = element_blank(),legend.direction="vertical",
          legend.key.width = unit(0.5, "cm"),
          legend.spacing.x = unit(0.3, "cm"),
          legend.position = c(0.8,0.8))
}
