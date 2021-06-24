calculate_effect_size <- function(inDF) {
    
    
    ### Calculate all the effect size (abs diff and ratio)
    
    ### set as data frame
    inDF <- as.data.frame(inDF)
    
    ### split into those with effect size and those without, based on subtraction
    inDF1 <- subset(inDF, !is.na(Effect_size_mean_subtraction))
    inDF2 <- subset(inDF, is.na(Effect_size_mean_subtraction))
    
    ### calculate subtraction
    inDF2$Effect_size_mean_subtraction <- with(inDF2, Response_mean-Control_mean)
    
    inDF2$Effect_size_SE_subtraction <- with(inDF2, sqrt(Response_SE^2+Control_SE^2))
    
    inDF2$Effect_size_sample_size_subtraction <- inDF2$Response_sample_size
    
    ### combine the dataset
    outDF1 <- rbind(inDF1, inDF2)
    
    ### split into those with effect size and those without, based on subtraction
    inDF1 <- subset(outDF1, !is.na(Effect_size_mean_ratio))
    inDF2 <- subset(outDF1, is.na(Effect_size_mean_ratio))
    
    
    ### calculate response ratio
    inDF2$Effect_size_mean_ratio <- with(inDF2, Response_mean/Control_mean) 
    
    inDF2$Effect_size_SE_ratio <- with(inDF2, sqrt(Response_SE^2+Control_SE^2)) 
    
    inDF2$Effect_size_sample_size_ratio <- inDF2$Response_sample_size
    
    
    #summary(inDF2$Effect_size_mean_ratio)
    #which.min(inDF2$Effect_size_mean_ratio)
    #inDF2[2192,]
    
    ### merge the dataset
    outDF2 <- rbind(inDF1, inDF2)
    
    ### replace inf values
    outDF2[sapply(outDF2, is.infinite)] <- NA
    
    
    ### return
    return(outDF2)
    
}