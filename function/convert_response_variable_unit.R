convert_response_variable_unit <- function(inDF) {
    
    ### set as data frame
    inDF <- as.data.frame(inDF)
    
    ### remove no values
    inDF <- inDF[complete.cases(inDF$Total_substrate_added_per_unit_soil),]
    inDF <- inDF[complete.cases(inDF$Effect_size_mean_subtraction),]
    
    unique(inDF$Response_variable)
    
    ### look at priming effect only
    outDF1 <- inDF[inDF$Response_variable%in%c("Cumulative priming effect per unit soil", 
                                               "Cumulative CO2 efflux", 
                                               "_Cumulative priming effect per unit soil",
                                               "Cumulative CO2 emission"),]
    
    unique(outDF1$Response_variable_unit)
    
    outDF1 <- outDF1[outDF1$Response_variable_unit%in%c("mg C g-1 soil", 
                                                        "_mg C g-1 soil", 
                                                        "mg g-1 soil",
                                                        "mg C g-1"),]
    
    
    unique(outDF1$Citation)
    
    summary(outDF1$Total_substrate_added_per_unit_soil)
    summary(outDF1$Time_since_substrate_addition)
    
    with(outDF1, plot(Effect_size_mean_subtraction~Total_substrate_added_per_unit_soil))
    with(outDF1, plot(Effect_size_mean_subtraction~Time_since_substrate_addition))
    
    outDF1$Effect_size_mean_subtraction_per_substrate <- with(outDF1, Effect_size_mean_subtraction / Total_substrate_added_per_unit_soil)
    
    with(outDF1, plot(Effect_size_mean_subtraction_per_substrate~Total_substrate_added_per_unit_soil))
    with(outDF1, plot(Effect_size_mean_subtraction_per_substrate~Time_since_substrate_addition))
    
    
    
    outDF1$Effect_size_mean_ratio <- with(outDF1, Response_mean/Control_mean)
    outDF1$Effect_size_SE_ratio <- with(outDF1, sqrt(Response_SE^2 + Control_SE^2))
    outDF1$Effect_size_sample_size_ratio <- outDF1$Response_sample_size
    
    outDF2 <- outDF1[complete.cases(outDF1$Effect_size_mean_ratio),]
    
    
    test1 <- rma.mv(Effect_size_mean_subtraction, Effect_size_SE_subtraction, 
                    mod = ~Total_substrate_added_per_unit_soil+Time_since_substrate_addition+Organic_soil_C,
                    random = ~1|Citation, data=outDF2)
    
    
    
    ### return
    return(outDF2)
    
}