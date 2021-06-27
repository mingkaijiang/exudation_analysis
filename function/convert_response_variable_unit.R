convert_response_variable_unit <- function(inDF) {
    
    ### set as data frame
    inDF <- as.data.frame(inDF)
    inDF$Microbial_biomass_response_ratio <- as.numeric(inDF$Microbial_biomass_response_ratio)
    
    ### remove no values
    inDF <- inDF[complete.cases(inDF$Total_substrate_added_per_unit_soil),]
    inDF <- inDF[complete.cases(inDF$Effect_size_mean_subtraction),]
    
    unique(inDF$Response_variable)
    
    ### look at cumulative priming effect only
    outDF1 <- inDF[inDF$Response_variable%in%c("Cumulative priming effect per unit soil", 
                                               "Cumulative CO2 efflux", 
                                               "Cumulative CO2 emission"),]
    
    unique(outDF1$Response_variable_unit)
    
    outDF1 <- outDF1[outDF1$Response_variable_unit%in%c("mg C g-1 soil", 
                                                        "mg g-1 soil"),]
    
    
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
    
    with(outDF2, plot(Effect_size_mean_ratio~Total_substrate_added_per_unit_soil))
    with(outDF2, plot(Effect_size_mean_ratio~Time_since_substrate_addition))
    with(outDF2, plot(Effect_size_mean_ratio~Microbial_biomass_response_ratio, ylim=c(0,10)))
    
    
    test1 <- rma.mv(Effect_size_mean_subtraction, Effect_size_SE_subtraction, 
                    mod = ~Total_substrate_added_per_unit_soil+Time_since_substrate_addition,
                    random = ~1|Citation, data=outDF2)
    
    test1
    
    test2 <- rma.mv(Effect_size_mean_ratio, Effect_size_SE_ratio, 
                    mod = ~Total_substrate_added_per_unit_soil+Time_since_substrate_addition+Microbial_biomass_response_ratio,
                    random = ~1|Citation, data=outDF2)
    
    test2
    
    
    ### return
    return(outDF2)
    
}