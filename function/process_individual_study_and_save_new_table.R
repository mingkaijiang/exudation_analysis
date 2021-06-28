process_individual_study_and_save_new_table <- function(){
    
    ### read input
    myDF <- read.csv("Data/D1_SubstrateAddition_V1.02.csv",
                      strip.white=T, skip=3, header=F)
    my.header <- read.csv("Data/D1_SubstrateAddition_V1.02.csv",
                           strip.white=T, nrow=1, skip=1, header=F)
    colnames(myDF) <- my.header
    
    ### remove empty rows
    myDF$Citation <- ifelse(myDF$Citation=="", NA, myDF$Citation)
    myDF <- myDF[complete.cases(myDF$Citation),]
    
    ### get dataset
    dataset <- unique(myDF$Citation)
    
    ### get substrate type
    substrate.type <- unique(myDF$Substrate_type); substrate.type
    myDF$Substrate_type <- gsub("_Malate", "Malate", myDF$Substrate_type)
    myDF$Substrate_type <- gsub("_Glucose", "Glucose", myDF$Substrate_type)
    substrate.type <- unique(myDF$Substrate_type); substrate.type
    
    ### get substrate unit
    substrate.unit <- unique(myDF$Substrate_application_rate_unit);substrate.unit
    myDF$Substrate_application_rate_unit <- gsub("__ug C cm-2 simulator surface area day-1", 
                                                 "ug C cm-2 simulator surface area day-1", 
                                                 myDF$Substrate_application_rate_unit)
    substrate.unit <- unique(myDF$Substrate_application_rate_unit);substrate.unit
    
    ### get substrate application rate
    substrate.rate <- unique(myDF$Substrate_application_rate);substrate.rate
    
    ###
    #summary(myDF$Incubation_temperature)
    #summary(myDF$Total_substrate_added)
    #summary(myDF$Total_substrate_added_per_unit_soil)
    
    ### Response variable
    response.variable <- unique(myDF$Response_variable);response.variable
    
    myDF$Response_variable <- gsub("_14CO2 efflux rate", 
                                   "14CO2 efflux rate", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_N-acetyl-glucosaminidase (NAG) activity", 
                                   "N-acetyl-glucosaminidase (NAG) activity", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_N-acetyl-glucosaminidase (NAG) activity", 
                                   "N-acetyl-glucosaminidase (NAG) activity", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_NAG", 
                                   "N-acetyl-glucosaminidase (NAG) activity", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Microbial N", 
                                   "Microbial biomass N", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Microbial C/N", 
                                   "Microbial CN ratio", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_TDN", 
                                   "Total dissolved N", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_DOC", 
                                   "DOC", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_CBH", 
                                   "CBH", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_BG", 
                                   "B-glucosidase", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_LAP", 
                                   "Leucine aminopeptidase", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_PHO", 
                                   "Phosphatase", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("_POX", 
                                   "Phenoloxidase", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_PER", 
                                   "Peroxidase", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Acid phosphate enzyme", 
                                   "Acid phosphate enzyme", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("__-1,4-N-acetylglucosaminidase", 
                                   "B-1,4-N-acetylglucosaminidase", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("__-1,4-glucosidase enzym", 
                                   "B-1,4-glucosidase enzym", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("_Polyphenol oxidase", 
                                   "Polyphenol oxidase", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("_Peroxidase enzyme", 
                                   "Peroxidase enzyme", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("_\xa7-1,4-glucosidase", 
                                   "B-1,4-glucosidase", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_\xa7-1,4-N-acetyl-glucosaminidase ", 
                                   "B-1,4-N-acetyl-glucosaminidase", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Peroxidase", 
                                   "Peroxidase", 
                                   myDF$Response_variable)
    
    
    myDF$Response_variable <- gsub("_Al bound in\nMOCs", 
                                   "Al bound in MOCs", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Fe bound in\nMOCs", 
                                   "Fe bound in MOCs", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Al bound in\nSROs", 
                                   "Al bound in SROs", 
                                   myDF$Response_variable)
    
    myDF$Response_variable <- gsub("_Fe bound in\nSROs", 
                                   "Fe bound in SROs", 
                                   myDF$Response_variable)
    
    response.variable <- unique(myDF$Response_variable);response.variable
    
    
    ##############################################################
    #### Calculate response ratios where possible
    myDF$Effect_size_mean_ratio <- ifelse(is.na(myDF$Effect_size_mean_ratio),
                                          myDF$Response_mean/myDF$Control_mean,
                                          myDF$Effect_size_mean_ratio)
    
    myDF$Effect_size_SE_ratio <- ifelse(is.na(myDF$Effect_size_SE_ratio),
                                        sqrt(myDF$Control_SE^2 + myDF$Response_SE^2),
                                        myDF$Effect_size_SE_ratio)
    
    
    myDF$Effect_size_sample_size_ratio <- ifelse(is.na(myDF$Effect_size_sample_size_ratio),
                                                 myDF$Control_sample_size,
                                                 myDF$Effect_size_sample_size_ratio)

        
    myDF$Microbial_biomass_response_ratio <- ifelse(is.na(myDF$Microbial_biomass_response_ratio),
                                          myDF$Microbial_biomass_mean_treatment/myDF$Microbial_biomass_mean_control,
                                          myDF$Microbial_biomass_response_ratio)
    
    
    
    ### replace inf values
    myDF[sapply(myDF, is.infinite)] <- NA
    
    
    ##############################################################
    ### make some summary tables and figures
    sumDF1 <- summaryBy(Total_substrate_added_per_unit_soil~Citation+Experiment_method+Land_use+Substrate+type+Experimental_duration+Incubation_temperature+Time_series_data+Time+since_substrate_addition,
                        FUN=mean, data=myDF, keep.names=T, na.rm=T)
    
    
    p1 <- ggplot() +
        geom_point(sumDF1, mapping=aes(Experimental_duration, 
                                       Total_substrate_added_per_unit_soil,
                                       pch=Time_series_data, 
                                       size=Incubation_temperature,
                                       fill=Land_use,
                                       col=Land_use))+
        scale_y_continuous(trans='log2',limits=c(0.05, 50))+
        scale_x_continuous(trans='log2', limits=c(1, 200))+
        ylab("Total substrate added per unit soil (mg C g-1 soil)")+
        xlab("Experimental duration (d)")+
        scale_shape_manual(name="Time series data",
                           values=c("yes" = 21,
                                    "no" = 22))#+
        #scale_fill_viridis_d(name="Land use type")
    p1
    
    ##############################################################
    ### response ratios
    ### remove no values
    myDF <- myDF[complete.cases(myDF$Total_substrate_added_per_unit_soil),]
    #myDF <- myDF[complete.cases(myDF$Effect_size_mean_subtraction),]
    
    unique(myDF$Response_variable)
    
    ### look at cumulative priming effect only
    outDF1 <- myDF[myDF$Response_variable%in%c("Cumulative priming effect per unit soil", 
                                               "Cumulative CO2 efflux", 
                                               "Cumulative CO2 emission"),]
    
    outDF2 <- myDF[myDF$Response_variable%in%c("Priming effect per unit soil", 
                                               "CO2 efflux per day", 
                                               "Priming rate per unit soil",
                                               "14CO2 efflux rate",
                                               "Priming SOC mineralisation",
                                               "Priming effect per day",
                                               "CO2 efflux",
                                               "SOM respiration",
                                               "Soil respiration"),]
    
    unique(outDF1$Response_variable_unit)
    
    outDF1 <- outDF1[outDF1$Response_variable_unit%in%c("mg C g-1 soil", 
                                                        "mg g-1 soil"),]
    
    
    unique(outDF2$Response_variable_unit)
    
    outDF2 <- outDF2[outDF2$Response_variable_unit%in%c("mg C g-1 soil", 
                                                        "mg C g-1 d-1",
                                                        "mg C g-1 soil d-1"),]

    #require(plotly)
    #plot_ly(x=outDF1$Time_since_substrate_addition, 
    #        y=outDF1$Effect_size_mean_ratio, z=outDF1$Total_substrate_added_per_unit_soil, 
    #        type="scatter3d", mode="markers", color=outDF1$Land_use)
    
    ##############################################################
    ### look at individual dataset separately
    subdataset1 <- unique(outDF1$Citation)
    
    subDF1 <- subset(outDF1, Citation%in%c("Guenet et al. 2014", # negative response for forest
                                           "Iimura et al. 2020",
                                           "Liu et al. 2020",
                                           "Murphy et al. 2015",
                                           "Qiao et al. 2014",
                                           "Qiao et al. 2019",
                                           #"Rousk et al. 2015", # sharp increase
                                           "Shahzad et al. 2019",
                                           #"Yin et al. 2016",   # vertical line
                                           "Zhou et al. 2021",
                                           "Zwetsloot et al. 2020")) # vertical line
    
    p2 <- ggplot() +
        geom_point(subDF1, mapping=aes(x=Time_since_substrate_addition, 
                                       y=Effect_size_mean_subtraction,
                                       pch=Land_use, 
                                       size=Total_substrate_added_per_unit_soil,
                                       fill=Citation,
                                       col=Citation))+
        geom_smooth()+
        #scale_y_continuous(trans='log2',limits=c(0.05, 50))+
        ylab("Effect size (mg C g-1 soil)")+
        xlab("Experimental duration (d)")+
        #scale_shape_manual(name="Land use",
        #                   values=c("Cropland" = 21,
        #                            "Forest" = 22,
        #                            "Grassland" = 23))+
        theme_bw(); p2
    
    
    subDF1$Effect_size_mean_per_substrate <- with(subDF1, Effect_size_mean_subtraction/Total_substrate_added_per_unit_soil)
    
    summary(subDF1$Effect_size_mean_per_substrate)
    
    p3 <- ggplot() +
        geom_point(subDF1, mapping=aes(x=Time_since_substrate_addition, 
                                       y=Effect_size_mean_per_substrate,
                                       pch=Land_use, 
                                       #size=Total_substrate_added_per_unit_soil,
                                       fill=Citation,
                                       col=Citation))+
        geom_smooth()+
        #scale_y_continuous(trans='log2',limits=c(0.05, 50))+
        ylab("Effect size per substrate (unitless)")+
        xlab("Experimental duration (d)")+
        #scale_shape_manual(name="Land use",
        #                   values=c("Cropland" = 21,
        #                            "Forest" = 22,
        #                            "Grassland" = 23))+
        theme_bw(); p3
    
    
    
    
    
    
    #### let's look at daily rate of priming effect
    ### look at individual dataset separately
    subdataset2 <- unique(outDF2$Citation); subdataset2
    
    subDF2 <- subset(outDF2, Citation%in%c(#"Basiliko et al. 2012",
                                           #"Feng et al. 2021", # one time point, elevation gradient
                                           "Girkin et al. 2020", 
                                           "Guenet et al. 2014",
                                           "Karhu et al. 2016",
                                           "Qiao et al. 2014",
                                           "Qiao et al. 2019",
                                           "Shahzad et al. 2019",
                                           "Vance_Chapin 2000",
                                           "Zhou et al. 2021",
                                           "Zwetsloot et al. 2020"))
        
    
    p2 <- ggplot() +
        geom_point(subDF2, mapping=aes(x=Time_since_substrate_addition, 
                                       y=Effect_size_mean_subtraction,
                                       pch=Land_use, 
                                       size=Total_substrate_added_per_unit_soil,
                                       fill=Citation,
                                       col=Citation))+
        geom_smooth()+
        #scale_y_continuous(trans='log2',limits=c(0.05, 50))+
        ylab("Effect size (mg C g-1 soil)")+
        xlab("Experimental duration (d)")+
        #scale_shape_manual(name="Land use",
        #                   values=c("Cropland" = 21,
        #                            "Forest" = 22,
        #                            "Grassland" = 23))+
        theme_bw(); p2
    
    
    subDF1$Effect_size_mean_per_substrate <- with(subDF1, Effect_size_mean_subtraction/Total_substrate_added_per_unit_soil)
    
    summary(subDF1$Effect_size_mean_per_substrate)
    
    p3 <- ggplot() +
        geom_point(subDF1, mapping=aes(x=Time_since_substrate_addition, 
                                       y=Effect_size_mean_per_substrate,
                                       pch=Land_use, 
                                       #size=Total_substrate_added_per_unit_soil,
                                       fill=Citation,
                                       col=Citation))+
        geom_smooth()+
        #scale_y_continuous(trans='log2',limits=c(0.05, 50))+
        ylab("Effect size per substrate (unitless)")+
        xlab("Experimental duration (d)")+
        #scale_shape_manual(name="Land use",
        #                   values=c("Cropland" = 21,
        #                            "Forest" = 22,
        #                            "Grassland" = 23))+
        theme_bw(); p3
    
    
    
    test1 <- rma.mv(Effect_size_mean_subtraction, Effect_size_SE_subtraction, 
                    mod = ~Total_substrate_added_per_unit_soil+Time_since_substrate_addition,
                    random = ~1|Citation, data=outDF2)
    
    test1
    
    test2 <- rma.mv(Effect_size_mean_ratio, Effect_size_SE_ratio, 
                    mod = ~Total_substrate_added_per_unit_soil+Time_since_substrate_addition+Microbial_biomass_response_ratio,
                    random = ~1|Citation, data=outDF2)
    
    test2
    
    
    sumDF2 <- summaryBy(Effect_size_mean_ratio+Microbial_biomass_response_ratio~Total_substrate_added_per_unit_soil+Citation+Experiment_method+Land_use+Substrate+type+Experimental_duration+Incubation_temperature+Time_series_data+Time+since_substrate_addition,
                        FUN=mean, data=myDF, keep.names=T, na.rm=T)
    
    
    p2 <- ggplot() +
        geom_point(sumDF2, mapping=aes(Effect_size_mean_ratio, 
                                       Total_substrate_added_per_unit_soil,
                                       pch=Time_series_data, 
                                       size=Incubation_temperature,
                                       fill=Land_use,
                                       col=Land_use))+
        #scale_y_continuous(trans='log2',limits=c(0.05, 50))+
        #scale_x_continuous(trans='log2', limits=c(1, 200))+
        ylab("Total substrate added per unit soil (mg C g-1 soil)")+
        xlab("Experimental duration (d)")+
        scale_shape_manual(name="Time series data",
                           values=c("yes" = 21,
                                    "no" = 22))#+
    #scale_fill_viridis_d(name="Land use type")
    p2
    
    ##############################################################
    ### let's only focus on priming effect
    
    
    
    ##############################################################
    ### check individual dataset
    subDF1 <- subset(myDF, Citation == "Basiliko et al. 2012")
    
    ### check substrate addition-related variables
    substrate.type <- unique(subDF1$Substrate_type); substrate.type
    
    
    
    
    #### check number of response variables
    
    
    dim(myDF1)
    
}