#_______________________________________________________________________________
#
# PREVALENCE ESTIMATOR DATA EXPORT
#
# AUTHOR: Georgianna Silveira
# ERROR HANDLING: Brenda Hanley
# MODEL LOGGING: Brenda Hanley
# QAQC: Brenda Hanley
#
# DATE: August 2024
# Location: Cornell Wildlife Health Laboratory
# License: MIT
#
# This code formats data from the CWD Data Warehouse for upload into the
# external Weighted Surveillance CWD Detection model for use in the Estimation
# Tool.The external model is available here:
# https://public.spdgt.com/app/wtsurv
#
# This code is adapted from code originally written by Cara Them.

# This code was written under:
# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Copyright (C) 2024 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
#
#_______________________________________________________________________________

# Load packages
library(tidyverse) # Version 2.0.0

# Logging

    # Model log file started with Python data processing script.
    model_log_filepath <- file.path("/", "data", "attachments", "info.html")

# Info about Required Data. 

# There are three required files that will always be imported:
# 1. A .csv file with user-selected parameters.
# 2. A .csv file with the list of subadmin for the focal state.
# 3. A .csv file with the animals sampled that match the user inputs.

# Continue the log started with the Python script. 
  line='<h3>User-Selected Sample Data</h3>'
  write(line,file=model_log_filepath,append=TRUE)

# Read in the (Required) Parameters file. 
  params_filepath=file.path("/", "data", "params.csv")
  params=readr::read_csv(params_filepath)
  # Note: The params.csv has to exist b/c Python generated it and b/c model has 
  # to create it. Therefore, this error handling is in the python code and this 
  # R script will not run if it does not exist. 

# Read in the (Required) Subadmin file. 
  subadmin_filepath=file.path("", "data", "sub_administrative_area.csv")
  subadmin=readr::read_csv(subadmin_filepath) 
  # Note: The sub_administrative_area.csv has to exist b/c Python generated it 
  # and b/c model has to create it. Therefore, this error handling is in the 
  # python code and this R script will not run if it does not exist. 

# Read in (Required) Samples file. 
  sample_filepath=file.path("", "data", "sample.csv")
  sample=readr::read_csv(sample_filepath) 
  # Note: The sub_administrative_area.csv has to exist b/c Python generated it 
  # and b/c model has to create it. Therefore, this error handling is in the 
  # python code and this R script will not run if it does not exist.

# Notes. 
# At this point in the code, if any of the files above do not exist, then the 
# code has been terminated. 

# Create standard reference of subadmin IDs and their full names.
  SubAdmin_Standard=data.frame(SubAdminID=subadmin$id,SubAdminName=subadmin$full_name) 

# Create the initial data frame with variables of interest.  
	Data_df0=as.data.frame(cbind(sample$sub_administrative_area_id,sample$species,sample$sample_source,sample$age_group,sample$sex,sample$result))
	colnames(Data_df0)=c("SubAdminID","species","sample_source","age_group","sex","result")
	
# Append the names from subadminID. 
	Data_df=merge(Data_df0,SubAdmin_Standard, by=c("SubAdminID"),all.x=TRUE)
	colnames(Data_df)=c("SubAdminID","species","sample_source","age_group","sex","result","SubAdminName")
		
# GLOBAL PROCESSING ____________________________________________________________
# Obtain the global tidy data.

	# Write a note to user about non-definitive results. 
	line="<p>Note: The CWD Data Warehouse can store records with 'Inconclusive', 'Pending', and 'Not tested' results. 
	However, the SpeedGoat Estimation tool only utilizes records with 'Detected' or 'Not Detected' results.</p>"
	write(line,file=model_log_filepath,append=TRUE)
	
    line="<h4>Summary of sample data loading process</h4>"
    write(line,file=model_log_filepath,append=TRUE)
		
  # Retain samples with Result=Detected. 
	  TidyData_Result_Detected=subset(Data_df,result=="Detected")
	  PosDim=as.numeric(nrow(TidyData_Result_Detected))
	  
      # Report the number of samples Detected.
		  line=paste("<p>Successfully loaded",PosDim, "CWD-positive records.</p>")
		  write(line,file=model_log_filepath,append=TRUE)
		
	# Retain samples with Result=Not Detected. 
		TidyData_Result_NotDetected=subset(Data_df,result=="Not Detected")
		NegDim=as.numeric(nrow(TidyData_Result_NotDetected))
		
	    # Report the number of samples Not Detected.
		  line=paste("<p>Successfully loaded",NegDim, "CWD-non detect records.</p>")
		  write(line,file=model_log_filepath,append=TRUE)
		  
	      # If both Detected and Non Detected exist: 
		    if (PosDim>0 & NegDim>0){
		    TidyDataDefinitiveTest=as.data.frame(rbind(TidyData_Result_Detected,TidyData_Result_NotDetected))
		    # Don't tell the user anything at this point, because we still don't know if 
		    # records exist in the specific combinations of interest. 
		    } # End if both Detected and Non Detected exist.
		
	      # If only Detected exists, quit the program. 
		    if (PosDim>0 & NegDim==0){
	      # Tell the user you cannot use this tool. 
		    line="<p>We're sorry. The SpeedGoat Estimation tool requires a 
			    series of CWD-non detect results. Only CWD-positive tests 
			    appear in the selected data. Return to the CWD Data 
			    Warehouse and select a different set of sample data.</p>"
		    write(line,file=model_log_filepath,append=TRUE) 
		    # Quit the session.
		    quit(status=70)
		    } # End if only Detected exists.
		
	      # If only Not Detected exist. 
		    if (PosDim==0 & NegDim>0){
		    TidyDataDefinitiveTest=as.data.frame(TidyData_Result_NotDetected)
		    # Don't tell the user anything at this point , because we still don't 
		    # know if records exist in the specific combinations of interest. 
		    } # End if only Not Detected exist.	
		
	      # If nothing exists, quit the program. 
		    if (PosDim==0 & NegDim==0) {
		    # Tell the user you cannot use this tool. 
		    line="<p>We're sorry. The SpeedGoat Estimation tool requires a series of 
			    CWD-non detect results. Once this program completed data cleaning, 
			    no samples remained. Return to the CWD Data Warehouse and 
			    select a different set of sample data.</p>"
            	    write(line,file=model_log_filepath,append=TRUE) 
		    # Quit the session.
		    quit(status=70) 
		    } # End if nothing exists. 

# Note: 
# At this point, the data frame named TidyDataDefinitiveTest must exist and 
# contain data, or else the program was terminated. 
		  
	# Now retain samples with known sub-administrative area. 
		TidyData0=subset(TidyDataDefinitiveTest,complete.cases(TidyDataDefinitiveTest$SubAdminID))
		TidyData=subset(TidyData0,complete.cases(TidyData0$SubAdminName))
		TidyDataDim=as.numeric(nrow(TidyData))

		    # If at least one record with known sub-admin area still exists. 
		    if (TidyDataDim>0){
	      	    # Report the number of samples with sub-admin area. 
		    line=paste("<p>Successfully loaded",TidyDataDim, "records with 'Detected' or 'Not Detected' result AND known sub-administrative area.</p>")
		    write(line,file=model_log_filepath,append=TRUE)
		    } # End if at least one eligible record still exists. 

		    # If eligible records no longer exist. 
		    if (TidyDataDim==0){
		    # Write a note that model is only dealing with known sub-administrative area.
		    line="<p>The CWD Data Warehouse can store records with unknown sub-administrative area. 
			    However, the SpeedGoat Estimation tool only utilizes records with known 
			    sub-administrative area. The records you selected did not have this 
			    information, so you cannot use this tool. Return to the CWD Data Warehouse 
			    and select a different set of sample data.</p>"
		    write(line,file=model_log_filepath,append=TRUE)
		    # Quit the session.
		    quit(status=70)
		    } # End if eligible records no longer exist. 
		  
		    # Note: In order for the code to get here, TidyData must have records, or 
		    # else the script has been terminated. 
		
	        # Retain samples with known species. 
		      TidyData2=subset(TidyData,complete.cases(TidyData$species))
		      TidyData2Dim=as.numeric(nrow(TidyData2))
		  
		          # If at least one record with known species still exists. 
		          if (TidyData2Dim>0){
	            	  # Report the number of samples with all the criteria. 
		          line=paste("<p>Successfully loaded",TidyData2Dim, "records with 'Detected' or 'Not Detected' result, known sub-administrative area, AND known species.</p>")
		          write(line,file=model_log_filepath,append=TRUE)
		          } # End if at least one eligible record still exists.
		  
		          # If eligible records no longer exist. 
		          if (TidyData2Dim==0){
		          # Write a note that model is only dealing with known species.
		          line="<p>The CWD Data Warehouse can store records with unknown species. 
				  However, the SpeedGoat Estimation tool only utilizes records with 
				  recorded species. The records you selected did not have this 
				  information, so you cannot use this tool. Return to the CWD 
				  Data Warehouse and select a different set of sample data.</p>" 
		          write(line,file=model_log_filepath,append=TRUE)
		          # Quit the session.
		          quit(status=70)
		          } # End if eligible records no longer exist. 
		  
# Note. 
# At this point in the code, all the information required for the SpeedGoat 
# Estimation tool exists in each record, and there exists at least one record
# rendering the user-selected data eligible for this model. If at least one
# eligible record does not exist, then the session has already been terminated.

# Also at this point in the code, missing data in a record's source, age, or 
# sex category is OK because the record can still be binned into the 'other',
# 'all ages', or 'all sex' categories for use in the SpeedGoat Estimation tool.
# However, we can't have missing/null data in any cell, so we need to
# explicitly replace missing sample source with 'unknown', missing sex with
# 'unknown', and missing age with 'unknown'.

	# Change any remaining missing data cells to unknowns. 
	TidyData2[is.na(TidyData2)]="Unknown"

	# Use the definitions in the CWD Data Warehouse to map the terminology in the data to 
	# precisely match the terminology and definitions in the SpeedGoat Estimation tool.

		  # Note: Changes are consistent with SpeedGoat Estimation tool definitions as of August 2024. 
		  TidyDataSG=TidyData2
		  for (i in 1:TidyData2Dim){
		  # Standardize species.
		  # [The first is standard CWD Data Warehouse, the second is equivalent in SpeedGoat.]
		  if(TidyData2$species[i]=="white-tailed deer"){TidyDataSG$species[i]="White-tailed Deer"}
		  if(TidyData2$species[i]=="elk"){TidyDataSG$species[i]="Elk"}
		  if(TidyData2$species[i]=="mule deer"){TidyDataSG$species[i]="Mule Deer"}
		  # Standardize source.
		  # [The first is standard CWD Data Warehouse, the second is equivalent in SpeedGoat.]
		  if(TidyData2$sample_source[i]=="Captive cervid facility"){TidyDataSG$sample_source[i]="NA"} # SpeedGoat does not deal with captive animals. 
		  if(TidyData2$sample_source[i]=="Clinical suspect"){TidyDataSG$sample_source[i]="Clinical suspect"}
		  if(TidyData2$sample_source[i]=="Escaped from captive cervid facility"){TidyDataSG$sample_source[i]="NA"}	# SpeedGoat does not deal with captive animals. 
	    if(TidyData2$sample_source[i]=="Hunter harvest"){TidyDataSG$sample_source[i]="Hunter-harvested"}
		  if(TidyData2$sample_source[i]=="Illegal import"){TidyDataSG$sample_source[i]="NA"}		  # SpeedGoat does not deal with captive animals.
		  if(TidyData2$sample_source[i]=="Illegal take"){TidyDataSG$sample_source[i]="Hunter-harvested"} # This category was taken by a hunter but just out of season
		  if(TidyData2$sample_source[i]=="Removal for crop damage"){TidyDataSG$sample_source[i]="Hunter-harvested"}	# SpeedGoat sharp shot is only relative to CWD status, hence this being hunter-harvested.
		  if(TidyData2$sample_source[i]=="Removal for population management"){TidyDataSG$sample_source[i]="Hunter-harvested"} # SpeedGoat sharp shot is only relative to CWD status, hence this being hunter-harvested.
		  if(TidyData2$sample_source[i]=="Research"){TidyDataSG$sample_source[i]="Other"}	
		  if(TidyData2$sample_source[i]=="Road kill"){TidyDataSG$sample_source[i]="Vehicle collision (direct or indirect)"}	
		  if(TidyData2$sample_source[i]=="Targeted removal"){TidyDataSG$sample_source[i]="Sharp shot"} # Removed in relation to its CWD status. 
		  if(TidyData2$sample_source[i]=="Unknown"){TidyDataSG$sample_source[i]="Other"}		  
      # Standardize age.
		  # [The first is standard CWD Data Warehouse, the second is equivalent in SpeedGoat.]
		  if(TidyData2$age_group[i]=="No age"){TidyDataSG$age_group[i]="All Ages"}
		  if(TidyData2$age_group[i]=="Adult"){TidyDataSG$age_group[i]="Adult"}
		  if(TidyData2$age_group[i]=="Yearling"){TidyDataSG$age_group[i]="Yearling"}
		  if(TidyData2$age_group[i]=="Fawn"){TidyDataSG$age_group[i]="Fawn"}
		  if(TidyData2$age_group[i]=="Unknown"){TidyDataSG$age_group[i]="All Ages"}
		  # Standardize sex.
		  # [The first is standard CWD Data Warehouse, the second is equivalent in SpeedGoat.]
		  if(TidyData2$sex[i]=="Unknown"){TidyDataSG$sex[i]="All Sexes"}
		  if(TidyData2$sex[i]=="Female"){TidyDataSG$sex[i]="Female"}
		  if(TidyData2$sex[i]=="Male"){TidyDataSG$sex[i]="Male"}
          } # End for loop.

        # Remove records with sample source=NA as they do not fit any definition in the SpeedGoat Estimation tool. 
		    SG_Data=subset(TidyDataSG,TidyDataSG$sample_source!="NA")
		    SG_DataDim=as.numeric(nrow(SG_Data))
		
		        # If eligible records still exist. 
		        if (SG_DataDim>0){
	          	# Report the number of samples that fit all the criteria. 
	          	line=paste("<p>Successfully loaded",SG_DataDim, "records with 'Detected' or 'Not Detected' result, 
			     known sub-administrative area, known species, AND known sample source that is 
			     consistent with the sample source definitions in the SpeedGoat Estimation tool.</p>")
	          	write(line,file=model_log_filepath,append=TRUE)
		        } # End if eligible records still exist. 
	    
	          	# If eligible records no longer exist. 
		        if (SG_DataDim==0){
		        # Write a note that model is only dealing with data that fit the SpeedGoat Estimation tool definitions. 
		        line="<p>The CWD Data Warehouse can store records with a variety of sample sources. However, the SpeedGoat 
				Estimation tool can only handle certain sample sources. The records you selected did not fit 
				these sources, so you cannot use this tool. Return to the CWD Data Warehouse and select 
				a different set of sample data.</p>" 
		        write(line,file=model_log_filepath,append=TRUE)
		        # Quit the session.
		        quit(status=70)
		        } # End if eligible records no longer exist. 

# Note.
# Our data frame is now cleaned to appropriately fit into the SpeedGoat Estimation tool
# definitions. Anything that does not fit a SpeedGoat Estimation tool definition has been removed. 
# Anything that fits the a SpeedGoat Estimation tool definition has been retained. At least one 
# eligible record exists in the SG_Data or else the session would have been terminated. 
	  
# Here we begin summarizing the data to fit into the specific list of priors 
# available for batch estimation. 
	
    	# Partition the Cleaned SpeedGoat Estimation tool data frame by species. 
	# Note consistent with SpeedGoat Estimation tool offerings as of August 2024. 
	ELK=subset(SG_Data,species=="Elk")
	ElkDim=as.numeric(nrow(ELK))
		  
    	WTD=subset(SG_Data,species=="White-tailed Deer")
    	WTDDim=as.numeric(nrow(WTD))
      
    	MULE=subset(SG_Data,species=="Mule Deer")
    	MuleDim=as.numeric(nrow(MULE))
    
    	ELKMULE=subset(SG_Data,species=="Mule Deer" | species=="Elk")
    	ElkMuleDim=as.numeric(nrow(ELKMULE))

# Note.
# Elk+WTD+Mule do not necessarily add up to all the records in SG_Data because 
# SG_Data contains records on many possible cervid species. 

# End summary of input sample data.

# Initialize an output table. 
    OUTPUT=c()

# Add a new header to the report. 
line="<h4>Report of Priors</h4>"
write(line,file=model_log_filepath,append=TRUE) 

# ______________________________________________________________________________
# SPECIES: ELK _________________________________________________________________ 
# ______________________________________________________________________________ 
    
# If user selected elk. 
if(params$species=="Elk"){

  # If elk data does not globally exist. 
  if (ElkDim==0){
  # Add information to the report.
  line="<p>We're sorry. You selected elk, but there does not exist any elk testing records in the 
	  season-year of interest. Please return to the CWD Data Warehouse and select 
	  a different season-year.</p>"
  write(line,file=model_log_filepath,append=TRUE) 
  # Quit the session.
  quit(status=70)
  } # End if elk data does not globally exist.
  
  # If elk data globally exists. 
  if (ElkDim>0){
      
  # Opening Note. 
  # As of August 2024, SpeedGoat Estimation tool elk priors are limited to Clinical suspect, 
  # Hunter-harvested, and Other. 
  
  # ELK CLINICAL SUSPECT _______________________________________________________ 
  ELK_CS=subset(ELK,sample_source=="Clinical suspect")
  ElkCSDim=as.numeric(nrow(ELK_CS))

    # If elk clinical suspect is empty. 
    if (ElkCSDim==0){
    # Tell the user insufficient records exist.
    line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
    write(line,file=model_log_filepath,append=TRUE)  
    line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    } # End elk clinical suspect is empty. 
        
    # If elk clinical suspect contains data.
    if (ElkCSDim>0){

        # PRIOR________________________________________________________________
        # Get summary for Elk + Clinical Suspect + All Ages + Female prior.
        ELK_CS_Female=subset(ELK_CS,sex=="Female")
        ElkCSDimF=as.numeric(nrow(ELK_CS_Female))
                
            # if ELK clinical suspect Female is empty.
            if (ElkCSDimF==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)  
            } # End if female clinical suspect is empty. 
        
            # if ELK clinical suspect Female contains data.             
            if (ElkCSDimF>0){
                      
                # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
                ELK_CS_Female$age_group=rep("All Ages", ElkCSDimF)
                      
                # Summarize the data. 
                TABLE1=as.data.frame(ELK_CS_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE1_Dim=as.numeric(nrow(TABLE1))
                      
                    # If only one record exists and its detected, tell user data doesn't fit criteria.
                    if (TABLE1_Dim==1 & TABLE1$result[1]=="Detected"){
                    # Write a note that the data does not fit this prior.  
                    line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and detected. 
                      
                    # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                    if (TABLE1_Dim==1 & TABLE1$result[1]=="Not Detected"){
                    # Add the record to the output file. 
                    OUTPUT=rbind(OUTPUT, TABLE1)
		    # Add to the report.
    		    line="<p>Elk, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
    		    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and not detected. 
                      
                    # If two or more lines exist in the summary table. 
                    if (TABLE1_Dim>1){
                            
                        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                        TABLE2=as.data.frame(TABLE1%>%group_by(SubAdminName)%>%filter(n()<2))
                        TABLE2_Dim=as.numeric(nrow(TABLE2))
                          
                            # If no records remains after duality filter. 
                            if (TABLE2_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE)
                            } # End if no records exist. 
                          
                            # If at least one record remains after duality filter. 
                            if (TABLE2_Dim>0){
                                
                                # Retain only the non-detect records. 
                                TABLE3=subset(TABLE2,result=="Not Detected")
                                TABLE3_Dim=as.numeric(nrow(TABLE3))
                                
                                    # If no non-detect records remain. 
                                    if (TABLE3_Dim==0){
                                    # Tell the user insufficient records exist.
                                    line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
                                    write(line,file=model_log_filepath,append=TRUE) 
                                    } # End if no records exist. 
                                  
                                    # If non-detect records remain. 
                                    if (TABLE3_Dim>0){
                                    # Add the record to the output file. 
                                    OUTPUT=rbind(OUTPUT, TABLE3)
		    		    # Add to the report.
    				    line="<p>Elk, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
    				    write(line,file=model_log_filepath,append=TRUE) 
                                    } # End if one and not detected. 
                                  
                            } # If at least one record remains after duality filter. 
                      } # End if two or more lines exist in summary table. 
                  } # End if female clinical suspect contains data. 
              
        # PRIOR________________________________________________________________  
        # Get summary for Elk + Clinical Suspect + All Ages + Male prior.
        ELK_CS_Male=subset(ELK_CS,sex=="Male")
        ElkCSDimM=as.numeric(nrow(ELK_CS_Male))
              
            # if ELK clinical suspect Male is empty.
            if (ElkCSDimM==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
            } # End if male clinical suspect is empty. 
              
            # if ELK clinical suspect Male contains data.              
            if (ElkCSDimM>0){
                
                # Replace age data with 'All Ages'. [We do this because the prior for SpeedGoat Estimation tool is All Ages.] 
                ELK_CS_Male$age_group=rep("All Ages", ElkCSDimM)
                
                # Summarize the data. 
                TABLE4=as.data.frame(ELK_CS_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE4_Dim=as.numeric(nrow(TABLE4))
                
                    # If only one record exists and its detected, tell user data doesn't fit criteria.
                    if (TABLE4_Dim==1 & TABLE4$result[1]=="Detected"){
                    # Write a note that the data does not fit this prior.  
                    line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and detected. 
                
                    # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                    if (TABLE4_Dim==1 & TABLE4$result[1]=="Not Detected"){
                    # Add the record to the output file. 
                    OUTPUT=rbind(OUTPUT, TABLE4)
		    # Add to the report.
    		    line="<p>Elk, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
    		    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and not detected. 
                
                    # If two or more lines exist in the summary table. 
                    if (TABLE4_Dim>1){
                      
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE5=as.data.frame(TABLE4%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE5_Dim=as.numeric(nrow(TABLE5))
                  
                        # If no records remains after duality filter. 
                        if (TABLE5_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE)
                        } # End if no records exist. 
                  
                        # If at least one record remains after duality filter. 
                        if (TABLE5_Dim>0){
                    
                        # Retain only the non-detect records. 
                        TABLE6=subset(TABLE5,result=="Not Detected")
                        TABLE6_Dim=as.numeric(nrow(TABLE6))
                    
                            # If no non-detect records remain. 
                            if (TABLE6_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE) 
                            } # End if no records exist. 
                    
                            # If non-detect records remain. 
                            if (TABLE6_Dim>0){
                            # Add the record to the output file. 
                            OUTPUT=rbind(OUTPUT, TABLE6)
		    	    # Add to the report.
    			    line="<p>Elk, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
    			    write(line,file=model_log_filepath,append=TRUE)
                            } # End if one and not detected. 
                    
                        } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table. 
                } # End if male clinical suspect contains data. 
        
            } # End If elk clinical suspect contains data.

  # ELK HUNTER HARVESTED ________________________________________________________________        
  ELK_HH=subset(ELK,sample_source=="Hunter-harvested")
  ElkHHDim=as.numeric(nrow(ELK_HH))

      # If elk hunter harvest is empty. 
      if (ElkHHDim==0){
      # Tell the user insufficient records exist.
      line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      } # End if elk hunter harvest is empty.
        
      # If elk hunter harvest contains data.
      if (ElkHHDim>0){

          # PRIOR________________________________________________________________
          # Get summary for Elk + Hunter Harvest + Adult + Female prior.
          ELK_HH_Female=subset(ELK_HH,sex=="Female" & age_group=="Adult")
          ElkHHDimFA=as.numeric(nrow(ELK_HH_Female))
            
              # if ELK hunter harvest Female Adult is empty.
              if (ElkHHDimFA==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if elk female adult hunter harvest is empty. 
              
              # if ELK hunter harvest adult Female contains data.            
              if (ElkHHDimFA>0){
              
                  # Summarize the data. 
                  TABLE7=as.data.frame(ELK_HH_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE7_Dim=as.numeric(nrow(TABLE7))
              
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE7_Dim==1 & TABLE7$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
              
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE7_Dim==1 & TABLE7$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE7)
		      # Add to the report.
    		      line="<p>Elk, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
    		      write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
              
                      # If two or more lines exist in the summary table. 
                      if (TABLE7_Dim>1){
                      
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE8=as.data.frame(TABLE7%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE8_Dim=as.numeric(nrow(TABLE8))
                
                          # If no records remains after duality filter. 
                          if (TABLE8_Dim==0){
                          # Tell the user insufficient records exist.
                          line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE)
                          } # End if no records exist. 
                
                          # If at least one record remains after duality filter. 
                          if (TABLE8_Dim>0){
                  
                          # Retain only the non-detect records. 
                          TABLE9=subset(TABLE8,result=="Not Detected")
                          TABLE9_Dim=as.numeric(nrow(TABLE9))
                  
                              # If no non-detect records remain. 
                              if (TABLE9_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
                  
                              # If non-detect records remain. 
                              if (TABLE9_Dim>0){
                              # Add the record to the output file. 
                              OUTPUT=rbind(OUTPUT, TABLE9)
		    	      # Add to the report.
    			      line="<p>Elk, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
    			      write(line,file=model_log_filepath,append=TRUE)
                              } # End if one and not detected. 
                          
                          } # If at least one record remains after duality filter. 
                      } # End if two or more lines exist in summary table. 
                } # End if female adult hunter harvest contains data. 
            
          # PRIOR________________________________________________________________
          # Get summary for Elk + Hunter Harvest + Yearling + Female prior.
          ELK_HH_Female2=subset(ELK_HH,sex=="Female" & age_group=="Yearling")
          ElkHHDimF2=as.numeric(nrow(ELK_HH_Female2))
            
              # if ELK hunter harvest Female yearling is empty.
              if (ElkHHDimF2==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if elk female yearling hunter harvest is empty. 
              
              # if ELK hunter harvest yearling Female contains data.             
              if (ElkHHDimF2>0){
              
                  # Summarize the data. 
                  TABLE10=as.data.frame(ELK_HH_Female2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE10_Dim=as.numeric(nrow(TABLE10))
              
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE10_Dim==1 & TABLE10$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
              
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE10_Dim==1 & TABLE10$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE10)
		      # Add to the report.
    		      line="<p>Elk, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
    		      write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
              
                      # If two or more lines exist in the summary table. 
                      if (TABLE10_Dim>1){
                      
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE11=as.data.frame(TABLE10%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE11_Dim=as.numeric(nrow(TABLE11))
                
                              # If no records remains after duality filter. 
                              if (TABLE11_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
                
                              # If at least one record remains after duality filter. 
                              if (TABLE11_Dim>0){
                  
                              # Retain only the non-detect records. 
                              TABLE12=subset(TABLE11,result=="Not Detected")
                              TABLE12_Dim=as.numeric(nrow(TABLE12))
                  
                                  # If no non-detect records remain. 
                                  if (TABLE12_Dim==0){
                                  # Tell the user insufficient records exist.
                                  line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                                  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if no records exist. 
                  
                                  # If non-detect records remain. 
                                  if (TABLE12_Dim>0){
                                  # Add the record to the output file. 
                                  OUTPUT=rbind(OUTPUT, TABLE12)
		    		  # Add to the report.
    				  line="<p>Elk, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
    				  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if one and not detected. 
                  
                              } # If at least one record remains after duality filter. 
                        } # End if two or more lines exist in summary table. 
                } # End if elk female yearling hunter harvest contains data. 
            
          # PRIOR________________________________________________________________
          # Get summary for Elk + Hunter Harvest + Adult + Male prior.
          ELK_HH_Male=subset(ELK_HH,sex=="Male" & age_group=="Adult")
          ElkHHDimM=as.numeric(nrow(ELK_HH_Male))
            
              # if ELK hunter harvest Male is empty.
              if (ElkHHDimM==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if male adult hunter harvest is empty. 
            
              # if ELK hunter harvest Male adult contains data.             
              if (ElkHHDimM>0){
                
              # Summarize the data. 
              TABLE13=as.data.frame(ELK_HH_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
              TABLE13_Dim=as.numeric(nrow(TABLE13))
              
                  # If only one record exists and its detected, tell user data doesn't fit criteria.
                  if (TABLE13_Dim==1 & TABLE13$result[1]=="Detected"){
                  # Write a note that the data does not fit this prior.  
                  line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                  write(line,file=model_log_filepath,append=TRUE) 
                  } # End if one and detected. 
              
                  # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                  if (TABLE13_Dim==1 & TABLE13$result[1]=="Not Detected"){
                  # Add the record to the output file. 
                  OUTPUT=rbind(OUTPUT, TABLE13)
		  # Add to the report.
    		  line="<p>Elk, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                  } # End if one and not detected. 
              
                  # If two or more lines exist in the summary table. 
                  if (TABLE13_Dim>1){
                    
                  # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                  # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                  TABLE14=as.data.frame(TABLE13%>%group_by(SubAdminName)%>%filter(n()<2))
                  TABLE14_Dim=as.numeric(nrow(TABLE14))
                
                      # If no records remains after duality filter. 
                      if (TABLE14_Dim==0){
                      # Tell the user insufficient records exist.
                      line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if no records exist. 
                
                      # If at least one record remains after duality filter. 
                      if (TABLE14_Dim>0){
                  
                      # Retain only the non-detect records. 
                      TABLE15=subset(TABLE14,result=="Not Detected")
                      TABLE15_Dim=as.numeric(nrow(TABLE15))
                  
                        # If no non-detect records remain. 
                        if (TABLE15_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE)
                        } # End if no records exist. 
                  
                        # If non-detect records remain. 
                        if (TABLE15_Dim>0){
                        # Add the record to the output file. 
                        OUTPUT=rbind(OUTPUT, TABLE15)
		  	# Add to the report.
    		  	line="<p>Elk, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
    		  	write(line,file=model_log_filepath,append=TRUE)
                        } # End if one and not detected. 
                  
                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
          } # End if male hunter-harvest contains data.
      } # End if elk hunter harvest contains data.
        
  # ELK OTHER  ________________________________________________________________         
  ELK_O=subset(ELK,sample_source!="Clinical suspect" & sample_source!="Hunter-harvested")
  ElkODim=as.numeric(nrow(ELK_O))
  
    # If elk other is empty. 
    if (ElkODim==0){
    # Tell the user insufficient records exist.
    line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
    write(line,file=model_log_filepath,append=TRUE)
    } # End elk other is empty. 
  
    # If elk other contains data.
    if (ElkODim>0){
    
      # PRIOR________________________________________________________________
      # Get Elk + Other + All Ages + All Sexes.
      # Replace age data with 'All Ages'. [We do this because the prior for SpeedGoat Estimation tool is "All Ages.'] 
      ELK_O$age_group=rep("All Ages", ElkODim)
      # Replace age data with 'All Ages'. [We do this because the prior for SpeedGoat Estimation tool is "All Sexes.'] 
      ELK_O$sex=rep("All Sexes", ElkODim)
      # Replace sample_source with 'Other'. [We do this because the prior for SpeedGoat Estimation tool is "Other.'] 
      ELK_O$sample_source=rep("Other", ElkODim)
    
      # Summarize the data. 
      TABLE16=as.data.frame(ELK_O%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
      TABLE16_Dim=as.numeric(nrow(TABLE16))
    
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE16_Dim==1 & TABLE16$result[1]=="Detected"){
        # Write a note that the data does not fit this prior.  
        line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
    
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE16_Dim==1 & TABLE16$result[1]=="Not Detected"){
        # Add the record to the output file. 
        OUTPUT=rbind(OUTPUT, TABLE16)
	# Add to the report.
    	line="<p>Elk, Other, All Ages, All Sexes: Prior fulfilled.</p>"
    	write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
    
        # If two or more lines exist in the summary table. 
        if (TABLE16_Dim>1){
      
        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
        TABLE17=as.data.frame(TABLE16%>%group_by(SubAdminName)%>%filter(n()<2))
        TABLE17_Dim=as.numeric(nrow(TABLE17))
      
          # If no records remains after duality filter. 
          if (TABLE17_Dim==0){
          # Tell the user insufficient records exist.
          line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
      
          # If at least one record remains after duality filter. 
          if (TABLE17_Dim>0){
        
          # Retain only the non-detect records. 
          TABLE18=subset(TABLE17,result=="Not Detected")
          TABLE18_Dim=as.numeric(nrow(TABLE18))
        
            # If no non-detect records remain. 
            if (TABLE18_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
        
            # If non-detect records remain. 
            if (TABLE18_Dim>0){
            # Add the record to the output file. 
            OUTPUT=rbind(OUTPUT, TABLE18)
	    # Add to the report.
    	    line="<p>Elk, Other, All Ages, All Sexes: Prior fulfilled.</p>"
    	    write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
        
        } # If at least one record remains after duality filter. 
      } # End if two or more lines exist in summary table. 
    } # End if elk other contains data.
  
  } # End if elk data globally exists.
} # End if elk species selected.

# ______________________________________________________________________________
# SPECIES: MULE DEER ___________________________________________________________
# ______________________________________________________________________________ 
    
# If user selected mule deer. 
if(params$species=="Mule Deer"){
      
  # If mule deer data does not globally exist. 
  if (MuleDim==0){
  # Add information to the report.
  line="<p>We're sorry. You selected mule deer, but there does not exist any mule deer 
	  testing records in the season-year of interest. Please return to the CWD 
	  Data Warehouse and select a different season-year.</p>"
  write(line,file=model_log_filepath,append=TRUE)
  # Quit the session.
  quit(status=70)
  } # End if mule deer data does not globally exist.
      
  # If mule deer data globally exists. 
  if (MuleDim>0){
        
  # Opening Note. 
  # As of August 2024, SpeedGoat Estimation tool mule deer priors are limited to Clinical suspect, 
  # Hunter-harvested, and Other. 
        
  # MULE CLINICAL SUSPECT _______________________________________________________ 
  MULE_CS=subset(MULE,sample_source=="Clinical suspect")
  MuleCSDim=as.numeric(nrow(MULE_CS))
        
    # If mule deer clinical suspect is empty. 
    if (MuleCSDim==0){
    # Tell the user insufficient records exist.
    line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
    write(line,file=model_log_filepath,append=TRUE)  
    line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    } # End mule deer clinical suspect is empty. 
        
    # If mule deer clinical suspect contains data.
    if (MuleCSDim>0){
          
        # PRIOR________________________________________________________________
        # Get summary for Mule Deer + Clinical Suspect + All Ages + Female prior.
        MULE_CS_Female=subset(MULE_CS,sex=="Female")
        MuleCSDimF=as.numeric(nrow(MULE_CS_Female))
          
          # if MULE clinical suspect Female is empty.
          if (MuleCSDimF==0){
          # Tell the user insufficient records exist.
          line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          } # End if female clinical suspect is empty. 
          
          # if MULE clinical suspect Female contains data.             
          if (MuleCSDimF>0){
            
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
          MULE_CS_Female$age_group=rep("All Ages", MuleCSDimF)
          
          # Summarize the data. 
          TABLE19=as.data.frame(MULE_CS_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
          TABLE19_Dim=as.numeric(nrow(TABLE19))
            
              # If only one record exists and its detected, tell user data doesn't fit criteria.
              if (TABLE19_Dim==1 & TABLE19$result[1]=="Detected"){
              # Write a note that the data does not fit this prior.  
              line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
              } # End if one and detected. 
            
              # If only one record exists and its not detected, store the output for use in the Estimation tool. 
              if (TABLE19_Dim==1 & TABLE19$result[1]=="Not Detected"){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE19)
	      # Add to the report.
    	      line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
    	      write(line,file=model_log_filepath,append=TRUE)
              } # End if one and not detected. 
            
              # If two or more lines exist in the summary table. 
              if (TABLE19_Dim>1){
              
                  # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                  # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                  TABLE20=as.data.frame(TABLE19%>%group_by(SubAdminName)%>%filter(n()<2))
                  TABLE20_Dim=as.numeric(nrow(TABLE20))
              
                      # If no records remains after duality filter. 
                      if (TABLE20_Dim==0){
                      # Tell the user insufficient records exist.
                      line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE)  
                      } # End if no records exist. 
              
                      # If at least one record remains after duality filter. 
                      if (TABLE20_Dim>0){
                
                          # Retain only the non-detect records. 
                          TABLE21=subset(TABLE20,result=="Not Detected")
                          TABLE21_Dim=as.numeric(nrow(TABLE21))
                
                              # If no non-detect records remain. 
                              if (TABLE21_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE) 
                              } # End if no records exist. 
                
                              # If non-detect records remain. 
                              if (TABLE21_Dim>0){
                              # Add the record to the output file. 
                              OUTPUT=rbind(OUTPUT, TABLE21)
		  	      # Add to the report.
    		  	      line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
    		  	      write(line,file=model_log_filepath,append=TRUE)
                              } # End if one and not detected. 

                    } # If at least one record remains after duality filter. 
              } # End if two or more lines exist in summary table. 
          } # End if female clinical suspect contains data. 
          
        # PRIOR________________________________________________________________  
        # Get summary for Mule Deer + Clinical Suspect + All Ages + Male prior.
        MULE_CS_Male=subset(MULE_CS,sex=="Male")
        MuleCSDimM=as.numeric(nrow(MULE_CS_Male))
          
            # if MULE clinical suspect Male is empty.
            if (MuleCSDimM==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
            } # End if male clinical suspect is empty. 
          
            # if MULE clinical suspect Male contains data.              
            if (MuleCSDimM>0){
            
            # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is All Ages.] 
            MULE_CS_Male$age_group=rep("All Ages", MuleCSDimM)
            
            # Summarize the data. 
            TABLE22=as.data.frame(MULE_CS_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
            TABLE22_Dim=as.numeric(nrow(TABLE22))
            
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE22_Dim==1 & TABLE22$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
            
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE22_Dim==1 & TABLE22$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE22)
		# Add to the report.
    		line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
            
                # If two or more lines exist in the summary table. 
                if (TABLE22_Dim>1){
              
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE23=as.data.frame(TABLE22%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE23_Dim=as.numeric(nrow(TABLE23))
              
                        # If no records remains after duality filter. 
                        if (TABLE23_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE) 
                        } # End if no records exist. 
              
                        # If at least one record remains after duality filter. 
                        if (TABLE23_Dim>0){
                
                        # Retain only the non-detect records. 
                        TABLE24=subset(TABLE23,result=="Not Detected")
                        TABLE24_Dim=as.numeric(nrow(TABLE24))
                
                            # If no non-detect records remain. 
                            if (TABLE24_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE) 
                            } # End if no records exist. 
                
                            # If non-detect records remain. 
                            if (TABLE24_Dim>0){
                            # Add the record to the output file. 
                            OUTPUT=rbind(OUTPUT, TABLE24)
		  	    # Add to the report.
    		  	    line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
    		  	    write(line,file=model_log_filepath,append=TRUE)
                            } # End if one and not detected. 
                
                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
          } # End if male clinical suspect contains data. 
        } # End If mule deer clinical suspect contains data.
        
  # MULE HUNTER HARVESTED ________________________________________________________________        
  MULE_HH=subset(MULE,sample_source=="Hunter-harvested")
  MuleHHDim=as.numeric(nrow(MULE_HH))
        
      # If mule deer hunter harvest is empty. 
      if (MuleHHDim==0){
      # Tell the user insufficient records exist.
      line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      } # End if mule deer hunter harvest is empty.
        
      # If mule deer hunter harvest contains data.
      if (MuleHHDim>0){
          
          # PRIOR________________________________________________________________
          # Get summary for Mule Deer + Hunter Harvest + Adult + Female prior.
          MULE_HH_Female=subset(MULE_HH,sex=="Female" & age_group=="Adult")
          MuleHHDimF=as.numeric(nrow(MULE_HH_Female))
          
          # if mule deer hunter-harvest Female Adult is empty.
          if (MuleHHDimF==0){
          # Tell the user insufficient records exist.
          line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          } # End if mule deer female adult hunter-harvest is empty. 
          
          # if mule deer hunter-harvest adult Female contains data.            
          if (MuleHHDimF>0){
            
              # Summarize the data. 
              TABLE25=as.data.frame(MULE_HH_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
              TABLE25_Dim=as.numeric(nrow(TABLE25))
            
                  # If only one record exists and its detected, tell user data doesn't fit criteria.
                  if (TABLE25_Dim==1 & TABLE25$result[1]=="Detected"){
                  # Write a note that the data does not fit this prior.  
                  line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                  write(line,file=model_log_filepath,append=TRUE) 
                  } # End if one and detected. 
            
                  # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                  if (TABLE25_Dim==1 & TABLE25$result[1]=="Not Detected"){
                  # Add the record to the output file. 
                  OUTPUT=rbind(OUTPUT, TABLE25)
		  # Add to the report.
    		  line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                  } # End if one and not detected. 
            
                  # If two or more lines exist in the summary table. 
                  if (TABLE25_Dim>1){
              
                      # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                      # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                      TABLE26=as.data.frame(TABLE25%>%group_by(SubAdminName)%>%filter(n()<2))
                      TABLE26_Dim=as.numeric(nrow(TABLE26))
              
                          # If no records remains after duality filter. 
                          if (TABLE26_Dim==0){
                          # Tell the user insufficient records exist.
                          line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE)
                          } # End if no records exist. 
              
                          # If at least one record remains after duality filter. 
                          if (TABLE26_Dim>0){
                
                          # Retain only the non-detect records. 
                          TABLE27=subset(TABLE26,result=="Not Detected")
                          TABLE27_Dim=as.numeric(nrow(TABLE27))
                
                              # If no non-detect records remain. 
                              if (TABLE27_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
                
                              # If non-detect records remain. 
                              if (TABLE27_Dim>0){
                              # Add the record to the output file. 
                              OUTPUT=rbind(OUTPUT, TABLE27)
		  	      # Add to the report.
    		  	      line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
    		  	      write(line,file=model_log_filepath,append=TRUE)
                              } # End if one and not detected. 
                
                        } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table. 
              } # End if female adult hunter-harvest contains data. 
          
          # PRIOR________________________________________________________________
          # Get summary for Mule Deer + Hunter Harvest + Yearling + Female prior.
          MULE_HH_Female2=subset(MULE_HH,sex=="Female" & age_group=="Yearling")
          MuleHHDimF2=as.numeric(nrow(MULE_HH_Female2))
          
          # if MULE hunter-harvest Female yearling is empty.
          if (MuleHHDimF2==0){
          # Tell the user insufficient records exist.
          line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          } # End if mule deer female yearling hunter-harvest is empty. 
          
          # if MULE hunter-harvest yearling Female contains data.             
          if (MuleHHDimF2>0){
            
            # Summarize the data. 
            TABLE28=as.data.frame(MULE_HH_Female2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
            TABLE28_Dim=as.numeric(nrow(TABLE28))
            
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE28_Dim==1 & TABLE28$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>" 
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
            
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE28_Dim==1 & TABLE28$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE28)
		# Add to the report.
    		line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
            
                # If two or more lines exist in the summary table. 
                if (TABLE28_Dim>1){
              
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE29=as.data.frame(TABLE28%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE29_Dim=as.numeric(nrow(TABLE29))
              
                        # If no records remains after duality filter. 
                        if (TABLE29_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE)
                        } # End if no records exist. 
              
                        # If at least one record remains after duality filter. 
                        if (TABLE29_Dim>0){
                
                            # Retain only the non-detect records. 
                            TABLE30=subset(TABLE29,result=="Not Detected")
                            TABLE30_Dim=as.numeric(nrow(TABLE30))
                
                                # If no non-detect records remain. 
                                if (TABLE30_Dim==0){
                                # Tell the user insufficient records exist.
                                line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                                write(line,file=model_log_filepath,append=TRUE)
                                } # End if no records exist. 
                
                                # If non-detect records remain. 
                                if (TABLE30_Dim>0){
                                # Add the record to the output file. 
                                OUTPUT=rbind(OUTPUT, TABLE30)
		  		# Add to the report.
    		  		line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                } # End if one and not detected. 
                
                        } # If at least one record remains after duality filter. 
                  } # End if two or more lines exist in summary table. 
          } # End if mule deer female yearling hunter-harvest contains data. 
          
          # PRIOR________________________________________________________________
          # Get summary for Mule Deer + Hunter Harvest + Fawn + Female prior.
          MULE_HH_Female3=subset(MULE_HH,sex=="Female" & age_group=="Fawn")
          MuleHHDimF3=as.numeric(nrow(MULE_HH_Female3))
          
          # if MULE hunter-harvest Female fawn is empty.
          if (MuleHHDimF3==0){
          # Tell the user insufficient records exist.
          line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          } # End if mule deer female fawn hunter-harvest is empty. 
          
          # if MULE hunter-harvest fawn Female contains data.             
          if (MuleHHDimF3>0){
            
              # Summarize the data. 
              TABLE31=as.data.frame(MULE_HH_Female3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
              TABLE31_Dim=as.numeric(nrow(TABLE31))
            
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE31_Dim==1 & TABLE31$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
            
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE31_Dim==1 & TABLE31$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE31)
		# Add to the report.
    		line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
            
                # If two or more lines exist in the summary table. 
                if (TABLE31_Dim>1){
              
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE32=as.data.frame(TABLE31%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE32_Dim=as.numeric(nrow(TABLE32))
              
                      # If no records remains after duality filter. 
                      if (TABLE32_Dim==0){
                      # Tell the user insufficient records exist.
                      line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if no records exist. 
              
                      # If at least one record remains after duality filter. 
                      if (TABLE32_Dim>0){
                
                          # Retain only the non-detect records. 
                          TABLE33=subset(TABLE32,result=="Not Detected")
                          TABLE33_Dim=as.numeric(nrow(TABLE33))
                
                              # If no non-detect records remain. 
                              if (TABLE33_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
                
                              # If non-detect records remain. 
                              if (TABLE33_Dim>0){
                              # Add the record to the output file. 
                              OUTPUT=rbind(OUTPUT, TABLE33)
		  	      # Add to the report.
    		  	      line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior fulfilled.</p>"
    		  	      write(line,file=model_log_filepath,append=TRUE)
                              } # End if one and not detected. 
                
                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
          } # End if mule deer female fawn hunter-harvest contains data.
          
          # PRIOR________________________________________________________________
          # Get summary for Mule + Hunter Harvest + Adult + Male prior.
          MULE_HH_Male=subset(MULE_HH,sex=="Male" & age_group=="Adult")
          MuleHHDimM=as.numeric(nrow(MULE_HH_Male))
          
          # if MULE hunter-harvest Male is empty.
          if (MuleHHDimM==0){
          # Tell the user insufficient records exist.
          line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          } # End if male adult hunter-harvest is empty. 
          
          # if MULE hunter-harvest Male adult contains data.             
          if (MuleHHDimM>0){
            
              # Summarize the data. 
              TABLE34=as.data.frame(MULE_HH_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
              TABLE34_Dim=as.numeric(nrow(TABLE34))
            
                  # If only one record exists and its detected, tell user data doesn't fit criteria.
                  if (TABLE34_Dim==1 & TABLE34$result[1]=="Detected"){
                  # Write a note that the data does not fit this prior.  
                  line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                  write(line,file=model_log_filepath,append=TRUE) 
                  } # End if one and detected. 
            
                  # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                  if (TABLE34_Dim==1 & TABLE34$result[1]=="Not Detected"){
                  # Add the record to the output file. 
                  OUTPUT=rbind(OUTPUT, TABLE34)
		  # Add to the report.
    		  line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                  } # End if one and not detected. 
            
                  # If two or more lines exist in the summary table. 
                  if (TABLE34_Dim>1){
              
                      # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                      # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                      TABLE35=as.data.frame(TABLE34%>%group_by(SubAdminName)%>%filter(n()<2))
                      TABLE35_Dim=as.numeric(nrow(TABLE35))
              
                          # If no records remains after duality filter. 
                          if (TABLE35_Dim==0){
                          # Tell the user insufficient records exist.
                          line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE)
                          } # End if no records exist. 
              
                          # If at least one record remains after duality filter. 
                          if (TABLE35_Dim>0){
                
                              # Retain only the non-detect records. 
                              TABLE36=subset(TABLE35,result=="Not Detected")
                              TABLE36_Dim=as.numeric(nrow(TABLE36))
                
                                  # If no non-detect records remain. 
                                  if (TABLE36_Dim==0){
                                  # Tell the user insufficient records exist.
                                  line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                                  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if no records exist. 
                
                                  # If non-detect records remain. 
                                  if (TABLE36_Dim>0){
                                  # Add the record to the output file. 
                                  OUTPUT=rbind(OUTPUT, TABLE36)
		  		  # Add to the report.
    		  		  line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
    		  		  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if one and not detected. 
                
                        } # If at least one record remains after duality filter. 
                  } # End if two or more lines exist in summary table. 
            } # End if male adult hunter-harvest contains data.
        
        # PRIOR________________________________________________________________
        # Get summary for Mule Deer + Hunter Harvest + Yearling + Male prior.
        MULE_HH_Male2=subset(MULE_HH,sex=="Male" & age_group=="Yearling")
        MuleHHDimM2=as.numeric(nrow(MULE_HH_Male2))
        
        # if MULE hunter-harvest Male yearling is empty.
        if (MuleHHDimM2==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
        } # End if mule deer male yearling hunter-harvest is empty. 
        
        # if MULE hunter-harvest yearling Male contains data.             
        if (MuleHHDimM2>0){
          
            # Summarize the data. 
            TABLE37=as.data.frame(MULE_HH_Male2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
            TABLE37_Dim=as.numeric(nrow(TABLE37))
          
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE37_Dim==1 & TABLE37$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
          
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE37_Dim==1 & TABLE37$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE37)
		# Add to the report.
    		line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
          
                # If two or more lines exist in the summary table. 
                if (TABLE37_Dim>1){
            
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE38=as.data.frame(TABLE37%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE38_Dim=as.numeric(nrow(TABLE38))
            
                    # If no records remains after duality filter. 
                    if (TABLE38_Dim==0){
                    # Tell the user insufficient records exist.
                    line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE)
                    } # End if no records exist. 
            
                    # If at least one record remains after duality filter. 
                    if (TABLE38_Dim>0){
              
                        # Retain only the non-detect records. 
                        TABLE39=subset(TABLE38,result=="Not Detected")
                        TABLE39_Dim=as.numeric(nrow(TABLE39))
              
                            # If no non-detect records remain. 
                            if (TABLE39_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE)
                            } # End if no records exist. 
              
                            # If non-detect records remain. 
                            if (TABLE39_Dim>0){
                            # Add the record to the output file. 
                            OUTPUT=rbind(OUTPUT, TABLE39)
		  	    # Add to the report.
    		  	    line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior fulfilled.</p>"
    		  	    write(line,file=model_log_filepath,append=TRUE)
                            } # End if one and not detected. 
              
                    } # If at least one record remains after duality filter. 
              } # End if two or more lines exist in summary table. 
        } # End if mule deer male yearling hunter-harvest contains data. 
        
        # PRIOR________________________________________________________________
        # Get summary for Mule Deer + Hunter Harvest + Fawn + Male prior.
        MULE_HH_Male3=subset(MULE_HH,sex=="Male" & age_group=="Fawn")
        MuleHHDimM3=as.numeric(nrow(MULE_HH_Male3))
        
        # if MULE hunter-harvest Male fawn is empty.
        if (MuleHHDimM3==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
        } # End if mule deer male fawn hunter-harvest is empty. 
        
        # if MULE hunter-harvest fawn Male contains data.             
        if (MuleHHDimM3>0){
          
            # Summarize the data. 
            TABLE40=as.data.frame(MULE_HH_Male3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
            TABLE40_Dim=as.numeric(nrow(TABLE40))
          
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE40_Dim==1 & TABLE40$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
          
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE40_Dim==1 & TABLE40$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE40)
		# Add to the report.
    		line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
          
                # If two or more lines exist in the summary table. 
                if (TABLE40_Dim>1){
            
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE42=as.data.frame(TABLE40%>%group_by(SubAdminName)%>%filter(n()<2)) # Note the random skip of 41. It is ok. 
                    TABLE42_Dim=as.numeric(nrow(TABLE42))
            
                        # If no records remains after duality filter. 
                        if (TABLE42_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE) 
                        } # End if no records exist. 
            
                        # If at least one record remains after duality filter. 
                        if (TABLE42_Dim>0){
              
                            # Retain only the non-detect records. 
                            TABLE43=subset(TABLE42,result=="Not Detected")
                            TABLE43_Dim=as.numeric(nrow(TABLE43))
              
                                # If no non-detect records remain. 
                                if (TABLE43_Dim==0){
                                # Tell the user insufficient records exist.
                                line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
                                write(line,file=model_log_filepath,append=TRUE) 
                                } # End if no records exist. 
              
                                # If non-detect records remain. 
                                if (TABLE43_Dim>0){
                                # Add the record to the output file. 
                                OUTPUT=rbind(OUTPUT, TABLE43)
		  		# Add to the report.
    		  		line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                } # End if one and not detected. 
              
                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
        } # End if mule deer male fawn hunter-harvest contains data.
      } # #End if mule deer hunter harvest contains data. 
        
  # MULE OTHER  ________________________________________________________________         
  MULE_O=subset(MULE,sample_source!="Clinical suspect" & sample_source!="Hunter-harvested")
  MuleODim=as.numeric(nrow(MULE_O))
        
  # If mule deer other is empty. 
  if (MuleODim==0){
  # Tell the user insufficient records exist.
  line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
  write(line,file=model_log_filepath,append=TRUE) 
  } # End mule deer other is empty. 
        
  # If mule deer other contains data.
  if (MuleODim>0){
          
          # PRIOR________________________________________________________________
          # Get Mule + Other + All Ages + All Sexes.
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
          MULE_O$age_group=rep("All Ages", MuleODim)
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Sexes.'] 
          MULE_O$sex=rep("All Sexes", MuleODim)
          # Replace sample source with 'Other'. [We do this because the prior for the SpeedGoat Estimation tool is "Other.'] 
          MULE_O$sample_source=rep("Other", MuleODim)
          
          # Summarize the data. 
          TABLE44=as.data.frame(MULE_O%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
          TABLE44_Dim=as.numeric(nrow(TABLE44))
          
          # If only one record exists and its detected, tell user data doesn't fit criteria.
          if (TABLE44_Dim==1 & TABLE44$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          } # End if one and detected. 
          
          # If only one record exists and its not detected, store the output for use in the Estimation tool. 
          if (TABLE44_Dim==1 & TABLE44$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE44)
	  # Add to the report.
    	  line="<p>Mule Deer, Other, All Ages, All Sexes: Prior fulfilled.</p>"
    	  write(line,file=model_log_filepath,append=TRUE)
          } # End if one and not detected. 
          
          # If two or more lines exist in the summary table. 
          if (TABLE44_Dim>1){
            
              # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
              # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
              TABLE45=as.data.frame(TABLE44%>%group_by(SubAdminName)%>%filter(n()<2))
              TABLE45_Dim=as.numeric(nrow(TABLE45))
            
                  # If no records remains after duality filter. 
                  if (TABLE45_Dim==0){
                  # Tell the user insufficient records exist.
                  line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
                  write(line,file=model_log_filepath,append=TRUE)  
                  } # End if no records exist. 
            
                  # If at least one record remains after duality filter. 
                  if (TABLE45_Dim>0){
              
                      # Retain only the non-detect records. 
                      TABLE46=subset(TABLE45,result=="Not Detected")
                      TABLE46_Dim=as.numeric(nrow(TABLE46))
              
                          # If no non-detect records remain. 
                          if (TABLE46_Dim==0){
                          # Tell the user insufficient records exist.
                          line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE) 
                          } # End if no records exist. 
              
                          # If non-detect records remain. 
                          if (TABLE46_Dim>0){
                          # Add the record to the output file. 
                          OUTPUT=rbind(OUTPUT, TABLE46)
		  	  # Add to the report.
    		  	  line="<p>Mule Deer, Other, All Ages, All Sexes: Prior fulfilled.</p>"
    		  	  write(line,file=model_log_filepath,append=TRUE)
                          } # End if one and not detected. 
              
                } # If at least one record remains after duality filter. 
            } # End if two or more lines exist in summary table. 
        } # End if mule deer other contains data.
  
  } # End if mule deer data globally exists.
} # End if mule deer species selected.
    
# ______________________________________________________________________________
# SPECIES: WHITE-TAILED DEER ___________________________________________________
# ______________________________________________________________________________ 
    
# If user selected white-tailed deer. 
if(params$species=="White-tailed Deer"){

  # If white-tailed deer data does not globally exist. 
  if (WTDDim==0){
  # Add information to the report.
  line="<p>We're sorry. You selected white-tailed deer, but there does not exist any white-tailed 
	  deer testing records in the season-year of interest. Please return to the CWD Data 
	  Warehouse and select a different season-year.</p>"
  write(line,file=model_log_filepath,append=TRUE) 
  # Quit the session.
  quit(status=70)
  } # End if white-tailed deer data does not globally exist.
      
  # If white-tailed deer data globally exists. 
  if (WTDDim>0){
    
    line="<p>The prior for White-tailed Deer, Clinical suspect; community reported, All Ages, 
	  All Sexes is not reported, because the CWD Data Warehouse and SpeedGoat Estimation tool definitions differ.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    line="<p>The prior for White-tailed Deer, Clinical suspect; hunter reported, All Ages, 
	  All Sexes is not reported, because the CWD Data Warehouse and SpeedGoat Estimation tool definitions differ.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
        
      # Opening Note. 
      # As of August 2024, SpeedGoat Estimation tool white-tailed deer priors are limited to 
      # Hunter-harvested, Sharp shot, Vehicle collision, Found dead, and Clinical 
      # suspect (community reported and hunter reported).
        
      # WTD HUNTER HARVESTED ________________________________________________________________        
      WTD_HH=subset(WTD,sample_source=="Hunter-harvested")
      WTDHHDim=as.numeric(nrow(WTD_HH))
        
          # If white-tailed deer hunter harvest is empty. 
          if (WTDHHDim==0){
          # Tell the user insufficient records exist.
          line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          } # End if white-tailed deer hunter harvest is empty.
        
          # If white-tailed deer hunter harvest contains data.
          if (WTDHHDim>0){
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed Deer + Hunter Harvest + Adult + Female prior.
          WTD_HH_Female=subset(WTD_HH,sex=="Female" & age_group=="Adult")
          WTDHHDimF=as.numeric(nrow(WTD_HH_Female))
          
            # if WTD hunter-harvest Female Adult is empty.
            if (WTDHHDimF==0){
            # Tell the user insufficient records exist.
            line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
            } # End if white-tailed deer female adult hunter-harvest is empty. 
          
            # if WTD hunter-harvest adult Female contains data.            
            if (WTDHHDimF>0){
            
            # Summarize the data. 
            TABLE47=as.data.frame(WTD_HH_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
            TABLE47_Dim=as.numeric(nrow(TABLE47))
            
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE47_Dim==1 & TABLE47$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
            
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE47_Dim==1 & TABLE47$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE47)
		# Add to the report.
    		line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
            
                # If two or more lines exist in the summary table. 
                if (TABLE47_Dim>1){
              
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE48=as.data.frame(TABLE47%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE48_Dim=as.numeric(nrow(TABLE48))
              
                        # If no records remains after duality filter. 
                        if (TABLE48_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE)
                        } # End if no records exist. 
              
                        # If at least one record remains after duality filter. 
                        if (TABLE48_Dim>0){
                
                            # Retain only the non-detect records. 
                            TABLE49=subset(TABLE48,result=="Not Detected")
                            TABLE49_Dim=as.numeric(nrow(TABLE49))
                
                                # If no non-detect records remain. 
                                if (TABLE49_Dim==0){
                                # Tell the user insufficient records exist.
                                line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
                                write(line,file=model_log_filepath,append=TRUE) 
                                } # End if no records exist. 
                
                                # If non-detect records remain. 
                                if (TABLE49_Dim>0){
                                # Add the record to the output file. 
                                OUTPUT=rbind(OUTPUT, TABLE49)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                } # End if one and not detected. 

                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
          } # End if female adult hunter-harvest contains data. 
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed Deer + Hunter Harvest + Yearling + Female prior.
          WTD_HH_Female2=subset(WTD_HH,sex=="Female" & age_group=="Yearling")
          WTDHHDimF2=as.numeric(nrow(WTD_HH_Female2))
          
              # if WTD hunter-harvest Female yearling is empty.
              if (WTDHHDimF2==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if white-tailed deer female yearling hunter-harvest is empty. 
          
              # if WTD hunter-harvest yearling Female contains data.             
              if (WTDHHDimF2>0){
            
                  # Summarize the data. 
                  TABLE50=as.data.frame(WTD_HH_Female2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE50_Dim=as.numeric(nrow(TABLE50))
            
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE50_Dim==1 & TABLE50$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
            
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE50_Dim==1 & TABLE50$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE50)
		      # Add to the report.
    		      line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
    		      write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
            
                      # If two or more lines exist in the summary table. 
                      if (TABLE50_Dim>1){
              
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE51=as.data.frame(TABLE50%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE51_Dim=as.numeric(nrow(TABLE51))
              
                          # If no records remains after duality filter. 
                          if (TABLE51_Dim==0){
                          # Tell the user insufficient records exist.
                          line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE)
                          } # End if no records exist. 
              
                          # If at least one record remains after duality filter. 
                          if (TABLE51_Dim>0){
                
                              # Retain only the non-detect records. 
                              TABLE52=subset(TABLE51,result=="Not Detected")
                              TABLE52_Dim=as.numeric(nrow(TABLE52))
                
                                  # If no non-detect records remain. 
                                  if (TABLE52_Dim==0){
                                  # Tell the user insufficient records exist.
                                  line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
                                  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if no records exist. 
                
                                  # If non-detect records remain. 
                                  if (TABLE52_Dim>0){
                                  # Add the record to the output file. 
                                  OUTPUT=rbind(OUTPUT, TABLE52)
		  		  # Add to the report.
    		  		  line="<p>White-tailed Deer, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
    		  		  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if one and not detected. 
                
                          } # If at least one record remains after duality filter. 
                  } # End if two or more lines exist in summary table. 
          } # End if white-tailed deer female yearling hunter-harvest contains data. 
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed Deer + Hunter Harvest + Fawn + Female prior.
          WTD_HH_Female3=subset(WTD_HH,sex=="Female" & age_group=="Fawn")
          WTDHHDimF3=as.numeric(nrow(WTD_HH_Female3))
          
              # if WTD hunter-harvest Female fawn is empty.
              if (WTDHHDimF3==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
              } # End if white-tailed deer female fawn hunter-harvest is empty. 
          
              # if WTD hunter-harvest fawn Female contains data.             
              if (WTDHHDimF3>0){
            
                  # Summarize the data. 
                  TABLE53=as.data.frame(WTD_HH_Female3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE53_Dim=as.numeric(nrow(TABLE53))
            
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE53_Dim==1 & TABLE53$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
            
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE53_Dim==1 & TABLE53$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE53)
		  	# Add to the report.
    		  	line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior fulfilled.</p>"
    		  	write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
            
                      # If two or more lines exist in the summary table. 
                      if (TABLE53_Dim>1){
              
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE54=as.data.frame(TABLE53%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE54_Dim=as.numeric(nrow(TABLE54))
              
                              # If no records remains after duality filter. 
                              if (TABLE54_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
              
                              # If at least one record remains after duality filter. 
                              if (TABLE54_Dim>0){
                
                                  # Retain only the non-detect records. 
                                  TABLE55=subset(TABLE54,result=="Not Detected")
                                  TABLE55_Dim=as.numeric(nrow(TABLE55))
                
                                      # If no non-detect records remain. 
                                      if (TABLE55_Dim==0){
                                      # Tell the user insufficient records exist.
                                      line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
                                      write(line,file=model_log_filepath,append=TRUE) 
                                      } # End if no records exist. 
                
                                      # If non-detect records remain. 
                                      if (TABLE55_Dim>0){
                                      # Add the record to the output file. 
                                      OUTPUT=rbind(OUTPUT, TABLE55)
		  			# Add to the report.
    		  			line="<p>White-tailed Deer, Hunter-harvested, Fawn, Female: Prior fulfilled.</p>"
    		  			write(line,file=model_log_filepath,append=TRUE)
                                      } # End if one and not detected. 
                
                            } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table. 
              } # End if white-tailed deer female fawn hunter-harvest contains data.
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed + Hunter Harvest + Adult + Male prior.
          WTD_HH_Male=subset(WTD_HH,sex=="Male" & age_group=="Adult")
          WTDHHDimM=as.numeric(nrow(WTD_HH_Male))
          
              # if WTD hunter-harvest Male is empty.
              if (WTDHHDimM==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if male adult hunter-harvest is empty. 
          
              # if WTD hunter-harvest Male adult contains data.             
              if (WTDHHDimM>0){
            
                # Summarize the data. 
                TABLE56=as.data.frame(WTD_HH_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE56_Dim=as.numeric(nrow(TABLE56))
            
                # If only one record exists and its detected, tell user data doesn't fit criteria.
                if (TABLE56_Dim==1 & TABLE56$result[1]=="Detected"){
                # Write a note that the data does not fit this prior.  
                line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                write(line,file=model_log_filepath,append=TRUE) 
                } # End if one and detected. 
            
                # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                if (TABLE56_Dim==1 & TABLE56$result[1]=="Not Detected"){
                # Add the record to the output file. 
                OUTPUT=rbind(OUTPUT, TABLE56)
		# Add to the report.
    		line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
    		write(line,file=model_log_filepath,append=TRUE)
                } # End if one and not detected. 
            
                # If two or more lines exist in the summary table. 
                if (TABLE56_Dim>1){
              
                    # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                    # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                    TABLE57=as.data.frame(TABLE56%>%group_by(SubAdminName)%>%filter(n()<2))
                    TABLE57_Dim=as.numeric(nrow(TABLE57))
              
                        # If no records remains after duality filter. 
                        if (TABLE57_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE)
                        } # End if no records exist. 
              
                        # If at least one record remains after duality filter. 
                        if (TABLE57_Dim>0){
                
                            # Retain only the non-detect records. 
                            TABLE58=subset(TABLE57,result=="Not Detected")
                            TABLE58_Dim=as.numeric(nrow(TABLE58))
                
                                # If no non-detect records remain. 
                                if (TABLE58_Dim==0){
                                # Tell the user insufficient records exist.
                                line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
                                write(line,file=model_log_filepath,append=TRUE)
                                } # End if no records exist. 
                
                                # If non-detect records remain. 
                                if (TABLE58_Dim>0){
                                # Add the record to the output file. 
                                OUTPUT=rbind(OUTPUT, TABLE58)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                } # End if one and not detected. 
                
                        } # If at least one record remains after duality filter. 
                  } # End if two or more lines exist in summary table. 
            } # End if male adult hunter-harvest contains data.
        
        # PRIOR________________________________________________________________
        # Get summary for White-tailed Deer + Hunter Harvest + Yearling + Male prior.
        WTD_HH_Male2=subset(WTD_HH,sex=="Male" & age_group=="Yearling")
        WTDHHDimM2=as.numeric(nrow(WTD_HH_Male2))
        
            # if WTD hunter-harvest Male yearling is empty.
            if (WTDHHDimM2==0){
            # Tell the user insufficient records exist.
            line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
            } # End if white-tailed deer male yearling hunter-harvest is empty. 
        
            # if WTD hunter-harvest yearling Male contains data.             
            if (WTDHHDimM2>0){
          
                # Summarize the data. 
                TABLE59=as.data.frame(WTD_HH_Male2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE59_Dim=as.numeric(nrow(TABLE59))
          
                    # If only one record exists and its detected, tell user data doesn't fit criteria.
                    if (TABLE59_Dim==1 & TABLE59$result[1]=="Detected"){
                    # Write a note that the data does not fit this prior.  
                    line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and detected. 
          
                    # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                    if (TABLE59_Dim==1 & TABLE59$result[1]=="Not Detected"){
                    # Add the record to the output file. 
                    OUTPUT=rbind(OUTPUT, TABLE59)
		  # Add to the report.
    		  line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                    } # End if one and not detected. 
          
                    # If two or more lines exist in the summary table. 
                    if (TABLE59_Dim>1){
            
                        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                        TABLE60=as.data.frame(TABLE59%>%group_by(SubAdminName)%>%filter(n()<2))
                        TABLE60_Dim=as.numeric(nrow(TABLE60))
            
                        # If no records remains after duality filter. 
                        if (TABLE60_Dim==0){
                        # Tell the user insufficient records exist.
                        line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
                        write(line,file=model_log_filepath,append=TRUE) 
                        } # End if no records exist. 
            
                        # If at least one record remains after duality filter. 
                        if (TABLE60_Dim>0){
              
                            # Retain only the non-detect records. 
                            TABLE61=subset(TABLE60,result=="Not Detected")
                            TABLE61_Dim=as.numeric(nrow(TABLE61))
              
                                # If no non-detect records remain. 
                                if (TABLE61_Dim==0){
                                # Tell the user insufficient records exist.
                                line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
                                write(line,file=model_log_filepath,append=TRUE)
                                } # End if no records exist. 
              
                                # If non-detect records remain. 
                                if (TABLE61_Dim>0){
                                # Add the record to the output file. 
                                OUTPUT=rbind(OUTPUT, TABLE61)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Hunter-harvested, Yearling, Male: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                } # End if one and not detected. 
              
                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
            } # End if white-tailed deer male yearling hunter-harvest contains data. 
        
        # PRIOR________________________________________________________________
        # Get summary for White-tailed Deer + Hunter Harvest + Fawn + Male prior.
        WTD_HH_Male3=subset(WTD_HH,sex=="Male" & age_group=="Fawn")
        WTDHHDimM3=as.numeric(nrow(WTD_HH_Male3))
        
            # if WTD hunter-harvest Male fawn is empty.
            if (WTDHHDimM3==0){
            # Tell the user insufficient records exist.
            line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
            } # End if white-tailed deer male fawn hunter-harvest is empty. 
        
            # if WTD hunter-harvest fawn Male contains data.             
            if (WTDHHDimM3>0){
          
                # Summarize the data. 
                TABLE62=as.data.frame(WTD_HH_Male3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE62_Dim=as.numeric(nrow(TABLE62))
          
                    # If only one record exists and its detected, tell user data doesn't fit criteria.
                    if (TABLE62_Dim==1 & TABLE62$result[1]=="Detected"){
                    # Write a note that the data does not fit this prior.  
                    line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and detected. 
          
                    # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                    if (TABLE62_Dim==1 & TABLE62$result[1]=="Not Detected"){
                    # Add the record to the output file. 
                    OUTPUT=rbind(OUTPUT, TABLE62)
		  # Add to the report.
    		  line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                    } # End if one and not detected. 
          
                    # If two or more lines exist in the summary table. 
                    if (TABLE62_Dim>1){
            
                        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                        TABLE63=as.data.frame(TABLE62%>%group_by(SubAdminName)%>%filter(n()<2)) # Note the random skip of 41. It is ok. 
                        TABLE63_Dim=as.numeric(nrow(TABLE63))
            
                            # If no records remains after duality filter. 
                            if (TABLE63_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE)
                            } # End if no records exist. 
            
                            # If at least one record remains after duality filter. 
                            if (TABLE63_Dim>0){
              
                                # Retain only the non-detect records. 
                                TABLE64=subset(TABLE63,result=="Not Detected")
                                TABLE64_Dim=as.numeric(nrow(TABLE64))
              
                                    # If no non-detect records remain. 
                                    if (TABLE64_Dim==0){
                                    # Tell the user insufficient records exist.
                                    line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
                                    write(line,file=model_log_filepath,append=TRUE)
                                    } # End if no records exist. 
              
                                    # If non-detect records remain. 
                                    if (TABLE64_Dim>0){
                                    # Add the record to the output file. 
                                    OUTPUT=rbind(OUTPUT, TABLE64)
		  			# Add to the report.
    		  			line="<p>White-tailed Deer, Hunter-harvested, Fawn, Male: Prior fulfilled.</p>"
    		  			 write(line,file=model_log_filepath,append=TRUE)
                                    } # End if one and not detected. 
              
                            } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table. 
            } # End if white-tailed deer male fawn hunter-harvest contains data.   
        
  } # End if white-tailed deer hunter-harvest contains data.
  
      # WTD VEHICLE  ________________________________________________________________         
      WTD_VC=subset(WTD,sample_source=="Vehicle collision (direct or indirect)")
      WTDVCDim=as.numeric(nrow(WTD_VC))
        
        # If white-tailed deer vehicle collision is empty. 
        if (WTDVCDim==0){
        # Tell the user insufficient records exist.
        line="<p>White-tailed Deer, Vehicle collision (direct or indirect), All Ages, All Sexes: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
        } # End white-tailed deer vehicle collision is empty. 
        
        # If white-tailed deer vehicle collision contains data.
        if (WTDVCDim>0){
          
          # PRIOR________________________________________________________________
          # Get White-tailed + vehicle collision + All Ages + All Sexes.
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
          WTD_VC$age_group=rep("All Ages", WTDVCDim)
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Sexes.'] 
          WTD_VC$sex=rep("All Sexes", WTDVCDim)
          
              # Summarize the data. 
              TABLE65=as.data.frame(WTD_VC%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
              TABLE65_Dim=as.numeric(nrow(TABLE65))
          
              # If only one record exists and its detected, tell user data doesn't fit criteria.
              if (TABLE65_Dim==1 & TABLE65$result[1]=="Detected"){
              # Write a note that the data does not fit this prior.  
              line="<p>White-tailed Deer, Vehicle collision (direct or indirect), All Ages, All Sexes: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
              } # End if one and detected. 
          
              # If only one record exists and its not detected, store the output for use in the Estimation tool. 
              if (TABLE65_Dim==1 & TABLE65$result[1]=="Not Detected"){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE65)
		# Add to the report.
    		line="<p>White-tailed Deer, Vehicle collision (direct or indirect), All Ages, All Sexes: Prior fulfilled.</p>"
    		 write(line,file=model_log_filepath,append=TRUE)
              } # End if one and not detected. 
          
              # If two or more lines exist in the summary table. 
              if (TABLE65_Dim>1){
            
                  # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                  # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                  TABLE66=as.data.frame(TABLE65%>%group_by(SubAdminName)%>%filter(n()<2))
                  TABLE66_Dim=as.numeric(nrow(TABLE66))
            
                      # If no records remains after duality filter. 
                      if (TABLE66_Dim==0){
                      # Tell the user insufficient records exist.
                      line="<p>White-tailed Deer, Vehicle collision (direct or indirect), All Ages, All Sexes: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE)
                      } # End if no records exist. 
            
                      # If at least one record remains after duality filter. 
                      if (TABLE66_Dim>0){
              
                          # Retain only the non-detect records. 
                          TABLE67=subset(TABLE66,result=="Not Detected")
                          TABLE67_Dim=as.numeric(nrow(TABLE67))
              
                              # If no non-detect records remain. 
                              if (TABLE67_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>White-tailed Deer, Vehicle collision (direct or indirect), All Ages, All Sexes: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
              
                              # If non-detect records remain. 
                              if (TABLE67_Dim>0){
                              # Add the record to the output file. 
                              OUTPUT=rbind(OUTPUT, TABLE67)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Vehicle collision (direct or indirect), All Ages, All Sexes: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                              } # End if one and not detected. 
              
                      } # If at least one record remains after duality filter. 
                } # End if two or more lines exist in summary table. 
        } # End if white-tailed deer vehicle collision contains data. 
        
      # WTD FOUND DEAD  ________________________________________________________________         
      WTD_FD=subset(WTD,sample_source=="Found dead")
      WTDFDDim=as.numeric(nrow(WTD_FD))
        
          # If white-tailed deer found dead is empty. 
          if (WTDFDDim==0){
          # Tell the user insufficient records exist.
          line="<p>White-tailed Deer, Found dead, All Ages, All Sexes: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          } # End white-tailed deer found dead is empty. 
        
          # If white-tailed deer found dead contains data.
          if (WTDFDDim>0){
          
          # PRIOR________________________________________________________________
          # Get White-tailed + Found Dead + All Ages + All Sexes.
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
          WTD_FD$age_group=rep("All Ages", WTDFDDim)
          # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Sexes.'] 
          WTD_FD$sex=rep("All Sexes", WTDFDDim)
          
              # Summarize the data. 
              TABLE69=as.data.frame(WTD_FD%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
              TABLE69_Dim=as.numeric(nrow(TABLE69))
          
                  # If only one record exists and its detected, tell user data doesn't fit criteria.
                  if (TABLE69_Dim==1 & TABLE69$result[1]=="Detected"){
                  # Write a note that the data does not fit this prior.  
                  line="<p>White-tailed Deer, Found dead, All Ages, All Sexes: Prior unfulfilled.</p>"
                  write(line,file=model_log_filepath,append=TRUE) 
                  } # End if one and detected. 
          
                  # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                  if (TABLE69_Dim==1 & TABLE69$result[1]=="Not Detected"){
                  # Add the record to the output file. 
                  OUTPUT=rbind(OUTPUT, TABLE69)
		  # Add to the report.
    		  line="<p>White-tailed Deer, Found dead, All Ages, All Sexes: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                  } # End if one and not detected. 
          
                  # If two or more lines exist in the summary table. 
                  if (TABLE69_Dim>1){
            
                      # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                      # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                      TABLE70=as.data.frame(TABLE69%>%group_by(SubAdminName)%>%filter(n()<2))
                      TABLE70_Dim=as.numeric(nrow(TABLE70))
            
                          # If no records remains after duality filter. 
                          if (TABLE70_Dim==0){
                          # Tell the user insufficient records exist.
                          line="<p>White-tailed Deer, Found dead, All Ages, All Sexes: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE) 
                          } # End if no records exist. 
            
                          # If at least one record remains after duality filter. 
                          if (TABLE70_Dim>0){
              
                              # Retain only the non-detect records. 
                              TABLE71=subset(TABLE70,result=="Not Detected")
                              TABLE71_Dim=as.numeric(nrow(TABLE71))
              
                                  # If no non-detect records remain. 
                                  if (TABLE71_Dim==0){
                                  # Tell the user insufficient records exist.
                                  line="<p>White-tailed Deer, Found dead, All Ages, All Sexes: Prior unfulfilled.</p>"
                                  write(line,file=model_log_filepath,append=TRUE) 
                                  } # End if no records exist. 
              
                                  # If non-detect records remain. 
                                  if (TABLE71_Dim>0){
                                  # Add the record to the output file. 
                                  OUTPUT=rbind(OUTPUT, TABLE71)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Found dead, All Ages, All Sexes: Prior fulfilled.</p>"
    		  		  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if one and not detected. 
              
                          } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table.
              
            } # End if white-tailed deer found dead contains data.  
        
      # WTD SHARP SHOT ________________________________________________________________        
      WTD_SS=subset(WTD,sample_source=="Sharp shot")
      WTDSSDim=as.numeric(nrow(WTD_SS))
        
          # If white-tailed deer sharp shot is empty. 
          if (WTDSSDim==0){
          # Tell the user insufficient records exist.
          line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
          line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
          } # End if white-tailed deer sharp shot is empty.
        
          # If white-tailed deer sharp shot contains data.
          if (WTDSSDim>0){
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed Deer + Hunter Harvest + Adult + Female prior.
          WTD_SS_Female=subset(WTD_SS,sex=="Female" & age_group=="Adult")
          WTDSSDimF=as.numeric(nrow(WTD_SS_Female))
          
              # if WTD sharp shot Female Adult is empty.
              if (WTDSSDimF==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
              } # End if white-tailed deer female adult sharp shot is empty. 
          
              # if WTD sharp shot adult Female contains data.            
              if (WTDSSDimF>0){
            
                  # Summarize the data. 
                  TABLE72=as.data.frame(WTD_SS_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE72_Dim=as.numeric(nrow(TABLE72))
            
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE72_Dim==1 & TABLE72$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
            
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE72_Dim==1 & TABLE72$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE72)
		  	# Add to the report.
    		  	line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior fulfilled.</p>"
    		  	write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
            
                      # If two or more lines exist in the summary table. 
                      if (TABLE72_Dim>1){
              
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE73=as.data.frame(TABLE72%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE73_Dim=as.numeric(nrow(TABLE73))
              
                              # If no records remains after duality filter. 
                              if (TABLE73_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
              
                              # If at least one record remains after duality filter. 
                              if (TABLE73_Dim>0){
                
                                  # Retain only the non-detect records. 
                                  TABLE74=subset(TABLE73,result=="Not Detected")
                                  TABLE74_Dim=as.numeric(nrow(TABLE74))
                
                                      # If no non-detect records remain. 
                                      if (TABLE74_Dim==0){
                                      # Tell the user insufficient records exist.
                                      line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior unfulfilled.</p>"
                                      write(line,file=model_log_filepath,append=TRUE)
                                      } # End if no records exist. 
                
                                      # If non-detect records remain. 
                                      if (TABLE74_Dim>0){
                                      # Add the record to the output file. 
                                      OUTPUT=rbind(OUTPUT, TABLE74)
		  			# Add to the report.
    		  			line="<p>White-tailed Deer, Sharp shot, Adult, Female: Prior fulfilled.</p>"
    		  			write(line,file=model_log_filepath,append=TRUE)
                                      } # End if one and not detected. 
                
                              } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table. 
              } # End if female adult sharp shot contains data. 
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed Deer + Hunter Harvest + Yearling + Female prior.
          WTD_SS_Female2=subset(WTD_SS,sex=="Female" & age_group=="Yearling")
          WTDSSDimF2=as.numeric(nrow(WTD_SS_Female2))
          
              # if WTD sharp shot Female yearling is empty.
              if (WTDSSDimF2==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
              } # End if white-tailed deer female yearling sharp shot is empty. 
          
              # if WTD sharp shot yearling Female contains data.             
              if (WTDSSDimF2>0){
            
                      # Summarize the data. 
                      TABLE75=as.data.frame(WTD_SS_Female2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                      TABLE75_Dim=as.numeric(nrow(TABLE75))
            
                          # If only one record exists and its detected, tell user data doesn't fit criteria.
                          if (TABLE75_Dim==1 & TABLE75$result[1]=="Detected"){
                          # Write a note that the data does not fit this prior.  
                          line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior unfulfilled.</p>"
                          write(line,file=model_log_filepath,append=TRUE) 
                          } # End if one and detected. 
            
                          # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                          if (TABLE75_Dim==1 & TABLE75$result[1]=="Not Detected"){
                          # Add the record to the output file. 
                          OUTPUT=rbind(OUTPUT, TABLE75)
		  	# Add to the report.
    		  	line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior fulfilled.</p>"
    		  	write(line,file=model_log_filepath,append=TRUE)
                          } # End if one and not detected. 
            
                          # If two or more lines exist in the summary table. 
                          if (TABLE75_Dim>1){
              
                              # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                              # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                              TABLE76=as.data.frame(TABLE75%>%group_by(SubAdminName)%>%filter(n()<2))
                              TABLE76_Dim=as.numeric(nrow(TABLE76))
              
                                  # If no records remains after duality filter. 
                                  if (TABLE76_Dim==0){
                                  # Tell the user insufficient records exist.
                                  line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior unfulfilled.</p>"
                                  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if no records exist. 
              
                                  # If at least one record remains after duality filter. 
                                  if (TABLE76_Dim>0){
                
                                      # Retain only the non-detect records. 
                                      TABLE77=subset(TABLE76,result=="Not Detected")
                                      TABLE77_Dim=as.numeric(nrow(TABLE77))
                
                                          # If no non-detect records remain. 
                                          if (TABLE77_Dim==0){
                                          # Tell the user insufficient records exist.
                                          line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior unfulfilled.</p>"
                                          write(line,file=model_log_filepath,append=TRUE)
                                          } # End if no records exist. 
                
                                          # If non-detect records remain. 
                                          if (TABLE77_Dim>0){
                                          # Add the record to the output file. 
                                          OUTPUT=rbind(OUTPUT, TABLE77)
		  			# Add to the report.
    		  			line="<p>White-tailed Deer, Sharp shot, Yearling, Female: Prior fulfilled.</p>"
    		  			write(line,file=model_log_filepath,append=TRUE)
                                          } # End if one and not detected. 
                
                                  } # If at least one record remains after duality filter.
                              
                        } # End if two or more lines exist in summary table. 
              } # End if white-tailed deer female yearling sharp shot contains data. 
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed Deer + Hunter Harvest + Fawn + Female prior.
          WTD_SS_Female3=subset(WTD_SS,sex=="Female" & age_group=="Fawn")
          WTDSSDimF3=as.numeric(nrow(WTD_SS_Female3))
          
              # if WTD sharp shot Female fawn is empty.
              if (WTDSSDimF3==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if white-tailed deer female fawn sharp shot is empty. 
          
              # if WTD sharp shot fawn Female contains data.             
              if (WTDSSDimF3>0){
            
                  # Summarize the data. 
                  TABLE78=as.data.frame(WTD_SS_Female3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE78_Dim=as.numeric(nrow(TABLE78))
            
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE78_Dim==1 & TABLE78$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior unfulfilled.</p>" 
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
            
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE78_Dim==1 & TABLE78$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE78)
		  	# Add to the report.
    		  	line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior fulfilled.</p>"
    		  	write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
            
                      # If two or more lines exist in the summary table. 
                      if (TABLE78_Dim>1){
              
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE79=as.data.frame(TABLE78%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE79_Dim=as.numeric(nrow(TABLE79))
              
                              # If no records remains after duality filter. 
                              if (TABLE79_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
              
                              # If at least one record remains after duality filter. 
                              if (TABLE79_Dim>0){
                
                                  # Retain only the non-detect records. 
                                  TABLE80=subset(TABLE79,result=="Not Detected")
                                  TABLE80_Dim=as.numeric(nrow(TABLE80))
                
                                  # If no non-detect records remain. 
                                  if (TABLE80_Dim==0){
                                  # Tell the user insufficient records exist.
                                  line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior unfulfilled.</p>"
                                  write(line,file=model_log_filepath,append=TRUE)
                                  } # End if no records exist. 
                
                                  # If non-detect records remain. 
                                  if (TABLE80_Dim>0){
                                  # Add the record to the output file. 
                                  OUTPUT=rbind(OUTPUT, TABLE80)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Sharp shot, Fawn, Female: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                  } # End if one and not detected. 
                
                              } # If at least one record remains after duality filter. 
                      } # End if two or more lines exist in summary table. 
          } # End if white-tailed deer female fawn sharp shot contains data.
          
          # PRIOR________________________________________________________________
          # Get summary for White-tailed + Hunter Harvest + Adult + Male prior.
          WTD_SS_Male=subset(WTD_SS,sex=="Male" & age_group=="Adult")
          WTDSSDimM=as.numeric(nrow(WTD_SS_Male))
          
              # if WTD sharp shot Male adult is empty.
              if (WTDSSDimM==0){
              # Tell the user insufficient records exist.
              line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
              } # End if male adult sharp shot is empty. 
          
              # if WTD sharp shot Male adult contains data.             
              if (WTDSSDimM>0){
            
                  # Summarize the data. 
                  TABLE81=as.data.frame(WTD_SS_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                  TABLE81_Dim=as.numeric(nrow(TABLE81))
            
                      # If only one record exists and its detected, tell user data doesn't fit criteria.
                      if (TABLE81_Dim==1 & TABLE81$result[1]=="Detected"){
                      # Write a note that the data does not fit this prior.  
                      line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior unfulfilled.</p>"
                      write(line,file=model_log_filepath,append=TRUE) 
                      } # End if one and detected. 
            
                      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                      if (TABLE81_Dim==1 & TABLE81$result[1]=="Not Detected"){
                      # Add the record to the output file. 
                      OUTPUT=rbind(OUTPUT, TABLE81)
		  	# Add to the report.
    		  	line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior fulfilled.</p>"
    		  	write(line,file=model_log_filepath,append=TRUE)
                      } # End if one and not detected. 
            
                      # If two or more lines exist in the summary table. 
                      if (TABLE81_Dim>1){
              
                          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                          TABLE82=as.data.frame(TABLE81%>%group_by(SubAdminName)%>%filter(n()<2))
                          TABLE82_Dim=as.numeric(nrow(TABLE82))
              
                              # If no records remains after duality filter. 
                              if (TABLE82_Dim==0){
                              # Tell the user insufficient records exist.
                              line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior unfulfilled.</p>"
                              write(line,file=model_log_filepath,append=TRUE)
                              } # End if no records exist. 
              
                              # If at least one record remains after duality filter. 
                              if (TABLE82_Dim>0){
                
                                  # Retain only the non-detect records. 
                                  TABLE83=subset(TABLE82,result=="Not Detected")
                                  TABLE83_Dim=as.numeric(nrow(TABLE83))
                
                                      # If no non-detect records remain. 
                                      if (TABLE83_Dim==0){
                                      # Tell the user insufficient records exist.
                                      line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior unfulfilled.</p>"
                                      write(line,file=model_log_filepath,append=TRUE)
                                      } # End if no records exist. 
                
                                      # If non-detect records remain. 
                                      if (TABLE83_Dim>0){
                                      # Add the record to the output file. 
                                      OUTPUT=rbind(OUTPUT, TABLE83)
		  			# Add to the report.
    		  			line="<p>White-tailed Deer, Sharp shot, Adult, Male: Prior fulfilled.</p>"
    		  			write(line,file=model_log_filepath,append=TRUE)
                                      } # End if one and not detected. 
                
                              } # If at least one record remains after duality filter. 
                      } # End if two or more lines exist in summary table. 
                } # End if male adult sharp shot contains data.
        
        # PRIOR________________________________________________________________
        # Get summary for White-tailed Deer + Hunter Harvest + Yearling + Male prior.
        WTD_SS_Male2=subset(WTD_SS,sex=="Male" & age_group=="Yearling")
        WTDSSDimM2=as.numeric(nrow(WTD_SS_Male2))
        
            # if WTD sharp shot Male yearling is empty.
            if (WTDSSDimM2==0){
            # Tell the user insufficient records exist.
            line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
            } # End if white-tailed deer male yearling sharp shot is empty. 
        
            # if WTD sharp shot yearling Male contains data.             
            if (WTDSSDimM2>0){
          
                # Summarize the data. 
                TABLE84=as.data.frame(WTD_SS_Male2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE84_Dim=as.numeric(nrow(TABLE84))
          
                    # If only one record exists and its detected, tell user data doesn't fit criteria.
                    if (TABLE84_Dim==1 & TABLE84$result[1]=="Detected"){
                    # Write a note that the data does not fit this prior.  
                    line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and detected. 
          
                    # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                    if (TABLE84_Dim==1 & TABLE84$result[1]=="Not Detected"){
                    # Add the record to the output file. 
                    OUTPUT=rbind(OUTPUT, TABLE84)
		  # Add to the report.
    		  line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                    } # End if one and not detected. 
          
                    # If two or more lines exist in the summary table. 
                    if (TABLE84_Dim>1){
            
                        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                        TABLE85=as.data.frame(TABLE84%>%group_by(SubAdminName)%>%filter(n()<2))
                        TABLE85_Dim=as.numeric(nrow(TABLE85))
            
                            # If no records remains after duality filter. 
                            if (TABLE85_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE)
                            } # End if no records exist. 
            
                            # If at least one record remains after duality filter. 
                            if (TABLE85_Dim>0){
              
                                # Retain only the non-detect records. 
                                TABLE86=subset(TABLE85,result=="Not Detected")
                                TABLE86_Dim=as.numeric(nrow(TABLE86))
              
                                    # If no non-detect records remain. 
                                    if (TABLE86_Dim==0){
                                    # Tell the user insufficient records exist.
                                    line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior unfulfilled.</p>"
                                    write(line,file=model_log_filepath,append=TRUE)
                                    } # End if no records exist. 
              
                                    # If non-detect records remain. 
                                    if (TABLE86_Dim>0){
                                    # Add the record to the output file. 
                                    OUTPUT=rbind(OUTPUT, TABLE86)
		  		# Add to the report.
    		  		line="<p>White-tailed Deer, Sharp shot, Yearling, Male: Prior fulfilled.</p>"
    		  		write(line,file=model_log_filepath,append=TRUE)
                                    } # End if one and not detected. 
              
                            } # If at least one record remains after duality filter. 
                    } # End if two or more lines exist in summary table. 
            } # End if white-tailed deer male yearling sharp shot contains data. 
        
        # PRIOR________________________________________________________________
        # Get summary for White-tailed Deer + Hunter Harvest + Fawn + Male prior.
        WTD_SS_Male3=subset(WTD_SS,sex=="Male" & age_group=="Fawn")
        WTDSSDimM3=as.numeric(nrow(WTD_SS_Male3))
        
            # if WTD sharp shot Male fawn is empty.
            if (WTDSSDimM3==0){
            # Tell the user insufficient records exist.
            line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
            } # End if white-tailed deer male fawn sharp shot is empty. 
        
            # if WTD sharp shot fawn Male contains data.             
            if (WTDSSDimM3>0){
          
                # Summarize the data. 
                TABLE87=as.data.frame(WTD_SS_Male3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
                TABLE87_Dim=as.numeric(nrow(TABLE87))
          
                    # If only one record exists and its detected, tell user data doesn't fit criteria.
                    if (TABLE87_Dim==1 & TABLE87$result[1]=="Detected"){
                    # Write a note that the data does not fit this prior.  
                    line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior unfulfilled.</p>"
                    write(line,file=model_log_filepath,append=TRUE) 
                    } # End if one and detected. 
          
                    # If only one record exists and its not detected, store the output for use in the Estimation tool. 
                    if (TABLE87_Dim==1 & TABLE87$result[1]=="Not Detected"){
                    # Add the record to the output file. 
                    OUTPUT=rbind(OUTPUT, TABLE87)
		  # Add to the report.
    		  line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior fulfilled.</p>"
    		  write(line,file=model_log_filepath,append=TRUE)
                    } # End if one and not detected. 
          
                    # If two or more lines exist in the summary table. 
                    if (TABLE87_Dim>1){
            
                        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
                        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
                        TABLE88=as.data.frame(TABLE87%>%group_by(SubAdminName)%>%filter(n()<2)) # Note the random skip of 41. It is ok. 
                        TABLE88_Dim=as.numeric(nrow(TABLE88))
            
                            # If no records remains after duality filter. 
                            if (TABLE88_Dim==0){
                            # Tell the user insufficient records exist.
                            line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior unfulfilled.</p>"
                            write(line,file=model_log_filepath,append=TRUE)
                            } # End if no records exist. 
            
                            # If at least one record remains after duality filter. 
                            if (TABLE88_Dim>0){
              
                                # Retain only the non-detect records. 
                                TABLE89=subset(TABLE88,result=="Not Detected")
                                TABLE89_Dim=as.numeric(nrow(TABLE89))
              
                                    # If no non-detect records remain. 
                                    if (TABLE89_Dim==0){
                                    # Tell the user insufficient records exist.
                                    line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior unfulfilled.</p>"
                                    write(line,file=model_log_filepath,append=TRUE)
                                    } # End if no records exist. 
              
                                    # If non-detect records remain. 
                                    if (TABLE89_Dim>0){
                                    # Add the record to the output file. 
                                    OUTPUT=rbind(OUTPUT, TABLE89)
		  			# Add to the report.
    		  			line="<p>White-tailed Deer, Sharp shot, Fawn, Male: Prior fulfilled.</p>"
    		  			write(line,file=model_log_filepath,append=TRUE)
                                    } # End if one and not detected. 
              
                            } # If at least one record remains after duality filter. 
                      } # End if two or more lines exist in summary table. 
                } # End if white-tailed deer male fawn sharp shot contains data. 
        
          } # End if white-tailed deer sharp shot data exists.
      
      } # End if white-tailed deer data globally exists.
} # End if white-tailed deer species selected.

# ______________________________________________________________________________
# SPECIES: ELK/MULE DEER COMBINATION____________________________________________
# ______________________________________________________________________________      
# If user selected elk/mule combo. 
if(params$species=="Elk/Mule Deer Combined"){
  
  # Initialize elk output. 
  Elk_OUTPUT=c()
  QUIT=0 
  
  # Run Elk First. ----------------
  # If elk data does not globally exist. 
  if (ElkDim==0){
    # Add information to the report.
    line="<p>You selected the elk/mule deer combination option, but there does not exist any elk testing records in the season-year of interest. </p>"
    write(line,file=model_log_filepath,append=TRUE) 
    # Don't quit the session just yet becuase their might be mule deer records. 
    # Just log that elk don't exist. 
    QUIT=1
  } # End if elk data does not globally exist.
  
  # If elk data globally exists. 
  if (ElkDim>0){
    
    # Opening Note. 
    # As of August 2024, SpeedGoat Estimation tool elk priors are limited to Clinical suspect, 
    # Hunter-harvested, and Other. 
    
    # ELK CLINICAL SUSPECT _______________________________________________________ 
    ELK_CS=subset(ELK,sample_source=="Clinical suspect")
    ElkCSDim=as.numeric(nrow(ELK_CS))
    
    # If elk clinical suspect is empty. 
    if (ElkCSDim==0){
      # Tell the user insufficient records exist.
      line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)  
      line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE) 
    } # End elk clinical suspect is empty. 
    
    # If elk clinical suspect contains data.
    if (ElkCSDim>0){
      
      # PRIOR________________________________________________________________
      # Get summary for Elk + Clinical Suspect + All Ages + Female prior.
      ELK_CS_Female=subset(ELK_CS,sex=="Female")
      ElkCSDimF=as.numeric(nrow(ELK_CS_Female))
      
      # if ELK clinical suspect Female is empty.
      if (ElkCSDimF==0){
        # Tell the user insufficient records exist.
        line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)  
      } # End if female clinical suspect is empty. 
      
      # if ELK clinical suspect Female contains data.             
      if (ElkCSDimF>0){
        
        # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
        ELK_CS_Female$age_group=rep("All Ages", ElkCSDimF)
        
        # Summarize the data. 
        TABLE1=as.data.frame(ELK_CS_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE1_Dim=as.numeric(nrow(TABLE1))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE1_Dim==1 & TABLE1$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE1_Dim==1 & TABLE1$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE1)
          # Add to the report.
          line="<p>Elk, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE1_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE2=as.data.frame(TABLE1%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE2_Dim=as.numeric(nrow(TABLE2))
          
          # If no records remains after duality filter. 
          if (TABLE2_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE2_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE3=subset(TABLE2,result=="Not Detected")
            TABLE3_Dim=as.numeric(nrow(TABLE3))
            
            # If no non-detect records remain. 
            if (TABLE3_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE3_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE3)
              # Add to the report.
              line="<p>Elk, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if female clinical suspect contains data. 
      
      # PRIOR________________________________________________________________  
      # Get summary for Elk + Clinical Suspect + All Ages + Male prior.
      ELK_CS_Male=subset(ELK_CS,sex=="Male")
      ElkCSDimM=as.numeric(nrow(ELK_CS_Male))
      
      # if ELK clinical suspect Male is empty.
      if (ElkCSDimM==0){
        # Tell the user insufficient records exist.
        line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if male clinical suspect is empty. 
      
      # if ELK clinical suspect Male contains data.              
      if (ElkCSDimM>0){
        
        # Replace age data with 'All Ages'. [We do this because the prior for SpeedGoat Estimation tool is All Ages.] 
        ELK_CS_Male$age_group=rep("All Ages", ElkCSDimM)
        
        # Summarize the data. 
        TABLE4=as.data.frame(ELK_CS_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE4_Dim=as.numeric(nrow(TABLE4))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE4_Dim==1 & TABLE4$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE4_Dim==1 & TABLE4$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE4)
          # Add to the report.
          line="<p>Elk, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE4_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE5=as.data.frame(TABLE4%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE5_Dim=as.numeric(nrow(TABLE5))
          
          # If no records remains after duality filter. 
          if (TABLE5_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE5_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE6=subset(TABLE5,result=="Not Detected")
            TABLE6_Dim=as.numeric(nrow(TABLE6))
            
            # If no non-detect records remain. 
            if (TABLE6_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE6_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE6)
              # Add to the report.
              line="<p>Elk, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if male clinical suspect contains data. 
      
    } # End If elk clinical suspect contains data.
    
    # ELK HUNTER HARVESTED ________________________________________________________________        
    ELK_HH=subset(ELK,sample_source=="Hunter-harvested")
    ElkHHDim=as.numeric(nrow(ELK_HH))
    
    # If elk hunter harvest is empty. 
    if (ElkHHDim==0){
      # Tell the user insufficient records exist.
      line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
    } # End if elk hunter harvest is empty.
    
    # If elk hunter harvest contains data.
    if (ElkHHDim>0){
      
      # PRIOR________________________________________________________________
      # Get summary for Elk + Hunter Harvest + Adult + Female prior.
      ELK_HH_Female=subset(ELK_HH,sex=="Female" & age_group=="Adult")
      ElkHHDimFA=as.numeric(nrow(ELK_HH_Female))
      
      # if ELK hunter harvest Female Adult is empty.
      if (ElkHHDimFA==0){
        # Tell the user insufficient records exist.
        line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if elk female adult hunter harvest is empty. 
      
      # if ELK hunter harvest adult Female contains data.            
      if (ElkHHDimFA>0){
        
        # Summarize the data. 
        TABLE7=as.data.frame(ELK_HH_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE7_Dim=as.numeric(nrow(TABLE7))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE7_Dim==1 & TABLE7$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE7_Dim==1 & TABLE7$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE7)
          # Add to the report.
          line="<p>Elk, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE7_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE8=as.data.frame(TABLE7%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE8_Dim=as.numeric(nrow(TABLE8))
          
          # If no records remains after duality filter. 
          if (TABLE8_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE8_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE9=subset(TABLE8,result=="Not Detected")
            TABLE9_Dim=as.numeric(nrow(TABLE9))
            
            # If no non-detect records remain. 
            if (TABLE9_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE9_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE9)
              # Add to the report.
              line="<p>Elk, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if female adult hunter harvest contains data. 
      
      # PRIOR________________________________________________________________
      # Get summary for Elk + Hunter Harvest + Yearling + Female prior.
      ELK_HH_Female2=subset(ELK_HH,sex=="Female" & age_group=="Yearling")
      ElkHHDimF2=as.numeric(nrow(ELK_HH_Female2))
      
      # if ELK hunter harvest Female yearling is empty.
      if (ElkHHDimF2==0){
        # Tell the user insufficient records exist.
        line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if elk female yearling hunter harvest is empty. 
      
      # if ELK hunter harvest yearling Female contains data.             
      if (ElkHHDimF2>0){
        
        # Summarize the data. 
        TABLE10=as.data.frame(ELK_HH_Female2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE10_Dim=as.numeric(nrow(TABLE10))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE10_Dim==1 & TABLE10$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE10_Dim==1 & TABLE10$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE10)
          # Add to the report.
          line="<p>Elk, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE10_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE11=as.data.frame(TABLE10%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE11_Dim=as.numeric(nrow(TABLE11))
          
          # If no records remains after duality filter. 
          if (TABLE11_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE11_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE12=subset(TABLE11,result=="Not Detected")
            TABLE12_Dim=as.numeric(nrow(TABLE12))
            
            # If no non-detect records remain. 
            if (TABLE12_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE12_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE12)
              # Add to the report.
              line="<p>Elk, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if elk female yearling hunter harvest contains data. 
      
      # PRIOR________________________________________________________________
      # Get summary for Elk + Hunter Harvest + Adult + Male prior.
      ELK_HH_Male=subset(ELK_HH,sex=="Male" & age_group=="Adult")
      ElkHHDimM=as.numeric(nrow(ELK_HH_Male))
      
      # if ELK hunter harvest Male is empty.
      if (ElkHHDimM==0){
        # Tell the user insufficient records exist.
        line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if male adult hunter harvest is empty. 
      
      # if ELK hunter harvest Male adult contains data.             
      if (ElkHHDimM>0){
        
        # Summarize the data. 
        TABLE13=as.data.frame(ELK_HH_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE13_Dim=as.numeric(nrow(TABLE13))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE13_Dim==1 & TABLE13$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE13_Dim==1 & TABLE13$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE13)
          # Add to the report.
          line="<p>Elk, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE13_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE14=as.data.frame(TABLE13%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE14_Dim=as.numeric(nrow(TABLE14))
          
          # If no records remains after duality filter. 
          if (TABLE14_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE14_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE15=subset(TABLE14,result=="Not Detected")
            TABLE15_Dim=as.numeric(nrow(TABLE15))
            
            # If no non-detect records remain. 
            if (TABLE15_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Elk, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE15_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE15)
              # Add to the report.
              line="<p>Elk, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if male hunter-harvest contains data.
    } # End if elk hunter harvest contains data.
    
    # ELK OTHER  ________________________________________________________________         
    ELK_O=subset(ELK,sample_source!="Clinical suspect" & sample_source!="Hunter-harvested")
    ElkODim=as.numeric(nrow(ELK_O))
    
    # If elk other is empty. 
    if (ElkODim==0){
      # Tell the user insufficient records exist.
      line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
    } # End elk other is empty. 
    
    # If elk other contains data.
    if (ElkODim>0){
      
      # PRIOR________________________________________________________________
      # Get Elk + Other + All Ages + All Sexes.
      # Replace age data with 'All Ages'. [We do this because the prior for SpeedGoat Estimation tool is "All Ages.'] 
      ELK_O$age_group=rep("All Ages", ElkODim)
      # Replace age data with 'All Ages'. [We do this because the prior for SpeedGoat Estimation tool is "All Sexes.'] 
      ELK_O$sex=rep("All Sexes", ElkODim)
      # Replace sample_source with 'Other'. [We do this because the prior for SpeedGoat Estimation tool is "Other.'] 
      ELK_O$sample_source=rep("Other", ElkODim)
      
      # Summarize the data. 
      TABLE16=as.data.frame(ELK_O%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
      TABLE16_Dim=as.numeric(nrow(TABLE16))
      
      # If only one record exists and its detected, tell user data doesn't fit criteria.
      if (TABLE16_Dim==1 & TABLE16$result[1]=="Detected"){
        # Write a note that the data does not fit this prior.  
        line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
      } # End if one and detected. 
      
      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
      if (TABLE16_Dim==1 & TABLE16$result[1]=="Not Detected"){
        # Add the record to the output file. 
        OUTPUT=rbind(OUTPUT, TABLE16)
        # Add to the report.
        line="<p>Elk, Other, All Ages, All Sexes: Prior fulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if one and not detected. 
      
      # If two or more lines exist in the summary table. 
      if (TABLE16_Dim>1){
        
        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
        TABLE17=as.data.frame(TABLE16%>%group_by(SubAdminName)%>%filter(n()<2))
        TABLE17_Dim=as.numeric(nrow(TABLE17))
        
        # If no records remains after duality filter. 
        if (TABLE17_Dim==0){
          # Tell the user insufficient records exist.
          line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if no records exist. 
        
        # If at least one record remains after duality filter. 
        if (TABLE17_Dim>0){
          
          # Retain only the non-detect records. 
          TABLE18=subset(TABLE17,result=="Not Detected")
          TABLE18_Dim=as.numeric(nrow(TABLE18))
          
          # If no non-detect records remain. 
          if (TABLE18_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Elk, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If non-detect records remain. 
          if (TABLE18_Dim>0){
            # Add the record to the output file. 
            OUTPUT=rbind(OUTPUT, TABLE18)
            # Add to the report.
            line="<p>Elk, Other, All Ages, All Sexes: Prior fulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if one and not detected. 
          
        } # If at least one record remains after duality filter. 
      } # End if two or more lines exist in summary table. 
    } # End if elk other contains data.
    
  } # End if elk data globally exists.
  Elk_OUTPUT=OUTPUT
  
  # Run Mule Deer Second. -----------------
  # Initialize mule deer output. 
  OUTPUT=c()
  Deer_OUTPUT=c()
  
  # If mule deer data does not globally exist. 
  if (MuleDim==0){
    # Add information to the report.
    line="<p>You selected the elk/mule deer combination option, but there does not exist any mule deer testing records in the season-year of interest. </p>"
    write(line,file=model_log_filepath,append=TRUE)
    # Only quit the session if both elk and mule deer are non-existent. 
    if (QUIT==1){quit(status=70)}
  } # End if mule deer data does not globally exist.
  
  # If mule deer data globally exists. 
  if (MuleDim>0){
    
    # Opening Note. 
    # As of August 2024, SpeedGoat Estimation tool mule deer priors are limited to Clinical suspect, 
    # Hunter-harvested, and Other. 
    
    # MULE CLINICAL SUSPECT _______________________________________________________ 
    MULE_CS=subset(MULE,sample_source=="Clinical suspect")
    MuleCSDim=as.numeric(nrow(MULE_CS))
    
    # If mule deer clinical suspect is empty. 
    if (MuleCSDim==0){
      # Tell the user insufficient records exist.
      line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)  
      line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE) 
    } # End mule deer clinical suspect is empty. 
    
    # If mule deer clinical suspect contains data.
    if (MuleCSDim>0){
      
      # PRIOR________________________________________________________________
      # Get summary for Mule Deer + Clinical Suspect + All Ages + Female prior.
      MULE_CS_Female=subset(MULE_CS,sex=="Female")
      MuleCSDimF=as.numeric(nrow(MULE_CS_Female))
      
      # if MULE clinical suspect Female is empty.
      if (MuleCSDimF==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
      } # End if female clinical suspect is empty. 
      
      # if MULE clinical suspect Female contains data.             
      if (MuleCSDimF>0){
        
        # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
        MULE_CS_Female$age_group=rep("All Ages", MuleCSDimF)
        
        # Summarize the data. 
        TABLE19=as.data.frame(MULE_CS_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE19_Dim=as.numeric(nrow(TABLE19))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE19_Dim==1 & TABLE19$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE19_Dim==1 & TABLE19$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE19)
          # Add to the report.
          line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE19_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE20=as.data.frame(TABLE19%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE20_Dim=as.numeric(nrow(TABLE20))
          
          # If no records remains after duality filter. 
          if (TABLE20_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)  
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE20_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE21=subset(TABLE20,result=="Not Detected")
            TABLE21_Dim=as.numeric(nrow(TABLE21))
            
            # If no non-detect records remain. 
            if (TABLE21_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE21_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE21)
              # Add to the report.
              line="<p>Mule Deer, Clinical suspect, All Ages, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if female clinical suspect contains data. 
      
      # PRIOR________________________________________________________________  
      # Get summary for Mule Deer + Clinical Suspect + All Ages + Male prior.
      MULE_CS_Male=subset(MULE_CS,sex=="Male")
      MuleCSDimM=as.numeric(nrow(MULE_CS_Male))
      
      # if MULE clinical suspect Male is empty.
      if (MuleCSDimM==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
      } # End if male clinical suspect is empty. 
      
      # if MULE clinical suspect Male contains data.              
      if (MuleCSDimM>0){
        
        # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is All Ages.] 
        MULE_CS_Male$age_group=rep("All Ages", MuleCSDimM)
        
        # Summarize the data. 
        TABLE22=as.data.frame(MULE_CS_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE22_Dim=as.numeric(nrow(TABLE22))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE22_Dim==1 & TABLE22$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE22_Dim==1 & TABLE22$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE22)
          # Add to the report.
          line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE22_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE23=as.data.frame(TABLE22%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE23_Dim=as.numeric(nrow(TABLE23))
          
          # If no records remains after duality filter. 
          if (TABLE23_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE23_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE24=subset(TABLE23,result=="Not Detected")
            TABLE24_Dim=as.numeric(nrow(TABLE24))
            
            # If no non-detect records remain. 
            if (TABLE24_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE24_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE24)
              # Add to the report.
              line="<p>Mule Deer, Clinical suspect, All Ages, Male: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if male clinical suspect contains data. 
    } # End If mule deer clinical suspect contains data.
    
    # MULE HUNTER HARVESTED ________________________________________________________________        
    MULE_HH=subset(MULE,sample_source=="Hunter-harvested")
    MuleHHDim=as.numeric(nrow(MULE_HH))
    
    # If mule deer hunter harvest is empty. 
    if (MuleHHDim==0){
      # Tell the user insufficient records exist.
      line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
      line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE)
    } # End if mule deer hunter harvest is empty.
    
    # If mule deer hunter harvest contains data.
    if (MuleHHDim>0){
      
      # PRIOR________________________________________________________________
      # Get summary for Mule Deer + Hunter Harvest + Adult + Female prior.
      MULE_HH_Female=subset(MULE_HH,sex=="Female" & age_group=="Adult")
      MuleHHDimF=as.numeric(nrow(MULE_HH_Female))
      
      # if mule deer hunter-harvest Female Adult is empty.
      if (MuleHHDimF==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
      } # End if mule deer female adult hunter-harvest is empty. 
      
      # if mule deer hunter-harvest adult Female contains data.            
      if (MuleHHDimF>0){
        
        # Summarize the data. 
        TABLE25=as.data.frame(MULE_HH_Female%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE25_Dim=as.numeric(nrow(TABLE25))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE25_Dim==1 & TABLE25$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE25_Dim==1 & TABLE25$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE25)
          # Add to the report.
          line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE25_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE26=as.data.frame(TABLE25%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE26_Dim=as.numeric(nrow(TABLE26))
          
          # If no records remains after duality filter. 
          if (TABLE26_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE26_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE27=subset(TABLE26,result=="Not Detected")
            TABLE27_Dim=as.numeric(nrow(TABLE27))
            
            # If no non-detect records remain. 
            if (TABLE27_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE27_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE27)
              # Add to the report.
              line="<p>Mule Deer, Hunter-harvested, Adult, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if female adult hunter-harvest contains data. 
      
      # PRIOR________________________________________________________________
      # Get summary for Mule Deer + Hunter Harvest + Yearling + Female prior.
      MULE_HH_Female2=subset(MULE_HH,sex=="Female" & age_group=="Yearling")
      MuleHHDimF2=as.numeric(nrow(MULE_HH_Female2))
      
      # if MULE hunter-harvest Female yearling is empty.
      if (MuleHHDimF2==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if mule deer female yearling hunter-harvest is empty. 
      
      # if MULE hunter-harvest yearling Female contains data.             
      if (MuleHHDimF2>0){
        
        # Summarize the data. 
        TABLE28=as.data.frame(MULE_HH_Female2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE28_Dim=as.numeric(nrow(TABLE28))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE28_Dim==1 & TABLE28$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>" 
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE28_Dim==1 & TABLE28$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE28)
          # Add to the report.
          line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE28_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE29=as.data.frame(TABLE28%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE29_Dim=as.numeric(nrow(TABLE29))
          
          # If no records remains after duality filter. 
          if (TABLE29_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE29_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE30=subset(TABLE29,result=="Not Detected")
            TABLE30_Dim=as.numeric(nrow(TABLE30))
            
            # If no non-detect records remain. 
            if (TABLE30_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE30_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE30)
              # Add to the report.
              line="<p>Mule Deer, Hunter-harvested, Yearling, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if mule deer female yearling hunter-harvest contains data. 
      
      # PRIOR________________________________________________________________
      # Get summary for Mule Deer + Hunter Harvest + Fawn + Female prior.
      MULE_HH_Female3=subset(MULE_HH,sex=="Female" & age_group=="Fawn")
      MuleHHDimF3=as.numeric(nrow(MULE_HH_Female3))
      
      # if MULE hunter-harvest Female fawn is empty.
      if (MuleHHDimF3==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if mule deer female fawn hunter-harvest is empty. 
      
      # if MULE hunter-harvest fawn Female contains data.             
      if (MuleHHDimF3>0){
        
        # Summarize the data. 
        TABLE31=as.data.frame(MULE_HH_Female3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE31_Dim=as.numeric(nrow(TABLE31))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE31_Dim==1 & TABLE31$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE31_Dim==1 & TABLE31$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE31)
          # Add to the report.
          line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE31_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE32=as.data.frame(TABLE31%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE32_Dim=as.numeric(nrow(TABLE32))
          
          # If no records remains after duality filter. 
          if (TABLE32_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE32_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE33=subset(TABLE32,result=="Not Detected")
            TABLE33_Dim=as.numeric(nrow(TABLE33))
            
            # If no non-detect records remain. 
            if (TABLE33_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE33_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE33)
              # Add to the report.
              line="<p>Mule Deer, Hunter-harvested, Fawn, Female: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if mule deer female fawn hunter-harvest contains data.
      
      # PRIOR________________________________________________________________
      # Get summary for Mule + Hunter Harvest + Adult + Male prior.
      MULE_HH_Male=subset(MULE_HH,sex=="Male" & age_group=="Adult")
      MuleHHDimM=as.numeric(nrow(MULE_HH_Male))
      
      # if MULE hunter-harvest Male is empty.
      if (MuleHHDimM==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if male adult hunter-harvest is empty. 
      
      # if MULE hunter-harvest Male adult contains data.             
      if (MuleHHDimM>0){
        
        # Summarize the data. 
        TABLE34=as.data.frame(MULE_HH_Male%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE34_Dim=as.numeric(nrow(TABLE34))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE34_Dim==1 & TABLE34$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE34_Dim==1 & TABLE34$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE34)
          # Add to the report.
          line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE34_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE35=as.data.frame(TABLE34%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE35_Dim=as.numeric(nrow(TABLE35))
          
          # If no records remains after duality filter. 
          if (TABLE35_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE35_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE36=subset(TABLE35,result=="Not Detected")
            TABLE36_Dim=as.numeric(nrow(TABLE36))
            
            # If no non-detect records remain. 
            if (TABLE36_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE36_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE36)
              # Add to the report.
              line="<p>Mule Deer, Hunter-harvested, Adult, Male: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if male adult hunter-harvest contains data.
      
      # PRIOR________________________________________________________________
      # Get summary for Mule Deer + Hunter Harvest + Yearling + Male prior.
      MULE_HH_Male2=subset(MULE_HH,sex=="Male" & age_group=="Yearling")
      MuleHHDimM2=as.numeric(nrow(MULE_HH_Male2))
      
      # if MULE hunter-harvest Male yearling is empty.
      if (MuleHHDimM2==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if mule deer male yearling hunter-harvest is empty. 
      
      # if MULE hunter-harvest yearling Male contains data.             
      if (MuleHHDimM2>0){
        
        # Summarize the data. 
        TABLE37=as.data.frame(MULE_HH_Male2%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE37_Dim=as.numeric(nrow(TABLE37))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE37_Dim==1 & TABLE37$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE37_Dim==1 & TABLE37$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE37)
          # Add to the report.
          line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE37_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE38=as.data.frame(TABLE37%>%group_by(SubAdminName)%>%filter(n()<2))
          TABLE38_Dim=as.numeric(nrow(TABLE38))
          
          # If no records remains after duality filter. 
          if (TABLE38_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE38_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE39=subset(TABLE38,result=="Not Detected")
            TABLE39_Dim=as.numeric(nrow(TABLE39))
            
            # If no non-detect records remain. 
            if (TABLE39_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE39_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE39)
              # Add to the report.
              line="<p>Mule Deer, Hunter-harvested, Yearling, Male: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if mule deer male yearling hunter-harvest contains data. 
      
      # PRIOR________________________________________________________________
      # Get summary for Mule Deer + Hunter Harvest + Fawn + Male prior.
      MULE_HH_Male3=subset(MULE_HH,sex=="Male" & age_group=="Fawn")
      MuleHHDimM3=as.numeric(nrow(MULE_HH_Male3))
      
      # if MULE hunter-harvest Male fawn is empty.
      if (MuleHHDimM3==0){
        # Tell the user insufficient records exist.
        line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
      } # End if mule deer male fawn hunter-harvest is empty. 
      
      # if MULE hunter-harvest fawn Male contains data.             
      if (MuleHHDimM3>0){
        
        # Summarize the data. 
        TABLE40=as.data.frame(MULE_HH_Male3%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
        TABLE40_Dim=as.numeric(nrow(TABLE40))
        
        # If only one record exists and its detected, tell user data doesn't fit criteria.
        if (TABLE40_Dim==1 & TABLE40$result[1]=="Detected"){
          # Write a note that the data does not fit this prior.  
          line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE) 
        } # End if one and detected. 
        
        # If only one record exists and its not detected, store the output for use in the Estimation tool. 
        if (TABLE40_Dim==1 & TABLE40$result[1]=="Not Detected"){
          # Add the record to the output file. 
          OUTPUT=rbind(OUTPUT, TABLE40)
          # Add to the report.
          line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior fulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)
        } # End if one and not detected. 
        
        # If two or more lines exist in the summary table. 
        if (TABLE40_Dim>1){
          
          # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
          # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
          TABLE42=as.data.frame(TABLE40%>%group_by(SubAdminName)%>%filter(n()<2)) # Note the random skip of 41. It is ok. 
          TABLE42_Dim=as.numeric(nrow(TABLE42))
          
          # If no records remains after duality filter. 
          if (TABLE42_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
          } # End if no records exist. 
          
          # If at least one record remains after duality filter. 
          if (TABLE42_Dim>0){
            
            # Retain only the non-detect records. 
            TABLE43=subset(TABLE42,result=="Not Detected")
            TABLE43_Dim=as.numeric(nrow(TABLE43))
            
            # If no non-detect records remain. 
            if (TABLE43_Dim==0){
              # Tell the user insufficient records exist.
              line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior unfulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE) 
            } # End if no records exist. 
            
            # If non-detect records remain. 
            if (TABLE43_Dim>0){
              # Add the record to the output file. 
              OUTPUT=rbind(OUTPUT, TABLE43)
              # Add to the report.
              line="<p>Mule Deer, Hunter-harvested, Fawn, Male: Prior fulfilled.</p>"
              write(line,file=model_log_filepath,append=TRUE)
            } # End if one and not detected. 
            
          } # If at least one record remains after duality filter. 
        } # End if two or more lines exist in summary table. 
      } # End if mule deer male fawn hunter-harvest contains data.
    } # #End if mule deer hunter harvest contains data. 
    
    # MULE OTHER  ________________________________________________________________         
    MULE_O=subset(MULE,sample_source!="Clinical suspect" & sample_source!="Hunter-harvested")
    MuleODim=as.numeric(nrow(MULE_O))
    
    # If mule deer other is empty. 
    if (MuleODim==0){
      # Tell the user insufficient records exist.
      line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
      write(line,file=model_log_filepath,append=TRUE) 
    } # End mule deer other is empty. 
    
    # If mule deer other contains data.
    if (MuleODim>0){
      
      # PRIOR________________________________________________________________
      # Get Mule + Other + All Ages + All Sexes.
      # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Ages.'] 
      MULE_O$age_group=rep("All Ages", MuleODim)
      # Replace age data with 'All Ages'. [We do this because the prior for the SpeedGoat Estimation tool is "All Sexes.'] 
      MULE_O$sex=rep("All Sexes", MuleODim)
      # Replace sample source with 'Other'. [We do this because the prior for the SpeedGoat Estimation tool is "Other.'] 
      MULE_O$sample_source=rep("Other", MuleODim)
      
      # Summarize the data. 
      TABLE44=as.data.frame(MULE_O%>%group_by(SubAdminName,species,sample_source,age_group,sex)%>%count(result))
      TABLE44_Dim=as.numeric(nrow(TABLE44))
      
      # If only one record exists and its detected, tell user data doesn't fit criteria.
      if (TABLE44_Dim==1 & TABLE44$result[1]=="Detected"){
        # Write a note that the data does not fit this prior.  
        line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE) 
      } # End if one and detected. 
      
      # If only one record exists and its not detected, store the output for use in the Estimation tool. 
      if (TABLE44_Dim==1 & TABLE44$result[1]=="Not Detected"){
        # Add the record to the output file. 
        OUTPUT=rbind(OUTPUT, TABLE44)
        # Add to the report.
        line="<p>Mule Deer, Other, All Ages, All Sexes: Prior fulfilled.</p>"
        write(line,file=model_log_filepath,append=TRUE)
      } # End if one and not detected. 
      
      # If two or more lines exist in the summary table. 
      if (TABLE44_Dim>1){
        
        # If a subadmin area shows up twice in the table, that means it has both positives and negatives, and is therefore ineligible. 
        # Filter the table only include subadmin areas that show up once. [i.e., Completely remove subadmin areas that show up twice.]
        TABLE45=as.data.frame(TABLE44%>%group_by(SubAdminName)%>%filter(n()<2))
        TABLE45_Dim=as.numeric(nrow(TABLE45))
        
        # If no records remains after duality filter. 
        if (TABLE45_Dim==0){
          # Tell the user insufficient records exist.
          line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
          write(line,file=model_log_filepath,append=TRUE)  
        } # End if no records exist. 
        
        # If at least one record remains after duality filter. 
        if (TABLE45_Dim>0){
          
          # Retain only the non-detect records. 
          TABLE46=subset(TABLE45,result=="Not Detected")
          TABLE46_Dim=as.numeric(nrow(TABLE46))
          
          # If no non-detect records remain. 
          if (TABLE46_Dim==0){
            # Tell the user insufficient records exist.
            line="<p>Mule Deer, Other, All Ages, All Sexes: Prior unfulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE) 
          } # End if no records exist. 
          
          # If non-detect records remain. 
          if (TABLE46_Dim>0){
            # Add the record to the output file. 
            OUTPUT=rbind(OUTPUT, TABLE46)
            # Add to the report.
            line="<p>Mule Deer, Other, All Ages, All Sexes: Prior fulfilled.</p>"
            write(line,file=model_log_filepath,append=TRUE)
          } # End if one and not detected. 
          
        } # If at least one record remains after duality filter. 
      } # End if two or more lines exist in summary table. 
    } # End if mule deer other contains data.
    
  } # End if mule deer data globally exists.
  Deer_OUTPUT=OUTPUT
  
  # Combine deer and elk into into a single output.
  # If both elk and mule deer are null, no ouput exists. 
  if (is.null(Elk_OUTPUT) & is.null(Deer_OUTPUT)){OUTPUT=c()}
  # If elk exists but deer does not, only elk output exists.
  if (!is.null(Elk_OUTPUT) & is.null(Deer_OUTPUT)){OUTPUT=Elk_OUTPUT}
  # If elk does not exist but deer does, only deer output exists. 
  if (is.null(Elk_OUTPUT) & !is.null(Deer_OUTPUT)){OUTPUT=Deer_OUTPUT}
  # If both elk and deer exist, then combine both outputs. 
  if (!is.null(Elk_OUTPUT) & !is.null(Deer_OUTPUT)){OUTPUT=rbind(Elk_OUTPUT,Deer_OUTPUT)}
} # End if elk/deer combo selected. 
  
# ______________________________________________________________________________    
# SPECIES: FALLOW DEER
# ______________________________________________________________________________ 
# If user selected fallow deer. 
  if(params$species=="fallow deer"){
    line="<p>We're sorry. You selected fallow deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected fallow deer.
  
# ______________________________________________________________________________    
# SPECIES: MOOSE
# ______________________________________________________________________________  
# If user selected moose. 
  if(params$species=="moose"){
    line="<p>We're sorry. You selected moose, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected moose.
  
# ______________________________________________________________________________    
# SPECIES: MUNTJAC
# ______________________________________________________________________________  
# If user selected muntjac. 
  if(params$species=="muntjac"){
    line="<p>We're sorry. You selected muntjac, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected muntjac.
  
# ______________________________________________________________________________    
# SPECIES: SAMBAR DEER
# ______________________________________________________________________________ 
# If user selected sambar deer. 
  if(params$species=="sambar deer"){
    line="<p>We're sorry. You selected sambar deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected sambar deer.
  
# ______________________________________________________________________________    
# SPECIES: TUFTED DEER
# ______________________________________________________________________________  
# If user selected tufted deer. 
  if(params$species=="tufted deer"){
    line="<p>We're sorry. You selected tufted deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected tufted deer.
  
# ______________________________________________________________________________    
# SPECIES: UNKNOWN
# ______________________________________________________________________________ 
# If user selected unknown species. 
  if(params$species=="unknown"){
    line="We're sorry. The Speedgoat Estimation Tool requires records to have known 
	  species. Please return to the CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected unknown species.
  
# ______________________________________________________________________________    
# SPECIES: AXIS DEER
# ______________________________________________________________________________ 
# If user selected axis deer species. 
  if(params$species=="axis deer"){
    line="<p>We're sorry. You selected axis deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected axis deer species.

# ______________________________________________________________________________    
# SPECIES: BLACK-TAILED DEER
# ______________________________________________________________________________ 
# If user selected black-tailed deer species. 
  if(params$species=="black-tailed deer"){
    line="<p>We're sorry. You selected black-tailed deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected black-tailed deer species.

# ______________________________________________________________________________    
# SPECIES: CARIBOU
# ______________________________________________________________________________ 
# If user selected caribou species. 
  if(params$species=="caribou"){
    line="<p>We're sorry. You selected caribou, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected caribou species.

# ______________________________________________________________________________    
# SPECIES: ELD's DEER
# ______________________________________________________________________________ 
# If user selected Eld's deer species. 
  if(params$species=="Eld's deer"){
    line="<p>We're sorry. You selected Eld's deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected Eld's deer species.

# ______________________________________________________________________________    
# SPECIES: HYBRID
# ______________________________________________________________________________ 
# If user selected hybrid species. 
  if(params$species=="hybrid"){
    line="<p>We're sorry. You selected hybrid, but the Speedgoat Estimation Tool 
	  does not have a prior for this type. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected hybrid species.

# ______________________________________________________________________________    
# SPECIES: PERE DAVIDS's DEER
# ______________________________________________________________________________ 
# If user selected Pere David's deer species. 
  if(params$species=="Pere David's deer"){
    line="<p>We're sorry. You selected Pere David's deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected Pere David's deer species.

# ______________________________________________________________________________    
# SPECIES: RED DEER
# ______________________________________________________________________________ 
# If user selected red deer species. 
  if(params$species=="red deer"){
    line="<p>We're sorry. You selected red deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected red deer species.

# ______________________________________________________________________________    
# SPECIES: REINDEER
# ______________________________________________________________________________ 
# If user selected reindeer species. 
  if(params$species=="reindeer"){
    line="<p>We're sorry. You selected reindeer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected reindeer species.

# ______________________________________________________________________________    
# SPECIES: SIKA
# ______________________________________________________________________________ 
# If user selected sika species. 
  if(params$species=="sika"){
    line="<p>We're sorry. You selected sika deer, but the Speedgoat Estimation Tool 
	  does not have a prior for this species. Please return to the 
	  CWD Data Warehouse and select a different species.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
    quit(status=70)
  } # End if user selected sika species.
  
# ______________________________________________________________________________
# At this point in the code, the OUTPUT file contains summaries of each batch and
# prior. If insufficient data exists for any given prior, it is written in the log.
  
  # If output is not null. 
  if (!is.null(OUTPUT)){OutputDim=as.numeric(nrow(OUTPUT))}

  # If output is null.
  if (is.null(OUTPUT)){OutputDim=0}
    
  # If OUTPUT has at least one record.
  if (OutputDim>0){

      # Remove the columns that are unneeded by SpeedGoat. 
      MATRIX=as.data.frame(cbind(OUTPUT$SubAdminName,OUTPUT$species,OUTPUT$sample_source,OUTPUT$age_group,OUTPUT$sex,OUTPUT$n))

      # Get the dimension of the output. 
      MATRIXDim=as.numeric(nrow(MATRIX))
  
      # Add the default column.
      default=rep("Default",MATRIXDim)
      MATRIX2=cbind(MATRIX,default)
  
      # Rename the Output Matrix to be the same as SpeedGoat template.
      names(MATRIX2)=c("Batch","Species","Collection Method","Age", "Sex","Samples","Test Name")
      
      # Write the output table to CSV file. 
      setwd("/data")
      write.csv(MATRIX2, "SpeedGoatOutputMatrix.csv", row.names=FALSE)
      
  } # End if OUTPUT has at least one record. 
  
# If OUTPUT has no rows. 
  if (OutputDim==0){
    # Add information to the report.
    line="<p>We're sorry. None of the selected sample data fits the priors in the 
	  Estimation Tool. Please return to the CWD Data Warehouse and select 
	  different sample data.</p>"
    write(line,file=model_log_filepath,append=TRUE) 
  } # End if OUTPUT has no rows. 
