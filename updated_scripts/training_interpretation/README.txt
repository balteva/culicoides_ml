######################################################
DETAILS ON df_to_model.csv files - why are there two?

df_model_to_RF.csv :
has less variables, initially used for prior model construction

VARS:
"num_releve"       "idpointdecapture" "ID_SITE"          "PRES_CUL"         "DATE"             "NBINDIV"         
 [7] "ECO_CLI"          "ELEV_OVIN"        "ELEV_CAPRIN"      "ELEV_BOVIN"       "ACTIV_LAIT"       "ACTIV_VIANDE"    
[13] "OUVERTURE_BAT"    "TEMPERATUREDEBUT" "TEMPERATUREFIN"   "ANIMAUXDEBUT"     "SITUATIONPIEGE"   "VENTDEBUT"       
[19] "NUAGEDEBUT"       "PLUIEDEBUT"       "VENTFIN"          "NUAGEFIN"         "PLUIEFIN"         "ANIMAUXFIN"      
[25] "TEMPMINI"         "TEMPMAX"          "ALT"              "OVI"              "EQU"              "BETAIL"          
[31] "CAP"              "BOV"              "SURF_CANT"        "CLC_level2"       "TM_0_0"           "TM_0_2"          
[37] "TN_0_0"           "TN_0_5"           "TX_0_4"           "UM_0_0"           "UM_0_3"           "TPS_0_0"         
[43] "TPS_0_2"          "QQ_1_6"           "SWV_0_4"          "SWV_2_5"          "PP_5_5"           "PP_2_5"          
[49] "FG_4_6"           "NDVI_0_1"         "NDVI_0_4"         "EVI_0_2"          "EVI_0_4"          "TM_0_1"          
[55] "TN_0_2"           "TX_0_0"           "TX_0_3"           "UM_0_1"           "SWV_0_0"          "SWV_0_3"         
[61] "QQ_0_5"           "PP_3_3"           "PP_1_5"           "FG_0_3"           "FG_0_1"           "NDVI_0_5"        
[67] "EVI_0_6"          "PRES_CUL_NUMERIC" "Cell"            

######################################################
predictive_df_model_for_RF_training.csv :
(old name: "predictive_df_model_for_RF.csv" 

has extra variables based on updated glmm and CMM outputs.
VARS:
[1] "num_releve"       "idpointdecapture" "ID_SITE"          "PRES_CUL"         "DATE"             "NBINDIV"         
 [7] "ECO_CLI"          "ELEV_OVIN"        "ELEV_CAPRIN"      "ELEV_BOVIN"       "ACTIV_LAIT"       "ACTIV_VIANDE"    
[13] "OUVERTURE_BAT"    "TEMPERATUREDEBUT" "TEMPERATUREFIN"   "ANIMAUXDEBUT"     "SITUATIONPIEGE"   "VENTDEBUT"       
[19] "NUAGEDEBUT"       "PLUIEDEBUT"       "VENTFIN"          "NUAGEFIN"         "PLUIEFIN"         "ANIMAUXFIN"      
[25] "TEMPMINI"         "TEMPMAX"          "ALT"              "OVI"              "EQU"              "BETAIL"          
[31] "CAP"              "BOV"              "SURF_CANT"        "CLC_level2"       "TM_0_0"           "TM_0_2"          
[37] "TN_0_0"           "TN_0_5"           "TX_0_4"           "UM_0_0"           "UM_0_3"           "TPS_0_0"         
[43] "TPS_0_2"          "QQ_1_6"           "SWV_0_4"          "SWV_2_5"          "PP_5_5"           "PP_2_5"          
[49] "FG_4_6"           "NDVI_0_1"         "NDVI_0_4"         "EVI_0_2"          "EVI_0_4"          "TM_0_1"          
[55] "TN_0_2"           "TX_0_0"           "TX_0_3"           "UM_0_1"           "SWV_0_0"          "SWV_0_3"         
[61] "QQ_0_5"           "PP_3_3"           "PP_1_5"           "FG_0_3"           "FG_0_1"           "NDVI_0_5"        
[67] "EVI_0_6"          "EVI_0_0"          "NDVI_0_0"         "PRES_CUL_NUMERIC" "Cell"            
> 


#####################################################

Otherwise structure is identical. Preferential use is, of course, the dataframe with extra variables.
