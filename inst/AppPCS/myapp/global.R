
 if ("ISO8859-1"  %in% localeToCharset() )
 {load("data/DataW.RData") }

 if("UTF-8"  %in% localeToCharset() )
    {load("data/DataLM.RData")}


Recodage <- data.frame ("ProfBrutRec"= "",
                        "IntituleInsee" ="",
                        "LibN4" = "", 
                        "N4"="",
                        "LibN3" = "", 
                        "N3"="",
                        "LibN2" = "", 
                        "N2"="",
                        "LibN1" = "", 
                        "N1"="", 
                        stringsAsFactors = F)