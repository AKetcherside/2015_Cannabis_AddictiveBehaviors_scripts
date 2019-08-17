###Scoring NEO	



setwd("/Users/arielketcherside/Documents/UTD/My.Publications/2015_NEO_paper")


Data <- read.csv("MRN_NEO.Paper_NEO.scores.8.26.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "") 



#Neuroticism 


	#Total All Variables

	NEO.Nsum <- (Data$neo1REV+
	Data$neo6+
	Data$neo11+
	Data$neo16REV 
		+Data$neo21+
		Data$neo26+
		Data$neo31REV+
		Data$neo36   
		+Data$neo41+
		Data$neo46REV+
		Data$neo51+
		Data$neo56)

	#t scores
	#Neuroticism for Males

		NEO.Ntm = (1.3458* NEO.Nsum) + 26.236

	#Neuroticism for Females

		NEO.Ntf = (1.3123*NEO.Nsum) + 23.351

	#Combine boys and girls 

		NEO.N.TScore = NULL

		for (i in 1:66)

		if(Data$sex[i]==1) {
			NEO.N.TScore[i] <- NEO.Ntm[i]
			} else {
				NEO.N.TScore[i] <- NEO.Ntf[i]
				}




#Extroversion

	#Total All Variables

	NEO.Esum <- (Data$neo2+
	Data$neo7+
	Data$neo12REV+
	Data$neo17+
	Data$neo22
		+Data$neo27REV+
		Data$neo32+
		Data$neo37+
		Data$neo42REV
		+Data$neo47+
		Data$neo52+
		Data$neo57REV)

	#t scores
	#Extroversion for Males

		NEO.Etm = (1.717* NEO.Esum) + 3.2358

	#Extroversion for Females

		NEO.Etf= (1.7125 * NEO.Esum) + 1.7813

	#Combine boys and girls 

		NEO.E.TScore = NULL

		for (i in 1:66)

		if(Data$sex[i]==1) {
			NEO.E.TScore[i] <- NEO.Etm[i]
			} else {
				NEO.E.TScore[i] <- NEO.Etf[i]
				}


#Openness

	#total all questions

	NEO.Osum <- (Data$neo3REV+
	Data$neo8REV+
	Data$neo13+
	Data$neo18REV
		+Data$neo23REV+
		Data$neo28+
		Data$neo33REV+
		Data$neo38REV
		+Data$neo43+
		Data$neo48REV+
		Data$neo53+
		Data$neo58)

	#t scores
 	#Openness for Males
	
		NEO.Otm = (1.7185* NEO.Osum) + 3.4426


	#Openness for Females
		NEO.Otf = (1.701* NEO.Osum) + 4.1753

	#Combine boys and girls 

		NEO.O.TScore = NULL

		for (i in 1:66)

		if(Data$sex[i]==1) {
			NEO.O.TScore[i] <- NEO.Otm[i]
			} else {
				NEO.O.TScore[i] <- NEO.Otf[i]
				}


#Agreeableness


	#Combine boys and girls 
	
	NEO.Asum <- (Data$neo4+
	Data$neo9REV+
	Data$neo14REV+
	Data$neo19
		+Data$neo24REV+
		Data$neo29REV+
		Data$neo34+
		Data$neo39REV
		+Data$neo44REV+
		Data$neo49+
		Data$neo54REV+
		Data$neo59REV)	

	#t scores
	#Agreeableness for Males
	
		NEO.Atm = (2*NEO.Asum) - 14

	#Agreeableness for Females
		NEO.Atf = (2.1*NEO.Asum) - 20.7


	#Combine boys and girls 

		NEO.A.TScore = NULL

		for (i in 1:66)

		if(Data$sex[i]==1) {
			NEO.A.TScore[i] <- NEO.Atm[i]
			} else {
				NEO.A.TScore[i] <- NEO.Atf[i]
				}





#Conscientiousness

	#Total All Questions

	NEO.Csum <- (Data$neo5+
	Data$neo10+
	Data$neo15REV+
	Data$neo20+
	Data$neo25
		+Data$neo30REV+
		Data$neo35+
		Data$neo40+
		Data$neo45REV
		+Data$neo50+
		Data$neo55REV+
		Data$neo60)


	#Conscientiousness for Males

		NEO.Ctm = (1.68*NEO.Csum) - 7.2667

	#Conscientiousness for Females

		NEO.Ctf = (1.7177*NEO.Csum) - 10.113


	#Combine boys and girls 
		
		NEO.C.TScore = NULL

		for (i in 1:66)

		if(Data$sex[i]==1) {
			NEO.C.TScore[i] <- NEO.Ctm[i]
			} else {
				NEO.C.TScore[i] <- NEO.Ctf[i]
				}
				
				
				
x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

write.csv(Data, file = "NEO_with_Tscores")				
		