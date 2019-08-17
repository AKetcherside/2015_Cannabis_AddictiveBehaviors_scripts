#Stress Paper Mediation

library(mediation)
library(sandwich)

setwd("/Users/arielketcherside/Documents/UTD/My.Publications/2014_My_Stress_Paper/Data_for_R/Master.Dataset")




#Formulas: 
	#med.fit <- lm([mediator] ~ [IV] + age + educ + gender + income, data = [dataset])
	#out.fit <- glm([DV] ~ [mediator] + [IV] + age + educ + gender + income, data = [dataset], family = [DISTRIBUTION("LINK")])




MJ.Data <- read.csv("AllData.TEST.csv", header = TRUE, sep = 
",", quote = "\"", dec = ".", fill = TRUE, comment.char = "") 
#Demographics
sex <- MJ.Data$sex
age <- MJ.Data$age
edu <- MJ.Data$Edu

#Version 1: Sensical mediation, BAI & BDI = separate

#IVs

PS <- MJ.Data$PSStotal
ES <- MJ.Data$ELSQ
TS <- PS + ES

#Mediators
BAI <- MJ.Data$BAI_Score
BDI <- MJ.Data$BDI_Score
neg.affect <- BAI + BDI
MPS <- MJ.Data$mpstotal


##FOR THE PAPER 

	#Anxiety mediates rel. between ES and MPS
		med.fit <- lm(BAI ~ ES + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ BAI + ES + age + edu + sex, data = MJ.Data,
	 		family=gaussian("identity"))

			med.out.A.ES <- mediate(med.fit, out.fit, treat = "ES", mediator = "BAI", 				robustSE = FALSE, boot=TRUE, sims = 1000)
			summary(med.out.A.ES)

			plot(med.out.A.ES)

	#Depression mediates rel. between ES and MPS
		med.fit <- lm(BDI ~ ES + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ BDI + ES + age + edu + sex, data = MJ.Data, 
				family=gaussian("identity"))

			med.out.D.ES <- mediate(med.fit, out.fit, treat = "ES", mediator = "BDI", 				robustSE = FALSE, boot=TRUE, sims = 1000)
	
			summary(med.out.D.ES)

			plot(med.out.D.ES)


	#Anxiety DOES NOT mediate rel. between PS and MPS
		med.fit <- lm(BAI ~ PS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ BAI + PS + age + edu + sex, data = MJ.Data, 
				family=gaussian("identity"))

			med.out.A.PS <- mediate(med.fit, out.fit, treat = "PS", mediator = "BAI", 				robustSE = FALSE, boot=TRUE, sims = 1000)

			summary(med.out.A.PS)

			plot(med.out.A.PS)


	##DOES NOT MEDIATE. TRY REVERSE?
	#PS DOES NOT mediate rel. between BAI and MPS
		med.fit <- lm(PS ~ BAI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ PS + BAI + age + edu + sex, data = MJ.Data,
				family=gaussian("identity"))

			med.out.PS.A <- mediate(med.fit, out.fit, treat = "PS", mediator = "BAI",
				robustSE = FALSE, boot=TRUE, sims = 1000)

			summary(med.out.PS.A)


	#Depression mediates rel. between PS and MPS
		med.fit <- lm(BDI ~ PS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ BDI + PS + age + edu + sex, data = MJ.Data, 
				family=gaussian("identity"))

			med.out.D.PS <- mediate(med.fit, out.fit, treat = "PS", mediator = "BDI", 
				robustSE = FALSE, boot=TRUE, sims = 1000)
	
			summary(med.out.D.PS)

			plot(med.out.D.PS)



	#Anxiety mediates rel. between TOTAL stress and MPS
		med.fit <- lm(BAI ~ TS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ BAI + TS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.A.TS <- mediate(med.fit, out.fit, treat = "TS", mediator = "BAI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)

		summary(med.out.A.TS)

		plot(med.out.A.TS)
	

	#Depression mediates rel. between TOTAL stress and MPS
		med.fit <- lm(BDI ~ TS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ BDI + TS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.D.TS <- mediate(med.fit, out.fit, treat = "TS", mediator = "BDI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)

		summary(med.out.D.TS)

		plot(med.out.D.TS)
	
	
	
	
	
	
	
	
	
	

###FLIP FLOP

	#ES DOES NOT mediate rel. between ANX and MPS
		med.fit <- lm(ES ~ BAI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ ES + BAI + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.ES.A <- mediate(med.fit, out.fit, treat = "BAI", mediator = "ES", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.ES.A)

		plot(med.out.ES.A)


	#Depression DOES NOT mediate rel. between ES and MPS
		med.fit <- lm(ES ~ BDI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ ES + BDI + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.D.ES <- mediate(med.fit, out.fit, treat = "ES", mediator = "BDI", 
			robustSE = FASE, boot=TRUE, sims = 1000)
	
		summary(med.out.ES.D)

		plot(med.out.ES.D)



	#PS DOES NOT mediate rel. between BAI and MPS
		med.fit <- lm(PS ~ BAI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ PS + BAI + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.PS.A <- mediate(med.fit, out.fit, treat = "BAI", mediator = "PS", 
			robustSE = FALSE, boot=TRUE, sims = 1000)

		summary(med.out.PS.A)

		plot(med.out.PS.A)


	#PS DOES NOT mediate rel. between BDI and MPS
		med.fit <- lm(PS ~ BDI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ PS + BDI + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.PS.D <- mediate(med.fit, out.fit, treat = "BDI", mediator = "PS", 
			robustSE = FALSE, boot=TRUE, sims = 1000)

		summary(med.out.PS.D)

		plot(med.out.PS.D)



	#TS DOES NOT mediate rel. between BAI and MPS
		med.fit <- lm(TS ~ BAI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ TS + BAI + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.TS.A <- mediate(med.fit, out.fit, treat = "BAI", mediator = "TS", 
			robustSE = FALSE, boot=TRUE, sims = 1000)

		summary(med.out.TS.A)

		plot(med.out.TS.A)


	#TS DOES NOT mediate rel. between BDI and MPS
		med.fit <- lm(TS ~ BDI + age + edu + sex, data = MJ.Data)
		out.fit <- glm(MPS ~ TS + BDI + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.TS.D <- mediate(med.fit, out.fit, treat = "BDI", mediator = "TS", 
			robustSE = FALSE, boot=TRUE, sims = 1000)

		summary(med.out.TS.D)

		plot(med.out.TS.D)	
	
	
	
#Flip Flop again - MPS as IV


	#ES DOES NOT mediate rel. between MPS and BAI
		med.fit <- lm(ES ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(BAI ~ ES + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.ES.MA <- mediate(med.fit, out.fit, treat = "MPS", mediator = "ES", 	
			robustSE = FALSE, boot=TRUE, sims = 1000)
		
		summary(med.out.ES.MA)	

		plot(med.out.ES.MA)
	
	
	
	#ES DOES NOT mediate rel. between MPS and BDI
		med.fit <- lm(ES ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(BDI ~ ES + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.ES.MD <- mediate(med.fit, out.fit, treat = "MPS", mediator = "ES", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
		
		summary(med.out.ES.MD)

		plot(med.out.ES.MD)



	
	#PS partially mediates rel. between MPS and BAI
		med.fit <- lm(PS ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(BAI ~ PS + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.PS.MA <- mediate(med.fit, out.fit, treat = "MPS", mediator = "PS", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.PS.MA)

		plot(med.out.PS.MA)
	
	#PS DOES NOT mediate rel. between MPS and BDI
		med.fit <- lm(PS ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(BDI ~ PS + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.PS.MD <- mediate(med.fit, out.fit, treat = "MPS", mediator = "PS", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.PS.MD)
	
		plot(med.out.PS.MD)	
	

#!!!	#BAI mediates rel. between MPS and ES
		med.fit <- lm(BAI ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(ES ~ BAI + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.ES.A <- mediate(med.fit, out.fit, treat = "MPS", mediator = "BAI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.ES.A)

		plot(med.out.ES.A)		



##THIS ONE DOESN'T WORK UNLESS I IMPUTE VALUES FOR PSS (MEAN = 34.56756757)
	#BAI DOES NOT mediate rel. between MPS and PS
		med.fit <- lm(BAI ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(PS ~ BAI + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.PS.MA2 <- mediate(med.fit, out.fit, treat = "MPS", mediator = "BAI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
		
		summary(med.out.PS.MA2)

		plot(med.out.PS.MA2)			



	#BDI DOES NOT mediate rel. between MPS and ES
		med.fit <- lm(BDI ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(ES ~ BDI + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.ES.MD <- mediate(med.fit, out.fit, treat = "MPS", mediator = "BDI",
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.ES.MD2)

		plot(med.out.ES.MD2)			

	
##THIS ONE DOESN'T WORK UNLESS I IMPUTE VALUES FOR PSS  
!!!	#BDI MEDIATES rel. between MPS and PS
		med.fit <- lm(BDI ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(PS ~ BDI + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.PS.MD2 <- mediate(med.fit, out.fit, treat = "MPS", mediator = "BDI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.PS.MD2)

		plot(med.out.PS.MD2)
		
		
##Total Stress with MPS as IV  
#BDI mediates rel. between MPS and TS
		med.fit <- lm(BDI ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(TS ~ BDI + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.TS.MD2 <- mediate(med.fit, out.fit, treat = "MPS", mediator = "BDI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.TS.MD2)

		plot(med.out.TS.MD2)
		
#BAI DOES NOT rel. between MPS and TS
		med.fit <- lm(BAI ~ MPS + age + edu + sex, data = MJ.Data)
		out.fit <- glm(TS ~ BAI + MPS + age + edu + sex, data = MJ.Data, 
			family=gaussian("identity"))

		med.out.TS.MA2 <- mediate(med.fit, out.fit, treat = "MPS", mediator = "BAI", 
			robustSE = FALSE, boot=TRUE, sims = 1000)
	
		summary(med.out.TS.MA2)

		plot(med.out.TS.MA2)		
		
		
				