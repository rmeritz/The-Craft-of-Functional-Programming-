--Heat Transfer 

xc :: Float -> Float -> Float 
xc rho nu = ((5e5)*nu)/(rho)

reynolds :: Float -> Float -> Float -> Float
reynolds vel distance kvis = (distance*vel)/kvis 

lamBL :: Float -> Float -> Float
lamBL dis rey = (5*dis)/sqrt(rey)

thermBL :: Float -> Float -> Float 
thermBL lBL pr = lBL/(pr**(1/3))

ray beta deltaT len vis alpha = (9.8*beta*deltaT*(len**3))/(vis*alpha)

nuss1 ray pr = (0.825+(0.387*(ray**(1/6))/((1+((0.492/pr)**(9/16))**(8/27)))))**2

nuss2 ray pr h l = 0.42*(ray**(1/4))*(pr**(0.012))*((h/l)**(-0.3))

hbar nu l k = (nu*k)/l