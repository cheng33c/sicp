## problem2.13
PI(c, p) = PI(a, p1)*PI(b, p2)  
  :=> OI(a-a*p1, a+a*p1) * OI(b-b*p2, b+b*p2)  
       = OI((a-a*p1)*(b-b*p2), (a+a*p1)*(b+b*p2))  
       = OI(cL, cU)  

c = (cL + cU)/2  
  = ((a-a*p1)*(b-b*p2) + (a+a*p1)*(b+b*p2))/2  
  = a*b*(1+p1*p2)  

p = (c-cL)/c  
  = (a*b*(1+p1*p2) - (a-a*p1)*(b-b*p2))/a*b*(1+p1*p2)  
  = a*b*(p1 + p2)/a*b*(1+p1*p2)  
  = (p1+p2)/(1+p1*p2)

c = a*b*(1+p1*p2);  
p = (p1+p2)/(1+p1*p2);
