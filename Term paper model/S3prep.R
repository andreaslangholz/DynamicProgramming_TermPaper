

# OBS! Fra dette skridt bruger de i estimationen kun de 3 indkomstgrupper
# de gerne vil finde MWP fra

## Estimation step 3 prep - Recoveringm values and prep for getting back valuef

gamma <- c(-9.5, 0.02, 0.1, -0.2, 0.006) # will be given by former estimations

gammafmc <- rep(gamma[1:2], n.types)
gammapmc <- rep(gamma[4:5], n.types)

type.comb = matrix(0,n.types,2)
k = 0
for (i in 1:n.wealthtypes) {
  for (j in 1:n.incometypes) {
  k = k + 1
  type.comb[k,1] = wealth.bins[i]
  type.comb[k,2] = income.bins[j]
  }
}





temp =[];
for h = 1:numtype 
temp = [temp ones(MSb,1).*(ZvalsbS(:,3)==h) ZvalsbS(:,2).*(ZvalsbS(:,3)==h)];
end    
gammafmctau = temp*gammafmc;

mtau = zeros(MSb,1);
for m =1:MSb
mtau(m) = gammafmctau(m)*(ZvalsbS(m,1));






































