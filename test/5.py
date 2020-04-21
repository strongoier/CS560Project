a = [10, 15]
c = [20, -10]
b = [0, 1]
tot = [a[j] for j in b if j > 0]
tot2 = [a[j] for j in b if c[j] > 0]
tot3 = [j for j in b if j > 0]
tot4 = [a[j] for j in b]
tot5 = [a[j] for j in b if c[j] > 0 if j > 0]
sum(tot) + sum(tot2) + sum(tot3) + sum(tot4) + sum(tot5)
