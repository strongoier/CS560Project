def exchange(a: int, b: List[int]) -> int:
    for i in [1, 2, 3]:
        print([b(_) for _ in b if a > _])
        #print([len(j) for i in range(100) if i > 10 for j in range(i) if j < 20])
    if (1 == 1):
        return [_ for _ in a if a > _ if a < 4]
    if (1 < 2):
        return exchange(a, b)
    elif (1 >= 2):
        return a[len(a) // 2]
    else:
        return (1 <= 2 + 3 and 2 <= 2) or not (2 == 3)
