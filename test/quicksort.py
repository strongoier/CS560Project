def quicksort(a: List[int]) -> List[int]:
    if len(a) < 2:
        return a
    else:
        pivot = a[len(a) // 2]
        return quicksort([_ for _ in a if pivot > _]) + \
            [_ for _ in a if pivot == _] + \
            quicksort([_ for _ in a if pivot < _])


quicksort([10, 6, 8, 1, 0, 9]) == [0, 1, 6, 8, 9, 10]
