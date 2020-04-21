def binary_search(a: List[int], low: int, high: int, value: int) -> int:
    if low < high:
        mid = (low + high) // 2
        if a[mid] < value:
            return binary_search(a, mid + 1, high, value)
        elif value < a[mid]:
            return binary_search(a, low, mid, value)
        else:
            return mid
    else:
        return -1


binary_search([5, 10, 20, 100, 200], 0, 4, 20)
