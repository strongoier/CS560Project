def test(a: int, b: List[int]) -> int:
    if b[0] < a:
        return 1
    elif b[0] == a:
        return 2
    else:
        return 3

test(0, [1, 2]) * 100 + test(1, [1, 2]) * 10 + test(2, [1, 2])