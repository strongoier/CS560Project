# A Simple Python-to-Scala Transpiler

You should have Python 3.5+ as your default `python` in your command line environment.

You can use `sbt test` to run all the tests inside our project.

You can also use the project as a command-line tool. Usage (either way is fine):

- `sbt "run <inputFile>.py"`, the output filename will be `<inputFile>.scala`
- `sbt "run (--input-file | -i) <inputFile>.py (--output-file | -o) <outputFile>"`

Example by running `sbt "run test/quicksort.py"`:

Python

```python
def quicksort(a: List[int]) -> List[int]:
    if len(a) < 2:
        return a
    else:
        pivot = a[len(a) // 2]
        return quicksort([_ for _ in a if pivot > _]) + \
            [_ for _ in a if pivot == _] + \
            quicksort([_ for _ in a if pivot < _])


quicksort([10, 6, 8, 1, 0, 9]) == [0, 1, 6, 8, 9, 10]
```

Scala

```scala
def quicksort(a: List[Int]): List[Int] = {
  if (a.length < 2) {
    a
  } else {
    var pivot = a(a.length / 2)
    quicksort(a.filter(pivot > (_))) ++ a.filter(pivot == (_)) ++ quicksort(a.filter(pivot < (_)))
  }
}
quicksort(List(10, 6, 8, 1, 0, 9)) == List(0, 1, 6, 8, 9, 10)
```

Currently, our project supports the following features of Python:

- Arithmetic/Boolean expressions
- Variable assignments
- `if`/`for` statements
- Function definitions/calls
- List operations
- List comprehension

Notice:

- Parameters of a function declaration has to be explicitly typed
- Variables cannot be assigned to values with different types