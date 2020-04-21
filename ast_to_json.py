import ast
import typing as t
import numbers
import json
import argparse
from pathlib import Path


def to_dict(node: t.Union[ast.AST, str, numbers.Number, list]):

    if isinstance(node, complex):
        return {"class": "complex", "real": node.real, "imag": node.imag}
    elif isinstance(node, str):
        return node
    elif isinstance(node, numbers.Number):
        return node
    elif isinstance(node, list):
        return [to_dict(each) for each in node]
    elif isinstance(node, ast.AST):
        data = {
            "class": node.__class__.__name__,
            **{
                field: to_dict(value)
                for field, value in ast.iter_fields(node)
            }
        }

        # if hasattr(node, 'lineno'):
        #     data['lineno'] = node.lineno
        # if hasattr(node, 'col_offset'):
        #     data['colno'] = node.col_offset

        return data
    return node


def from_file(input: 'filename', to: 'filename'):
    """
    from python source to json file
    """
    path = Path(input)
    with path.open('r') as fr, Path(to).open('w') as fw:
        try:
            data = to_dict(ast.parse(fr.read()))
            data['name'] = path.name[:-3]  # remove `.py`
            json.dump([str(path), data], fw, indent=2)
        except SyntaxError as e:
            print(e)
            pass


def from_code(input: 'text', to: 'filename'):
    """
    from python source code to json file
    """
    with Path(to).open('w') as fw:
        try:
            data = to_dict(ast.parse(input))
            data['name'] = 'Default'
            json.dump(['<stdin>', data], fw, indent=2)
        except SyntaxError:
            pass


def msg(name=None):
    return '''ast_to_json.py
         [-h] [--fromFile | -f inputFile]
        '''


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Create AST from Python file or text', usage=msg())
    parser.add_argument('--fromFile', '-f', nargs=1, type=str,
                        help='Takes in input file')

    args = parser.parse_args()
    if (len(args.fromFile) == 1):
        fromFile = args.fromFile[0]
        from_file(fromFile, fromFile.replace(".py", ".json"))
