import sys
from contextlib import contextmanager
from io import StringIO


SYSTEM_STDOUT = sys.stdout


def test(func):
    def _inner(*args, **kwargs):
        print(f'test: {func.__name__}')
        try:
            func(*args, **kwargs)
        except Exception as e:
            raise e
        else:
            print('ok')
    return _inner


@test
def cleanup():
    from main import cleanup as testFunc

    assert testFunc('') == ''
    assert testFunc('+ +') == '++'
    assert testFunc('+\n+') == '++'
    assert testFunc('a+') == '+'


@contextmanager
def mock_stdout():
    out = StringIO()
    sys.stdout = out
    try:
        yield out
    finally:
        sys.stdout = SYSTEM_STDOUT


@test
def brainfuck():
    from main import brainfuck as testFunc

    with mock_stdout() as out:
        testFunc('++.')
        assert out.getvalue() == chr(2)
        out.seek(0)
        testFunc('++++++++++[>+++++++++<-]>+++++++.')
        assert out.getvalue() == 'a'
        out.seek(0)
        testFunc('++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.')
        assert out.getvalue() == 'Hello World!\n'


cleanup()
brainfuck()
