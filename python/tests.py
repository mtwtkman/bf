import sys
from io import StringIO
import unittest


class Test(unittest.TestCase):
    def setUp(self):
        self.output = StringIO()
        sys.stdout = self.output

    def tearDown(self):
        sys.stdout = sys.__stdout__

    def _callFUT(self, src):
        from bf import evaluate
        evaluate(src)

    def assertOutput(self, expect):
        self.assertEqual(expect, self.output.getvalue())

    def test1(self):
        src = '>+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++.'
        self._callFUT(src)
        self.assertOutput('Hello World!\n')

    def test2(self):
        src = '''
            ++++++++++++++++++++++++++++++++
            ++++++++++++++++++++++++++++++++
            ++++++++++++++++++++++++++++++++
            ++++++++.
            +++++++.
            --------.
            --.
        '''
        self._callFUT(src)
        self.assertOutput('hoge')

    def test3(self):
        src = '''
            ++++++++++[>++++++++++<-]>
            ++++.+++++++.--------.--.
        '''
        self._callFUT(src)
        self.assertOutput('hoge')

    def test4(self):
        src = '''
            +>++><<
            >[-<+>]<
            ++++++++++++++++++++++++++++++++++++++++++++++++.
        '''
        self._callFUT(src)
        self.assertOutput('3')

    def test5(self):
        src = '''
            +>++><<
            [->>>+<<<]
            >>>[-<+<<+>>>]<<
            [->>+<<]
            >>[-<+<+>>]<
            ++++++++++++++++++++++++++++++++++++++++++++++++.
        '''
        self._callFUT(src)
        self.assertOutput('3')

    def test6(self):
        src = '''
            ++++>++><<
            [-
              >[->>+<<]
              >>[-<+<+>>]
              <<<
            ]>>
            ++++++++++++++++++++++++++++++++++++++++++++++++.
        '''
        self._callFUT(src)
        self.assertOutput('8')


if __name__ == '__main__':
    unittest.main()
