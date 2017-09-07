package bf

object Brainfuck {
  def run(code: Array[Char]) = {
    var memory: Array[Integer] = Array.fill[Integer](30000)(0)
    var memptr: Integer = 0
    var prgptr: Integer = 0

    def jump(dir: Int) = {
      var loop = 1
      while(loop > 0) {
        prgptr += dir
        loop += (code(prgptr) match {
          case '[' => dir
          case ']' => -dir
          case _ => 0
        })
      }
    }

    while (prgptr < code.size) {
      code(prgptr) match {
        case '>' => memptr += 1
        case '<' => memptr -= 1
        case '+' => memory(memptr) += 1
        case '-' => memory(memptr) -= 1
        case '.' => print(memory(memptr).toChar)
        case ',' => memory(memptr) = readInt()
        case '[' => if(memory(memptr) == 0) jump(1)
        case ']' => if(memory(memptr) != 0) jump(-1)
      }
      prgptr += 1
    }
  }

  def main(args: Array[String]): Unit = {
    run("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.".toCharArray())
  }
}
