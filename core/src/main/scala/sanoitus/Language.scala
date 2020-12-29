package sanoitus

trait Language { self: Interpreter =>
  type Op[+A] <: Operation[A]

  trait Operation[+A] { operation: Op[A] =>
    def interpret: Program[A] = self(operation)
  }
}
