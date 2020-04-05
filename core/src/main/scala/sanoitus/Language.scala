package sanoitus

trait Language { self: Interpreter =>
  trait Operation[+A] { operation =>
    def interpret: Program[A] = self(operation)
  }
}
