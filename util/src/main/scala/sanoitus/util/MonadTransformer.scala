package sanoitus.util

trait MonadTransformer {
  // Thanks, Miles! (https://gist.github.com/milessabin/c9f8befa932d98dcc7a4)
  trait <:!<[A, B]
  implicit def nsub[A, B]: A <:!< B = null
  implicit def nsubAmbig1[A, B >: A]: A <:!< B = null
  implicit def nsubAmbig2[A, B >: A]: A <:!< B = null

  type |¬|[T] = {
    type λ[U] = U <:!< T
  }
}
