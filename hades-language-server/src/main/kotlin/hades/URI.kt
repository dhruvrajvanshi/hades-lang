package hades

inline class URI(val value: String)

val String.asURI get(): URI = URI(this)