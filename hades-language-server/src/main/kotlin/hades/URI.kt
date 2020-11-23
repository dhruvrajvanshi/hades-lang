package hades

import java.io.File

inline class URI(val value: String)

val String.asURI get(): URI = URI(this)

val URI.file get(): File = File(java.net.URI.create(value))
