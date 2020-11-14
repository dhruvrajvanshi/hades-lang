package hades.languageserver.logging

import java.util.*

interface Logger {
    fun info(message: String)
    fun debug(message: String)
}

inline fun <reified T> logger(): Logger {
    return object : Logger {
        override fun info(message: String) {
            return withTag("INFO", message)
        }

        override fun debug(message: String) {
            return withTag("DEBUG", message)
        }

        private fun withTag(tag: String, message: String) {
            System.err.println("$tag:\t(${Date()}) [${T::class.simpleName}] $message")
        }

    }
}