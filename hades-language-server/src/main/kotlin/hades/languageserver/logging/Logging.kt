package hades.languageserver.logging

import java.text.SimpleDateFormat
import java.util.*

interface Logger {
    fun info(message: String)
    fun debug(message: String)
}


inline fun <reified T> T.logger(): Logger {
    return object : Logger {
        override fun info(message: String) {
            return withTag("INFO", message)
        }

        override fun debug(message: String) {
            return withTag("DEBUG", message)
        }

        private fun withTag(tag: String, message: String) {
            System.err.println("$tag:(${SimpleDateFormat("hh:mm:ss").format(Date())}) {thread:${Thread.currentThread().name}(${Thread.currentThread().id})} [${T::class.simpleName}] $message")
        }

    }
}