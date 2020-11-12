package hades.languageserver

import kotlinx.coroutines.GlobalScope

suspend fun main(): Unit =
    LanguageServer().loop(GlobalScope)
