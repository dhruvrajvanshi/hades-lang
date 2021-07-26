package hadesc.logging

import org.slf4j.Logger
import org.slf4j.LoggerFactory


@Suppress("unused") // Receiver T isn't actually unused
inline fun <reified T> T.logger(): Logger = LoggerFactory.getLogger(T::class.java)