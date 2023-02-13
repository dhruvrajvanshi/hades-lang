package hadesc.logging

import org.slf4j.Logger
import org.slf4j.LoggerFactory

@Suppress("unused") // Receiver T isn't actually unused
fun <T> T.logger(klass: Class<T>): Logger = LoggerFactory.getLogger(klass)
