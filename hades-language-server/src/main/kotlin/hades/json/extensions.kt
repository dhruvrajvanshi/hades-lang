package hades.json

import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.decodeFromJsonElement


val format = Json {
    ignoreUnknownKeys = true
}

inline fun <reified T> JsonElement.decode(): T =
    format.decodeFromJsonElement(this)

inline fun <reified T> String.decodeJson(): T =
    format.decodeFromString(this)

