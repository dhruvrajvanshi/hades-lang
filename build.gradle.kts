plugins {
    java
    kotlin("jvm") version "2.0.0"
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}
