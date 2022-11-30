plugins {
    java
    application
    kotlin("jvm") version "1.7.22"
    kotlin("plugin.serialization") version "1.7.22"
    jacoco
}

val logbackVersion = "1.4.4"
val slf4jVersion = "2.0.5"
val junitVersion = "5.9.1"

application {
    group = "org.hades"
    applicationName = "hades"
    mainClass.set("hadesc.MainKt")
}

repositories {
    mavenCentral()
    maven { url = uri("https://oss.sonatype.org/content/repositories/snapshots") }
}

dependencies {
    implementation("ch.qos.logback:logback-classic:$logbackVersion")
    implementation("ch.qos.logback:logback-core:$logbackVersion")
    implementation("org.slf4j:slf4j-api:$slf4jVersion")
    implementation("org.apache.commons:commons-lang3:3.12.0")
    implementation("com.diogonunes:JColor:5.5.1")
    implementation("com.github.ajalt.clikt:clikt:3.5.0")
    implementation("org.bytedeco:llvm-platform:13.0.1-1.5.7")

    // Get the latest version number from https://github.com/charleskorn/kaml/releases/latest
    implementation("com.charleskorn.kaml:kaml:0.49.0")

    testImplementation(kotlin("test"))

    testImplementation("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-api:$junitVersion")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(15))
    }
}

tasks.test {
    workingDir = File("..")
    environment["HADES_HOME"] = "."
    useJUnitPlatform()
}

tasks.compileKotlin {
    kotlinOptions.jvmTarget = "15"
    kotlinOptions {
        freeCompilerArgs = listOf("-Xinline-classes")
    }
}
tasks.compileTestKotlin {
    kotlinOptions.jvmTarget = "15"
}


distributions {
    main {
        contents {
            from("../stdlib") {
                into("stdlib")
            }
        }
    }
    create("windows-x86_64") {
        distributionBaseName.set("hades-windows-x86_64")
        contents {
            with(main.get().contents)
            exclude("**/*linux*.jar")
            exclude("**/*macos*.jar")
            exclude("**/llvm-*-windows-armhf.jar")
            exclude("**/llvm-*-windows-arm64.jar")
            exclude("**/llvm-*-windows-ppc64le.jar")
            exclude("**/llvm-*-windows-x86.jar")
        }
    }

    create("linux-x86_64") {
        distributionBaseName.set("hades-linux-x86_64")
        contents {
            with(main.get().contents)
            exclude("**/*windows*.jar")
            exclude("**/*macos*.jar")
            exclude("**/llvm-*-linux-armhf.jar")
            exclude("**/llvm-*-linux-arm64.jar")
            exclude("**/llvm-*-linux-ppc64le.jar")
            exclude("**/llvm-*-linux-x86.jar")
        }
    }

    create("macos-x86_64") {
        distributionBaseName.set("hades-macos-x86_64")
        contents {
            with(distributions.main.get().contents)
            exclude("**/*windows*.jar")
            exclude("**/*linux*.jar")
            exclude("**/llvm-*-macos-armhf.jar")
            exclude("**/llvm-*-macos-arm64.jar")
            exclude("**/llvm-*-macos-ppc64le.jar")
            exclude("**/llvm-*-macos-x86.jar")
        }
    }
}
