plugins {
    java
    application
    kotlin("jvm") version "1.8.10"
    kotlin("plugin.serialization") version "1.8.10"
    jacoco
    id("io.gitlab.arturbosch.detekt").version("1.22.0")
}

val logbackVersion = "1.4.6"
val slf4jVersion = "2.0.6"
val junitVersion = "5.9.2"
val kotlinxSerializationVersion = "1.5.0"

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
    implementation("com.github.ajalt.clikt:clikt:3.5.2")
    implementation("org.bytedeco:llvm-platform:15.0.3-1.5.8")

    // Get the latest version number from https://github.com/charleskorn/kaml/releases/latest
    implementation("com.charleskorn.kaml:kaml:0.53.0")

    testImplementation(kotlin("test"))

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$kotlinxSerializationVersion")

    testImplementation("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-api:$junitVersion")
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

tasks.test {
    workingDir = File("..")
    environment["HADES_HOME"] = hadesHome
    useJUnitPlatform()
}

tasks.compileKotlin {
    kotlinOptions {
        freeCompilerArgs = listOf("-Xinline-classes")
    }
}
tasks.compileTestKotlin {
}

val hadesHome = "$buildDir/HADES_HOME"

val copyStdlib = tasks.register<Copy>("copyStdlib") {
    from("../stdlib")
    into("$hadesHome/stdlib")
}

val assembleHadesHome = tasks.register("assembleHadesHome") {
    dependsOn(copyStdlib, installGC)
}

val cleanHadesHome = tasks.register("cleanHadesHome") {
    delete(hadesHome)
}

tasks.classes {
    dependsOn(assembleHadesHome)
}

distributions {
    main {
        contents {
            from("../stdlib") {
                into("stdlib")
            }

            from("$hadesHome/lib") {
                into("lib")
            }
            from("$hadesHome/include") {
                into("include")
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

val configureGC = tasks.register<Exec>("configureGC") {
    workingDir = File("../bdwgc")
    commandLine(
        "cmake",
        "-DCMAKE_INSTALL_PREFIX=${hadesHome}",
        "-DCMAKE_BUILD_TYPE=Release",
        "-S", ".", "-B", "out")
}
val buildGC = tasks.register<Exec>("buildGC") {
    dependsOn(configureGC)
    inputs.dir("../bdwgc")
    workingDir = File("../bdwgc")
    commandLine("cmake", "--build", "out", "--config", "Release")
}
val installGC = tasks.register<Exec>("installGC") {
    dependsOn(buildGC)
    inputs.dir("../bdwgc")
    workingDir = File("../bdwgc")
    commandLine("cmake", "--install", "out")
}

val cleanGCBuild = tasks.register("cleanGCBuild") {
    delete("../bdwgc/out")
}

tasks.clean {
    dependsOn(cleanGCBuild, cleanHadesHome)
}
