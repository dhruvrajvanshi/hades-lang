#!/bin/sh

./gradlew distTar
./gradlew linux-x86_64DistTar
./gradlew macos-x86_64DistTar
./gradlew windows-x86_64DistTar