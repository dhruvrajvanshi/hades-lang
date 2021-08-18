#!/bin/sh

./gradlew -x test distTar
./gradlew -x test linux-x86_64DistTar
./gradlew -x test macos-x86_64DistTar
./gradlew -x test windows-x86_64DistTar