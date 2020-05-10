

CFLAGS=$(pkg-config --cflags gtk+-3.0)
LDFLAGS=$(pkg-config --libs gtk+-3.0)

../../gradlew -c ../../settings.gradle :hadesc:run --args="--main ../examples/gtk-hello-world/main.hds --directories ./stdlib ../bindings --runtime runtime.c --output ../examples/app --cflags ../examples/gtk-hello-world/main.c $CFLAGS $LDFLAGS"

