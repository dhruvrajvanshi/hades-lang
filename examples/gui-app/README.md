# GLFW GUI app

This is a bare bones application that creates a window and fills it
with black background.

Just as a demo that we can link against C libraries by using hand
written bindings.

We don't have while loops yet, the main loop is written in C and linked
with the object file

## Building
Install `libglfw3` using the OS package manager (I haven't tested on
windows but should probably work using cygwin)

From project root
```
./gradlew :hadesc:run --args="--main ../examples/gui-app/glfw_app.hds --directories std ../bindings --runtime runtime.c --output ../app --cflags  -lGL -lglfw -g"
```

This will create an `app` executable in the root directory.

```
./app
```
This will open a window. Pretty...pretty...pretty good!
