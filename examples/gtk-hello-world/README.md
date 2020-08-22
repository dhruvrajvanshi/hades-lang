Make sure that libgtk-3-dev is installed

on Ubuntu like distros,
```sh
sudo apt install libgtk-3-dev
```

Then, from project root
```
make --directory examples/gtk-hello-world
```

This will build examples/gtk-hello-world/gtk-hello-world
executable.

Run it:
```sh
./examples/gtk-hello-world/gtk-hello-world
```
You should see a window with a native GTK button.
