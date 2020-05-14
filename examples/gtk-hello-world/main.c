#include <gtk/gtk.h>

static void
print_hello (GtkWidget *widget,
             gpointer   data)
{
  g_print ("Hello World\n");
}
extern GtkWindow* make_window();

void connect_exit_button(GtkButton* button, GtkWindow* window) {
   g_signal_connect (button, "clicked", G_CALLBACK (print_hello), NULL);
   g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
}

extern void on_app_activate(GtkApplication* app, void* data);

int run(GtkApplication *app, int argc, char** argv)
{
  g_signal_connect (app, "activate", G_CALLBACK (on_app_activate), "data");
  int status = g_application_run (G_APPLICATION (app), argc, argv);
  return status;
}
