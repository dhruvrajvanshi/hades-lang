#include <gtk/gtk.h>

static void
print_hello (GtkWidget *widget,
             gpointer   data)
{
  g_print ("Hello World\n");
}
extern GtkWindow* make_window();
static void
activate (GtkApplication *app,
          gpointer        user_data)
{
  GtkWidget *button;
  GtkWidget *button_box;

  g_print("activate: %s", user_data);

  GtkWidget* window = make_window(app);

  button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
  gtk_container_add (GTK_CONTAINER (window), button_box);

  button = gtk_button_new_with_label ("Hello World");
  g_signal_connect (button, "clicked", G_CALLBACK (print_hello), NULL);
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  gtk_container_add (GTK_CONTAINER (button_box), button);

  gtk_widget_show_all (window);
}

int run(GtkApplication *app, int argc, char** argv)
{
  g_signal_connect (app, "activate", G_CALLBACK (activate), "data");
  int status = g_application_run (G_APPLICATION (app), argc, argv);
  return status;
}
