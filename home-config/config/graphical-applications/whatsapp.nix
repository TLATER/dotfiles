{config, ...}: {
  programs.firefox.webapps.whatsapp = {
    url = "https://web.whatsapp.com";
    id = 3;
    extraSettings = config.programs.firefox.profiles."tlater".settings;

    categories = ["Network" "InstantMessaging"];
  };
}
