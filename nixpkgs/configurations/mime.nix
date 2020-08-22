{ config, ... }:

{
  xdg.mime.enable = true;
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      # Set feh as the default for all image types
      "image/bmp" = "feh.desktop";
      "image/g3fax" = "feh.desktop";
      "image/gif" = "feh.desktop";
      "image/x-fits" = "feh.desktop";
      "image/x-pcx" = "feh.desktop";
      "image/x-portable-anymap" = "feh.desktop";
      "image/x-portable-bitmap" = "feh.desktop";
      "image/x-portable-graymap" = "feh.desktop";
      "image/x-portable-pixmap" = "feh.desktop";
      "image/x-psd" = "feh.desktop";
      "image/x-sgi" = "feh.desktop";
      "image/x-tga" = "feh.desktop";
      "image/x-xbitmap" = "feh.desktop";
      "image/x-xwindowdump" = "feh.desktop";
      "image/x-xcf" = "feh.desktop";
      "image/x-compressed-xcf" = "feh.desktop";
      "image/tiff" = "feh.desktop";
      "image/jpeg" = "feh.desktop";
      "image/x-psp" = "feh.desktop";
      "application/postscript" = "feh.desktop";
      "image/png" = "feh.desktop";
      "image/x-icon" = "feh.desktop";
      "image/x-xpixmap" = "feh.desktop";
      "image/x-exr" = "feh.desktop";
      "image/x-webp" = "feh.desktop";
      "image/heif" = "feh.desktop";
      "image/heic" = "feh.desktop";
      "image/svg+xml" = "feh.desktop";
      "application/pdf" = "feh.desktop";
      "image/x-wmf" = "feh.desktop";
      "image/jp2" = "feh.desktop";
      "image/x-xcursor" = "feh.desktop";

      # Set emacsclient as the default for all text types
      "application/rtf" = "emacsclient.desktop";
      "application/vnd.mozilla.xul+xml" = "emacsclient.desktop";
      "application/xhtml+xml" = "emacsclient.desktop";
      "application/xml" = "emacsclient.desktop";
      "application/x-shellscript" = "emacsclient.desktop";
      "application/x-wine-extension-ini" = "emacsclient.desktop";
      "application/zip" = "emacsclient.desktop";
      "text/english" = "emacsclient.desktop";
      "text/html" = "emacsclient.desktop";
      "text/markdown" = "emacsclient.desktop";
      "text/plain" = "emacsclient.desktop";
      "text/x-log" = "emacsclient.desktop";
      "text/x-makefile" = "emacsclient.desktop";
      "text/x-c++hdr" = "emacsclient.desktop";
      "text/x-c++src" = "emacsclient.desktop";
      "text/x-chdr" = "emacsclient.desktop";
      "text/x-csrc" = "emacsclient.desktop";
      "text/x-java" = "emacsclient.desktop";
      "text/x-moc" = "emacsclient.desktop";
      "text/x-pascal" = "emacsclient.desktop";
      "text/x-tcl" = "emacsclient.desktop";
      "text/x-tex" = "emacsclient.desktop";
      "text/xml" = "emacsclient.desktop";
      "text/x-c" = "emacsclient.desktop";
      "text/x-c++" = "emacsclient.desktop";
      "x-scheme-handler/ftp" = "emacsclient.desktop";
      "x-scheme-handler/http" = "emacsclient.desktop";
      "x-scheme-handler/https" = "emacsclient.desktop";
    };

    # xdg-open ignores these, but let's unregister all the wine cruft
    # anyway
    associations = {
      removed = {
        "application/vnd.ms-htmlhelp" = "wine-extension-chm.desktop";
        "image/gif" = ["wine-extension-gif.desktop"];
        "application/winhlp" = "wine-extension-hlp.desktop";
        "application/x-wine-extension-ini" = "wine-extension-ini.desktop";
        "image/jpeg" = ["wine-extension-jfif.desktop" "wine-extension-jpe.desktop"];
        "application/x-wine-extension-msp" = "wine-extension-msp.desktop";
        "application/pdf" = ["wine-extension-pdf.desktop"];
        "image/png" = ["wine-extension-png.desktop"];
        "application/rtf" = "wine-extension-rtf.desktop";
        "text/plain" = "wine-extension-txt.desktop";
        "application/x-mswinurl" = "wine-extension-url.desktop";
        "application/x-wine-extension-vbs" = "wine-extension-vbs.desktop";
        "application/x-mswrite" = "wine-extension-wri.desktop";
        "application/xml" = "wine-extension-xml.desktop";
        "text/html" = ["wine-extension-htm.desktop"];
      };
    };
  };
}
