{
  nextcloud-client,
  stdenv,
  kdePackages,
  cmake,
  pkg-config,
  libp11,
  openssl,
  zlib,
}:
stdenv.mkDerivation {
  inherit (nextcloud-client) src version;

  pname = "nextcloudcmd";
  nativeBuildInputs = [
    cmake
    pkg-config
    kdePackages.wrapQtAppsHook
  ];

  buildInputs = [
    libp11
    openssl
    zlib
    kdePackages.qtbase
    kdePackages.qtkeychain
    kdePackages.qtwebsockets
    kdePackages.qtsvg
    kdePackages.qt5compat
    kdePackages.karchive
  ];

  cmakeFlags = [
    "-DCMAKE_INSTALL_LIBDIR=lib"
    "-DBUILD_UPDATER=off"
    "-DBUILD_GUI=off"
    "-DBUILD_SHELL_INTEGRATION=off"
    "-DBUILD_WITH_WEBENGINE=off"
    "-DINSTALL_SYSTEMD=off"
  ];
}
