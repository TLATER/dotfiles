{
  nextcloud-client,
  stdenv,
  libsForQt5,
  cmake,
  pkg-config,
  openssl,
  zlib,
}:
stdenv.mkDerivation {
  inherit (nextcloud-client) src version;

  pname = "nextcloudcmd";
  nativeBuildInputs = [
    cmake
    pkg-config
    libsForQt5.wrapQtAppsHook
  ];

  buildInputs = [
    openssl
    zlib
    libsForQt5.qtbase
    libsForQt5.qtkeychain
    libsForQt5.qtwebsockets
    libsForQt5.qtwebengine
    libsForQt5.karchive
  ];

  cmakeFlags = [
    "-DCMAKE_INSTALL_LIBDIR=lib"
    "-DBUILD_UPDATER=off"
    "-DBUILD_GUI=off"
    "-DBUILD_SHELL_INTEGRATION=off"
  ];
}
