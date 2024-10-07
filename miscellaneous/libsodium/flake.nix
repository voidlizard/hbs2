{
  description = "Flake for libsodium 1.0.19-RELEASE";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages = nixpkgs.lib.attrsets.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system: let
      pkgs = import nixpkgs { inherit system; };
      version = "1.0.19";
    in rec {
      libsodium = pkgs.stdenv.mkDerivation {
        pname = "libsodium";
        version = version;

        src = ./source;

        nativeBuildInputs = [ pkgs.autoreconfHook ];
        buildInputs = [ pkgs.libtool pkgs.pkg-config ];

        configurePhase = ''
          ./configure --prefix=$out
        '';

        buildPhase = "make";

        installPhase = "make install";

        meta = with pkgs.lib; {
          description = "A modern, portable, easy to use crypto library (version 1.0.19-RELEASE)";
          homepage = "https://libsodium.org/";
          license = licenses.isc;
          platforms = platforms.unix;
          maintainers = with maintainers; [ name "voidlizard" ];
        };
      };
    });

    # Указываем пакет по умолчанию
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.libsodium;
    defaultPackage.aarch64-linux = self.packages.aarch64-linux.libsodium;
  };
}

