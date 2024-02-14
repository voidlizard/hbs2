# See how this flake is used in ./usage.sh
# on host: sudo sysctl -w net.ipv4.ip_forward=1
{
  description = "hbs2-container";

  inputs = {
    extra-container.url = "github:erikarvstedt/extra-container";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    hbs2.url = "git+http://git.hbs2/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP?rev=039d2bfefcd11f67ed957a71d650e877f8500611";
    hbs2.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs = { extra-container, nixpkgs, home-manager, ... }@inputs:
    extra-container.inputs.flake-utils.lib.eachSystem extra-container.lib.supportedSystems (system: {
      packages.default = extra-container.lib.buildContainers {
        # The system of the container host
        inherit system;

        # Only set this if the `system.stateVersion` of your container
        # host is < 22.05
        # legacyInstallDirs = true;

        # Optional: Set nixpkgs.
        # If unset, the nixpkgs input of extra-container flake is used
        nixpkgs = inputs.nixpkgs;


        # Set this to disable `nix run` support
        # addRunner = false;

        config = {
          containers.hbs2-test = {
            extra = {
              addressPrefix = "10.250.0";
              exposeLocalhost = true;
              firewallAllowHost = true;
              enableWAN = true;
            };

            config = { pkgs, ... }: {


              imports = [
                 home-manager.nixosModules.home-manager
                 {
                   home-manager.useGlobalPkgs = true;
                   home-manager.useUserPackages = true;
                   home-manager.users.hbs2 = {
                      # import ./config/home.nix;
                      home.stateVersion = "23.05";

                      xdg.configFile = {
                        ".hbs2-peer/config".text = ''
;; hbs2-peer config file

listen "0.0.0.0:7351"
listen-tcp "0.0.0.0:10351"

known-peer "10.250.0.1:7354"

; edit path to a keyring file
; key    "./key"
key "./default.key"
                        '';
                      };
                   };
                   home-manager.extraSpecialArgs = {
                                                     # inherit inputs;
                                                   };
                                                 }
              ];


              # settings.trusted-users = [ "root" "hbs2" ];


              nix = {
                package = pkgs.nixFlakes;
                extraOptions = ''
                  experimental-features = nix-command flakes
                '';
                #settings.trusted-users = [ "root" "dmz" ];
              };

              users.users.hbs2 = {
                isNormalUser = true;
                home  = "/home/hbs2";
                packages = with pkgs; [];
              };

              systemd.services.hello = {
                wantedBy = [ "multi-user.target" ];
                script = ''
                  while true; do
                    echo hello | ${pkgs.netcat}/bin/nc -lN 50
                  done
                '';
              };

              networking.firewall.enable = false;

              environment.systemPackages = with pkgs; [
                inputs.hbs2.packages.${pkgs.system}.default
                screen
                tshark
                tmux
                gitFull
                iptables
              ];

              environment.etc = {
                "tmux.conf".source = ./tmux.conf;
              };

              # environment.xdg.data."hbs2/wtf" = {
              #   text = "pwned";
              # };

              environment.etc."hbs2-peer/key"  = {
                text = ''
# hbs2 credentials file
# keep it private

rmbTHUgv7NPkygrgEbPWTjQbVTx9uwWJ7sJVQRKK7ygM73q5Q7JVYhpwfytK
kzoNq65jn1zDCUUMwLFzXeu4ETjG4bhYNM7FHU9xY4VdwEww156iJeoydJ2y
j1u3RJEr8kosBH2DR8XMY6Mj8s
'';
              };

              environment.etc."hbs2-peer/config"  = {
                text = ''
listen "0.0.0.0:7351"
listen-tcp "0.0.0.0:3003"
rpc    "127.0.0.1:13331"
http-port 5001
key    "./key"
storage  "/root/.local/share/hbs2"
accept-block-announce *
bootstrap-dns "bootstrap.hbs2.net"
known-peer "10.250.0.1:7354"

; poll reflog 1 "2YNGdnDBnciF1Kgmx1EZTjKUp1h5pvYAjrHoApbArpeX"

'';

              };

            };
          };
        };
      };
    });


}
