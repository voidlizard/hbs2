# See how this flake is used in ./usage.sh
{
  description = "hbs2-container";

  inputs = {
    extra-container.url = "github:erikarvstedt/extra-container";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    hbs2.url = "github:voidlizard/hbs2/announce-group";
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

        # home-manager.homeConfigurations = {
        #   # FIXME replace with your username@hostname
        #   "root@minipig" = home-manager.lib.homeManagerConfiguration {
        #     pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
        #     extraSpecialArgs = { inherit inputs; }; # Pass flake inputs to our config
        #     # > Our main home-manager configuration file <
        #     modules = [ ./home/home.nix ];
        #   };
        # };

        # Set this to disable `nix run` support
        # addRunner = false;

        config = {
          containers.hbs2-test = {
            extra.addressPrefix = "10.250.0";

            config = { pkgs, ... }: {

              users.users.hbs2 = {
                isNormalUser = true;
                home  = "/home/hbs2";
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
              ];

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
rpc    "127.0.0.1:13331"
key    "./key"
storage  "/root/hbs2"
accept-block-announce *
'';

              };

            };
          };
        };
      };
    });


}
