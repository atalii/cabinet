{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.backend =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./backend.nix { inherit pkgs; };

    packages.x86_64-linux.frontend =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./frontend { inherit pkgs; };

    devShells.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./shell.nix { inherit pkgs; };

    nixosModules.cabinet =
      { config, pkgs, ... }: {
        networking.firewall.allowedTCPPorts = [ 80 ];

        systemd.services.cabinet-frontend = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" "cabinet-backend.target" ];
          description = "Cabinet's frontend component.";
          serviceConfig = {
            ExecStart = "${pkgs.nodejs}/bin/node ${self.packages.x86_64-linux.frontend}/lib/cabinet-frontend";
          };

	  environment.PORT = "6445";
        };

        systemd.services.cabinet-backend = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "file storage web server";
          serviceConfig = {
            ExecStart = "${self.packages.x86_64-linux.backend}/bin/cabinet-srv";
          };

	  environment.CABINET_PORT = "6446";
        };
      };
  };
}
