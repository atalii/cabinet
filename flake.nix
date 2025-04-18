{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./default.nix { inherit pkgs; };

    devShells.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in import ./shell.nix { inherit pkgs; };

    nixosModules.cabinet =
      { config, ... }: {
        networking.firewall.allowedTCPPorts = [ 80 ];
        systemd.services.cabinet = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "file storage web server";
          serviceConfig = {
            ExecStart = "${self.packages.x86_64-linux.default}/bin/cabinet-srv";
          };

          # Picked randomly. Lol.
	  environment.CABINET_PORT = "6445";
        };
      };
  };
}
