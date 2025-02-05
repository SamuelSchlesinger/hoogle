{ hoogle, cores ? 4 }:
{ config, pkgs, ... }:

# The Plan:
# Hoogle serves on a uniquely-named UNIX domain socket which we
# configure nginx to forward to via the nginxConf configuration
# fragment. The Hoogle database is periodically updated by
# rotate-hoogle.service which generates a new Hoogle database,
# starts a new hoogle server, updates the nginx configuration
# and shuts down the old server.

let
  hoogleRun = "/run/hoogle";
  nginxConf = "${hoogleRun}/nginx.conf";
  socket = "/run/hoogle/hoogle.sock";
in
{
  users.users.hoogle = {
    isSystemUser = true;
    description = "hoogle server";
    group = "hoogle";
  };

  users.groups.hoogle = {
    members = [ "nginx" ];
  };

  systemd.timers."generate-hoogle" = {
    description = "Rotate Hoogle instance";
    timerConfig = {
      Unit = "generate-hoogle.service";
      OnCalendar = "hourly";
      Persistent = true;
    };
    wantedBy = [ "timers.target" ];
  };

  systemd.services."generate-hoogle" = {
    script = ''
      hoogle generate --database=$DB_DIR/haskell-new.hoo --insecure --download +RTS -N${toString cores} -RTS
      mv $DB_DIR/haskell-new.hoo $DB_DIR/haskell.hoo
    '';
    path = [ hoogle ];
    after = [ "network.target" ];
    serviceConfig = {
      User = "hoogle";
      Group = "hoogle";
      Type = "oneshot";
      TimeoutStartSec = 600;
      ExecStartPost = "+systemctl restart hoogle.service";
      BindReadOnlyPaths = [
        # mount the nix store read-only
        "/nix/store"
        # getAppUserDataDirectory needs getUserEntryForID
        "/etc/passwd"
      ];
      PrivateNetwork = false;
      RuntimeDirectory = "generate-hoogle";
      StateDirectory = [ "hoogle" ];
    };
    environment = {
      DB_DIR = "%S/hoogle";
    };
  };

  systemd.services."hoogle" = {
    script = ''
      hoogle serve \
        --database=$DB_DIR/haskell.hoo \
        --scope=set:stackage \
        --socket=${socket} \
        --links \
        +RTS -T -N${toString cores} -RTS;
    '';
    path = [ hoogle ];
    serviceConfig = {
      User = "nginx";
      Group = "hoogle";
      BindReadOnlyPaths = [
        # mount the nix store read-only
        "/nix/store"
        # getAppUserDataDirectory needs getUserEntryForID
        "/etc/passwd"
        #"/etc/ssl" "/etc/ssl/certs"
      ];
      PrivateTmp = false;
      ProtectSystem = false;
      ProtectHome = false;
      NoNewPrivileges = false;
      Restart = "on-failure";
      RestartSec = "5s";
      RuntimeDirectory = "hoogle";
    };
    environment = {
      DB_DIR = "%S/hoogle";
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    };
    wantedBy = [ "multi-user.target" ];
  };

  systemd.tmpfiles.rules = [
    "f ${nginxConf} 0755 nginx nginx -"
  ];

  services.nginx = {
    upstreams.hoogle.extraConfig = ''
      server unix:${socket};
    '';
  };
}

