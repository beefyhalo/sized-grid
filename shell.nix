# Compatibility shim for non-flake workflows: `nix-shell` enters the flake devShell.
(import
  (fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/refs/tags/v1.1.0.tar.gz";
    sha256 = "sha256:0m9grvfsbwmvgwaxvdzv6cmyvjnlww004gfxjvcl806ndqaxzy4j";
  })
  { src = ./.; }).shellNix
