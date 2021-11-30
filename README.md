# Plutus Platform starter project

This is the repository dedicated to the continued development of YoctoDAO which is being used as a basis for ₳DAO.

## Setting up

### VSCode devcontainer

Use the provided VSCode devcontainer to get an environment with the correct tools set up.

- Install Docker
- Install VSCode
  - Install the [Remote Development extension pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - You do *not* need to install the Haskell extension
- Ensure you have a `~/.cabal/packages` folder. You can create this via `mkdir -p ~/.cabal/packages`; it's used to cache cabal packages.
- Clone this repository and open it in VSCode
  - It will ask if you want to open it in the container, say yes.
  - The first time it will take a few minutes to download the devcontainer image from dockerhub,
  - `cabal build` from the terminal should work (unless you didn't have a `~/.cabal` folder, in which case you'll need to run `cabal update` first.)
  - Opening a Haskell file should give you IDE features (it takes a little while to set up the first time)

Note: This uses the [plutus-starter-devcontainer image on dockerhub](https://hub.docker.com/r/inputoutput/plutus-starter-devcontainer), if
you wish to build the image yourself, you can do so as follows:
  - Clone https://github.com/input-output-hk/plutus,
  - Set up your machine to build things with Nix, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!),
  - Build and load the docker container: `docker load < $(nix-build default.nix -A devcontainer)`,
  - Adjust the `.devcontainer/devcontainer.json` file to point to your local image.

### Cabal+Nix build

Alternatively, use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

## Building the code:
If you are able to get `cabal build` to run successfully inside of the appropriate commit of plutus then you should be able to run `cabal run script-dump` to get a whole host of files which are then able to be used to deploy the script.

You'll need to modify `exe/script-dump` to contain the appropriate constant values for `nftSymbol` (The DAO's identity NFT) and `identityMakerSymbol` which is used to mint validity tokens within the script, (these are used to validate the amount of time a UTxO has existed for).

## Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc please do so over on the main [here in this repository](https://github.com/ADADAO/YoctoDao-Development).

For more interactive discussion, you can join the [₳DAO Discord](https://discord.gg/QgxwsRrjb4).

Thanks!
