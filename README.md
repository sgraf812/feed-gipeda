# feed-gipeda [![Build Status](https://travis-ci.org/sgraf812/feed-gipeda.svg?branch=master)](https://travis-ci.org/sgraf812/feed-gipeda)

A kitchen-sink tool to run [gipeda](https://hackage.haskell.org/package/gipeda) on a list of repositories after having run a benchmark script on each (repo, commit) pair that is yet to be benchmarked.

This will take all the necessary steps to update and deploy a `gipeda`-based site.

Use this when you host a variety of repositories and want to display performance data about them. Although the defaults are biased towards Haskell projects using `cabal`/`stack`, the script to run on each new commit can be configured.

## I think I could use this but I haven't understood a word

This is a high-level example setup of how to use it:

![High-level architecture visualization](https://cdn.rawgit.com/sgraf812/feed-gipeda/master/docs/images/feed-gipeda.svg)

Below there is a detailed visualization of the FRP layer (of the master node):

![master architecture visualization](https://cdn.rawgit.com/sgraf812/feed-gipeda/master/docs/images/master.svg)

Without any special flags, `feed-gipeda` will enter the build once mode (and then exit).

`feed-gipeda` extracts repositories to watch from a \*.yaml file (c.f. Config file). For each remote repository, a unique but human-readable directory (`<base-name>-<hash-of-URI>`) is created, under which a mirror repository resides in the `repository/` folder.

Every commit for which `gipeda` requests a benchmark (e.g. because there is no \*.csv result file in `site/out/results`) causes a benchmark script to be run. The concrete example above uses [`cloben`](https://github.com/sgraf812/cloben) for that (it's also the default), but any other benchmark script which expects to be executed in the project folder and which writes its CSV output to `stdout` should do.

Either after a benchmark finishes or a repository was fetched, `gipeda` is executed for both new commits to benchmark and for producing the website to be deployed via `rsync` to a remote location (`--deploy-to` flag).

## `--watch` mode

When `gipeda` requests no more benchmarks, `feed-gipeda` exits.

This can be changed
via the `--watch=SECONDS` option. If specified, `feed-gipeda` will enter a daemon mode
and not exit if there is temporary nothing to benchmark. Instead, it will watch for
changes to the config file (handle updates to the watched repository list) and refetch
all currently watched repositories every `SECONDS` seconds.

## `--check` mode

With the `--check` flag supplied, `feed-gipeda` will check the config file
(c.f. Config file, `--config`) for syntax errors and report them as an error.
It will successfully exit when it can find none.

## How to get it to run

The following steps should get you all Haskell dependencies (using `cabal` in a similar way):

```
$ stack install cloben gipeda feed-gipeda
```

You will also need to install the [system dependencies for gipeda](https://github.com/nomeata/gipeda#setting-it-up) (sorry, fellow Windows users!), as well as there must be `rsync` and `ssh` available on the path, if
you choose to `--deploy-to` some location.

Using `cloben` is of course optional, but the simplest option for benchmarking (if you want to benchmark `cabal`-conformant Haskell repositories).

By now, `$ feed-gipeda --help` should spit out maybe more up-to-date documentation than this README and you can read about supplying custom paths to various files such as the configuration file. For the sake of the example, we will use the default location, which is at `~/.feed-gipeda/feed-gipeda.yaml`. Put this into that file:

```
repositories:
- https://github.com/sgraf812/benchmark-test
```

In `--watch` mode, that file would be watched for changes, so you can make it part of a git repository (like I did [here](https://github.com/sgraf812/.feed-gipeda)) and fetch updates as part of a `cron` job.

Alternatively, you can run `feed-gipeda` in the default mode (build once) any time you need updates.

Now find yourself a quiet little tmp folder and run
```
$ feed-gipeda --help
...
$ feed-gipeda --deploy-to=some/local/path/if/you/want
```

For some more usage examples, see the Cookbook section.

## Config file

Contains the list of repositories to watch encoded in a YAML file. Is supplied via the `--config` option, which defaults to `~/.feed-gipeda/feed-gipeda.yaml` (resp. `%APPDATA%/Roaming/.feed-gipeda/feed-gipeda.yaml` on Windows).

For an example, see <https://github.com/sgraf812/.feed-gipeda> or the example 2 line file above.
The file must contain a top-level `repositories` mapping to a list of repository URIs.

## Cookbook

- `feed-gipeda`  
Enter default mode (Exit when done). Read config from the default location (system-specific, see `--config` option under `--help`), act as both a master and a slave node.
- `feed-gipeda --watch=5`  
Enter watch mode. Watch for changes to config and re-fetch repositories every 5 seconds. Read config from the default location, act as both master and slave node
- `feed-gipeda --check --config=~/.feed-gipeda/feed-gipeda.yaml`  
Enter check mode. Check the specified config file for syntax errors. Useful in a CI setting.
- `feed-gipeda --master=localhost:12345`  
Enter default mode. Dispatch benchmark requests on registered slave nodes, don't work on them in this process
- `feed-gipeda --slave=localhost:12346`  
Enter slave-only mode. Listen via multicast for master nodes which request you to do benchmarks.
- `feed-gipeda --deploy-to=deploymentDir/`  
Enter default mode. Deploy changes via `rsync` to the local `deploymentDir`
