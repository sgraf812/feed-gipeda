# feed-gipeda [![Build Status](https://travis-ci.org/sgraf812/feed-gipeda.svg?branch=master)](https://travis-ci.org/sgraf812/feed-gipeda)

A daemon to continuously run [gipeda](https://hackage.haskell.org/package/gipeda) on a list of pre-configured repositories after having run a benchmark script on each (repo, commit) pair it cannot find a benchmark results file for.

Use this when you host a variety of repositories and want to display performance data about them. Although the defaults are clearly biased in towards of Haskell repositories using `cabal`/`stack`, the script to run on each new commit can be configured.

## I think I could use this but I haven't understood a word

This is a high-level example setup of how to use it:

![High-level architecture visualization](https://cdn.rawgit.com/sgraf812/feed-gipeda/master/docs/images/feed-gipeda.svg)

Below there is a detailed visualization of the FRP layer (of the master node):

![master architecture visualization](https://cdn.rawgit.com/sgraf812/feed-gipeda/master/docs/images/master.svg)

`feed-gipeda` extracts repositories to watch from a \*.yaml file (c.f. Config file), which is continuously watched for changes. For each remote repository, a unique but human-readable directory (`<base-name>-<hash-of-URI>`) is created, under which a mirror repository resides in the `repository/` folder.

All repositories are re-fetched at a configurable interval (`--dt`) and every commit for which there is no \*.csv result file in `site/out/results` generated yet causes a benchmark script to be run. The concrete example above uses [`cloben`](https://github.com/sgraf812/cloben) for that (it's also the default), but any other benchmark script which expects to be executed in the project folder and which writes its CSV output to `stdout` should do.

Either after a benchmark finishes or a repository was fetched, gipeda is executed for both new commits to benchmark and for producing the website to be deployed via `rsync` to a remote location (`--rsync` flag).

## How to get it to run

After having installed `gipeda`, the following steps should get you started (using `cabal` in a similar way):
```
$ git clone https://github.com/sgraf812/cloben
$ cd cloben; stack install; cd ..
$ git clone https://github.com/sgraf812/feed-gipeda
$ cd feed-gipeda; stack install; cd ..
```

Using `cloben` is of course optional, but the simplest option for benchmarking (if you want to benchmark `cabal`-conformant Haskell repositories).

By now, `$ feed-gipeda --help` should spit out maybe more up-to-date documentation than this README and you can read about supplying custom paths to various files such as the configuration file. For the sake of the example, we will use the default location, which is at `~/.feed-gipeda/feed-gipeda.yaml`. Put this into that file:
```
repositories:
- https://github.com/sgraf812/benchmark-test
```
That file will be watched for changes, so you can make it part of a git repository (like I did [here](https://github.com/sgraf812/.feed-gipeda)) and fetch updates as part of a `cron` job.

Now find yourself a quiet little tmp folder and run
```
$ feed-gipeda --help
...
$ feed-gipeda --gipeda <path/to/gipeda/executable>
```
You could also add the `gipeda` executable to your `$PATH` and `feed-gipeda` works with the defaults.

## Config file

Contains the list of repositories to watch encoded in a YAML file. Is supplied via the `--config` option, which defaults to `~/.feed-gipeda/feed-gipeda.yaml` (resp. `%APPDATA%/Roaming/.feed-gipeda/feed-gipeda.yaml` on Windows).

For an example, see <https://github.com/sgraf812/.feed-gipeda> or the example 2 line file above.
The file must contain a top-level `repositories` mapping to a list of repository URIs.
