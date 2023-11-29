# Contributing

Thank you very much for your interest in this project! We welcome contributions from anyone, given that they follow the few rules below.

A great first step is to join our
[![Discord](https://img.shields.io/badge/Discord-100000?style=flat&logo=Discord&logoColor=C3C3C3&labelColor=4179DA&color=010101)][discord] server

## Pull Request Process

- Feel free to open a draft PR to first discuss the high level changes you intend to make before going deep into the implementation
- Make focused changes on a dedicated branch. Smaller code diff are easier to review, and that way you will get earlier feedback from the maintainers.
- Do not bother with the changelog and version number of this project. Both will be addressed in a final PR from the maintainers just before a release.
- A PR can contain several commits; they don't need to be squashed, but they must have an expressive enough title and have a meaning on their own.
- When you get an approval after a PR review:
  - If you are a Tweager, you can press the Green Button
  - Otherwise, the maintainers team will promptly merge the PR

## Changelog editing

Just before a release, the changelog needs to be edited by the maintainer team. To make the process easier, it is recommended to use [github-changelog-generator](https://github.com/github-changelog-generator/github-changelog-generator) that is available in `nixpkgs`:

```bash
$ nix-shell -p github-changelog-generator
```

Then go on [this page](https://github.com/settings/tokens) and generate a personal access token with the following permissions:

+ `public_repo`
+ `repo:status`
+ `repo_deployment`

Then create a config file `.github_changelog_generator`:

```text
issues=true
future-release=v?.?.?  # eg: v0.1.1
since-tag=v?.?.?       # eg: v0.1.0
user=tweag
project=linear-base
token=<your personal access token>
output=CHANGELOG-gen.md
```

and run

```bash
[nix-shell]$ github_changelog_generator
```

You'll end with a file named `CHANGELOG-gen.md` that will list all the PRs and issues merged/closed since the last release. You then need to sort these entries into the following categories (most recent PR at the top of each category):

```markdown
### Breaking changes

### New additions

### Code improvements / Bug fixing

### CI/Tooling improvements

### Documentation improvements
```

- Only the closed issues that does not match a PR 1:1 needs to appear on this listing (e.g. a `tag:bug` issue that has been fixed by an unrelated PR)
- It is recommended to edit the PR titles in the listing, and add a concise description for the most important ones (or when a single line of text is not enough to describe the changes).
- You might group PRs that are related to the same feature (or when one PR is overridden by a following one)
- In case of doubt, use the changelog for `v0.2.0` as a reference of how to edit the changelog

[discord]: https://discord.com/invite/7yg5GxzvDJ
