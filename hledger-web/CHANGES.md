## 2014/2/10 hledger-web 0.22.2

New:

* web: new option `--static-root` to set the base url for static files

Improved:

* web: include unminified source of all javascript to help packagers (fixes #161)
* web: work around clang-related build failures with OS X mavericks/XCode 5
* web: allow blaze-html 0.7 (closes #159)
