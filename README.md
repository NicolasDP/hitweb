HitWeb
======

  Hitweb is a light git project viewer. It provides a simple interface to see
  logs, commits and diff. This is a Read-Only website: it means that this is
  not the purpose of Hitweb to add, push, merge or re-write commits.

Features
========

  * Hitweb provides usual functionalities like: git summary, log, commit, diff, exploring the sources (Trees and blobs)
  * Identification and authorization
    * create the file hitweb.authorized and add users authorized to access this repo
    * add "anybody" in this file will require authentified users (no public access)
    * if no file, this is a public repo
  * update the path where your repositories are in the file config/settings.yaml

  * Planned features:
    * code review

Contributions
=============

  * send me a pull-request
