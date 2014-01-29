hitweb
======

  First, hitweb was a project to learn Yesod and also used Data.Git.Diff (Hit).
  But as it works well for local projects I decided to open source it. I hope, It may help
  developpers/students to manage easily their projects.

Features
========

  * Hitweb provides usual functionalities like: git summary, log, commit, diff, exploring the sources (Trees and blobs)
  * Identification and authorization:
    * create the file .hitweb.authorized and add users authorized to access this repo
    * if no file, this is a plublic repo (or if the presence of the world "anybody" in the authorized file)
  * update the path where your repository is in the file config/settings.yaml

  * Planned features:
    * code review
    
Contributions
=============

  * send me a pull-request
