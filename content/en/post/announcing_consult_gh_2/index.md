---
title: "Announcing Consult-GH 2.0"
author: ["Armin Darvish"]
date: 2023-10-09T23:55:00+02:00
lastmod: 2023-10-09T23:55:06+02:00
draft: false
weight: 3008
subtitle: "now using async commands by utilizing consult-async"
summary: "In this post I am announcing a major update to consult-gh and will showcase some of the main improvements."
authors:
  - admin
projects: [software]
categories: []
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
---

If you follow my posts, you probably have seen my previous post [introducing consult-gh](https://www.armindarvish.com/en/post/consult-gh_working_with_github_inside_emacs_in_2023_/). Since I wrote that post, there were requests for some features (e.g. integration with [magit/forge](https://github.com/magit/forge)) that are all now implemented, and while I was working on that I started playing around with consult's [async search feature](https://github.com/minad/consult#asynchronous-search) and realized that this indeed enables a much more powerful interface for consult-gh. Therefore, I redesigned the whole package and reimplemented most functions from scratch. The results is now merged on the main branch of [consult-gh](https://github.com/armindarvish/consult-gh), and in this post I am going to show you some of the main improvements.


## async search {#async-search}

As noted on the comments on [this reddit post](https://www.reddit.com/r/emacsporn/comments/14p42xq/comment/jqii09k/?context=3), the previous interface for searching was subpar. It required the user to enter a search term, hit return and then see the results. If the results was not what the user wanted, he had to rerun the M-x command  or key-binding and start form scratch. If the user made a typo, he would see a message saying no results were found without knowing that a typo was made. Using consult-async solves all of that. It dynamically updates the completion table as the user is typing.
The screenshot below shows the async search and dynamic updates in action:

{{< figure src="/ox-hugo/consult-gh-search-repos.gif" width="800px" height="nilpx" >}}


## passing command line arguments {#passing-command-line-arguments}

Another advantage of using consult-async machienry is that the user can also pass other command-line arguments from within the emacs minibuffer. In case of consult-gh, this would allow the user to adjust the queries on the fly. For example the user can simply pass a command-line argument by adding `-- --limit {number}` to the minibuffer to change the maximum number of results fetched.

Note that the variables for setting limits (such as `consult-gh-repo-maxnum` and `consult-gh-issue-maxnum`) are still available to set the values globally but, per individual commands, the user can change the limit dynamically when needed. Other command line arguments (such as sort, ...) can also be passed similarly in the minibuffer. This also allows using the whole machinery of `gh`, especially [gh's search syntax](https://docs.github.com/en/search-github/getting-started-with-searching-on-github/understanding-the-search-syntax) to further filter the results.
For example, the screenshot below shows how to search issues and then use the extra command line arguments to filter issues to only show the ones assigned to yourself:

{{< figure src="/ox-hugo/consult-gh-search-issues.gif" width="800px" height="nilpx" >}}


## searching codes {#searching-codes}

In addition, consult-gh now allows searching codes from emacs minibuffer. This is far superior to using `gh` command line tool because not only it allows to preview/open the file containing the code, but also jumps to the code snippet in the file. Here is a screenshot of searching code in action:

{{< figure src="/ox-hugo/consult-gh-search-code.gif" width="800px" height="nilpx" >}}


## Integration with magit/forge {#integration-with-magit-forge}

Consult-gh now supports integration with [magit/forge](https://github.com/magit/forge) for viewing/editing issues and pull requests as well. This can simplym be done by loading `consult-gh-forge` and setting the actions for issues and pull requests accordingly. Here is a minimal configuration setting using [straight.el](https://github.com/radian-software/straight.el):

```emacs-lisp
(use-package consult-gh
  :straight (consult-gh :type git :host github :repo "armindarvish/consult-gh")
  :after forge
  :config
  (require 'consult-gh-embark)
  (require 'consult-gh-forge)
  (setq consult-gh-preview-key "M-o")
  (setq consult-gh-issue-action #'consult-gh-forge--issue-view-action) ;;this enables opening issues in magit/forge buffer
  (setq consult-gh-pr-action #'consult-gh-forge--pr-view-action) ;;this enables opening pull requests in magit/forge buffer
  (setq consult-gh-forge-timeout-seconds 20) ;;maximum time in seconds for consult-gh to try opening issues/prs in forge, and then reverts back to opening in normal emacs buffers.
)
```


## Transient menu {#transient-menu}

In addition, consult-gh now offers a transient menu to quickly find the relevant commands or change settings. It can be accessed by running `M-x consult-gh` after loading consult-gh-transient module. Just make sure that the transient package is installed before loading consult-gh-transient. Here is a minimal setup script:

```emacs-lisp
(use-package consult-gh
  :straight (consult-gh :type git :host github :repo "armindarvish/consult-gh")
  :after forge transient
  :config
  (require 'consult-gh-embark)
  (require 'consult-gh-transient)
)
```

and here is a screenshot of the transient menu:

{{< figure src="/ox-hugo/consult-gh-transient.gif" width="800px" height="nilpx" >}}

This is still work in progress. I will likely add more features to this transient menu. So give it a try and let me know if you have suggestions in the comments below or on the github repos here: [consult-gh](https://github.com/armindarvish/consult-gh).
