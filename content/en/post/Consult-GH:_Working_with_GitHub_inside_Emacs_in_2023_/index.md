---
title: "Consult-GH: Working with GitHub inside Emacs in 2023"
author: ["Armin Darvish"]
lastmod: 2023-06-12T18:31:01-07:00
draft: true
weight: 3008
subtitle: "Introducing a new package for using GitHub interactively inside Emacs based on Consult and Embark"
summary: "This post introduces my new Emacs package that uses the fabulous Consult and Embrak along with GitHub CLI tool to create an intuitive user-friendly interface for GitHub inside Emacs"
authors:
  - admin
projects: [software]
tage:
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
---

## Introduction or some food for thought {#introduction-or-some-food-for-thought}


### What is the need? Why make a new package? {#what-is-the-need-why-make-a-new-package}

I often find myself going back to the same github pages for various reasons. Sometimes I go back to a package repo to read the documentation/manuals for installing or troubleshooting. Other times I browse the repositories by users whose works are of interest to me (for example [Steve Purcell's Emacs Config](https://github.com/purcell/emacs.d) or [alphapapa (Adam Porter)](https://github.com/alphapapa)'s packages for org-mode workflows). This is often done by doing a Google search finding the relevant GitHub Page and navigating from there to find the right content. It would have been great if I could do all of that from inside Emacs especially if I could then take some code from those repositories, paste it in an org-mode source block and see how it works. But, as far as I know, no emacs package provides a user-friendly intuitive interface for such functionality. While there are multiple packages that interface with github api like [gh.el](https://github.com/sigma/gh.el), [magit/ghub](https://github.com/magit/ghub), [magit/forge](https://github.com/magit/forge), [git-link](https://github.com/sshaw/git-link), [browse-at-remote](https://github.com/rmuslimov/browse-at-remote) and ... None of them provide the kind of functionality I am looking for. Let's take a closer look at some of these to better understand why there is a need for something better. On one end of the spectrum, we have a package like [gh.el](https://github.com/sigma/gh.el) which seems to be an interesting option with a huge code base possibly covering a lot of low-level API calls to GitHub but there is not a single line of documentation on how to use it! The repo seems to be abandoned years ago, and the owner mentions on one of the issues that **"the whole thing was a gigantic experiment around the viability of eieio, and I'm pretty disappointed by the amount of suffering it generates, so I'm not sure I should push people to use it..."**  On the other end of the spectrum we have packages like git-link and browse-at-remote that are relatively small (&lt;1000 lines of code) but the functionality is very limited, getting the URL links for files and commits, etc. While these are very useful, especially because they cover sources other than GitHub too, they don't really provide a way to browse repositories or issues. Then there is of course magit packages, that are indeed incredibly useful. magit/forge for example provides a great interface with issues and pull requests on multiple repository sources. However, this is geared toward repositories you actively work on and not so much toward browsing any random repository. Let's also not forget these packages were started several years ago, and since then there has been updates in Emacs as well as other tools like GitHub CLI that provide new opportunities for how we can interface with GitHub from inside Emacs. This is exactly what Consult-GH is trying to do.


### What is the right way to address the need? {#what-is-the-right-way-to-address-the-need}

The principle idea behind consult-gh is that there is already tools that provide the basic functionalities for us and we can leave the heavy lifting part of the job to those and only provide the parts that are missing. For example, the GitHub CLI is the right tool to use for interfacing with GitHub. By leaving jobs such as authentication and api calls to `gh` command tools we simplify the code and make it really easy to setup and use. In addition we make the code more secure since we don't have to handle authentication tokens and secure information and at the same we keep it more maintainable because we don't have to worry about keeping up to date with the api. The official GitHub CLI will ensure that for us. For comparison, if you setup magit/forge and go through all the steps you need to do to interface with GitHub, you'll see while leaving this job to a tool like `gh` might be a better approach. On the other hand, `completing-read` inside emacs provides a perfect tool for interfacing with users and runing queries, etc. and in the recent years, there has been great improvements especially with a package like [consult](https://github.com/minad/consult) that wraps aeound completing read and provides easy to use functionalities. Along with the rest of completion framework packages ([vertico](https://github.com/minad/vertico), [embark](https://github.com/oantolin/embark), [orderless](https://github.com/oantolin/orderless), ...) it provides a great toolset for functions that need input form users. Therefore by bringing `consult` and `gh` together, we can easily provide a concise yet powerfool tool that improves on the current alternatives for interacting with GitHub inside Emacs. That said, let's keep in mind that the goal here is not replace the existing functionalities with yet another tool but fill the gaps between the current tools. For example, consult-gh does not provide (at least not yet) a way to edit issues or pull requests because that functionality is available in magit/forge for not just github but also other sources so I don't see any benefit in yet another tool for it.

we should leave the main jobs to functionalities and allows achieving great functionality with a minimal code base (currently less than a 1000 lines of code).   behind consult-gh that makes it what it is:

However,the workflows are often highly customized and specific, and naturally it is difficult to come up with a one-size-fits-all solution and practically speaking going back to the browser becomes the natural choice again.

As shown above, Consult-GH tries to balance between usability (a.k.a. having many commands) and utility (being practically more efficient for custom work flows) by providing **only a handful of interactive commands** to keep things simple, intuitive and and easy to use, but offering a range of customization to tweak the behavior so that the user can pick what is most useful in their common work flows. For example, `consult-gh-search-repos` simply returns possible matches for the search term and allows the user to customize the default action when a repository is selected. Whether that is opening the url in a browser, or viewing the README in an emacs buffer, or cloning the repository, it's up to the user to pick what is the default action. If the user needs more than one action, then they can use the [Embark](https://github.com/oantolin/embark) integration.

In this section I'll showcase some interesting examples that I find useful and efficient enough that is now my first choice for interacting with GitHub as opposed to using a browser.


## Can you show me examples on how this is useful? {#can-you-show-me-examples-on-how-this-is-useful}


### Following the users I like {#following-the-users-i-like}

There are a few GitHub accounts that I tend to keep going back to both for  finding new interesting work and also for learning from their code and approach. Before making Consult-GH My work flow was to do a goole search for example for "emacs operating system" or "steve purcell emacs config" and then finding the github page and browsing through recent updates and changes, etc. This is not very efficient because syntax highlighting inside a browser is sub par and there is no help/docs inside a browser, but if this is done inside Emacs, helps and documentations are just a "C-h key" away and subsections of the code can be evaluated for example inside an org-mode source block to probe the behavior and functionalities. Of course the question is how do you get [Steve Purcell's Emacs Config](https://github.com/purcell/emacs.d/) inside emacs without going to the browser. You can of course keep a local copy by cloning the repository and then pulling the latest updates every time you want to browse it, but that would waste some space on your local drive and often times you might not be interested in the whole repo anyway. In other cases, you may want to see people's new work, e.g. new repositories, and without knowing the name and links of the repo, you still have to go to the browser to find those. Consult-GH provides an easy way to see the repositories from people you want to follow and quickly browse single files or clone entire repositories.

As seen in the screenshot below, the command `consult-gh-orgs` allows you to search GitHub users and see their repositories. Importantly, you can also search for multiple users at the same time. By default your search history is saved and accounts but only the search terms that produce any results will be stored in `consult-gh--known-orgs-list`. As  a result you will have quick access to user accounts you have looked up before, and if you want to preserve that between emacs sessions, you can turn `savehist-mode` on and add `consult-gh--known-orgs-list` to `savehist-additional-variables`:

```emacs-lisp
(add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
```


### Features and Examples {#features-and-examples}

More often than looking for specific users, I have to look up specific packages (e.g. org-roam, lsp-mode, ...) to check manudals and documentation. This would have been done with Google and browing GitHub before Consult-GH, but now I prefer consult-gh because it is much faster and more efficient for my work flows. There are important features that make this more efficient and I am going to showcase some of the important ones for a typical work flow below. I will use an example and will show you configurations and screenshots of how you cna do each.

Let's say I want to look up a repo (such as org-roam, or ox-hugo) but I don't remember the name of the user/org on GitHub, so instead of looking up the user, I run the interactive command `consult-gh-search-repos` and search for them. In case of `org-roam` the first result is `"org-roam/org-roam"`. From here I can run a range of different functions depending on what I want to do. Here are some examples:


#### previewing a single repo {#previewing-a-single-repo}

By default, previews are off because previewing a repo requires downloading the README file and that can be slow depending on your environment and network speed, etc. But in my config, I have turned consult-gh's previews on. I have also set the preview-mode to `'org-mode`. By default this is set to `'markdown` to preserve the contents since most README files are `.md` format but I trun it to org-mode because this makes it much easier for me to see source-blocks, etc. and evaluate them inside Emacs.

```emacs-lisp
(consult-gh-show-preview t)
(consult-gh-preview-buffer-mode 'org-mode)
```

With this setting, when I move the cursor to `"org-roam/org-roam"` in minibuffer I see the README in org format in a preview buffer and I can and read the contents, evaluate source blocks, etc. without leaving Emacs.


#### Seeing the files contents of a repo {#seeing-the-files-contents-of-a-repo}

Now if I hit enter to select this repo, then the default action function that is bound to the variable `consult-gh-repo-action` is run. By thedefault this is bound to the function `consult-gh--repo-browse-url-action` which opens the github page in the browser. This is again to optimize the performance, but if performance is not a concern, you can change the default function to something more useful for your work flow. Personally, I have changed it to `consult-gh--repo-browse-files-action` which allows me to browse the files inside Emacs. Here is my config

```emacs-lisp
(consult-gh-repo-action #'consult-gh--repo-browse-files-action)
```

Note that this only fetches the file tree and does not download the files until you open the file (or a preview buffer). This way, you will only download the contents you want to see. Also, the files are stored in the system's default temporary directory so once you restart your computer, the space that is used to store the files is restored. Moreover, in a resource-limited environment where performance is of concern, you can turn the preview on but enable it on demand by setting the `consult-gh-preview-key` to a key binding like the code below

```emacs-lisp
(setq consult-gh-show-preview t)
(setq consult-gh-preview-key "M-o")
```

The code above turns the preview on for consult-gh but sets the preview key to `"M-o"`, so you can see the preview of a file or repo when you hit `"M-o"`. This is likely the ideal setup for both performance and usability.


#### Additional commands to run on a repo such as cloning or forking {#additional-commands-to-run-on-a-repo-such-as-cloning-or-forking}

In addition to seeing the file contents, you may want to run other commands such cloning a repo or copying the url link to kill-ring or ... These commands are provided as separate functions that can also be bound to the default repo action or alternatively can be used by the consult-gh-embark integration. The consult-gh-embrak.el provides some examples of how to set embark up for this. In the screen shot below you can see me cloning a repository by using embark. You can also define other custom functions and bound them to the embark keymap following the same patterns as those provided.

There are some additional functions provided for example to view the issues or to copy relevant links (homepage, https or ssh links for cloning, and and org-mode link) to kill-ring. In addition I provide an example that copies a drop-in snippet with `straight.el` and `use-package` to kill ring. I can paste this snippet in my emacs config to install the package! As you can see, this is very flexible and you can append the embark actions following the built-in examples provided to create any other custom work flow you need.


#### Commands to run on files {#commands-to-run-on-files}

Similarly to what was mentioned for repos, you can also run different functions on files. By default consult-gh opens the file url on github when you select it, but you can configure it to open the file in emacs in the appropriate major mode. This makes an API call and downloads the file contents, so it may be slow for large files but in my tests so far the speed has been great even for some large files. Here is a configuration to will set consult-gh to open files inside emacs.

```emacs-lisp
(consult-gh-file-action #'consult-gh--files-view-action)
```

As mentioned above, previewing a file will also fetch the contents and therefore you may want to limit previews to on-demand only by setting a preview key.

Similar to what I said for repos you can use other funcitons such as `consult-gh--files-save-file-action`  to save a file locally as opposed to opening it in a temporary buffer. This might be especially of interest if you are selecting multiple files (see below).


#### Working with multiple repos or files {#working-with-multiple-repos-or-files}

Moreover the integration with embark allows some more complex functionalities such as working on multiple repos or files. Let's say I want to look at all the repos by alphapapa and clone a few of them that are related to org-mode. Now since alphapapa has over 700 repositories in his account, I first need to increase the maximum number of results I normally get:

```emacs-lisp
(setq consult-gh--repo-maxnum 1000)
```

Then I can run `consult-gh-orgs` and look for `alphapapa`. This takes some time to load all the repositories but this is way faster than browsing through GitHub to find the repositories manually. Then in the result I search for `"org-"` and run embark select (in my case this is bound to SPC so in my case this is `"C-. SPC"`) and ocne I select everything I am
looking for, I run embark act on all ("C-. A") plus the consult-gh-embark-repo-clone (bound to "c" in my setup). By default, consult-gh wil lgo through every repository and confirms the repo I want to clone and the path, etc. But if you happen to do this often, you can configure consult-gh to always clone the repositories in a folder where you kep your repositories (e.g. "~/code" or "~/projects") and then set `consult-gh-confirm-before-clone` to nil:

```emacs-lisp
(setq consult-gh-default-clone-directory "~/code")
(setq consult-gh-confirm-before-clone nil)
```

With this setting consult-gh just clones the repositories with the repo's default name under `"~/code"` folder. Browsing GitHub in the browser or using gh in the command line will never be this fast!

Similarly you can use embark-select and embark-act-all to run commands on multiple files possibly even from different repos or from different branches of the same repo.

For example in the screenshot below I download the README files from two different repos vertico from minad and selectrum from radian software accounts.

One interesting example is to run a diff on the same file in two different branches of the same repo withouc cloning the repo and runing git diff. In the screen shot below, I search for the repo "minad/consult" then I select it twice (separated by my default crm-separator in this case ",") and select two different branches, then I see the file trees for the two branches. I search for a file (e.g. "README")that narrow downs the items in both branches and I select the and I open previews for both branches by hiting "M-o". Now I have both files in an open buffer. I quit consult-gh and run vdiff-buffer and select the two buffers visiting those files. Of course you can achieve this by cloning the repo and running git diff but this is much faster especially if you don't want to clone the entire repo.


#### Viewing Issues {#viewing-issues}

In addition to repos and files, you can also browse issues with `consult-gh-search-issues`. You then need to select a repo or possibly multiple repos and enter a search term. If you want to see all the issues you can pass an empty string to the search term. You can set the `consult-gh--issues-state-to-show` to `open`, `closed` or `all` to change the issues that are shown. Note that this is different from the behavior of `gh search issues` in the command line where you only have the option `open` or `closed`. Similar to what was mentioned for repos, and files you can then run different actions on issues. By default the action is set to `consult-gh--issue-browse-url-action` that opens the url in a browser but you can also change that to `consult-gh--issue-view-action` to see the issue inside an emacs buffer.


## Future Ideas {#future-ideas}

There are a few other ideas that i have in mind and I may implement them in the future as time allows and would welcome anybody that would like to contribute to these or potentially other interesting ideas:

1.  I would like to provide a simple interface for multiple profiles. This should be easily achievable using enviroment variable like GH-CONFIG-DIR, but I still need to decide what would be the right way of implementing it. Should be a global variable in consult-gh or shall we dfine local pe buffer variables, etc.

2.  I think we can improve the current interface of `consult-gh-find-file` to make it behave more like find-file in emacs where folders are expanded dynamically as the user makes step-by-step selections to navigate the folders. This is however not the best approach for looking at files in multiple repos, so I may have to keep the current aproach for multiple repos, but I'd love to hear suggestions on that. I briefly talked to minad (the owner of the consult repo) and it seems that there are good ways of implementing a dynamic completion table for file-names with consult. But so far between my limited elisp skills and lack of good in-detail documentation on emacs completion tables, I have only had partial success with it and there are still many quirks to deal with. So this may take some time to implement.

3.  The `gh` command in the terminal provides additional arguments to further filter search results for example to filter issues by date or tags, etc. This is not yet implemented in consult-gh. I think it can be added by some minimal effort if I enable extra arguments in some of the calls to gh (that I can of course turn into a rabbit hole that takes weeks but I think it'd be doable at the end anyway). However, I am not yet convinced that this will add much value. I think having too many options can negatively affect the user-friendliness and efficiency of the package. There is always going to be some use-cases where jumping to the browser would be a better option after all. I'd be interested to hear counter arguments if any though especially if there are ideas for clean and simple interface that allows using such options with an intuitive interface.

That converts markdown-files to 'org-mode
you can trun previews on if I select that, it will open the GitHub page in the browser, but in my setting, I have changed the default action for repos to `#'consult-gh--repo-browse-files-action` that means when I select a repo, a new minibuffer menu opens and allows me to browse file contents of the repo. Furthermore I can customize consult-gh to open the "HEAD" branch, ask me to select the branch, or confirm whether I want to open "HEAD" or select another branch (This last option is useful for browsing multiple repos, see below).  So with this setting, I have quick access to files in any repositories from within Emacs.

quick way to evaluate parts of the code and understand the behavior. Also no help available in a browser

For an example I would search for
