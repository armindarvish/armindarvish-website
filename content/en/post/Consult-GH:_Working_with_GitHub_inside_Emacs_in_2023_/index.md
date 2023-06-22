---
title: "Consult-GH: Working with GitHub inside Emacs in 2023"
author: ["Armin Darvish"]
lastmod: 2023-06-21T18:11:00-07:00
draft: true
weight: 3005
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

## Introduction {#introduction}

This section is essentially the philosophy behind this work addressing the question of "why I am doing this". If this is not of interest to you, you can skip to the next section and look at the examples and screenshots, etc.


### What is the need? Why make a new package? {#what-is-the-need-why-make-a-new-package}

I often find myself going back to the same github pages for various reasons. Sometimes I go back to a package repo to read the documentation/manuals for installing or troubleshooting. Other times I browse the repositories by users whose works are of interest to me (for example [Steve Purcell's Emacs Config](https://github.com/purcell/emacs.d) or [alphapapa (Adam Porter)](https://github.com/alphapapa)'s packages for org-mode workflows). This is often done by doing a Google search finding the relevant GitHub Page and navigating from there to find the right content. It would have been great if I could do all of that from inside Emacs especially if I could then take some code from those repositories, paste it in an org-mode source block and see how it works. But, as far as I know, no emacs package provides a user-friendly intuitive interface for such functionality. While there are multiple packages that interface with github api like [gh.el](https://github.com/sigma/gh.el), [magit/ghub](https://github.com/magit/ghub), [magit/forge](https://github.com/magit/forge), [git-link](https://github.com/sshaw/git-link), [browse-at-remote](https://github.com/rmuslimov/browse-at-remote) and ..., none of them provide the kind of functionality I am looking for.

Let's take a closer look at some of these to better understand why there is a need for something better. On one end of the spectrum, we have a package like [gh.el](https://github.com/sigma/gh.el) which seems to be an interesting option with a huge code base possibly covering a lot of low-level API calls to GitHub but there is not a single line of documentation on how to use it! The repo seems to be abandoned years ago, and the owner mentions on one of the issues that **"the whole thing was a gigantic experiment around the viability of eieio, and I'm pretty disappointed by the amount of suffering it generates, so I'm not sure I should push people to use it..."**. This is clearly not the right solution then to use in 2023! On the other end of the spectrum, we have packages like git-link and browse-at-remote that are relatively small (&lt;1000 lines of code), but the functionality is very limited, getting the URL links for files and commits, etc. While these are very useful, especially because they cover sources other than GitHub too, they don't really provide a way to browse repositories or issues or search for new content. Then there is of course magit packages, that are indeed incredibly useful for doing `git` commands from inside Emacs and it provides extended functionality in extra packages such as magit/forge to interface with issues and pull requests on multiple repository sources. However, this is geared toward repositories you actively work on and not so much toward browsing any random repository. In other words, magit/forge does not provide simple interactive commands to find repositories or browse their content on demand. In addition, all of these packages were started several years ago, and since then there has been updates in Emacs as well as other tools like GitHub CLI that provide new opportunities for how we can interface with GitHub from inside Emacs. This is exactly what Consult-GH is trying to do.


### What is the right way to address the need without duplicating other works? {#what-is-the-right-way-to-address-the-need-without-duplicating-other-works}

The principle idea behind consult-gh is that there is already tools that provide the basic functionalities for us and we can leave the heavy lifting part of the job to those and only provide the parts that are missing. For example, the [GitHub CLI](https://github.com/cli/cli) (`gh` commands in the terminal) is the right tool to use for interfacing with GitHub. By leaving jobs such as authentication and API calls to `gh`, we simplify the code and make it really easy to set up and use. In addition, we make the code more secure since we don't have to handle authentication tokens and at the same we keep it more maintainable because we don't have to worry about keeping up to date with the API. For comparison, if you set up magit/forge and go through all the steps you need to do to interface with GitHub, you'll see while leaving this job to a tool like `gh` might be a better approach. On the other hand, `completing-read` inside emacs provides a perfect tool for interfacing with users and running queries, etc. and in the recent years, there has been great improvements especially with a package like [consult](https://github.com/minad/consult) that wraps around completing read and provides easy to use features. Along with the rest of completion framework packages ([vertico](https://github.com/minad/vertico), [embark](https://github.com/oantolin/embark), [orderless](https://github.com/oantolin/orderless), ...), it provides a great toolset for functions that need input form users. Therefore by bringing `consult` and `gh` together, we can easily provide a concise, yet powerful tool that improves on the current alternatives for interacting with GitHub inside Emacs. That said, let's keep in mind that the goal here is not replace the existing functionalities with yet another tool but fill the gaps between the current tools. For example, consult-gh does not provide (at least not yet) a way to edit issues or pull requests because that functionality is available in magit/forge for not just GitHub but also other sources so I don't see any benefit in duplicating that in Consult-GH unless somebody can convince me otherwise.

Another important factor to consider is that when it comes to interacting with repositories, there is a vast range of actions and commands, and different people would use it in different ways and that means we need customizable tools that can mold to the users' desires. Naturally, it is difficult to come up with a one-size-fits-all solution and as a result a lot of tools that try to do this get bloated or fail to provide benefits compared to going back to the browser and therefore they don't stick. Emacs, however is a general tool with small packages that cover specific tasks very well and typical Emacs users often don't mind spending some time to thinker with configurations and build their own custom workflows. Therefore, inside Emacs we can build on available packages and tools that users will be using anyway and provide new functionalities with enough customizability that makes the overall experience better than "just the browser" without creating a completely new tool that is bloated and hard to learn from scratch.

This is what I am trying to create with Consult-GH. To do so, I am trying to balance between usability (a.k.a. having many commands for different use-cases and scenarios) and utility (being more efficient in practice for typical workflows than using a browser). To do so, I am providing **only a handful of interactive commands** to keep things simple, intuitive and easy to use, yet I am offering a range of customization to tweak the behavior so that the user can pick what is most useful in their day-to-day use. For example, the user can fetch a list of repositories matching a search term by running the command `consult-gh-search-repos`, but the action that happens after selecting a target repository is customizable. By default I provide a few useful functions for typical users, and allow advanced users to define their own custom commands if they wish to do so. If all the user wants is to open the url of a repository in the browser, that is provided by default. If they want to view the README in an emacs buffer, or see branches and browse the files they can do so by adding one line to their configuration. They can also choose to clone or fork the repository with one line of configuration. I also provide example of [Embark](https://github.com/oantolin/embark) actions that can be used to do multiple commands on multiple repositories, etc. This should cover most of the typical use-cases, but if the user still needs something more complex beyond those actions, they can write their own custom commands and set them as default action to run when a target repository is selected. In other words, typical users can get the typical functionalities they use on a daily basis in a browser inside Emacs, hence better efficiency and customization, etc. Advanced users, can come up with actions that are not possible in a browser and are hard to dynamically implement without a lot of scripting from scratch. Therefore, the balance between usability and utility should always be met no matter what the user is looking for.

In this section I'll showcase some interesting examples that I find useful and efficient enough that is now my first choice for interacting with GitHub as opposed to using a browser.


## Practical Examples of How this Package Can be Useful {#practical-examples-of-how-this-package-can-be-useful}

If you read the Intro, you may be thinking, that is a lot of vague ideas that sound interesting on paper but in reality how would you even be using this. Would be even useful or is it just another shiny tool that looks nice but does not provide any meaningful new feature or functionality. So in this section I am going to provide examples of use cases and put them in context before we get down to codes and screen shots and how-to instructions in the next part. Again, if this is not of interest t you and you just want to see the examples in action and screenshots, jump ahead and see the next parts.

There are a few GitHub accounts that I tend to keep going back to either because I am looking for some new tool or functionality I need or because I am trying to learn form their code, implementation and general approach. Before making Consult-GH, my workflow for this was:

1.  <span class="underline">Find the Source</span>: Do a Google search (for example to find "steve purcell emacs config") or open a bookmark (for example the GitHub repo of [emacs operating system](https://github.com/dakrone/eos))
2.  <span class="underline">Find the relevant content</span>: For example by browsing through the files on a GitHub repo or looking at recent updates, commits, etc.
3.  <span class="underline">Understand the Content</span>: This would usually require, copying code from the browser back to a REPL or IDE and interact with it and run examples to figure out what each part does and etc.
4.  <span class="underline">Implement my own solution</span>: Finally, I have to decide if I would like to use the content I just learn in my own projects. For example, after looking at "Steve Purcell's Config" I need to decide whether I want to use some parts of it in my own config and if so how do i need to change it.

This is not very efficient because the tools I was using were not specifically designed for this. For example, Google is a great "general" search engine but if I want to go back to the same page on eos GitHub repo then, I probably have to bookmark that page. But as you may have experienced that would mean that I end up with many bookmarks and now I need another tool to search through my bookmarks to find the page and sometimes I don't remember which bookmark was the right one and I perhaps have to go back and look at my notes if I took any and so on.
Moreover, when I finally find the source and content of interest, I am likely looking at some code in a browser, where syntax highlighting is often sub-optimal; no IDE or REPL is available to run and explore the code. Help and documentation is not available to parse different pieces of the code. Compare this with looking at code inside Emacs, where you can easily run snippets of codes to test and understand functionalities either by jumping to a REPL or by running code inside an org-mode source-block. In addition, at least in case of elisp code, help and documentation is always just a `C-h` key away! Such documentation is often proven extremely useful especially if you you use a package such as [helpful](https://github.com/Wilfred/helpful). For non-elisp code, the chances are you can also find documentation, debugging and other tools inside Emacs by installing the right packages. Finally, when it comes to implementing my own solution, this would mean either copying code from the browser into a local file or forking/cloning the repository and then editing the code which means I would leave the browser and would go to an IDE tool such as Emacs. Wouldn't it be great if I could do all of that inside Emacs to begin with? For example, let's say I am looking at [Steve Purcell's Emacs Config](https://github.com/purcell/emacs.d/) but instead of doing that in the browser, I can pull up the relevant repository and browse the files all from inside the emacs. If I see a piece of code I am interested in I can copy it to and org-mode source block and see what it does. If I need help and docs I can look things up in Emacs. I can do all of that without even cloning the repo. For another example, consider the scenario where I want to look at recent works from [alphapapa](https://github.com/alphapapa), see what new projects and repositories he has been working on. I can open his GitHub page in the browser and manually go over his more than 700 repos top find out, but wouldn't it be great if I could get a list of all his repos within Emacs, and search through them and interactively see README files and if I see something of interest browse the files or clone the repo and take things for a test drive all from within Emacs? If your answer to this question is yes or any of this rings a bell, then you will likely be interested in Consult-GH and what I am about to show you in the next sections.


## That's enough talking, just show me the real examples of features and use cases {#that-s-enough-talking-just-show-me-the-real-examples-of-features-and-use-cases}

In this section, I will show you (of course with screenshots as well as explanation) examples of using Consult-GH to interact with GitHub from within Emacs. The goal is to show the basic features and explain how things work and then provide examples of meaningful workflows that are more efficiently done with Consult-GH than opening a browser or using other tools. Please note that I do not intend to go through all the details of installation, customization, configuration etc. here. Instead, I focus on showing the functionality. For instructions please refer to the documentation on the official [consult-gh repo](https://github.com/armindarvish/consult-gh).


### Searching for Users/Organization {#searching-for-users-organization}

In the previous section I mentioned an example of looking at [alphapapa](https://github.com/alphapapa)'s repositories. Let's see how that would work with Consult-GH. As you will see in the screen shot below, you can run the interactive command `consult-gh-orgs`, enter a GitHub unsername and see a list of their repositories. One nice feature is that you can also enter multiple user names by using a crm-separator. and see all their repositories at the same time.

Then depending on configuration (see below), you can do a number of different actions:

1.  You can choose to see a preview (a.k.a. the README file inside an emacs buffer), as you navigate the repositories by turning the preview on:

<!--listend-->

```emacs-lisp
(setq consult-gh-show-preview t)
```

1.  You can change the function that runs when you select a repo. In the screenshot below I have set the action to show the file contents after asking me to chose a branch.
    ```emacs-lisp
          (setq consult-gh-repo-action #'consult-gh--repo-browse-files-action)
    ```
    You can change this to other built-in functions that are provided. In addition, you can use alternative actions by using the [embark](https://github.com/oantolin/embark) actions provided. For example you can get different types of links to the repo such url of the repo, or https or ssh links for cloning with `git` or an org-mode link for inserting in your notes. You can also run an action to fork the repo in your own GitHub account or clone the repo on your local machine. Note that you can set any of these functions as the default action if that makes mroe sense for your workflow. Personally, I have set the default action to browse the file contents as shown in the screenshot below and do the alternative actions whenever I need through embark integration.


### Searching for Repositories {#searching-for-repositories}

Now the above example started with a known username but what if you don't remember the username. What if you are looking for a package because you want to see the documentation. Let's say I want to look at ox-hugo's documentation for [my blogging workflow](https://www.armindarvish.com/en/post/building_an_efficient_blogging_workflow_in_emacs/). In this case, you can use the interactive command `consult-gh-search-repos` and enter a search term. As you can see in the screen shot below, once I search for ox-hugo I get a number of hits and then I can choose the one I am looking for. If there are multiple hits and you don't know which one is the one you are looking for, you cna always turn the preview on and look at the README. In the example below I turn the preview on but bind the preview key to `M-o`, so I can look at the preview on demand rather than automatically loading the preview.

```emacs-lisp
(setq consult-gh-show-preview t)
(setq consult-gh-preview-key "M-o")
```

Similar to the example above, once you have the repo you are interested in you can run different actions per your configuration. Since we already saw how browsing the file contents work in the previous example, in the screenshot below, I am using a different default action to open the repo url in the browser.

```emacs-lisp
(setq consult-gh-repo-action #'consult-gh--repo-browse-url-action)
```

In addition, I show searching with multiple search terms. This is of course of interest when you are not sure what exact terms to search for.


### Searching issues {#searching-issues}

Another interesting example is to search and view issues. This is especially of interest because normally if you go to GitHub in the browser and search for some general term, you will find many hits in the issue section with limited narrow down functionalities. But here you can search for some general terms, and then use the search in the minibuffer to narrow down to the issue of interest. One disadvantage might be that listing thousands of issues with consult-gh-search-issues can be slow inside emacs, so you may want to avoid very general search terms, but at the same time such general search won't be useful in the browser either because you get a huge list of hits that you can browse only 10-50 at a time! Of course a Google search may give you better results in this case, especially because in such a scenario you may be interested in discussions on websites other than GitHub such as StackOverflow or Reddit, ... But my hope is that pretty much in any scenario where you will be searching issues on GitHub specifically, using Consult-GH can potentially provide a better experience.

browse contents (see different branches and the files) and directly open them inside an emacs buffer or if you wish you can clone or fork the repo. You caneither open the repository urls in the browser or open

inside emacs without going to the browser. You can of course keep a local copy by cloning the repository and then pulling the latest updates every time you want to browse it, but that would waste some space on your local drive and often times you might not be interested in the whole repo anyway. In other cases, you may want to see people's new work, e.g. new repositories, and without knowing the name and links of the repo, you still have to go to the browser to find those. Consult-GH provides an easy way to see the repositories from people you want to follow and quickly browse single files or clone entire repositories.

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
