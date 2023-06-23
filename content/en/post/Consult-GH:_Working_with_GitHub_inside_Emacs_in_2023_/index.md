---
title: "consult-gh: Working with GitHub inside Emacs in 2023"
author: ["Armin Darvish"]
date: 2023-06-23T11:39:00-07:00
lastmod: 2023-06-23T11:39:30-07:00
draft: false
weight: 3004
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

This section is essentially the philosophy behind this work addressing the question of "why do we need yet another package?" If this is not of interest to you, you can skip to the next section and look at the examples and screen shots, etc.


### What is the need? Why make a new package? {#what-is-the-need-why-make-a-new-package}

I often find myself browsing GitHub for various reasons. Sometimes I go back to a package repo to read the documentation/manuals for installing or troubleshooting. Other times I browse the repositories by users whose works are of interest to me (for example [Steve Purcell's Emacs Config](https://github.com/purcell/emacs.d) or [alphapapa (Adam Porter)](https://github.com/alphapapa)'s packages for org-mode workflows). This is often done by doing a Google search finding the relevant GitHub Page and navigating from there to find the right content. It would have been great if I could do all of that from inside Emacs especially if I could then take some code from those repositories, paste it in an org-mode source block or a REPL and see how it works. But, as far as I know, no emacs package provides a user-friendly intuitive interface for such functionality. While there are multiple packages that interface with GitHub API like [gh.el](https://github.com/sigma/gh.el), [magit/ghub](https://github.com/magit/ghub), [magit/forge](https://github.com/magit/forge), [git-link](https://github.com/sshaw/git-link), [browse-at-remote](https://github.com/rmuslimov/browse-at-remote) and ..., none of them provide the kind of functionality I am looking for.

Let's take a closer look at some of these to better understand where the gap is.

On one end of the spectrum, we have a package like [gh.el](https://github.com/sigma/gh.el) which seems to be an interesting option with a huge code base possibly covering a lot of low-level API calls to GitHub but there is not a single line of documentation on how to use it! The repo seems to be abandoned years ago, and the owner mentions on one of the issues that **"the whole thing was a gigantic experiment around the viability of eieio, and I'm pretty disappointed by the amount of suffering it generates, so I'm not sure I should push people to use it..."**. This is clearly not the right solution to use in 2023!

On the other end of the spectrum, we have packages like git-link and browse-at-remote that are relatively small (&lt;1000 lines of code), but the functionality is very limited, getting the URL links for files and commits, etc. While these are very useful, especially because they cover sources other than GitHub too, they don't really provide a way to browse repositories or issues or search for new content.

Then there is of course magit packages, that are indeed incredibly useful for doing `git` commands from inside Emacs and it provides extended functionality in extra packages such as magit/forge to interface and work with issues and pull requests on multiple repository sources. However, this is geared toward repositories you actively work on and not so much toward browsing/finding new content. In other words, magit/forge does not provide simple interactive commands to find repositories or browse their content on demand without first cloning them or adding them to its database.

In addition, all of these packages were started several years ago, and since then there has been updates in Emacs as well as other tools like GitHub CLI that provide new opportunities for how we can interface with GitHub from inside Emacs. This is exactly what consult-gh is trying to do.


### How does consult-gh do it? {#how-does-consult-gh-do-it}

The principle idea in consult-gh is that I only focus on providing what is missing and leave the rest to existing tools that provide the basic functionalities. For example, the [GitHub CLI](https://github.com/cli/cli) (`gh` commands in the terminal) is the right tool to use for interfacing with GitHub. After all it is the official CLI tool. By leaving jobs such as authentication and API calls to `gh`, we simplify the code and make it really easy to set up and use. In addition, we make the code more secure since we don't have to handle authentication tokens and at the same time we keep the code more maintainable because we don't have to worry about keeping up to date with the API, etc.
For comparison, if you set up magit/forge and go through all the steps you need to do to interface with GitHub, you'll see while leaving this job to a tool like `gh` might be a better approach for consult-gh (obviously in case of magit/forge that's not an option since it supports sources other than GitHub).

On the other hand, `completing-read` inside emacs provides a perfect tool for interfacing with users and running queries, etc. and in the recent years, there has been great improvements especially with a package like [consult](https://github.com/minad/consult) that wraps around completing read and provides easy to use features. Along with the rest of completion framework packages ([vertico](https://github.com/minad/vertico), [embark](https://github.com/oantolin/embark), [orderless](https://github.com/oantolin/orderless), ...), they provide a great toolset for functions that need input form users. Therefore by bringing `consult` and `gh` together, we can easily provide a concise, yet powerful tool that improves on the current alternatives for interacting with GitHub inside Emacs.

That said, let's keep in mind that the goal here is not to replace the existing functionalities with yet another tool but fill the gaps between the current tools. For example, consult-gh does not provide (at least not yet) a way to edit issues or pull requests because that functionality is available in magit/forge for not just GitHub but also other sources so I don't see any benefit in duplicating that in consult-gh (unless of course somebody convinces me otherwise!)

Another important factor to consider is that when it comes to interacting with repositories, there is a vast range of actions and commands, and different people would use it in different ways and that means we need a customizable tool that can mold to the users' desires. Naturally, it is difficult to come up with a one-size-fits-all solution and as a result a lot of tools that try to do this get bloated or fail to provide benefits compared to going back to the browser and therefore they don't stick. Emacs, however is a general tool with small packages that cover specific tasks very well and typical Emacs users often don't mind spending some time to tinker with configurations and build their own custom workflows. Therefore, inside Emacs we can build on available packages and tools that users will be using anyway and provide new functionalities with enough customizability that makes the overall experience better than "just the browser" or "gh" without creating a completely new tool that is bloated and hard to learn from scratch.

This is what I am trying to create with consult-gh. To do so, I am trying to balance between usability (a.k.a. having many commands for different use-cases and scenarios) and utility (being more efficient in practice for typical workflows than using a browser for example). To do so, I am providing **only a handful of interactive commands** to keep things simple, intuitive and easy to use, yet I am offering a range of customization to tweak the behavior so that the user can pick what is most useful in their day-to-day use. For example, the user can fetch a list of repositories matching a search term by running the command `consult-gh-search-repos`, but the action that happens after selecting a target repository is customizable. By default I provide a few useful functions for typical users, and allow advanced users to define their own custom commands if they wish to do so. If all the user wants is to open the url of a repository in the browser, that is provided by default. If they want to view the README in an emacs buffer, or see branches and browse the files they can do so by adding one line to their configuration. They can also choose to clone or fork the repository with one line of configuration. I also provide examples of [Embark](https://github.com/oantolin/embark) actions that can be used to do multiple commands on repositories and more. This should cover most of the typical use-cases, but if the user still needs something more complex beyond those actions, they can write their own custom commands and set them as default action to run when a target repository is selected. In other words, typical users can get the typical functionalities they use on a daily basis in a browser inside Emacs, hence better efficiency and customization. Advanced users, can come up with actions that are not possible in a browser and are hard to dynamically implement without a lot of scripting from scratch. Therefore, the balance between usability and utility should always be met no matter what the user is looking for.


## How is Consult-GH Useful {#how-is-consult-gh-useful}

If you read the Intro, you may be thinking, that is a lot of vague ideas that sound interesting on paper but in reality what can consult-gh do that is better than using a browser or just `gh` itself. "Can you convince me that that this is not just another shiny tool that looks nice but does not provide any meaningful new feature or functionality?". So in this section I am going to provide examples of use cases with context before we get down to codes and screen shots and how-to instructions in the next part. Again, if this is not of interest to you and you just want to see the examples in action and screenshots, jump ahead and see the next parts, otherwise keep reading.

There are a few GitHub accounts that I tend to keep going back to either because I am looking for some new tool or functionality I need or because I am trying to learn form their code, implementation and general approach. Before making consult-gh, my workflow for this was:

1.  <span class="underline">Find the Source</span>: Do a Google search (for example to find "steve purcell emacs config") or open a bookmark (for example the GitHub repo of [emacs operating system](https://github.com/dakrone/eos))
2.  <span class="underline">Find the relevant content</span>: For example by browsing through the files on a GitHub repo or looking at recent updates, commits, etc.
3.  <span class="underline">Understand the Content</span>: This would usually require, copying code from the browser back to a REPL or IDE and interact with it and run examples to figure out what each part does and etc.
4.  <span class="underline">Implement my own solution</span>: Finally, I have to decide if I would like to use the content I just learned in my own projects. For example, after looking at "Steve Purcell's Config" I need to decide whether I want to use some parts of it in my own config and if so how do I need to adjust and use it.

This is not very efficient because the tools I was using were not specifically designed for this. For example, Google is a great "general" search engine but if I want to go back to the same page on **eos** GitHub repo then, I probably have to bookmark that page. But as you may have experienced that would mean that I end up with many bookmarks and now I need another tool to search through my bookmarks to find the right page and sometimes I don't remember which bookmark was the right one and I perhaps have to go back and look at my notes if I took any and so on.
Moreover, when I finally find the source and content of interest, I am likely looking at some code in a browser, where syntax highlighting is often sub-optimal; no IDE or REPL is available to run and explore the code; help and documentation is not available to parse different pieces of the code; etc. Compare this with looking at code inside Emacs, where you can easily run snippets of codes to test and understand functionalities either by jumping to a REPL or by running code inside an org-mode source-block. In addition, at least in case of elisp code, help and documentation is always just a `C-h` key away! Such documentation is often proven extremely useful especially if you you also use a package like [helpful](https://github.com/Wilfred/helpful). For non-elisp code, the chances are you can also find documentation, debugging and other tools inside Emacs by installing the right packages. Finally, when it comes to implementing my own solution, this would mean either copying code from the browser into a local file or forking/cloning the repository and then editing the code which means I would leave the browser and would go to an IDE tool such as Emacs.

Wouldn't it be great if I could do all of that inside Emacs to begin with? For example, let's say I am looking at [Steve Purcell's Emacs Config](https://github.com/purcell/emacs.d/) but instead of doing that in the browser, I can pull up the relevant repository and browse the files all from inside the emacs. If I see a piece of code I am interested in I can copy it to and org-mode source block and see what it does. If I need help and docs I can look things up in Emacs. I can do all of that without even cloning the repo. For another example, consider the scenario where I want to look at recent works from [alphapapa](https://github.com/alphapapa), see what new projects and repositories he has been working on. I can open his GitHub page in the browser and manually go over his more than 700 repos to find out, but wouldn't it be great if I could get a list of all his repos within Emacs, and search through them and interactively see README files? Then, if I see something of interest browse the files, or clone the repo and take things for a test drive all from within Emacs?

If your answer to those questions is yes or any of this rings a bell, then you will likely be interested in consult-gh and what I am about to show you in the next sections.


## Features and Demo {#features-and-demo}

In this section, I will show you examples of using consult-gh to interact with GitHub from within Emacs. The goal is to show the basic features and explain how things work and then provide examples of meaningful workflows that are more efficiently done with consult-gh than opening a browser or using other tools. Please note that I do not intend to go through all the details of installation, customization, configuration etc. here. Instead, I focus on showing the functionality. For instructions please refer to the documentation on the official [consult-gh](https://github.com/armindarvish/consult-gh) page.


### Searching Users/Organization {#searching-users-organization}

In the previous section I mentioned an example of looking at [alphapapa](https://github.com/alphapapa)'s repositories. Let's see how that would work with consult-gh. As you will see in the screen shot below, you can run the interactive command `consult-gh-orgs`, enter a GitHub unsername and see a list of their repositories. One nice feature is that you can also enter multiple user names by using a crm-separator. and see all their repositories at the same time grouped by the user name.

Then depending on configuration (see the next section for details), you can do a number of different actions. For example, you can choose to see a preview (a.k.a. the README file inside an emacs buffer), or see the file contents inside Emacs. In the screenshot below I have turned preview on and set the action to show the file contents. I'll show you how to do that in the next section but for now here is a screen shot of how it looks.

<video width="320" height="240" controls><source src="/assets/media/videos/test.mp4" type="video/mp4">
[[file:~/projects/armindarvish-website/assets/media/videos/test.mp4]]</video>

In the screenshot above, I first searched for the user `minad` and looked at the preview (a.k.a. **README** file) of some repos. As you can see I can copy code or even evaluate source blocks from the preview buffer without even cloning or saving files! Then I searched for multiple accounts using comma as crm-separator (e.g. `systemcrafters, minad`). Then I searched for `alphapap`. As you can see, it takes some time to load more than 700 repositories from alphapa, but then you can easily narrow down by completion in minibuffer as I did for repos that start with `org-` . Most of the time, users won't have that many repos and therefore performance is not an issue but if it is, you can always set the max number of results for repositories (per account not total) by setting `consult-gh-repo-maxnum`.


#### Looking at a Default List of Repositories {#looking-at-a-default-list-of-repositories}

You can also look at a default list of usernames to `consult-gh-default-orgs-list` and use `consult-gh-default-repos` to see their repositories. You can add any GitHub usernames to the list, but I use this to quickly open my own repos inside Emacs. Note that since we are using `gh` on the back-end, this can show private repositories of the account that is logged in as well.


### Searching Repositories {#searching-repositories}

More often than looking for specific users, I look up specific packages (e.g. org-roam, lsp-mode, ox-hugo, ...) to check manuals and documentation. Similar to what I mentioned above, before consult-gh, I would have used Google and a browser, but now I use most of it in consult-gh. Let's go through an example to see the functionalities.

Let's say I want to look at ox-hugo's documentation for [my blogging workflow](https://www.armindarvish.com/en/post/building_an_efficient_blogging_workflow_in_emacs/), or want to look at org-roam for my note-taking. In this case, I can use the interactive command `consult-gh-search-repos` and enter a search term.  When I search for `ox-hugo`, I get a number of hits and then I can choose the one I am looking for.

Now, let's go through some useful features and how to use them:


#### Previewing Repos {#previewing-repos}

By default, previews are off because previewing a repo requires downloading the README file and that can be slow depending on your environment and network speed, etc. But in my config, I have turned consult-gh's previews on. I have also set the preview-mode to `'org-mode`. By default this is set to `'markdown` to preserve the contents since most README files are `.md` format but I trun it to org-mode because this makes it much easier for me to see source-blocks, etc. and evaluate them inside Emacs.

```emacs-lisp
(consult-gh-show-preview t)
(consult-gh-preview-buffer-mode 'org-mode)
```

With this setting, when I move the cursor to `"kaushalmodi/ox-hugo"` in the minibuffer I see the README in org format in a preview buffer and I can read the contents. This is similar to visiting the homepage on GitHub except that I can also see codes with syntax highlighting, I can evaluate code in source blocks, and ... without leaving Emacs. Since loading previews can be slow, you may want to do this on demnad. You can do that by turning previews on and binding `consult-gh-preview-key` to a specific key. In the example below I turn the preview on and bind it to `M-o`.

```emacs-lisp
(setq consult-gh-show-preview t)
(setq consult-gh-preview-key "M-o")
```


#### The Default Action and Some Options {#the-default-action-and-some-options}

Now if I hit enter to select a repo, then the default action function that is bound to the variable `consult-gh-repo-action` is run. By default, this is bound to the function `consult-gh--repo-browse-url-action` which opens the github page in the browser. This is again to optimize the performance, but if performance is not a concern, you can change the default function to something more useful for your workflow. Personally, I have changed it to `consult-gh--repo-browse-files-action` which allows me to browse the files inside Emacs. Here is my config

```emacs-lisp
(consult-gh-repo-action #'consult-gh--repo-browse-files-action)
```

Note that this only fetches the file tree and does not download the files until you open the file (or preview it in a buffer). This way, you will only download the contents you want to see. Also, the files are stored in the system's default temporary directory so once you restart your computer, the space that is used to store the files is restored. Here is a screen shot where I search for the repo `minad/vertico` and then browse the file contents. In this case the preview is on and automatic.

There are other actions that are provided by default and you can use them as default action by setting `consult-gh-repo-action`. These actions include cloning or forking a repo, copying relevant links to kill-ring (e.g. homepage, https or ssh links for cloning, and and org-mode link). In addition I provide an example that copies a drop-in snippet with `straight.el` and `use-package` to kill ring. I can paste this snippet in my emacs config to install the package!


#### Embrak Integration {#embrak-integration}

In addition to the default action, you can use alternative actions by using [embark](https://github.com/oantolin/embark). The [consult-gh-embark.el](https://github.com/armindarvish/consult-gh/blob/main/consult-gh-embark.el) file provide some example of how to do this. Here is an example showing how to bounc `c` to embark action for cloning the repo:

```emacs-lisp
(defun consult-gh-embark-clone-repo (cand)
  "Clone the repo at point."
  (funcall (consult-gh--repo-clone-action) (get-text-property 0 :repo cand)))

(defvar-keymap consult-gh-embark-actions-map
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "c" #'consult-gh-embark-clone-repo
)

(add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-actions-map))
```

With the embark integration, you can have a quick way for doing alternative actions on items (repos, files, issues, etc.)

In the screen shot below you can see me cloning a repository by using embark. You can also define other custom functions and bound them to the embark keymap following the same patterns as those provided.


### Finding Files {#finding-files}

The next example is using `consult-gh-find-file`. This is a quicker way to see files if you already know the name of the repository. For example you can enter `minad/vertico` and browse the contents. Similarly to what was mentioned for repos, you can also run different functions on files. By default consult-gh opens the file url on github when you select it, but you can configure it to open the file in emacs. This makes an API call and downloads the file contents, so it may be slow for large files but in my tests so far the speed has been great even for some large files.

Here is a configuration to set consult-gh to open files inside emacs.

```emacs-lisp
(consult-gh-file-action #'consult-gh--files-view-action)
```

When you set the function to browsing files, consult-gh asks you to select a branch by default but you can change the setting (see documentation on [consult-gh-default-branch-to-load](https://github.com/armindarvish/consult-gh#consult-gh-default-branch-to-load) variable).

As mentioned above, previewing a file will also fetch the contents and therefore you may want to limit previews to on-demand only by setting a preview key.


### Searching Issues {#searching-issues}

In addition to repos and files, you can also browse issues with `consult-gh-search-issues`. You first asked enter a search term for issues (and you cna pass an empty string if you want to see all issues of specific repos), then you are asked to enter name of repos (in `user/repo` format for example `armindarvish/consult-gh`). You can pass an empty string to this and consult-gh will search for issues in any repo. You can set the `consult-gh--issues-state-to-show` to `open`, `closed` or `all` to change the issues that are shown. Similar to what was mentioned for repos, and files you can then run different actions on issues. By default the action is set to `consult-gh--issue-browse-url-action` that opens the url in a browser but you can also change that to `consult-gh--issue-view-action` to see the issue inside an emacs buffer.

Here is a screen shot


### Taking it to the Next Level {#taking-it-to-the-next-level}

The examples above show uses that are common for many users on a daily basis. But there are a lot more you can potentially do by using consult-gh.


#### Working with Multiple Repos All at Once {#working-with-multiple-repos-all-at-once}

Moreover the integration with embark allows some more complex functionalities such as working on multiple repos or files. Let's say I want to look at all the repos by alphapapa and clone a few of them that are related to org-mode. Now since alphapapa has over 700 repositories in his account, I first need to increase the maximum number of results I normally get:

```emacs-lisp
(setq consult-gh--repo-maxnum 1000)
```

Then I can run `consult-gh-orgs` and look for `alphapapa`. This takes some time to load all the repositories but this is way faster than browsing through GitHub to find the repositories manually. Then in the result I search for `"org-"` and run embark select (bound to `SPC` by default) and once I select everything, I run embark act on all (bound to `A`) plus the consult-gh-embark-repo-clone (bound to `c` in my setup). By default, consult-gh wil go through every repository and confirms the repo I want to clone and the path, etc. But if you happen to do this often, you can configure consult-gh to always clone the repositories in a folder where you keep your repositories (e.g. "~/code" or "~/projects") and then set `consult-gh-confirm-before-clone` to nil:

```emacs-lisp
(setq consult-gh-default-clone-directory "~/code")
(setq consult-gh-confirm-before-clone nil)
```

With this setting consult-gh just clones the repositories with the repo's default name under `"~/code"` folder. Browsing GitHub in the browser or using `gh` in the command line will never be this fast!


#### Actions on Multiple Files (e.g. Comparing Files) {#actions-on-multiple-files--e-dot-g-dot-comparing-files}

In addition to viewing files, you can also select multiple files by using `embark-select` and run a function on multiple files, for example if you want to download a selection of files without cloning the whole repository. In addition you can use a crm-separator and search for multiple repos (or the same repo multiple times and select different branches) and then compare files (for example with `ediff` or [emacs-vdiff](https://github.com/justbur/emacs-vdiff)). This can be done without cloning the repo or even permanently saving the file on your local machine (by default consult-gh loads previews by downloading files in system temp directory).

The screenshot below shows some interesting use cases.

-   Previewing files in a repo
-   Running vdiff on files in different branches of the same repo.
-   Comparing files across repositories (for example to compare LICENSEs or compare between different forks, ...)

Similarly you can use embark-select and embark-act-all to run commands on multiple files possibly even from different repos or from different branches of the same repo.

One interesting example is to run a diff on the same file in two different branches of the same repo without cloning the repo and runing git diff. In the screen shot below, I search for the repo `minad/consult` then I select it twice (separated by my default crm-separator in this case `,`) and select two different branches, then I see the file trees for the two branches. I search for a file (e.g. "README")that narrow downs the items in both branches and I select them and I open previews for both branches by hiting "M-o". Now I have both files in an open buffer. I quit consult-gh and run vdiff-buffer and select the two buffers visiting those files. Of course you can achieve this by cloning the repo and running git diff but this is much faster especially if you don't want to clone the entire repo.

For example, let's say you want to clone a bunch of different repos. Instead of navigating to GitHub pages and getting the links and running "git clone" commands, you can just search multiple accounts with `consult-gh-orgs` or multiple repos with `consult-gh-search-repos` using `consult-gh-crm-separator` and then clone all of them at once. Note that for this scenario you may want to turn the confirmation off and set a default path for cloning as shown in the code below.

```emacs-lisp
(setq consult-gh-default-clone-directory "~/code")
(setq consult-gh-confirm-before-clone nil)
```


#### Extending Functionality with Custom Functions {#extending-functionality-with-custom-functions}

Advanced users who don't mind writing a few lines of code to build custom workflows can further expand on the built-in functionalities by mixing different functions and actions. For example, let's say you want to add the same file (e.g. a new LICENSE) to multiple repos. You can create a custom function that clones the repo, adds a file, commits the changes and pushes to the remote. With the right packages already installed, this will only be a few line of codes in elisp. Then you make an embark action similar to examples provided in [consult-gh-embark.el](https://github.com/armindarvish/consult-gh/blob/main/consult-gh-embark.el). Now you can search GitHub, select any repo (or multiple ones) and run this command on them! Isn't that neat?

In fact let me just try to write the code for that  specific example with the simplest implementation right now:

```emacs-lisp
(defun consult-gh-clone-and-add-file (repo file &optional clonedir)
  (let* ((clonedir (or "~/tmp/code" clonedir))
        (reponame (car (last (split-string repo "\/"))))
        (filename (file-name-nondirectory file))
        (newname (expand-file-name filename (expand-file-name reponame clonedir)))
        )
  (consult-gh--repo-clone repo clonedir reponame)
  (copy-file file newname t)
  (expand-file-name reponame clonedir)
 ))

(defun consult-gh-embark-clone-and-add-file (cand)
  "Clone the repo at point."
  (let* ((repo (get-text-property 0 :repo cand))
        (file (read-file-name "Select File: "))
        (repodir (consult-gh-clone-and-add-file repo file))
        (default-directory repodir)
        )
    (shell-command "git add .")
    (shell-command (concat "git commit -m \"" file " added\""))
    (shell-command "git push origin main"
  )))

(defvar-keymap consult-gh-embark-actions-map
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "v" #'consult-gh-embark-clone-and-add-file
)

(add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-actions-map))
```

That is 30 lines of code (and could possibly be optimized since I just wrote it in 5 minutes) that allows me to do something quite complex. Of course in Emacs, there might be other better ways to achieve this specific task, but this shows you how you can quickly build custom workflows with consult-gh.

browse contents (see different branches and the files) and directly open them inside an emacs buffer or if you wish you can clone or fork the repo. You caneither open the repository urls in the browser or open

inside emacs without going to the browser. You can of course keep a local copy by cloning the repository and then pulling the latest updates every time you want to browse it, but that would waste some space on your local drive and often times you might not be interested in the whole repo anyway. In other cases, you may want to see people's new work, e.g. new repositories, and without knowing the name and links of the repo, you still have to go to the browser to find those. consult-gh provides an easy way to see the repositories from people you want to follow and quickly browse single files or clone entire repositories.

As seen in the screenshot below, the command `consult-gh-orgs` allows you to search GitHub users and see their repositories. Importantly, you can also search for multiple users at the same time. By default your search history is saved and accounts but only the search terms that produce any results will be stored in `consult-gh--known-orgs-list`. As  a result you will have quick access to user accounts you have looked up before, and if you want to preserve that between emacs sessions, you can turn `savehist-mode` on and add `consult-gh--known-orgs-list` to `savehist-additional-variables`:

```emacs-lisp
(add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
```


## Future Ideas {#future-ideas}

There are a few other ideas that i have in mind and I may implement them in the future as time allows and would welcome anybody that would like to contribute to these or potentially other interesting ideas:

1.  I would like to provide a simple interface for multiple profiles. This should be easily achievable using enviroment variable like GH-CONFIG-DIR, but I still need to decide what would be the right way of implementing it. Should be a global variable in consult-gh or shall we dfine local pe buffer variables, etc.

2.  I think we can improve the current interface of `consult-gh-find-file` to make it behave more like find-file in emacs where folders are expanded dynamically as the user makes step-by-step selections to navigate the folders. This is however not the best approach for looking at files in multiple repos, so I may have to keep the current aproach for multiple repos, but I'd love to hear suggestions on that. I briefly talked to minad (the owner of the consult repo) and it seems that there are good ways of implementing a dynamic completion table for file-names with consult. But so far between my limited elisp skills and lack of good in-detail documentation on emacs completion tables, I have only had partial success with it and there are still many quirks to deal with. So this may take some time to implement.

3.  The `gh` command in the terminal provides additional arguments to further filter search results for example to filter issues by date or tags, etc. This is not yet implemented in consult-gh. I think it can be added by some minimal effort if I enable extra arguments in some of the calls to gh (that I can of course turn into a rabbit hole that takes weeks but I think it'd be doable at the end anyway). However, I am not yet convinced that this will add much value. I think having too many options can negatively affect the user-friendliness and efficiency of the package. There is always going to be some use-cases where jumping to the browser would be a better option after all. I'd be interested to hear counter arguments if any though especially if there are ideas for clean and simple interface that allows using such options with an intuitive interface.
