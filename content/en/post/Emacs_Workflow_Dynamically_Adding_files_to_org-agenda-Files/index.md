---
title: "Emacs Workflow: Dynamically Adding Files to Org Agenda"
author: ["Armin Darvish"]
date: 2023-05-22T10:32:00-07:00
lastmod: 2023-09-08T05:55:42-07:00
categories: ["software", "emacs", "orgmode"]
draft: false
weight: 3002
subtitle: "How to automatically and dynamically build org-agenda-files to include any files with TODO items."
summary: "In this post, I'll show you my workflow for automatically adding files with TODO items to org-agenda-files as soon as we open or save the file."
authors:
  - admin
projects: ["software"]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
---

## UPDATE Sept. 8 2023 {#update-sept-dot-8-2023}

Since I wrote this post, Nicolas Graves has made a GitHub repository that implements this with some improvements using `org-ql` and etc. You can check it out here: [org-dynamic-agenda](https://github.com/nicolas-graves/org-dynamic-agenda).


## Intro {#intro}

If you use Emacs org-mode for task management, you have probably wondered if there is a way to dynamically add files with TODO items to org-agenda-files. A Google search will likely get you some initial ideas on how to do it. For example this post: [Boris Buliga - Task management with org-roam Vol. 5: Dynamic and fast agenda](https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html) covers a good approach, but since Boris's post focuses on using org-roam, it may not be the right solution when you don't use org-roam. So I had to hack my own solution and in this point I'm going to share that with you in case anyone is interested. You can see a [screenshot](en/post/Emacs_Workflow_Dynamically_Adding_files_to_org-agenda-Files/dynamic_org_agenda.gif) below.


## How does it work {#how-does-it-work}

For the main functionalities, I am using [Org Element API](https://orgmode.org/worg/dev/org-element-api.html) to parse org-mode buffers and find org TODO items. If there is a `TODO` item in the buffer, and it is visiting a file, I add the file to `org-agenda-files`. Additionally, I make sure that `org-agenda-files` is remembered between different Emacs sessions, I add `org-agenda-files` to `savehist-additional-variables`.

Then I define custom functions for and add them as hooks to org-mode to update `org-agenda-files` when an org-mode file is opened as well as when an org-mode file is saved. The redundancy helps make sure that nothing is lost if there is a crash.


## The code {#the-code}

-   Check if the file contains a `TODO` item:

    Use `org-element-map` and `org-element-parse-buffer` to walk the buffer, find all headlines and return true if there is any headline that is a `TODO` item. Note that I am looking for any heading that is a `TODO` item. Alternatively you can check for specific `TODO` types by looking at `:todo-keyword` instead of `:todo-type` e.g. `(org-element-property :todo-keyword h)...`

<!--listend-->

```emacs-lisp
(defun ad/agenda-file-p ()
    (org-element-map
       (org-element-parse-buffer 'headline)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match))
```

-   make a custom function to update `org-agenda-files` if the current org-mode file contains a `TODO` item:

If the current buffer contains a `TODO` item, I use seq-difference to find out if the files are already in org-agenda-files. If it does not contain `TODO` item, I make sure to remove it from `org-agenda-files`. This is important because when I'm done with a `TODO` item and remove it from the file, I would want the file to be removed from `org-agenda-files`.

```emacs-lisp
(defun ad/org-agenda-update-files (&rest ARG)
  ;; check if this is an org file buffer
  (interactive)
    (when (and (derived-mode-p 'org-mode) (buffer-file-name))
      (message "updating org-agenda-files...")
      ;; if there is an active TODO task, add this file to agenda files
      (if (ad/agenda-file-p)
      (add-to-list 'org-agenda-files (file-truename (buffer-file-name)))
      ;; if there is no active TODO task, remove the file from agenda files if needed
      (setq org-agenda-files (seq-difference org-agenda-files (list (buffer-file-name))))
      (customize-save-variable 'org-agenda-files org-agenda-files)
      ))
      )
```

-   cleaning up `org-agenda-files` and remove files that don't exist anymore.

When I delete some files, I want to make sure it gets removed from `org-agenda-files`.

```emacs-lisp
(defun ad/org-agenda-cleanup-files (&rest ARG)
  (interactive)
  (let ((temp/org-agenda-files org-agenda-files))
  (dolist (file org-agenda-files)
  (if (not (file-exists-p file))
      (setq temp/org-agenda-files (seq-difference temp/org-agenda-files (list file))))
    ())
  (setq org-agenda-files temp/org-agenda-files))
  )
```

-   Adding hooks

To get my functions to run automatically, I add `hooks` to `org-mode`. I make `lambda` functions that are added as hooks to `find-file-hook` and `before-save-hook` to make sure that `org-agenda-files` gets updated whenever I open an org-mode file and then again when I save the file.

```emacs-lisp
;; Add or remove individual file
(add-hook 'org-mode-hook (lambda () (add-hook 'find-file-hook #'ad/org-agenda-update-files)))
(add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook #'ad/org-agenda-update-files)))
```

-   Adding advice to functions that use `org-agenda-files` to show `TODO` items.

Before I run `org-agenda` or `dashboard-get-agenda`, or any other function that reads `org-agenda-files` to show my `TODO` items, I need to make sure to remove non-existing files from `org-agenda-files`.

```emacs-lisp
;; remove non-existing files before building agenda
(advice-add 'org-agenda :before #'ad/org-agenda-cleanup-files)
(advice-add 'org-todo-list :before #'ad/org-agenda-cleanup-files)
(advice-add 'dashboard-get-agenda :before #'ad/org-agenda-cleanup-files)
```

-   Make sure `org-agenda-files` is remembered between Emacs sessions.

I add `org-agenda-files` to `savehis-additional-variables` and make sure that `savehist-mode` is enabled.

```emacs-lisp
(add-to-list 'savehist-additional-variables 'org-agenda-files)
```


## Performance {#performance}

Of course adding files dynamically to the `org-agenda-files` comes with a cost. Make too many files with org-agenda items and it will be very slow. Personally I keep most of my general `TODO` items in one file under my main org directory and only put `TODO` items in other files when having it in the context of the specific project is useful.


## Screenshot {#screenshot}

Here is a screenshot showing it in action. I open an org file and a `TODO` item in it and save it and as you can see once I refresh the buffer describing `org-agenda-files` variable, the new file is added to the list. I also show that once I remove the `TODO` item or mark it as DONE, it is automatically removed from the list. Also, you can see that once I have the new file in the org-agenda list, it automatically shows up on my dashboard.

{{< figure src="/ox-hugo/dynamics_org_agenda.gif" width="800px" height="nil px" >}}
