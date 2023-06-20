---
title: "Building the Best Blog Editor with Emacs Org-Mode in 2023?"
author: ["Armin Darvish"]
date: 2023-06-20T01:23:00-07:00
lastmod: 2023-06-20T01:26:24-07:00
draft: false
weight: 3003
subtitle: "How to use org-mode and ox-hugo along with org-todo items and org-agenda to manage your blog posts"
summary: "In this post, I will show you my custom workflow in Emacs for writing blog posts. I use org-mode for writing the text and ox-hugo for exporting to markdown files. The post focuses on how to create a custom workflow starting with capturing the initial idea with org-capture, then using org-todo and org-agenda to track the post as you take it from idea to an initial draft through different stages of editing to finally publishing it on your website."
authors:
  - admin
projects: ["software"]
categories: ["software", "emacs", "org mode", "blogging"]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
---

In the previous post, I showed you how to automatically add files to your org-agenda. In this post I am going to expand on that and show you how to use `org-todo` items beyond tasks and todo lists and build custom workflows. As an example, I am going to show you my blogging workflow. This is what I am using right now to write this post, and in my opinion, it showcases the true power of Emacs and org-mode. If you want to know why Emacs is the one of the best tools for writing blogs in 2023, read on.

But before we get down to the point, let me make one thing clear: This is not a post about the basics of Emacs or org-mode and won't be giving you any configurations for writing text in Emacs either (although I will share some tips and tools as needed), instead it focuses on creating efficient workflows with automation using org-mode features that make Emacs superior to other available tools. Specifically I will show you:

1.  Dynamic org-capture templates using Yasnippet snippets
2.  Defining functions that are auto-triggered when an org-todo keyword is changed.
3.  Some tips on custom org-agenda views to only look at items in a specific workflow
4.  Extra tips for improving the look and feel and creating an intuitive user-friendly experience in org-mode

If you don't know much about Emacs or org-mode, or all you are looking for is a list of packages and settings that are useful for wiring non-programming text in Emacs, then I suggest to start elsewhere (You can find lots of good resources here:  [thinkhuman/writingwithemacs: Tips, Examples, and Resources for Writing with Emacs](https://github.com/thinkhuman/writingwithemacs)) and come back to this post later, when you feel the need to take your workflow to the next level.

Also, note that while I am using a blog post as an example here, this general approach can be applied to many other scenarios, for example a bunch of flash cards you want to memorize; a selection of books you want to read, digest or summarize; a list of companies you want to apply for a job at; a bunch of projects you want to work on; and so on.


## Using `org-todo` keywords {#using-org-todo-keywords}

By default [Emacs Org mode](https://orgmode.org/) comes with two `org-todo` keywords `"TODO"` and `"DONE"` but you can add any custom keyword and use them to track different objects and category of items such as notes, flashcards, ideas, drafts, etc. You can do this by adding the labels to `org-todo-keywords` in a sequence following [Workflow states (The Org Manual)](https://orgmode.org/manual/Workflow-states.html). For example for a blogging workflow I use `(sequence "DRAFT(d)" "POST(p)" "|" "PUBLISH(b)")` as shown below. Note that by putting **PUBLISH** after "|", we are setting **PUBLISH** as the done label for this workflow, which can be used for other useful functions such as logging time, etc. (see the section on automating workflow [below](#automating-the-workflow) for how I use it to automate logging times).

```emacs-lisp
(add-to-list 'org-todo-keywords '(sequence "DRAFT(d)" "POST(p)" "|" "PUBLISH(b)"))
```

Now, we can label org headings as a blog post by adding **DRAFT** todo keyword. This will serve as an entry point to capture ideas for a blog and add it to my agenda to edit later. As I edit the post and decide to publish it on my website, I'll change the label to **POST** or **PUBLISH**. I am going to show you how to use this to create some automated workflow but before that I am going to introduce another tool, Ox-Hugo, that makes it easy to write blog posts in org-mode if you use the static site generator, [Hugo](https://gohugo.io/).


## Ox-Hugo {#ox-hugo}

Next, we look at [Ox-Hugo](https://github.com/jethrokuan/ox-hugo) that allows you to export texts written in org-mode to markdown files that are used by the [Hugo](https://gohugo.io/) static generator. If you use Hugo to build your blog, and want to write your posts in org-mode (which you should!), then [Ox-Hugo](https://github.com/jethrokuan/ox-hugo) is a natural choice. There are other options such as `org-publish` as well, but I think Ox-Hugo is a better approach.

I recommend you go through Ox-Hugo's [manual](https://ox-hugo.scripter.co/) for setting it up and find the specific settings that fit your need. But here are some ideas and tips to consider:

You can organize your blog posts either in separate files (one post per file) or all in one file under different org headings (one post per heading). Personally I am using one post per heading right now because I like to see all my posts in the same file next to each other. But if you prefer having the blog posts in different files, then you may want to consider my previous post [to dynamically add files to org-agendas](https://www.armindarvish.com/en/post/emacs_workflow_dynamically_adding_files_to_org-agenda-files/) so that every file gets added to your `org-agenda-files` automatically. Then you can use org-agenda views to browse through all your posts (see [below](#bonus-1-quickly-see-all-your-posts-with-custom-agenda-views-or-consult-agenda) for more on that).

Currently, I am using [Wowchemy](https://wowchemy.com/templates/)'s [Academic](https://academic-demo.netlify.app/) template for my website, so in my ox-hugo configuration, I set everything up according to the structure defined by that template. I use one org file for all my blog posts, and since I have a multilingual site, I put this file in my hugo website's folders under content alongside separate folders for each language. I then set up my ox-hugo to save each post in the appropriate folder by setting `:EXPORT_HUGO_SECTION_FRAG:` property. Because the properties are inherited, I can then use the same property in the subheadings of each post to point ox-hugo to save the post in the appropriate folder. For example for English posts I have something like the following in my file. As you can see I also add `YAML` source blocks for Wowchemy's settings following the documentation here: [Hugo Documentation for Wowchemy](https://wowchemy.com/docs/).

```org
* en
:PROPERTIES:
:EXPORT_HUGO_SECTION_FRAG: en
:END:
** posts
:PROPERTIES:
:EXPORT_HUGO_SECTION_FRAG: post
:END:

## Here I enter new posts##

* DRAFT Title of the post

:PROPERTIES:
:EXPORT_HUGO_DRAFT: true
:EXPORT_HUGO_SECTION_FRAG: Title_of_the_post
:EXPORT_FILE_NAME: index
:TITLE: Title of the post
:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :subtitle subtitle goes here
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :summary summary goes here
:END:

#+begin_src yaml :front_matter_extra t
authors:
  - admin
projects: [project]
categories: [tags]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
#+end_src
```

I have a separate similar heading tree for other languages. With this setup I can use org-export with ox-hugo to automatically make the markdown files for each post.
Also, since my website is in a git repository, I keep a symlink copy of that file somewhere more convenient in my home folder such as `"~/blog/"` so I can quickly find my posts without navigating through the folder structure of my website.

Other than that there are some site-specific settings that I define in `:PROPERTIES:` drawers following Ox-Hugo's [manual](https://ox-hugo.scripter.co/). I also set up [auto export on saving.](https://ox-hugo.scripter.co/doc/auto-export-on-saving/)


## Automating the Workflow {#automating-the-workflow}

While the setup above is already great for writing posts in org-mode and quickly exporting them to markdown, we can do much more with org-mode `TODO` items. For example, we can create some automation flow by hooking functions to `org-after-todo-state-change-hook`, so your draft turns into a post on your hugo website automatically when you change the keyword. As a result, you can just focus on writing the contents rather than maintaining the blog files. This is what makes this workflow superior to a lot of other tools, because it brings the automation (that tools like WordPress offer) to a great text-editing environment like org-mode all within Emacs where I personally do a lot of other things (like coding, reading feeds, keeping notes, managing projects and ideas, ...).

**Here is my automated workflow (You can see a screenshot at the end of this post):**

I capture new ideas with org-capture (The next section, [Using Org Capture For Blog Ideas](#using-org-capture-for-blog-ideas), will cover how to set this up) under the **DRAFT** keyword and quickly add some rough ideas of what I want to add later. At this point, I rather not have it converted to markdown yet because maybe I end up deciding the topic is not right, and I don't even want to work on it until some other time. Later, when I find time to actually sit down and write up something, I go to my org-agenda list and find the blog post I want to work on and add some more contents and ideas and organize the post. I keep doing this over the next few days or weeks depending on availability until I get to a point when I feel that the content is indeed suitable for a blog post. At this point, I bump the heading to **POST** keyword, which automatically creates the markdown files. I am still not ready to publish this post because I probably don't have figures, and there are still minor details that need fixing. Therefore, in the markdown file, this is still marked as draft (determined by `:EXPORT_HUGO_DRAFT: true` in the properties' drawer), which allows me to see the post on my local web server (e.g. by running `hugo server -D` in the terminal) and decide how to polish and finalize it. Finally, when I'm done with editing the post and want to publish it on my website, I bump the heading up to **PUBLISH** and it automatically flags the post for publishing.

**Here is the code that enables the workflow:**

I have defined a function that changes `:EXPORT_HUGO_DRAFT:` property and also runs `(org-hugo-export-wim-to-md)` for me when I change the keyword.

```emacs-lisp
(defun ad/org-change-draft-when-blog-state-changes ()
  (interactive)
  (pcase (org-get-todo-state)
    ("PUBLISH" (org-set-property "EXPORT_HUGO_DRAFT" "false")
     (org-hugo-export-wim-to-md))
    ("DRAFT" (org-set-property "EXPORT_HUGO_DRAFT" "true"))
    ("POST" (org-set-property "EXPORT_HUGO_DRAFT" "true")
            (org-hugo-export-wim-to-md))
    (_ ())
    )
  )

(add-hook 'org-after-todo-state-change-hook
'my/org-change-draft-when-state-changes-to-publish)
```

In the code above I am adding a function, `ad/org-change-draft-when-state-changes-to-publis` as a hook to run whenever a `TODO` keyword changes. This function runs `(org-hugo-export-wim-to-md)` to create the markdown files and also sets the `:EXPORT_HUGO_DRAFT:` to `true` or `false` depending on the status. Note that I have hooked the custom function to `'org-after-todo-state-change-hook` which runs on any `TODO` changes but since I am checking the todo state by `(org-get-todo-state)` this hook will only do something when the heading is one of **DRAFT**, **POST** or **PUBLISH**.

There are two more settings for my ideal setup to work as intended:

```emacs-lisp
(setq org-log-done 'time
      org-log-into-drawer t)

(setq-default org-export-with-todo-keywords nil)
```

The code above makes sure that whenever a todo item is set as done (defined by putting the specific `TODO` keyword after `"|"` in the `'org-todo-keywords` list as mentioned [above](#using-org-todo-keywords)). This is useful because ox-hugo uses this log time drawer to add the _"last updated timestamp"_ to markdown files.
Also, I make sure that `org-export-with-todo-keywords` is set to `nil`.

With this setup, I don't need to worry about anything but writing the content. As I change the `TODO` keyword, everything happens automatically in the background. In addition, I have everything set up with [Netlify](https://www.netlify.com/?utm_medium=paid_search&utm_source=google&utm_campaign=12755510784&utm_term=netlify) and [GitHub](https://github.com/), to automatically update my website when I push new changes to my git repository. As a result, the blogging workflow is as simple as capturing an idea and editing it in org-mode whenever I have time. Everything else happens magically in the background without me having to think or worry about it. Here is a screenshot showing how the new post gets added to the website when I change the keyword from **DRAFT** to **POST**.

{{< figure src="/ox-hugo/automatic_posting.gif" width="800px" height="nilpx" >}}


## Using Org-Capture For Blog Ideas {#using-org-capture-for-blog-ideas}

The next useful step to take is to set up org-capture to quickly save new ideas. Often times blog ideas come to you randomly in the middle of something else, and you don't want to lose them. The org-mode ability to quickly fire up org-capture and write the idea down is therefore a crucial functionality that makes this workflow superior to a lot of other editors. To do this, I use the org-capture with a custom template. You can do this in different ways from minimal setup to more advanced ones, depending on your needs, as I show below.


### Minimal Setup {#minimal-setup}

You can add a simple template to your org-capture templates following the official instructions: [Capture templates (The Org Manual)](https://orgmode.org/manual/Capture-templates.html). For example, the code below creates a new heading in the `"Blog.org"` file in default org directory under the Heading `"* Posts"`.

```emacs-lisp
(add-to-list 'org-capture-templates
      `("b" "Blog Post" entry
         (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
         "* DRAFT %i %?\n"))
```


### Better Set Up that Adds Ox-Hugo {#better-set-up-that-adds-ox-hugo}

The simple template above is sufficient for most basic workflows, but I personally like to use something a bit more advanced with automation and add capabilities for ox-hugo. Here is a better template which adds ox-hugo settings:

```emacs-lisp
 (add-to-list 'org-capture-templates
      `(("b" "Blog Post" entry
         (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
         "* DRAFT %i %?\n:PROPERTIES:\n :EXPORT_HUGO_DRAFT: true\n :EXPORT_HUGO_SECTION_FRAG: \n :EXPORT_FILE_NAME: index\n :TITLE: \n :EXPORT_HUGO_CUSTOM_FRONT_MATTER: :subtitle \n :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :summary \n:END:\n\n#+begin_src yaml :front_matter_extra t\nauthors:\n  - admin\nprojects: []\nfeatured: false\ncommentable: true\nimage:\n  caption: 'caption'\n  focal_point: 'center'\n#+end_src\n"
         :empty-lines 1
         :prepend t
         :jump-to-captured t)
```

The code above adds the ox-hugo settings as `:PROPERTIES:` drawer to your entry and also adds some settings for convenience by `:prepend t` and `:jump-to-captured t`. It jumps to the file narrowed to the heading you just created in case you want to add some more text before finishing the capture process.


### Advanced Setup with Ox-Hugo + Yasnippet {#advanced-setup-with-ox-hugo-plus-yasnippet}

The template above covers provides all the essentials for an automated workflow, but it is still not the ideal version because but it is not dynamic enough, and some manual editing is still required. For example, I do not want to manually enter the file path for the markdown files. Therefore, in my own setup I take a different approach by using [Yasnippet](https://github.com/joaotavora/yasnippet) to do some auto-completion for me. Of course, this means that you need to install Yasnippet and set it up, which may not be what you want and hence the settings above, but in my case I am using Yasnippet for other things anyway, so I might as well use it here.

I define a capture function that calls org-capture but also enables yasnippet and inserts my snippet as a template. This is essentially a from with metadata fields that I can quickly jump to and fill by using the tab button.

```emacs-lisp
(defun ad/blog-capture-new (&optional text)
(interactive "sTitle: ")
  (let* ((org-capture-templates `(("d" "default" entry
                                   (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
                                   "* DRAFT %?"
                                   :empty-lines 1
                                   :prepend t
                                   :jump-to-captured t)
                                  ))
         (yas-wrap-around-region t))
    (org-capture nil "d")
    (let ((draft (and (yas-reload-all) (yas-lookup-snippet "hugo draft" 'org-mode t))))
        (yas-minor-mode-on)
        (yas-expand-snippet draft)
        (insert (format "%s" text)))
      ))
```

In the code above I use let-bindings to temporarily define a rather simple `org-capture-template` and call `(org-capture nil "d")` to get the org-capture buffer. Then I achieve the rest of what I need by loading and expanding a yasnippet snippet that contains the following:

```emacs-lisp
# -*- mode: snippet -*-
# name: blog post template
# key:
# --
${1:title}
:PROPERTIES:
 :EXPORT_HUGO_DRAFT: true
 :EXPORT_HUGO_SECTION_FRAG: ${1:$(string-replace "\s" "_" yas-text)}
 :EXPORT_FILE_NAME: index
 :TITLE: $1
 :EXPORT_HUGO_CUSTOM_FRONT_MATTER: :subtitle $2
 :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :summary $3
:END:

#+begin_src yaml :front_matter_extra t
authors:
  - admin
projects: [$4]
categories: [$5]
featured: false
commentable: true
image:
  caption: $6
  focal_point: 'center'
#+end_src
$0
```

Note how the snippet takes the title and automatically generates the path for the markdown files using `:EXPORT_HUGO_SECTION_FRAG:`. In the screenshot below, you can see my capture process:

{{< figure src="/ox-hugo/org-capture.gif" width="800px" height="nilpx" >}}


## Bonus 1: Quickly see all Your Posts with Custom-Agenda-Views or Consult-Agenda {#bonus-1-quickly-see-all-your-posts-with-custom-agenda-views-or-consult-agenda}

An advantage of using org-todo keywords for items such as blog posts is that you can then quickly find all your items using [Agenda Views](https://orgmode.org/manual/Agenda-Views.html).

```emacs-lisp
(add-to-list 'org-agenda-custom-commands
     '("b" "Blog"
        ((agenda)
         (todo "DRAFT\\|POST\\|PUBLISH"
               ((org-agenda-overriding-header "Blog Posts: "))))))
```

Alternatively, you can use the awesome [Consult Package](https://github.com/minad/consult) by Alexander Miller and use `consult-org-agenda` to quickly search through your blog posts in the minibuffer. Here is some example code you can use after you install consult:

```emacs-lisp
(consult-org-agenda "TODO={DRAFT\\|POST\\|PUBLISH}")
```

Personally, I have built some custom functions based on consult-agenda just for my blog posts that lists all my old posts with status and date, etc. and allows me to add new posts by entering a new title as well. I don't get into all that extra stuff in this post, but it is simply a custom setup for completing-read based on [Consult](https://github.com/minad/consult). It allows me to see my previous posts or start new ones by entering a new title. Here is a screenshot:

{{< figure src="/ox-hugo/consult-blog.gif" width="800px" height="nilpx" >}}


## Bonus 2: Better looks for org-todo keywords {#bonus-2-better-looks-for-org-todo-keywords}

If you want to have some better looks for your org todo keywords, you can check the [Org-Modern](https://github.com/minad/org-modern) by Alexander Miller. If you want to have different faces or colors for different labels, you can set `org-todo-keyword-faces` per official instruction: [Faces for TODO keywords (The Org Manual)](https://orgmode.org/manual/Faces-for-TODO-keywords.html). Here is an example:

```emacs-lisp
(add-to-list 'org-todo-keyword-faces
      '("DRAFT" . '((t :foreground "pink"))))
```


## Bonus 3: Better Writing Environment inside Emacs {#bonus-3-better-writing-environment-inside-emacs}

Don't forget to check out other packages that improve the actual content creation and writing texts in Emacs. [thinkhuman/writingwithemacs: Tips, Examples, and Resources for Writing with Emacs](https://github.com/thinkhuman/writingwithemacs) is a good collection of articles. Different people have obviously different opinions and take different approaches. [explog's wirting setup](https://explog.in/notes/writingsetup.html) is probably a good starting point for any user. Jacob Moena's post on [Creative Writing with Emacs](https://jacmoes.wordpress.com/2019/09/24/creative-writing-with-emacs/) provides a more comprehensive intro including some useful extra packages [here](https://jacmoes.wordpress.com/2019/09/24/creative-writing-with-emacs/#Extras).

Personally, I think you have to try different packages and find the settings that is appealing to you and your use-case. But here are some suggestions based on my experience.

1.  Toggle frame to full screen and hide all other Windows
2.  Hide the `properties drawers` that are used for ox-hugo settings, by using `org-fold-hide-drawer-all`.
3.  Use [flyspell-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html) for spell checking.

Put all of that in an interactive command, and can call it whenever you want to focus on writing:

```emacs-lisp
(defun ad/blog-focus-writing ()
(interactive)
(toggle-frame-fullscreen)
(delete-other-windows nil)
(org-fold-hide-drawer-all)
(flyspell-mode 1)
)
```

You can also add more settings to this function depending on your preferences. Here are some packages I suggest you try:

1.  Consider [flyspell-correct](https://github.com/d12frosted/flyspell-correct) and [consult-flyspell](https://gitlab.com/OlMon/consult-flyspell) for better, more user-friendly interactive commands with flyspell.
2.  Consider [Emacs-langtool](https://github.com/mhayashi1120/Emacs-langtool) for checking grammars etc.
3.  Consider [Focus](https://github.com/larstvei/Focus) mode to get an effect similar to what WordPress calls spotlight.
4.  Consider [zoom](https://github.com/cyrus-and/zoom) if you open multiple buffers and want the width to adjust dynamically
5.  Try out other packages such as [writegood-mode](https://github.com/bnbeckwith/writegood-mode) or [artbollocks-mode](https://github.com/sachac/artbollocks-mode).

Here is a screenshot of my setup with flyspell, olivetti and focus-mode.

{{< figure src="/ox-hugo/focus.gif" width="800px" height="nilpx" >}}


## Bonus 4: Seeing Previews with `Hugo Server` {#bonus-4-seeing-previews-with-hugo-server}

Obviously no good blog editor would be complete without a way to preview the post. If you use Hugo, you can see a preview by running `hugo server` in the terminal which fires up a local server (server default address is <http://localhost:1313/>). You can achieve the same by using a package like [emacs-easy-hugo](https://github.com/masasam/emacs-easy-hugo) from inside Emacs. However, that might be too overkill given that we can already do most of the functionality with org-mode and ox-hugo. Personally, I have defined a function that calls `hugo server` as a process within Emacs and a second function to kill the process.

```emacs-lisp
(defun ad/blog-start-hugo-server ()
  (interactive)
  (let ((default-directory "/path/to/your/blog-hugo-server/directory") ;;change the path to your website
        (buffer (get-buffer-create "*blog-hugo-server*")))
    (apply 'start-process "hugo-server" buffer "hugo" "server" '("--buildDrafts" "--navigateToChanged"))
    )
  )

(defun ad/blog-kill-hugo-server ()
  (interactive)
  (when-let ((proc (get-buffer-process (get-buffer "*blog-hugo-server*"))))
    (delete-process proc))
  )
```

Note that the arguments `-buildDrafts` and `--navigateToChanged` are passed to `hugo server` to make sure you see your drafts and also automatically navigate to the most recent changes.
If you also have xwidget-webkit in your Emacs, then you may want to create another interactive command to open the page in a second buffer like below. If you don't have xwidget, you can use any other browsers for example by invoking `browse-url` to open the link in an external browser.

```emacs-lisp
(defun ad/blog-preview ()
(interactive)
(unless (get-buffer-process (get-buffer "*blog-hugo-server*"))
  (ad/blog-start-hugo-server))
(let* ((post-url (downcase (org-hugo--entry-get-concat (point-marker) "EXPORT_HUGO_SECTION_FRAG" "/"))))
(delete-other-windows nil)
(split-window-right nil)
(other-window 1)
(xwidget-webkit-browse-url (concat "http://localhost:1313/" post-url))
))
```

This opens the local hugo server and navigates to the page for the post in the current buffer. Since we have `--navigateToChanged` turned on, as we edit the text we can see the changes right away. Here is a screenshot:

{{< figure src="/ox-hugo/preview.gif" width="800px" height="nilpx" >}}


## Putting everything together: Code and Screenshot {#putting-everything-together-code-and-screenshot}

Here is a screenshot that shows the complete workflow:

{{< figure src="/ox-hugo/full_workflow.gif" width="800px" height="nilpx" >}}

and here is my preferred entire code (see the post for some alternatives if you don't have/want all the required extra packages below):

```emacs-lisp

;; org-todo keywords
(add-to-list 'org-todo-keywords '(sequence "DRAFT(d)" "POST(p)" "|" "PUBLISH(b)"))

;; org-todo faces
(add-to-list 'org-todo-keyword-faces
      '("DRAFT" . '((t :foreground "pink"))))


;; function(s) to run on changing state
(defun ad/org-change-draft-when-blog-state-changes ()
  (interactive)
  (pcase (org-get-todo-state)
    ("PUBLISH" (org-set-property "EXPORT_HUGO_DRAFT" "false")
     (org-hugo-export-wim-to-md))
    ("DRAFT" (org-set-property "EXPORT_HUGO_DRAFT" "true"))
    ("POST" (org-set-property "EXPORT_HUGO_DRAFT" "true")
            (org-hugo-export-wim-to-md))
    (_ ())
    )
  )

(add-hook 'org-after-todo-state-change-hook
'my/org-change-draft-when-state-changes-to-publish)


(setq org-log-done 'time
      org-log-into-drawer t)

;; make sure org-todo keywords don't get exported
(setq-default org-export-with-todo-keywords nil)

;; capture function for blog posts
(defun ad/blog-capture-new (&optional text)
(interactive "sTitle: ")
  (let* ((org-capture-templates `(("d" "default" entry
                                   (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
                                   "* DRAFT %?"
                                   :empty-lines 1
                                   :prepend t
                                   :jump-to-captured t)
                                  ))
         (yas-wrap-around-region t))
    (org-capture nil "d")
    (let ((draft (and (yas-reload-all) (yas-lookup-snippet "hugo draft" 'org-mode t))))
        (yas-minor-mode-on)
        (yas-expand-snippet draft)
        (insert (format "%s" text)))
      ))

;; custom org-agenda view
(add-to-list 'org-agenda-custom-commands
     '("b" "Blog"
        ((agenda)
         (todo "DRAFT\\|POST\\|PUBLISH"
               ((org-agenda-overriding-header "Blog Posts: "))))))

;; enable extra packages for focusing on writing
(defun ad/blog-focus-writing ()
(interactive)
(toggle-frame-fullscreen)
(delete-other-windows nil)
(org-fold-hide-drawer-all)
(flyspell-mode 1)
)

;; start the hugo server with Drafts and navigateToChanged
(defun ad/blog-start-hugo-server ()
  (interactive)
  (let ((default-directory "/path/to/your/blog-hugo-server/directory") ;;change the path to your website
        (buffer (get-buffer-create "*blog-hugo-server*")))
    (apply 'start-process "hugo-server" buffer "hugo" "server" '("--buildDrafts" "--navigateToChanged"))
    )
  )

;; stop the hugo server process
(defun ad/blog-kill-hugo-server ()
  (interactive)
  (when-let ((proc (get-buffer-process (get-buffer "*blog-hugo-server*"))))
    (delete-process proc))
  )

;; get a preview of new drafts
(defun ad/blog-preview ()
(interactive)
(unless (get-buffer-process (get-buffer "*blog-hugo-server*"))
  (ad/blog-start-hugo-server))
(let* ((post-url (downcase (org-hugo--entry-get-concat (point-marker) "EXPORT_HUGO_SECTION_FRAG" "/"))))
(delete-other-windows nil)
(split-window-right nil)
(other-window 1)
(xwidget-webkit-browse-url (concat "http://localhost:1313/" post-url))
))
```
