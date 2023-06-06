---
title: "How to Efficiently Write Blogs in Emacs Org-Mode in 2023"
author: ["Armin Darvish"]
lastmod: 2023-06-06T14:55:36-07:00
categories: ["software", "emacs", "orgmode", "blogging"]
draft: true
weight: 3007
subtitle: "How to use org-mode and ox-hugo along with org-todo items and org-agenda to manage your blog posts"
summary: "In this post I show you my custom workflow in Emacs for writing blogposts. I use org-capture for capturing initial ideas, then I use org-todo and org-agenda to track the post as I take it from idea to an initial draft and to publishing it on my website. I wtie my posts in org-mode and use ox-hugo to turn them into a post.In this post I show you how I use"
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

In the previous post, I showed you how to automatically add files to your org-agenda. In this post I am going to show you how you can then use org-todo tasks beyond just tasks and todo-items and use it to build custom work flows that takes an idea through different stages. As an example I am going to show you my blogging workflow which I am using right now to use this posts.


## Using `org-todo` keywords {#using-org-todo-keywords}

By default [Emacs Org mode](https://orgmode.org/) comes with two `org-todo` keywords `"TODO"` and `"DONE"`, but you can add any custom keyword and use them to track different objects and category of items such as notes, flashcards, ideas, drafts, ... You can do this by adding the labels to `org-todo-keywords` in a sequence following [Workflow states (The Org Manual)](https://orgmode.org/manual/Workflow-states.html). For example for a blogging workflow I use `(sequence "DRAFT(d)" "POST(p)" "PUBLISH(b)")` as shown below. Note that by putting **PUBLISH** after "|", we are setting **PUBLISH** as the done label for this workflow, which can be used for other useful functions such as logging time, and etc. see the section on automating workflow [below](#automating-the-workflow) for how I use it to automate logging times.

```emacs-lisp
(add-to-list 'org-todo-keywords '(sequence "DRAFT(d)" "POST(p)" "|" "PUBLISH(b)"))
```

Now, we can label org headings as a blog post by adding **DRAFT** todo keyword.

```emacs-lisp
(add-to-list 'org-capture-templates
      `("b" "Blog Post" entry
         (file+olp ,(file-truename (expand-file-name "BlogPosts.org" org-directory)) "Blog Posts")
         "* DRAFT %i %?\n:PROPERTIES:\n :EXPORT_HUGO_DRAFT: true\n :EXPORT_HUGO_SECTION_FRAG: ${title}\n :EXPORT_FILE_NAME: index\n :TITLE: ${title}\n :EXPORT_HUGO_CUSTOM_FRONT_MATTER: :subtitle \n :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :summary \n:END:\n\n#+begin_src yaml :front_matter_extra t\nauthors:\n  - admin\nprojects: []\nfeatured: false\ncommentable: true\nimage:\n  caption: 'caption'\n  focal_point: 'center'\n#+end_src\n"
         :empty-lines 1
         :prepend t
         :jump-to-captured t)
```


## Ox-Hugo {#ox-hugo}

If you use [Hugo](https://gohugo.io/) to build your blog, then [Ox-Hugo](https://github.com/jethrokuan/ox-hugo) is a great package to use. It allows you to write your blog posts in Org-Mode and export them to the Hugo-compatible markdown. You can organize your  blog posts either in separate files (one post per file) or all in one file under different org headings (one post per heading). I personally am using one post per heading right now because I like to see all my posts in the same file next to each other. But if you prefer having the blog posts in different files, then using my previous post [to dynamically add files to org-agendas](https://www.armindarvish.com/en/post/emacs_workflow_dynamically_adding_files_to_org-agenda-files/) will be very useful because you can just label the heading with **DRAFT** todo-keyword and it will automatically get added to your `org-agenda-files`. Then you can use org-agenda views to browse through all your posts (see below for more on that).

I recommend you go through Ox-Hugo's [manual](https://ox-hugo.scripter.co/) for setting up Ox-Hugo since it can be different depending on your specific settings. But here are some ideas for consideration:

1.  Since I am using [Wowchemy](https://wowchemy.com/templates/)'s [Academic](https://academic-demo.netlify.app/) template for my website, I set everything up according to the structure defined by that template. I use one org file for all my blog posts, and since I have a multilingual site, I put this file in my hugo website's folders under content along side separate folders for each language. I then set up my ox-hugo to save each post in the appropriate folder by setting `:EXPORT_HUGO_SECTION_FRAAG:` property. Because the properties are inherited, I can then use the same property in the subheadings of each post to point Ox-Hugo to save the post in the appropriate folder. For example for English posts I have something like the following in my file. I also add `yaml` source blocks for Wowchemy's settings following the documentation here: [Hugo Documentation for Wowchemy](https://wowchemy.com/docs/).

<!--listend-->

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

* DRAFT Building An Efficient Blogging Workflow in Emacs

:PROPERTIES:
:EXPORT_HUGO_DRAFT: true
:EXPORT_HUGO_SECTION_FRAG: Building_An_Efficient_Blogging_Workflow_in_Emacs
:EXPORT_FILE_NAME: index
:TITLE: Building An Efficient Blogging Workflow in Emacs
:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :subtitle How to use org-mode and ox-hugo along with org-todo items and org-agenda to manage your blog posts
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :summary In this post I show you my custom workflow in Emacs for writing blogposts. I use org-capture for capturing initial ideas, then I use org-todo and org-agenda to track the post as I take it from idea to an initial draft and to publishing it on my website. I wtie my posts in org-mode and use ox-hugo to turn them into a post.In this post I show you how I use
:END:

#+begin_src yaml :front_matter_extra t
authors:
  - admin
projects: [software, emacs]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
#+end_src
```

I have a separate similar heading tree for other languages. With this setup I can use org-export with ox-hugo to automatically make the markdown files for each post.
Also, since my website is in a git repository, I keep a symlink copy of that file somewhere more convenient in my home folder such as `"~/blog/"` so i can quickly find my posts without navigating through the folder structure of my website.

Other than that there are some site-specific settings that I define in org-mode `:PROPERTIES:` drawers following Ox-Hugo's [manual](https://ox-hugo.scripter.co/).


## Automating the Workflow {#automating-the-workflow}

While the setup above is great for writing blogposts in org-mode and quickly exporting them to markdown. We can do more with org-mode `TODO` items to further automate the workflow so we can just focus on writing the posts rather than maintaining the blog posts. For example, I have my workflow setup that whenever I start a new posts it goes under **DRAFT** keyword. Then after I add some content and I want to view the draft on the website (without publishing it yet), I bump the heading to **POST** keyword and finally when I'm done with editing the post and want to publish it on my website I bump the heading up to `POST`. Then  I have defined a custom function that changes `:EXPORT_HUGO_DRAFT:` property and also runs `(org-hugo-export-wim-to-md)` for me.

```emacs-lisp
(defun ad/org-change-draft-when-state-changes-to-publish ()
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

Here is the explanation. When I start a **DRAFT** I rather not have it converted to markdown because may be I end up deciding the topic is not right and I don't even want to work on it anymore. But if I do work on the topic and get it to a point where I want to put it on my website, I probably would want to view it on my website as before publishing it publicly so I bump the keyword to **POST** which automatically runs automatically runs `(org-hugo-export-wim-to-md)` to create the markdown files while making sure that `:EXPORT_HUGO_DRAFT:` is still `true` so the post does not get published yet. This way I can view the post as a draft by running `"hugo server -D --navigateToChanged"` in terminal and decide if it is ready for publishing. Finally when I am done with writing and editing my post I change the keyword to **PUBLISH**, which sets `:EXPORT_HUGO_DRAFT:` to `false` and reruns `(org-hugo-export-wim-to-md)` to update the markdown files.

Note that I have hooked the custom function `ad/org-change-draft-when-state-changes-to-publis` to `'org-after-todo-state-change-hook` but since I am checking the todo state by `(org-get-todo-state)` this hook will only do things when the heading is one of **DRAFT**, **POST** or **PUBLISH**.

There are two more important settings for this to work.

```emacs-lisp
(setq org-log-done 'time
      org-log-into-drawer t)

(setq-default org-export-with-todo-keywords nil)
```

The code above makes sure that whenever a todo item is set done (defined by putting the specific `TODO` keyword after `"|"` in the `'org-todo-keywords` list as mentioned [above](#using-org-todo-keywords)). This is useful because Ox-Hugo uses this log time drawer to add the date of the post to markdown files, so whenever I change the `TODO` keyword to **PUBLISH**, the date of the post gets updated automatically.
Also I make sure that `org-export-with-todo-keywords` is set to `nil` so my todo labels do not show up on my blog posts.

With this setup, I don't need to worry about anything but writing the post. As I change the `TODO` keyword. I have everything setup with [Netlify](https://www.netlify.com/?utm_medium=paid_search&utm_source=google&utm_campaign=12755510784&utm_term=netlify) and [GitHub](https://github.com/), so whenever I push the changes to my git repository everything gets automatically uploaded to my website. As a result the blogging workflow is as simple of capturing an idea and editing it in org-mode. Everything else happens magically in the background without me having to think or worry about it.


## Using Org Capture For Blog Ideas {#using-org-capture-for-blog-ideas}

The next useful step to do is to setup org-capture to quickly save new ideas. Often times blog ideas come to you randomly in the middle of something else and you don';t want to lose them so you can quickly fire org-capture and write the idea down before going back to whatever you were doing. To do this we use the awesome org-capture-templates. You can add a simple template to your org-capture templates following the official instructions: [Capture templates (The Org Manual)](https://orgmode.org/manual/Capture-templates.html)  For example the code below creates a new heading in the `"Blog.org"` file in default org directory under the Heading `"* Posts"`.


### Minimal Setup {#minimal-setup}

```emacs-lisp
(add-to-list 'org-capture-templates
      `("b" "Blog Post" entry
         (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
         "* DRAFT %i %?\n"))
```

Note that is you don't have `org-capture-templates` set up, you may want to use `setq` instead of add-to-list

```emacs-lisp
(setq org-capture-templates
      `(("b" "Blog Post" entry
         (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
         "* DRAFT %i %?\n")))
```

While this simple template is good enough for most basic workflows, I personally like to use something a bit more advanced and add capabilities for ox-hugo (see more details further below). So, in my setup I use something like this


### Better Set Up that Adds Ox-Hugo {#better-set-up-that-adds-ox-hugo}

```emacs-lisp
 (add-to-list 'org-capture-templates
      `(("b" "Blog Post" entry
         (file+olp ,(file-truename (expand-file-name "Blog.org" org-directory)) "Posts")
         "* DRAFT %i %?\n:PROPERTIES:\n :EXPORT_HUGO_DRAFT: true\n :EXPORT_HUGO_SECTION_FRAG: \n :EXPORT_FILE_NAME: index\n :TITLE: \n :EXPORT_HUGO_CUSTOM_FRONT_MATTER: :subtitle \n :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :summary \n:END:\n\n#+begin_src yaml :front_matter_extra t\nauthors:\n  - admin\nprojects: []\nfeatured: false\ncommentable: true\nimage:\n  caption: 'caption'\n  focal_point: 'center'\n#+end_src\n"
         :empty-lines 1
         :prepend t
         :jump-to-captured t)
```

The code above adds some template settings for ox-hugo to be able to export the post later and also by doing `:prepend t` I makes sure that the latest blog posts are on top of the file and by `:jump-to-captured t` I make sure that after I am done with the capture buffer I jump to the file and narrowed to that heading. This is useful for adding more details when you initially capture the idea.


### Advanced Setup with Ox-Hugo + Yasnippet {#advanced-setup-with-ox-hugo-plus-yasnippet}

While the capture template above works great, it is still not dynamic enough, for example, I do not want to repeat entering the title of the post in my ox-hugo settings. Therefore in my own setup I take a different approach by using Yasnippet to do some auto completion for me. I define a capture function that calls org-capture but also enables yasnippet and insert my template:

```emacs-lisp
(defun ad/blog-capture-new ()
(lambda (text)
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
      )))
```

In the code above I use let-bindings to temporary define a rather simple `org-capture-template` and call `(org-capture nil "d")` to get the org-capture buffer. Then I achieve the rest of what I need using loading and expanding a yasnippet snippet that contains the following:

```emacs-lisp
# -*- mode: snippet -*-
# name: hugo draft
# key: hd
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
featured: false
commentable: true
image:
  caption: $5
  focal_point: 'center'
#+end_src
$0
```

The snippet takes the title and automatically generates the folder name to be used with `:EXPORT_HUGO_SECTION_FRAG:` making sure that space characters are replaces with "_".
With this setup, whenever I have an idea for a blog I can just run `ad/blog-capture-new` which I have bound to some key bindings and write everything down and save it for later.


## Bonus 1: Better looks for org-todo keywords {#bonus-1-better-looks-for-org-todo-keywords}

If you want to have some better looks for your org todo keywords, you can check the [Org-Modern](https://github.com/minad/org-modern) by Alexander Miller. You can also change the face of the org-todo keywords by setting _'org-todo-keyword-face_.

```emacs-lisp
(add-to-list 'org-todo-keyword-faces
      '("DRAFT" . '((t :foreground "pink"))))
```


## Bonus 2: Quickly see all Your Posts with Custom-Agenda-Views or Consult-Agenda {#bonus-2-quickly-see-all-your-posts-with-custom-agenda-views-or-consult-agenda}

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

Personally, I have built some custom functions based on consult-agenda just for my blog posts that allows me to add new posts by entering a new title and takes me to the org-capture buffer shown above. I have this bound to some key bindings. So whenever I have an idea or want to edit my posts, I just hit this key binding and I can quickly jump to editing an old post or create a new one from scratch. I don't get into all that extra stuff in this post, but here is a screen shot of everything in my final setup:
